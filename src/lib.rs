use std::{collections::HashMap, fmt::{Display, Write as _}};
pub mod region;
mod expr;
use kdl::{KdlDocument, KdlError, KdlNode, KdlValue};
pub use region::Region;

use crate::expr::{EvalError, Namespace, Value};


#[derive(Clone, Debug)]
pub struct Regions {
    region: Region,
    subregions: HashMap<String, Regions>,
}

impl Regions {
    fn new(region: Region) -> Self {
        Self { region, subregions: Default::default() }
    }

    pub fn region(&self) -> &Region {
        &self.region
    }

    pub fn origin(&self) -> u64 {
        self.region.origin()
    }

    pub fn length(&self) -> u64 {
        self.region.length()
    }

    pub fn end(&self) -> u64 {
        self.region.end()
    }

    pub fn get(&self, key: &str) -> Option<&Regions> {
        self.subregions.get(key)
    }
}

impl std::ops::Index<&str> for Regions {
    type Output = Regions;

    fn index(&self, index: &str) -> &Self::Output {
        &self.subregions[index]
    }
}

#[derive(Clone, Default, Debug)]
pub struct MemorySpec {
    regions: HashMap<String, Regions>,
    symbols: HashMap<String, u64>,
}

impl MemorySpec {
    pub fn from_str(content: &str) -> Result<Self, Error> {
        let doc: KdlDocument = content.parse()?;
        let mut namespace = Namespace::default();
        let mut spec = Self::default();
        for node in doc.nodes() {
            match node.name().value() {
                "vars" => spec.handle_vars(node, &mut namespace)?,
                "regions" => spec.handle_regions(node, &mut namespace)?,
                "symbols" => spec.handle_symbols(node, &mut namespace)?,
                _ => (),
            }
        }
        Ok(spec)
    }

    pub fn regions(&self) -> &HashMap<String, Regions> {
        &self.regions
    }

    pub fn render_symbols(&self) -> String {
        let mut r = String::new();
        for (name, value) in &self.symbols {
            writeln!(&mut r, "{name} = 0x{value:08x};").unwrap();
        }
        r
    }

    fn handle_vars(&self, node: &KdlNode, namespace: &mut Namespace<'_>) -> Result<(), Error> {
        // TODO warn/reject params
        for child in node.children().ok_or_else(|| Error::InvalidNode("vars".into()))?.nodes() {
            let name = child.name().value();
            let path = || format!("vars.{}", name);
            let value = child.get(0).ok_or_else(|| Error::InvalidNode(path()))?;
            let value = eval_kdl_value(value, namespace, path)?;
            namespace.insert(name.into(), expr::Value::N(value));
        }
        Ok(())
    }

    // basically the same as handle_vars but add the entries to the symbols table too
    fn handle_symbols(&mut self, node: &KdlNode, namespace: &mut Namespace<'_>) -> Result<(), Error> {
        // TODO warn/reject params
        for child in node.children().ok_or_else(|| Error::InvalidNode("vars".into()))?.nodes() {
            let name = child.name().value();
            let path = || format!("symbols.{}", name);
            let value = child.get(0).ok_or_else(|| Error::InvalidNode(path()))?;
            let value = eval_kdl_value(value, namespace, path)?;
            namespace.insert(name.into(), expr::Value::N(value));
            let value = u64::try_from(value)
                .map_err(|_| Error::InvalidNode(path()))?;
            let prev = self.symbols.insert(name.into(), value);
            if prev.is_some() {
                return Err(Error::NameExists(path()));
            }
        }
        Ok(())
    }

    fn handle_regions(&mut self, node: &KdlNode, namespace: &mut Namespace<'_>) -> Result<(), Error> {
        let mut subregions = vec![];
        for child in node.children().ok_or_else(|| Error::InvalidNode("regions".into()))?.nodes() {
            let r = self.handle_region(child, namespace, None, &mut vec![])?;
            subregions.push((r, String::from(child.name().value())));
        }
        check_overlap(subregions, &[])?;
        Ok(())
    }

    fn handle_region(
        &mut self,
        node: &KdlNode,
        namespace: &mut Namespace<'_>,
        parent_region: Option<&Region>,
        path: &mut Vec<String>,
    ) -> Result<Region, Error> {
        let name = node.name().value();
        let path_str = || format!("regions.{}.{}", path.join("."), name);
        let origin = node.get("origin").map(|v| eval_kdl_value(v, namespace, path_str)).transpose()?;
        let length = node.get("length").map(|v| eval_kdl_value(v, namespace, path_str)).transpose()?;
        let end = node.get("end").map(|v| eval_kdl_value(v, namespace, path_str)).transpose()?;
        let align = node.get("align").map(|v| eval_kdl_value(v, namespace, path_str)).transpose()?;
        let origin = origin.map(u64::try_from).transpose().map_err(|_| Error::InvalidNode(path_str()))?;
        let length = length.map(u64::try_from).transpose().map_err(|_| Error::InvalidNode(path_str()))?;
        let end = end.map(u64::try_from).transpose().map_err(|_| Error::InvalidNode(path_str()))?;
        let align = align.map(u64::try_from).transpose().map_err(|_| Error::InvalidNode(path_str()))?;

        let region = Region::new(origin, length, end)
            .map_err(|_| Error::InvalidNode(path_str()))?;
        if let Some(align) = align {
            if region.origin() % align != 0 || region.end() % align != 0 {
                return Err(Error::AlignError(path_str()));
            }
        }
        if let Some(parent_region) = parent_region && !parent_region.contains(&region) {
            return Err(Error::SubregionError {
                outer: format!("regions.{}", path.join(".")),
                inner: name.into()
            });
        }

        path.push(name.into());
        self.add_region(region, path);
        add_value(namespace, &path, Value::Namespace(Namespace::default()))?;
        path.push("origin".into());
        add_value(namespace, &path, Value::N(i64::try_from(region.origin()).unwrap()))?;
        path.last_mut().unwrap().replace_range(.., "length");
        add_value(namespace, &path, Value::N(i64::try_from(region.length()).unwrap()))?;
        path.last_mut().unwrap().replace_range(.., "end");
        add_value(namespace, &path, Value::N(i64::try_from(region.end()).unwrap()))?;
        path.pop();

        let children = node.children().map(|d| d.nodes());
        if let Some(children) = children {
            let mut subregions = vec![];
            for child in children {
                let subregion = self.handle_region(child, namespace, Some(&region), path)?;
                let name = child.name().value();
                subregions.push((subregion, String::from(name)));
            }
            check_overlap(subregions, path)?;
        }
        path.pop();

        Ok(region)
    }

    fn add_region(&mut self, region: Region, path: &[String]) {
        let mut map = &mut self.regions;
        for name in &path[..path.len() - 1] {
            map = &mut map.get_mut(name).unwrap().subregions;
        }
        map.insert(path.last().unwrap().clone(), Regions::new(region));
    }
}

fn eval_kdl_value(value: &KdlValue, namespace: &Namespace<'_>, path: impl Fn() -> String) -> Result<i64, Error> {
    match value {
        KdlValue::Integer(n) => Ok(i64::try_from(*n).map_err(|_| Error::InvalidValue(path()))?),
        KdlValue::String(ex) => Ok(expr::eval(ex, namespace)?),
        _ => Err(Error::InvalidValue(path())),
    }
}

fn check_overlap(mut regions: Vec<(Region, String)>, path: &[String]) -> Result<(), Error> {
    let path_str = || if path.is_empty() {
        "regions".into()
    } else {
        format!("regions.{}", path.join("."))
    };
    regions.sort_unstable();
    for w in regions.windows(2) {
        let ((l, lname), (r, rname)) = (&w[0], &w[1]);
        if l.overlaps(r) {
            return Err(Error::OverlapError {
                parent_region: path_str(),
                region1: lname.clone(),
                region2: rname.clone(),
            });
        }
    }
    Ok(())
}

fn add_value<'a>(namespace: &mut Namespace<'a>, path: &[String], value: expr::Value<'a>) -> Result<(), Error> {
    match path {
        [k] => {
            let r = namespace.insert(k.clone(), value);
            if r.is_some() {
                Err(Error::NameExists(k.clone()))
            } else {
                Ok(())
            }
        }
        [k, tail @ ..] => {
            add_value(namespace.get_mut(k).unwrap().namespace_mut().unwrap(), tail, value)
        }
        [] => unreachable!(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    KdlError(KdlError),
    EvalError(EvalError),
    AlignError(String),
    InvalidNode(String),
    InvalidValue(String),
    NameExists(String),
    OverlapError {
        parent_region: String,
        region1: String,
        region2: String,
    },
    SubregionError {
        outer: String,
        inner: String,
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::KdlError(e) => e.fmt(f),
            Self::EvalError(e) => e.fmt(f),
            Self::AlignError(r) => write!(f, "{} is not aligned", r),
            Self::InvalidNode(n) => write!(f, "invalid node name {}", n),
            Self::InvalidValue(n) => write!(f, "invalid value in {}", n),
            Self::NameExists(n) => write!(f, "{} already exists", n),
            Self::OverlapError { parent_region, region1, region2 } =>
                write!(f, "{parent_region}: {region1} overlaps with {region2}"),
            Self::SubregionError { outer, inner } =>
                write!(f, "{} is not contained by {}", inner, outer),
        }
    }
}

impl From<KdlError> for Error {
    fn from(t: KdlError) -> Self {
        Self::KdlError(t)
    }
}

impl From<EvalError> for Error {
    fn from(t: EvalError) -> Self {
        Self::EvalError(t)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn regions_test() {
        let content = r#"regions {
            region1 origin=0 end=2
            region2 origin=2 end=3
        }"#;

        let r = MemorySpec::from_str(content).unwrap();
    }

    #[test]
    fn overlap_test() {
        let content = r#"regions {
            region1 origin=0 end=2
            region2 origin=1 end=3
        }"#;

        let _err = MemorySpec::from_str(content).unwrap_err();
    }

    #[test]
    fn subregion_test() {
        let content = r#"regions {
            region1 origin=0 end=2 {
                region2 origin=1 end=3
            }
        }"#;
        let _err = MemorySpec::from_str(content).unwrap_err();
    }
}
