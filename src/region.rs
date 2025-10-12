#[derive(Copy, Clone, PartialEq, Eq, Debug, PartialOrd, Ord)]
pub struct Region {
    origin: u64,
    end: u64,
}

impl Region {
    pub fn new(
        origin: Option<u64>,
        length: Option<u64>,
        end: Option<u64>,
    ) -> Result<Self, NewRegionError> {
        let (origin, end) = match (origin, length, end) {
            (Some(origin), Some(length), Some(end)) => (origin.checked_add(length) == Some(end))
                .then_some((origin, end))
                .ok_or_else(|| NewRegionError::inconsistent("origin + length != end")),
            (Some(origin), Some(length), None) => origin
                .checked_add(length)
                .map(|end| (origin, end))
                .ok_or_else(|| NewRegionError::overflow("overflow evaluating origin + length")),
            (Some(origin), None, Some(end)) => (origin <= end)
                .then_some((origin, end))
                .ok_or_else(|| NewRegionError::inconsistent("origin > end")),
            (None, Some(length), Some(end)) => end
                .checked_sub(length)
                .map(|origin| (origin, end))
                .ok_or_else(|| NewRegionError::overflow("overflow evaluating end - length")),
            (Some(_), None, None) => Err(NewRegionError::underspecified("missing length")),
            (None, Some(_), None) | (None, None, Some(_)) | (None, None, None) => {
                Err(NewRegionError::underspecified("missing origin"))
            }
        }?;
        Ok(Region { origin, end })
    }

    pub fn origin(&self) -> u64 {
        self.origin
    }

    pub fn length(&self) -> u64 {
        self.end - self.origin
    }

    pub fn end(&self) -> u64 {
        self.end
    }

    /// Whether `other` is fully contained by this region.
    pub fn contains(&self, other: &Region) -> bool {
        self.origin <= other.origin && self.end >= other.end
    }

    pub fn intersection(&self, other: &Region) -> Option<Region> {
        let origin = self.origin.max(other.origin);
        let end = self.end.min(other.end);
        Region::new(Some(origin), None, Some(end)).ok()
    }

    pub fn overlaps(&self, other: &Region) -> bool {
        let Some(overlap) = self.intersection(other) else {
            return false;
        };
        overlap.length() != 0
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct NewRegionError {
    message: &'static str,
    kind: NewRegionErrorKind,
}

impl NewRegionError {
    fn overflow(message: &'static str) -> Self {
        Self {
            message,
            kind: NewRegionErrorKind::Overflow,
        }
    }

    fn inconsistent(message: &'static str) -> Self {
        Self {
            message,
            kind: NewRegionErrorKind::Inconsistent,
        }
    }

    fn underspecified(message: &'static str) -> Self {
        Self {
            message,
            kind: NewRegionErrorKind::Underspecified,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NewRegionErrorKind {
    Overflow,
    Inconsistent,
    Underspecified,
}

impl std::fmt::Display for NewRegionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let region1: Region = Region::new(Some(1), Some(1), None).unwrap();
        let region2: Region = Region::new(Some(1), None, Some(2)).unwrap();
        let region3: Region = Region::new(None, Some(1), Some(2)).unwrap();
        let region4: Region = Region::new(Some(1), Some(1), Some(2)).unwrap();
        assert_eq!(region1, Region { origin: 1, end: 2 });
        assert_eq!(region1, region2);
        assert_eq!(region2, region3);
        assert_eq!(region3, region4);
    }

    #[test]
    fn it_errors() {
        let err1 = Region::new(Some(1), Some(1), Some(3)).unwrap_err();
        let err2 = Region::new(Some(0xFFFFFFFF_FFFFFFFF), Some(1), None).unwrap_err();
        let err3 = Region::new(Some(2), None, Some(1)).unwrap_err();
        let _err4 = Region::new(Some(1), None, None).unwrap_err();
        let _err5 = Region::new(None, None, Some(1)).unwrap_err();

        assert_eq!(err1.message, "origin + length != end");
        assert_eq!(err2.message, "overflow evaluating origin + length");
        assert_eq!(err3.message, "origin > end");
    }
}
