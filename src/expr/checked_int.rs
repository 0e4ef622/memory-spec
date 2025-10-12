use std::ops::{Add, Mul};

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Checked<T>(pub Option<T>);

impl Add<i64> for Checked<i64> {
    type Output = Self;
    fn add(self, rhs: i64) -> Self {
        match self.0 {
            None => self,
            Some(n) => Self(n.checked_add(rhs)),
        }
    }
}

impl Add<Checked<i64>> for i64 {
    type Output = Checked<i64>;
    fn add(self, rhs: Checked<i64>) -> Checked<i64> {
        match rhs.0 {
            None => rhs,
            Some(n) => Checked(n.checked_add(self)),
        }
    }
}

impl Add for Checked<i64> {
    type Output = Self;
    fn add(self, rhs: Checked<i64>) -> Self {
        match (self.0, rhs.0) {
            (None, _) | (_, None) => Self(None),
            (Some(l), Some(r)) => Self(l.checked_add(r)),
        }
    }
}

impl Mul<i64> for Checked<i64> {
    type Output = Self;
    fn mul(self, rhs: i64) -> Self {
        match self.0 {
            None => self,
            Some(n) => Self(n.checked_mul(rhs)),
        }
    }
}

impl Mul<Checked<i64>> for i64 {
    type Output = Checked<i64>;
    fn mul(self, rhs: Checked<i64>) -> Checked<i64> {
        match rhs.0 {
            None => rhs,
            Some(n) => Checked(n.checked_mul(self)),
        }
    }
}

impl Mul for Checked<i64> {
    type Output = Self;
    fn mul(self, rhs: Checked<i64>) -> Self {
        match (self.0, rhs.0) {
            (None, _) | (_, None) => Self(None),
            (Some(l), Some(r)) => Self(l.checked_mul(r)),
        }
    }
}
