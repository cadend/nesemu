use std::convert;
use std::fmt;

#[derive(Debug, Clone)]
pub struct IoError {
    kind: std::io::ErrorKind,
}

impl fmt::Display for IoError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "io error: {:?}", self.kind)
    }
}

impl convert::From<std::io::Error> for IoError {
    fn from(e: std::io::Error) -> Self {
        IoError { kind: e.kind() }
    }
}
