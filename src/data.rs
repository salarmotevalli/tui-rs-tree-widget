use std::fmt::Display;

#[derive(Clone, Debug)]
pub struct Data(pub String);

impl<T: Display> From<T> for Data {
    fn from(value: T) -> Self {
        Data(format!("{value}"))
    }
}

