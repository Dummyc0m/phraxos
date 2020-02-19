#[derive(Debug)]
enum Error {
    IOError(std::io::Error),
    TypeError
}

impl From<std::io::Error> for Error {
    fn from(other: std::io::Error) -> Self {
        Error::IOError(other)
    }
}
