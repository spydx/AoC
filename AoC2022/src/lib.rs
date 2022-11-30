use std::{io::Error, path::PathBuf};

pub fn read_content(filename: &str) -> Result<String, Error>{
    let sourcepath = concat!(env!("CARGO_MANIFEST_DIR"), "/files");
    let mut sourcepath = PathBuf::from(sourcepath);
    sourcepath.push(filename);
    println!("Read path: {}", sourcepath.display());
    match std::fs::read_to_string(&sourcepath) {
        Ok(content) => Ok(content),
        Err(err) => Err(err)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn filereader_works() {
        let filename = "example.txt";
        let content = read_content(filename).unwrap();
        let name = &content[..7];
        assert_eq!(name, "Kenneth");
    }
}
