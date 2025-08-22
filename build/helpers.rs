use std::env::VarError;

pub fn env_opt(name: &str) -> String {
    match std::env::var(name) {
        Ok(value) => value,
        Err(VarError::NotPresent) => String::new(),
        Err(VarError::NotUnicode(_)) => {
            panic!("Environment variable `{name}` is not valid Unicode")
        }
    }
}
