use std::io::Write as _;

pub fn main() {
    stdext::arena::init(128 * 1024 * 1024).unwrap();

    let res = lsh_gen::generate();
    _ = std::io::stdout().write_all(res.as_bytes());
}
