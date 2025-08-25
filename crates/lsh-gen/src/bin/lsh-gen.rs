use std::io::Write as _;

pub fn main() {
    let res = lsh_gen::generate();
    _ = std::io::stdout().write_all(res.as_bytes());
}
