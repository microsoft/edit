# Grapheme Table Generator

This tool processes Unicode Character Database (UCD) XML files to generate efficient, multi-stage trie lookup tables for properties relevant to terminal applications:
* Grapheme cluster breaking rules
* Line breaking rules (optional)
* Character width properties

## Usage

* Download [ucd.nounihan.grouped.zip](https://www.unicode.org/Public/UCD/latest/ucdxml/ucd.nounihan.grouped.zip)
* Run some equivalent of:
  ```sh
  grapheme-table-gen --lang=rust --extended --no-ambiguous --line-breaks path/to/ucd.nounihan.grouped.xml
  ```
* Place the result in `src/unicode/tables.rs`
