# trashy-xml

Less than stellar xml parser, but does what I need in a "simple" way

## Example

```rust
use trashy_xml::{XmlKind, XmlMethods, XmlParser};

let mut parser = XmlParser::new("sample_files/small.xml");
parser.parse();
for token in &parser.xml_tokens {
    if let XmlKind::OpenElement(name, _) = &token.kind {
        if name == "element" {
            for i in parser.xml_tokens.get_attributes(token) {
                dbg!(&parser.xml_tokens[i]);
            }
        }
    }
}
```