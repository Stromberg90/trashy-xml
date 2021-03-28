# trashy-xml

A non-spec compliant xml parser that does not stop parsing when encountering errors.

## Example

```rust
use trashy_xml::XmlParser;

// Gets each open element matching "this_element"
// then prints the debug representation of its attributes.
let parsed = XmlParser::str("<this_element attribute=\"value\" />").parse();
for token in parsed.elements_from_name("this_element") {
    dbg!(token.attributes());
}
```