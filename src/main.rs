use trashy_xml::XmlParser;

fn main() {
    let parsed = XmlParser::str(
        r#"<?xml version="1.0" encoding="UTF-8" standalone="no" ?>
<funk maybe_so="true" />
<or should_it_be = "this_one()" true:er="Skeet" words="wee have 
not bben" spiken="yes?" >
</or>
    "#,
    )
    .parse();

    for token in parsed.tokens {
        if token.borrow().is_comment() || token.borrow().is_inner_text() {
            continue;
        }
        dbg!(token);
    }

    // dbg!(parsed.tokens);
    dbg!(parsed.errors);

    let a = "<String new=\"true\" />".to_string();
    let parsed = XmlParser::str(&a).parse();
    for token in parsed.elements_from_name("String") {
        dbg!(token.attributes());
    }
}
