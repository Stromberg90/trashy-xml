use std::str::FromStr;
use tokens::XmlKind;
use trashy_xml::*;

#[test]
fn small_file_len_check() {
    let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
    parser.ignore_comments(false);
    parser.parse();
    let mut number_of_errors = 0;
    for token in &parser.xml_tokens {
        if let XmlKind::Error(..) = &token.kind {
            number_of_errors += 1;
        }
    }
    assert_eq!(parser.xml_tokens.len(), 38);
    assert_eq!(number_of_errors, 0);
}

#[test]
fn nested_comment_check() {
    let mut parser = XmlParser::from_str(
        r#"
<!-- Bob <!-- -->
<bobby>
</bobby>
    "#,
    )
    .unwrap();
    parser.ignore_comments(false);
    parser.parse();
    let mut number_of_errors = 0;
    for token in &parser.xml_tokens {
        if let XmlKind::Error(..) = &token.kind {
            number_of_errors += 1;
        }
    }
    assert_eq!(number_of_errors, 1);
}

#[test]
fn missing_closing_bracket_end_check() {
    let mut parser = XmlParser::from_str(
        r#"
<bob>
</bob
    "#,
    )
    .unwrap();
    parser.parse();
    let mut number_of_errors = 0;
    for token in &parser.xml_tokens {
        if let XmlKind::Error(..) = &token.kind {
            number_of_errors += 1;
        }
    }
    assert_eq!(number_of_errors, 1);
}

#[test]
fn xml_declaration_01() {
    let mut parser = XmlParser::from_str(
        r#"
<?xml version="1.0" encoding="UTF-8" standalone="no" ?>
<bob>
</bob>
        "#,
    )
    .unwrap();
    parser.parse();
    let mut number_of_errors = 0;
    for token in &parser.xml_tokens {
        if let XmlKind::Error(..) = &token.kind {
            number_of_errors += 1;
        }
    }
    assert_eq!(number_of_errors, 0);
}

#[test]
fn xml_declaration_02() {
    let mut parser = XmlParser::from_str(
        r#"
<?xml version="1.0"?>
<unit type="being" slot="21">
</unit>
"#,
    )
    .unwrap();
    parser.parse();
    let mut number_of_errors = 0;
    for token in &parser.xml_tokens {
        if let XmlKind::Error(..) = &token.kind {
            number_of_errors += 1;
        }
    }
    assert_eq!(number_of_errors, 0);
}

#[test]
fn missing_closing_bracket_check() {
    let mut parser = XmlParser::from_str(
        r#"
<bob>
<fud>
</fud o>
</bob>
    "#,
    )
    .unwrap();
    parser.parse();
    let mut number_of_errors = 0;
    for token in &parser.xml_tokens {
        if let XmlKind::Error(..) = &token.kind {
            number_of_errors += 1;
        }
    }
    assert_eq!(number_of_errors, 1);
}
#[test]
fn small_file_attributes_len_check() {
    let mut attributes_len = 0;
    let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
    parser.parse();
    let mut number_of_errors = 0;
    for token in &parser.xml_tokens {
        if let XmlKind::OpenElement(name, _) = &token.kind {
            if parser.str(name) == "var_compond" {
                for _ in parser.attributes(token) {
                    attributes_len += 1;
                }
            }
        } else if let XmlKind::Error(..) = &token.kind {
            number_of_errors += 1;
        }
    }
    assert_eq!(attributes_len, 3);
    assert_eq!(number_of_errors, 0);
}
#[test]
fn no_hang_01() {
    let mut parser = XmlParser::from_str(r#"`\"⽣ቋ<¥Ⱥ$+𛲜䒓ჇN&=១వ**À \"¥𐊣{¥ ""#).unwrap();
    parser.parse();
}
#[test]
fn no_hang_02() {
    let mut parser = XmlParser::from_str(r#"?$i૧<ᮙ8\'\\9×g🩺ౚ᭖῁𝈄𒂳🕴*ᣍኾ?$fY    \\𑴃 \"%¥Z""#).unwrap();
    parser.parse();
}
#[test]
fn fixed_index_out_of_bounds_crash_01() {
    let mut parser = XmlParser::from_str(r#"<A=🌀=a"#).unwrap();
    parser.parse();
}

#[test]
fn fixed_index_out_of_bounds_crash_02() {
    let mut parser = XmlParser::from_str(r#"<Ⱥ\'`=<Ô"#).unwrap();
    parser.parse();
}

#[test]
fn fixed_index_out_of_bounds_crash_03() {
    let mut parser = XmlParser::from_str(r#"<\u{fe00} #=\"0"#).unwrap();
    parser.parse();
}

#[test]
fn fixed_index_out_of_bounds_crash_04() {
    let mut parser = XmlParser::from_str(r#"<?"#).unwrap();
    parser.parse();
}

#[test]
fn empty_string() {
    let mut parser = XmlParser::from_str("").unwrap();
    parser.parse();
}

#[test]
fn large_file_len_check() {
    let mut parser = XmlParser::new("sample_files/large.xml").unwrap();
    parser.parse();
    let mut number_of_errors = 0;
    for token in &parser.xml_tokens {
        if let XmlKind::Error(..) = &token.kind {
            number_of_errors += 1;
        }
    }
    assert_eq!(
        parser
            .xml_tokens
            .iter()
            .filter(|token| token.is_open_element())
            .count(),
        81841
    );
    assert_eq!(parser.xml_tokens.len(), 368284);
    assert_eq!(number_of_errors, 0);
}

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash_01(s in "<\\PC* \\PC*=\"\\PC*\">\n</\\PC*>") {
        let mut parser = XmlParser::from_str(&s)
        .unwrap();
        parser.parse();
    }

    #[test]
    fn doesnt_crash_02(s in "\\PC*") {
        let mut parser = XmlParser::from_str(&s)
        .unwrap();
        parser.parse();
    }
}
