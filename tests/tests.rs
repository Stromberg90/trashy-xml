use proptest::prelude::*;
use trashy_xml::{
    tokens::{FilePosition, XmlToken},
    Settings, XmlParser,
};

#[test]
fn small_file_len_check() {
    let parsed = XmlParser::file_with_settings(
        "sample_files/small.xml",
        Settings {
            ignore_comments: false,
            create_position_map: false,
        },
    )
    .unwrap()
    .parse();
    assert_eq!(parsed.elements().len(), 7);
    assert_eq!(parsed.errors.len(), 0);
}

#[test]
fn nested_comment_check() {
    let parsed = XmlParser::str_with_settings(
        r#"
    <!-- Bob <!-- -->
    <bobby>
    </bobby>
        "#,
        Settings {
            ignore_comments: false,
            create_position_map: false,
        },
    )
    .parse();
    assert_eq!(parsed.errors.len(), 1);
}

#[test]
fn missing_closing_bracket_end_check() {
    let parser = XmlParser::str(
        r#"
    <bob>
    </bob
        "#,
    )
    .parse();
    assert_eq!(parser.errors.len(), 1);
}

#[test]
fn xml_declaration_01() {
    let parser = XmlParser::str(
        r#"<?xml version="1.0" encoding="UTF-8" standalone="no" ?>
    <bob>
    </bob>
            "#,
    )
    .parse();
    assert_eq!(parser.errors.len(), 0);
}

#[test]
fn xml_declaration_02() {
    let parser = XmlParser::str(
        r#"<?xml version="1.0"?>
    <unit type="being" slot="21">
    </unit>
    "#,
    )
    .parse();
    assert_eq!(parser.errors.len(), 0);
}

#[test]
fn missing_closing_bracket_check() {
    let parser = XmlParser::str(
        r#"
    <bob>
    <fud>
    </fud o>
    </bob>
        "#,
    )
    .parse();
    assert_eq!(parser.errors.len(), 1);
}
#[test]
fn small_file_attributes_len_check() {
    let mut attributes_len = 0;
    let parser = XmlParser::file("sample_files/small.xml").unwrap().parse();
    for token in parser.elements_from_name("var_compond") {
        for _ in &token.attributes() {
            attributes_len += 1;
        }
    }
    assert_eq!(attributes_len, 3);
    assert_eq!(parser.errors.len(), 0);
}
#[test]
fn no_hang_01() {
    let _ = XmlParser::str(r#"`\"⽣ቋ<¥Ⱥ$+𛲜䒓ჇN&=១వ**À \"¥𐊣{¥ ""#).parse();
}
#[test]
fn no_hang_02() {
    let _ = XmlParser::str(r#"?$i૧<ᮙ8\'\\9×g🩺ౚ᭖῁𝈄𒂳🕴*ᣍኾ?$fY    \\𑴃 \"%¥Z""#).parse();
}

#[test]
fn fixed_index_out_of_bounds_crash_01() {
    let _ = XmlParser::str(r#"<A=🌀=a"#).parse();
}

#[test]
fn fixed_index_out_of_bounds_crash_02() {
    let _ = XmlParser::str(r#"<Ⱥ\'`=<Ô"#).parse();
}

#[test]
fn fixed_index_out_of_bounds_crash_03() {
    let _ = XmlParser::str(r#"<\u{fe00} #=\"0"#).parse();
}

#[test]
fn fixed_index_out_of_bounds_crash_04() {
    let _ = XmlParser::str(r#"<?"#).parse();
}

#[test]
fn empty_string() {
    let _ = XmlParser::str("").parse();
}

#[test]
fn large_file_len_check() {
    let parser = XmlParser::file("sample_files/large.xml").unwrap().parse();
    let total_elements = parser.elements().len();
    assert_eq!(total_elements, 81841);
    assert_eq!(parser.errors.len(), 0);
}

#[test]
fn tokenizer() {
    let parsed = XmlParser::str_with_settings(
        "<Bob slacker=\n\"Ain't me this time of one\" />",
        Settings {
            ignore_comments: true,
            create_position_map: false,
        },
    )
    .parse();
    let tokens = parsed.tokens;
    assert_eq!(tokens[0].borrow().as_open_element().name, "Bob");
    assert_eq!(
        tokens[0].borrow().as_open_element().position,
        FilePosition { line: 1, column: 2 }
    );
    match &*tokens[1].borrow() {
        XmlToken::Attribute(a) => {
            assert_eq!(a.key.0, "slacker");
            assert_eq!(a.key.1, FilePosition { line: 1, column: 6 });
            assert_eq!(a.value.as_ref().unwrap().0, "Ain't me this time of one");
            assert_eq!(
                a.value.as_ref().unwrap().1,
                FilePosition { line: 2, column: 1 }
            );
        }
        _ => panic!("Expected Attribute"),
    }
    match &*tokens[2].borrow() {
        XmlToken::CloseElement(c) => {
            assert_eq!(c.name, None);
            assert_eq!(
                c.position,
                FilePosition {
                    line: 2,
                    column: 29
                }
            );
        }
        _ => panic!("Expected CloseElement"),
    };
}

#[test]
fn return_then_newline() {
    let parsed = XmlParser::str("<a>\r\n</a>").parse();
    assert_eq!(
        parsed.tokens[0].borrow().as_open_element().position,
        FilePosition { line: 1, column: 2 }
    );
    assert_eq!(
        parsed.tokens[2].borrow().as_close_element().position,
        FilePosition { line: 2, column: 2 }
    );
}

#[test]
fn newline() {
    let parsed = XmlParser::str("<a>\n</a>").parse();
    assert_eq!(
        parsed.tokens[0].borrow().as_open_element().position,
        FilePosition { line: 1, column: 2 }
    );
    assert_eq!(
        parsed.tokens[2].borrow().as_close_element().position,
        FilePosition { line: 2, column: 2 }
    );
}

#[test]
fn comments() {
    let parsed = XmlParser::str_with_settings(
        "<!--Comment --><!-- Bob --><!-- James-->",
        Settings {
            ignore_comments: false,
            create_position_map: true,
        },
    )
    .parse();
    assert_eq!(&parsed.tokens[0].borrow().as_comment().string, "Comment ");
    assert_eq!(&parsed.tokens[1].borrow().as_comment().string, " Bob ");
    assert_eq!(&parsed.tokens[2].borrow().as_comment().string, " James");
}

#[test]
fn token_from_position() {
    let parsed = XmlParser::str_with_settings(
        "<Bob slacker=\n\"Ain't me this time of one\" />",
        Settings {
            ignore_comments: true,
            create_position_map: true,
        },
    );
    let parsed = parsed.parse();
    assert_eq!(
        parsed.token_from_position(FilePosition { line: 1, column: 2 }),
        Some(parsed.tokens[0].borrow().clone())
    );
}

#[test]
fn invalid_attributes() {
    let parsed = XmlParser::str_with_settings(
        r#"
<a>
    <body  en="wd<aw" />
</a>
    "#,
        Settings {
            ignore_comments: true,
            create_position_map: true,
        },
    )
    .parse();
    assert_eq!(
        parsed.elements_from_name("body")[0].attributes_from_name("en")[0]
            .key
            .1,
        FilePosition {
            line: 3,
            column: 12
        }
    );
}

proptest! {
    #[test]
    fn doesnt_crash_01(s in "<\\PC* \\PC*=\"\\PC*\">\n</\\PC*>") {
        let _ = XmlParser::str(&s).parse();
    }

    #[test]
    fn doesnt_crash_02(s in "\\PC*") {
        let _ = XmlParser::str(&s).parse();
    }
}
