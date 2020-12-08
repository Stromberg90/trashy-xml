const SAMPLE: &str = r#"
<unit>
    <three a="b" />
</unit>
"#;

#[derive(Debug, PartialEq)]
enum XmlState {
    Data(Option<char>),
    TagOpenState(Option<char>),
    EOF,
}

pub struct XmlParser {
    state: XmlState,
    return_state: Option<XmlState>,
}

impl XmlParser {
    pub fn parse() {
        let mut stream = SAMPLE.chars();
        let mut parser = XmlParser {
            state: XmlState::Data(stream.next()),
            return_state: None,
        };
        while parser.state != XmlState::EOF {
            match parser.state {
                XmlState::Data(c) => match c {
                    Some(c) => {
                        if c == '<' {
                            parser.state = XmlState::TagOpenState(stream.next());
                        } else {
                            parser.state = XmlState::Data(stream.next());
                        }
                    }
                    None => parser.state = XmlState::EOF,
                },
                XmlState::EOF => {
                    unreachable!()
                }
            }
            dbg!(&parser.state);
        }
    }
}
