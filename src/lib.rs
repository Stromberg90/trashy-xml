use std::collections::VecDeque;
use std::todo;

const SAMPLE: &str = r#"
<unit>
    <three a="b" />
    <ø ="E">
    </ø>
</unit>
"#;

#[derive(Debug, PartialEq)]
enum XmlState {
    Data(Option<char>),
    TagOpen(Option<char>),
    TagName(Option<char>),
    BogusComment(Option<char>),
    EOF,
}

pub struct XmlParser {
    state: XmlState,
    return_state: Option<XmlState>,
}

impl XmlParser {
    pub fn parse() {
        // When a state says to reconsume a matched character in a specified state,
        // that means to switch to that state,
        // but when it attempts to consume the next input character, provide it with the current input character instead.

        // TODO: Maybe using as_bytes instead of chars,
        // char is utf-8 while bytes are ascii,
        // utf-8 is slower, although I think the xml specs says to support utf-8
        let mut stream = SAMPLE.chars();
        let mut parser = XmlParser {
            state: XmlState::Data(stream.next()),
            return_state: None,
        };
        let mut emitted_tokens: VecDeque<char> = VecDeque::new();
        while parser.state != XmlState::EOF {
            match parser.state {
                XmlState::Data(c) => match c {
                    Some(c) => {
                        if c == '<' {
                            parser.state =
                                XmlState::TagOpen(emitted_tokens.pop_back().or(stream.next()));
                        } else {
                            parser.state =
                                XmlState::Data(emitted_tokens.pop_back().or(stream.next()));
                        }
                    }
                    None => parser.state = XmlState::EOF,
                },
                XmlState::TagOpen(c) => match c {
                    Some(c) => {
                        // I used is_ascii_alphabetic but choose to support utf-8 instead, not sure if I should or not.
                        if c.is_alphabetic() {
                            // Create a new start tag token, set its tag name to the empty string. Reconsume in the https://html.spec.whatwg.org/multipage/parsing.html#tag-name-state
                            parser.state = XmlState::TagName(Some(c));
                        } else if c == '/' {
                            // Switch to https://html.spec.whatwg.org/multipage/parsing.html#end-tag-open-state
                            todo!();
                        } else if c == '!' {
                            // Switch to https://html.spec.whatwg.org/multipage/parsing.html#markup-declaration-open-state
                            todo!();
                        } else if c == '?' {
                            eprintln!("unexpected-question-mark-instead-of-tag-name");
                            parser.state = XmlState::BogusComment(Some(c));
                        } else {
                            eprintln!("invalid-first-character-of-tag-name");
                            emitted_tokens.push_back('<');
                            parser.state =
                                XmlState::Data(emitted_tokens.pop_back().or(stream.next()));
                        }
                    }
                    None => {
                        eprintln!("eof-before-tag-name");
                        emitted_tokens.push_back('<');
                        parser.state = XmlState::EOF;
                    }
                },
                XmlState::TagName(c) => match c {
                    Some(c) => {
                        if c == '>' {
                            parser.state =
                                XmlState::Data(emitted_tokens.pop_back().or(stream.next()));
                        } else {
                            parser.state =
                                XmlState::TagName(emitted_tokens.pop_back().or(stream.next()));
                        }
                    }
                    None => {
                        eprintln!("eof-in-tag");
                        parser.state = XmlState::EOF;
                    }
                },
                XmlState::BogusComment(_) => {
                    todo!();
                }
                XmlState::EOF => {
                    unreachable!()
                }
            }
            dbg!(&parser.state);
        }
    }
}
