use std::char;
use std::collections::VecDeque;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
enum TokenKind {
    KeyChar(usize),
    Whitespace(usize, usize),
    Text(usize, usize),
}

#[derive(Debug, PartialEq)]
pub enum XmlKind {
    Comment(String),
    Attribute(String, String),
    InnerText(String),
    OpenElement(String, usize),
    CloseElement(String),
}

impl XmlKind {
    pub fn is_comment(&self) -> bool {
        match self {
            XmlKind::Comment(..) => true,
            _ => false,
        }
    }
    pub fn is_attribute(&self) -> bool {
        match self {
            XmlKind::Attribute(..) => true,
            _ => false,
        }
    }
    pub fn is_inner_text(&self) -> bool {
        match self {
            XmlKind::InnerText(..) => true,
            _ => false,
        }
    }
    pub fn is_open_element(&self) -> bool {
        match self {
            XmlKind::OpenElement(..) => true,
            _ => false,
        }
    }
    pub fn is_close_element(&self) -> bool {
        match self {
            XmlKind::CloseElement(..) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct FilePosition {
    pub line: usize,
    pub column: usize,
}

impl FilePosition {
    fn new() -> FilePosition {
        FilePosition { line: 1, column: 1 }
    }
}

#[derive(Debug)]
pub struct Token {
    position: FilePosition,
    kind: TokenKind,
}

#[derive(Debug, PartialEq)]
pub struct XmlToken {
    pub position: FilePosition,
    pub kind: XmlKind,
    pub parent: Option<usize>,
}

#[derive(Debug)]
pub struct XmlError {
    pub position: FilePosition,
    pub message: String,
}

/// # Examples
///
/// ```
/// use trashy_xml::{XmlKind, XmlMethods, XmlParser};
///
/// let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
/// parser.parse();
/// for token in &parser.xml_tokens {
///     if let XmlKind::OpenElement(name, _) = &token.kind {
///         if name == "element" {
///             for attribute in parser.xml_tokens.get_attributes(token) {
///                 dbg!(&attribute);
///             }
///         }
///     }
/// }
/// ```
#[derive(Debug)]
pub struct XmlParser {
    tab_width: usize,
    index: usize,
    started_parsing: bool,
    ignore_comments: bool,
    position: FilePosition,
    stream: Vec<u8>,
    raw_tokens: Vec<Token>,
    pub xml_tokens: Vec<XmlToken>,
    pub errors: Vec<XmlError>,
}

/// Helper methods
pub trait XmlMethods {
    /// Gets all children tokens
    ///
    /// ```
    /// use trashy_xml::{XmlKind, XmlMethods, XmlParser};
    ///
    /// let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
    /// parser.parse();
    /// for token in &parser.xml_tokens {
    ///    for child in parser.xml_tokens.get_children(token) {
    ///        dbg!(&child);
    ///    }
    /// }
    /// ```
    fn get_children(&self, token: &XmlToken) -> Vec<&XmlToken>;

    /// Gets all attributes tokens
    ///
    /// ```
    /// use trashy_xml::{XmlKind, XmlMethods, XmlParser};
    ///
    /// let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
    /// parser.parse();
    /// for token in &parser.xml_tokens {
    ///    for attribute in parser.xml_tokens.get_attributes(token) {
    ///        dbg!(&attribute);
    ///    }
    /// }
    /// ```
    fn get_attributes(&self, token: &XmlToken) -> Vec<&XmlToken>;

    /// Gets all siblings tokens
    ///
    /// ```
    /// use trashy_xml::{XmlKind, XmlMethods, XmlParser};
    ///
    /// let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
    /// parser.parse();
    /// for token in &parser.xml_tokens {
    ///    for sibling in parser.xml_tokens.get_siblings(token) {
    ///        dbg!(&sibling);
    ///    }
    /// }
    /// ```
    fn get_siblings(&self, token: &XmlToken) -> Vec<&XmlToken>;
}

impl XmlMethods for Vec<XmlToken> {
    fn get_children(&self, token: &XmlToken) -> Vec<&XmlToken> {
        let mut result = Vec::<&XmlToken>::new();
        if let XmlKind::OpenElement(_, i) = &token.kind {
            for token in self.iter() {
                if let Some(parent) = token.parent {
                    if parent == *i {
                        result.push(token);
                    }
                }
            }
        }
        result
    }

    fn get_attributes(&self, token: &XmlToken) -> Vec<&XmlToken> {
        let mut result = Vec::<&XmlToken>::new();
        if let XmlKind::OpenElement(_, i) = &token.kind {
            for token in self.iter() {
                if let Some(parent) = token.parent {
                    if let XmlKind::Attribute(..) = token.kind {
                        if parent == *i {
                            result.push(token);
                        }
                    }
                }
            }
        }
        result
    }

    fn get_siblings(&self, token: &XmlToken) -> Vec<&XmlToken> {
        let mut result = Vec::<&XmlToken>::new();
        if let Some(token_parent) = &token.parent {
            for token in self.iter() {
                if let Some(parent) = token.parent {
                    if parent == *token_parent {
                        result.push(token);
                    }
                }
            }
        }
        result
    }
}

fn is_key_char(c: u8) -> bool {
    match c {
        b'<' | b'/' | b'!' | b'-' | b'>' | b'"' | b'\'' | b'=' | b'?' | b'(' | b')' => true,
        _ => false,
    }
}

mod lexer {
    use super::*;
    pub fn peek(xml_parser: &XmlParser) -> Option<u8> {
        if let Some(character) = xml_parser.stream.get(xml_parser.index + 1) {
            return Some(*character);
        }
        None
    }

    pub fn next(xml_parser: &mut XmlParser) -> Option<u8> {
        if xml_parser.started_parsing {
            xml_parser.index += 1;
        }
        xml_parser.started_parsing = true;
        if let Some(character) = xml_parser.stream.get(xml_parser.index) {
            match *character {
                b'\r' => {
                    if let Some(v) = peek(xml_parser) {
                        if v == b'\n' {
                            xml_parser.index += 1;
                            xml_parser.position.line += 1;
                            xml_parser.position.column = 1;
                        }
                    }
                }
                b'\t' => {
                    xml_parser.position.column += xml_parser.tab_width;
                }
                b'\n' => {
                    xml_parser.position.line += 1;
                    xml_parser.position.column = 1;
                }
                _ => {
                    xml_parser.position.column += 1;
                }
            }
            return Some(*character);
        }
        None
    }
}

impl Iterator for XmlParser {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(v) = lexer::next(self) {
            if !is_key_char(v) {
                if v.is_ascii_whitespace() {
                    let start_index = self.index;
                    while lexer::peek(self)?.is_ascii_whitespace() {
                        lexer::next(self)?;
                    }
                    return Some(Token {
                        position: self.position,
                        kind: TokenKind::Whitespace(start_index, self.index + 1),
                    });
                }
                let start_index = self.index;
                while let Some(peeked_character) = lexer::peek(self) {
                    if !peeked_character.is_ascii_whitespace() && !is_key_char(peeked_character) {
                        lexer::next(self)?;
                    } else {
                        break;
                    }
                }
                Some(Token {
                    position: self.position,
                    kind: TokenKind::Text(start_index, self.index + 1),
                })
            } else {
                Some(Token {
                    position: self.position,
                    kind: TokenKind::KeyChar(self.index),
                })
            }
        } else {
            None
        }
    }
}

impl FromStr for XmlParser {
    type Err = std::string::ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let input = s.to_owned();
        Ok(XmlParser {
            index: 0,
            tab_width: 4,
            started_parsing: false,
            ignore_comments: true,
            position: FilePosition::new(),
            stream: input.into_bytes(),
            raw_tokens: Vec::new(),
            xml_tokens: Vec::new(),
            errors: Vec::new(),
        })
    }
}

impl XmlParser {
    /// Constructs a new `XmlParser`.
    pub fn new<P: AsRef<Path>>(filepath: P) -> Option<XmlParser> {
        let mut buffer = Vec::new();
        File::open(filepath).ok()?.read_to_end(&mut buffer).ok()?;
        Some(XmlParser {
            index: 0,
            tab_width: 4,
            started_parsing: false,
            ignore_comments: true,
            position: FilePosition::new(),
            stream: buffer,
            raw_tokens: Vec::new(),
            xml_tokens: Vec::new(),
            errors: Vec::new(),
        })
    }

    pub fn ignore_comments(&mut self, ignore_comments: bool) -> &mut XmlParser {
        self.ignore_comments = ignore_comments;
        self
    }

    pub fn tab_width(&mut self, tab_width: usize) -> &mut XmlParser {
        self.tab_width = tab_width;
        self
    }

    fn match_next_str(&self, index: usize, characters: &str) -> bool {
        if index + characters.chars().count() < self.raw_tokens.len() {
            for (token, character) in self.raw_tokens
                [index + 1..=index + characters.chars().count()]
                .iter()
                .zip(characters.chars())
            {
                if let TokenKind::KeyChar(kc) = token.kind {
                    if self.stream[kc] as char != character {
                        return false;
                    }
                } else {
                    return false;
                }
            }
        } else {
            return false;
        }
        true
    }

    fn match_next_char(&self, index: usize, character: char) -> bool {
        if let Some(token) = self.raw_tokens.get(index + 1) {
            if let TokenKind::KeyChar(kc) = token.kind {
                return self.stream[kc] as char == character;
            }
        }
        false
    }

    pub fn parse(&mut self) -> Option<&Self> {
        use TokenKind::*;
        use XmlKind::*;

        while let Some(token) = self.next() {
            self.raw_tokens.push(token);
        }
        self.xml_tokens = Vec::with_capacity(self.raw_tokens.len() / 3);

        let mut open_element_index_stack = VecDeque::<usize>::new();
        let mut raw_token_index = 0;
        while let Some(raw_token) = self.raw_tokens.get(raw_token_index) {
            match raw_token.kind {
                Text(start_index, end_index) => {
                    raw_token_index += 1;
                    if open_element_index_stack.is_empty() {
                        self.errors.push(XmlError {
                            position: raw_token.position,
                            message: "Document is empty".to_owned(),
                        });
                        continue;
                    }

                    let mut skip_loop = false;
                    while let Some(raw_token) = self.raw_tokens.get(raw_token_index) {
                        raw_token_index += 1;
                        match raw_token.kind {
                            KeyChar(kc) => {
                                if self.stream[kc] == b'=' {
                                    raw_token_index -= 1;
                                    break;
                                }
                            }
                            Text(..) => {
                                self.errors.push(XmlError {
                                    position: raw_token.position,
                                    message: format!(
                                        "Specification mandates value for attribute {}",
                                        String::from_utf8_lossy(
                                            &self.stream[start_index..end_index]
                                        )
                                    ),
                                });
                                skip_loop = true;
                                break;
                            }
                            _ => {}
                        }
                    }
                    if raw_token_index >= self.raw_tokens.len() {
                        break;
                    }
                    if skip_loop {
                        continue;
                    }
                    while let Some(raw_token) = self.raw_tokens.get(raw_token_index) {
                        raw_token_index += 1;
                        match raw_token.kind {
                            KeyChar(kc) => {
                                if self.stream[kc] == b'"' || self.stream[kc] == b'\'' {
                                    raw_token_index -= 1;
                                    break;
                                }
                            }
                            Text(..) => {
                                self.errors.push(XmlError {
                                    position: raw_token.position,
                                    message: "\" or \' expected".to_owned(),
                                });
                                break;
                            }
                            _ => {}
                        }
                    }
                    if let Some(token) = self.raw_tokens.get(raw_token_index) {
                        if let KeyChar(attribute_value_start) = token.kind {
                            let boundary_character = self.stream[attribute_value_start];
                            let attribute_value_start = attribute_value_start + 1;
                            let mut attribute_value_end = attribute_value_start;
                            loop {
                                raw_token_index += 1;
                                if let Some(token) = self.raw_tokens.get(raw_token_index) {
                                    match token.kind {
                                        KeyChar(key_char_index) => {
                                            if self.stream[key_char_index] == boundary_character {
                                                break;
                                            }
                                            attribute_value_end += 1;
                                        }
                                        Text(start_index, end_index)
                                        | Whitespace(start_index, end_index) => {
                                            attribute_value_end += end_index - start_index;
                                        }
                                    }
                                } else {
                                    break;
                                }
                            }
                            let token = XmlToken {
                                kind: Attribute(
                                    String::from_utf8_lossy(&self.stream[start_index..end_index])
                                        .to_string(),
                                    String::from_utf8_lossy(
                                        &self.stream[attribute_value_start..attribute_value_end],
                                    )
                                    .to_string(),
                                ),
                                position: raw_token.position,
                                parent: open_element_index_stack.front().copied(),
                            };
                            self.xml_tokens.push(token);
                        }
                    }
                }
                KeyChar(kc) => match self.stream[kc] {
                    b'<' => {
                        if self.match_next_str(raw_token_index, "!--") {
                            raw_token_index += 4;
                            let position = self.raw_tokens[raw_token_index].position;
                            let comment_start = kc + 4;
                            let mut comment_end = comment_start;
                            while let Some(raw_token) = self.raw_tokens.get(raw_token_index + 1) {
                                if !self.ignore_comments {
                                    match raw_token.kind {
                                        KeyChar(..) => {
                                            comment_end += 1;
                                        }
                                        Whitespace(start_index, end_index)
                                        | Text(start_index, end_index) => {
                                            comment_end += end_index - start_index;
                                        }
                                    }
                                }
                                if self.match_next_str(raw_token_index, "--") {
                                    raw_token_index += 2;
                                    if self.match_next_char(raw_token_index, '>') {
                                        raw_token_index += 1;
                                        break;
                                    } else {
                                        if !self.ignore_comments {
                                            self.errors.push(XmlError {
                                                position,
                                                message: "-- is not permitted within comments"
                                                    .to_owned(),
                                            });
                                        }
                                        raw_token_index -= 2;
                                    }
                                }
                                raw_token_index += 1;
                            }
                            if !self.ignore_comments {
                                let token = XmlToken {
                                    kind: Comment(
                                        String::from_utf8_lossy(
                                            &self.stream[comment_start..comment_end],
                                        )
                                        .to_string(),
                                    ),
                                    position,
                                    parent: open_element_index_stack.front().copied(),
                                };
                                self.xml_tokens.push(token);
                            }
                        } else if let Some(raw_token) = self.raw_tokens.get(raw_token_index + 1) {
                            let position = raw_token.position;
                            match raw_token.kind {
                                Text(start_index, end_index) => {
                                    let token = XmlToken {
                                        kind: OpenElement(
                                            String::from_utf8_lossy(
                                                &self.stream[start_index..end_index],
                                            )
                                            .to_string(),
                                            self.xml_tokens.len(),
                                        ),
                                        position,
                                        parent: open_element_index_stack.front().copied(),
                                    };
                                    open_element_index_stack.push_front(self.xml_tokens.len());
                                    self.xml_tokens.push(token);
                                    raw_token_index += 1;
                                }
                                KeyChar(kc) => {
                                    if let b'/' = self.stream[kc] {
                                        if let Some(raw_token) =
                                            self.raw_tokens.get(raw_token_index + 2)
                                        {
                                            raw_token_index += 2;
                                            if let Text(start_index, end_index) = raw_token.kind {
                                                if let Some(front) =
                                                    open_element_index_stack.pop_front()
                                                {
                                                    if let OpenElement(name, index) =
                                                        &self.xml_tokens[front].kind
                                                    {
                                                        let text = &String::from_utf8_lossy(
                                                            &self.stream[start_index..end_index],
                                                        );
                                                        if *index != front || name != text {
                                                            self.errors.push(XmlError {
                                                                        position,
                                                                        message: format!("Mismatch between closing {} and opening {} elements",
                                                                        text, name),
                                                                    });
                                                        }
                                                    }
                                                } else {
                                                    self.errors.push(XmlError {
                                                        position,
                                                        message: "Mismatch between closing and opening elements".to_owned()
                                                    });
                                                }
                                                let token = XmlToken {
                                                    kind: CloseElement(
                                                        String::from_utf8_lossy(
                                                            &self.stream[start_index..end_index],
                                                        )
                                                        .to_string(),
                                                    ),
                                                    position,
                                                    parent: open_element_index_stack
                                                        .front()
                                                        .copied(),
                                                };
                                                self.xml_tokens.push(token);
                                                if (raw_token_index + 1) >= self.raw_tokens.len() {
                                                    self.errors.push(XmlError {
                                                        position,
                                                        message: "Expected '>'".to_owned(),
                                                    });
                                                    break;
                                                }
                                                while let Whitespace(..) =
                                                    self.raw_tokens[raw_token_index + 1].kind
                                                {
                                                    raw_token_index += 1;
                                                }
                                                match self.raw_tokens[raw_token_index + 1].kind {
                                                    KeyChar(index) => {
                                                        if self.stream[index] != b'>' {
                                                            self.errors.push(XmlError {
                                                                position,
                                                                message: "Expected '>'".to_owned(),
                                                            });
                                                            raw_token_index += 1;
                                                        }
                                                    }
                                                    _ => {
                                                        self.errors.push(XmlError {
                                                            position,
                                                            message: "Expected '>'".to_owned(),
                                                        });
                                                        raw_token_index += 1;
                                                    }
                                                }
                                            }
                                        }
                                    } else if let b'?' = self.stream[kc] {
                                        raw_token_index += 1;
                                        if raw_token_index + 1 >= self.raw_tokens.len() {
                                            break;
                                        }
                                        raw_token_index += 1;
                                        match self.raw_tokens[raw_token_index].kind {
                                            Text(start_index, end_index) => {
                                                let text = String::from_utf8_lossy(
                                                    &self.stream[start_index..end_index],
                                                );
                                                if text == "xml" {
                                                    let element_name = format!(
                                                        "?{}",
                                                        String::from_utf8_lossy(
                                                            &self.stream[start_index..end_index]
                                                        )
                                                    );
                                                    let token = XmlToken {
                                                        kind: OpenElement(
                                                            element_name,
                                                            self.xml_tokens.len(),
                                                        ),
                                                        position,
                                                        parent: open_element_index_stack
                                                            .front()
                                                            .copied(),
                                                    };
                                                    self.xml_tokens.push(token);
                                                    open_element_index_stack
                                                        .push_front(self.xml_tokens.len() - 1);
                                                } else {
                                                    self.errors.push(XmlError {
                                                        position,
                                                        message: "Expected 'xml'".to_owned(),
                                                    });
                                                }
                                            }
                                            _ => {
                                                self.errors.push(XmlError {
                                                    position,
                                                    message: "Expected 'xml'".to_owned(),
                                                });
                                            }
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    b'/' | b'?' => {
                        if self.match_next_char(raw_token_index, '>') {
                            if let Some(front) = open_element_index_stack.pop_front() {
                                if let OpenElement(parent_name, _) = &self.xml_tokens[front].kind {
                                    let position = self.raw_tokens[raw_token_index].position;
                                    let token = XmlToken {
                                        kind: CloseElement(parent_name.into()),
                                        position,
                                        parent: open_element_index_stack.front().copied(),
                                    };
                                    self.xml_tokens.push(token);
                                }
                            }
                        }
                    }
                    b'>' => {
                        raw_token_index += 1;
                        let text_start = kc + 1;
                        let mut text_end = text_start;
                        while let Some(raw_token) = self.raw_tokens.get(raw_token_index) {
                            match raw_token.kind {
                                Text(start_index, end_index)
                                | Whitespace(start_index, end_index) => {
                                    text_end += end_index - start_index;
                                }
                                KeyChar(kc) => match self.stream[kc] {
                                    b'<' => {
                                        raw_token_index -= 1;
                                        break;
                                    }
                                    _ => {
                                        text_end += 1;
                                    }
                                },
                            }
                            raw_token_index += 1;
                        }
                        let token = XmlToken {
                            kind: InnerText(
                                String::from_utf8_lossy(&self.stream[text_start..text_end])
                                    .to_string(),
                            ),
                            position: raw_token.position,
                            parent: open_element_index_stack.front().copied(),
                        };
                        self.xml_tokens.push(token);
                    }
                    _ => {}
                },
                _ => {}
            }
            raw_token_index += 1;
        }
        if let Some(last) = open_element_index_stack.iter().last() {
            self.errors.push(XmlError {
                position: self.xml_tokens[*last].position,
                message: "Mismatch between number of closing and opening elements".to_owned(),
            });
        }
        Some(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn small_file_len_check() {
        let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
        parser.ignore_comments(false);
        parser.parse();
        assert_eq!(parser.xml_tokens.len(), 38);
        assert_eq!(parser.raw_tokens.len(), 196);
        assert_eq!(parser.errors.len(), 0);
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
        assert_eq!(parser.errors.len(), 1);
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
        assert_eq!(parser.errors.len(), 1);
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
        dbg!(&parser.errors);
        dbg!(&parser.xml_tokens);
        assert_eq!(parser.errors.len(), 0);
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
        dbg!(&parser.errors);
        dbg!(&parser.xml_tokens);
        assert_eq!(parser.errors.len(), 0);
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
        assert_eq!(parser.errors.len(), 1);
    }
    #[test]
    fn small_file_attributes_len_check() {
        let mut attributes_len = 0;
        let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
        parser.parse();
        for token in &parser.xml_tokens {
            if let XmlKind::OpenElement(name, _) = &token.kind {
                if name == "var_compond" {
                    for _ in parser.xml_tokens.get_attributes(token) {
                        attributes_len += 1;
                    }
                }
            }
        }
        assert_eq!(attributes_len, 3);
        assert_eq!(parser.errors.len(), 0);
    }
    #[test]
    fn no_hang_01() {
        let mut parser = XmlParser::from_str(r#"`\"⽣ቋ<¥Ⱥ$+𛲜䒓ჇN&=១వ**À \"¥𐊣{¥ ""#).unwrap();
        assert!(parser.parse().is_some());
    }
    #[test]
    fn no_hang_02() {
        let mut parser =
            XmlParser::from_str(r#"?$i૧<ᮙ8\'\\9×g🩺ౚ᭖῁𝈄𒂳🕴*ᣍኾ?$fY    \\𑴃 \"%¥Z""#).unwrap();
        assert!(parser.parse().is_some());
    }
    #[test]
    fn fixed_index_out_of_bounds_crash_01() {
        let mut parser = XmlParser::from_str(r#"<A=🌀=a"#).unwrap();
        assert!(parser.parse().is_some());
    }

    #[test]
    fn fixed_index_out_of_bounds_crash_02() {
        let mut parser = XmlParser::from_str(r#"<Ⱥ\'`=<Ô"#).unwrap();
        assert!(parser.parse().is_some());
    }

    #[test]
    fn fixed_index_out_of_bounds_crash_03() {
        let mut parser = XmlParser::from_str(r#"<\u{fe00} #=\"0"#).unwrap();
        assert!(parser.parse().is_some());
    }

    #[test]
    fn fixed_index_out_of_bounds_crash_04() {
        let mut parser = XmlParser::from_str(r#"<?"#).unwrap();
        assert!(parser.parse().is_some());
    }

    #[test]
    fn empty_string() {
        let mut parser = XmlParser::from_str("").unwrap();
        assert!(parser.parse().is_some());
    }

    #[test]
    fn large_file_len_check() {
        let mut parser = XmlParser::new("sample_files/large.xml").unwrap();
        parser.parse();
        assert_eq!(
            parser
                .xml_tokens
                .iter()
                .filter(|token| token.kind.is_open_element())
                .count(),
            81841
        );
        assert_eq!(parser.xml_tokens.len(), 368284);
        assert_eq!(parser.raw_tokens.len(), 1118488);
        assert_eq!(parser.errors.len(), 0);
    }
}

#[cfg(test)]
use proptest::prelude::*;

#[cfg(test)]
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
