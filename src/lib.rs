mod lexer;

use std::char;
use std::collections::VecDeque;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::str::FromStr;

#[derive(PartialEq)]
enum TokenKind {
    KeyChar(usize),
    Whitespace(usize, usize),
    Text(usize, usize),
}

#[derive(PartialEq, Debug)]
pub enum XmlKind {
    Comment(String),
    Attribute(String, String),
    InnerText(String),
    OpenElement(String, usize),
    CloseElement(String),
    Error(String),
}

impl XmlToken {
    #[inline(always)]
    pub fn is_comment(&self) -> bool {
        matches!(self.kind, XmlKind::Comment(..))
    }
    #[inline(always)]
    pub fn is_attribute(&self) -> bool {
        matches!(self.kind, XmlKind::Attribute(..))
    }
    #[inline(always)]
    pub fn is_inner_text(&self) -> bool {
        matches!(self.kind, XmlKind::InnerText(..))
    }
    #[inline(always)]
    pub fn is_open_element(&self) -> bool {
        matches!(self.kind, XmlKind::OpenElement(..))
    }
    #[inline(always)]
    pub fn is_close_element(&self) -> bool {
        matches!(self.kind, XmlKind::CloseElement(..))
    }
    #[inline(always)]
    pub fn is_error(&self) -> bool {
        matches!(self.kind, XmlKind::Error(..))
    }
    #[inline(always)]
    pub fn as_attribute_unchecked(&self) -> (&str, &str) {
        match &self.kind {
            XmlKind::Attribute(k, v) => (k, v),
            _ => unreachable!(),
        }
    }
    #[inline(always)]
    pub fn as_comment_unchecked(&self) -> &str {
        match &self.kind {
            XmlKind::Comment(comment) => comment,
            _ => unreachable!(),
        }
    }
    #[inline(always)]
    pub fn as_inner_text_unchecked(&self) -> &str {
        match &self.kind {
            XmlKind::InnerText(inner_text) => inner_text,
            _ => unreachable!(),
        }
    }
    #[inline(always)]
    pub fn as_open_element_unchecked(&self) -> (&str, usize) {
        match &self.kind {
            XmlKind::OpenElement(k, v) => (k, *v),
            _ => unreachable!(),
        }
    }
    #[inline(always)]
    pub fn as_close_element_unchecked(&self) -> &str {
        match &self.kind {
            XmlKind::CloseElement(k) => k,
            _ => unreachable!(),
        }
    }
}

#[derive(PartialEq, Copy, Clone)]
pub struct FilePosition {
    pub line: usize,
    pub column: usize,
}

impl FilePosition {
    #[inline(always)]
    fn new() -> FilePosition {
        FilePosition { line: 1, column: 1 }
    }
}

pub struct Token {
    position: FilePosition,
    kind: TokenKind,
}

#[derive(PartialEq)]
pub struct XmlToken {
    pub position: FilePosition,
    pub kind: XmlKind,
    pub parent: Option<usize>,
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
///             for attribute in parser.xml_tokens.attributes(token) {
///             }
///         }
///     }
/// }
/// ```
pub struct XmlParser {
    tab_width: usize,
    index: usize,
    started_parsing: bool,
    ignore_comments: bool,
    position: FilePosition,
    buffer: Vec<u8>,
    raw_tokens: Vec<Token>,
    raw_token_index: usize,
    pub xml_tokens: Vec<XmlToken>,
}

/// Helper methods
pub trait XmlMethods {
    /// Returns references to children tokens
    ///
    /// ```
    /// use trashy_xml::{XmlKind, XmlMethods, XmlParser};
    ///
    /// let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
    /// parser.parse();
    /// for token in &parser.xml_tokens {
    ///    for child in parser.xml_tokens.children(token) {
    ///    }
    /// }
    /// ```
    fn children(&self, token: &XmlToken) -> Vec<&XmlToken>;

    /// Returns references to attributes tokens
    ///
    /// ```
    /// use trashy_xml::{XmlKind, XmlMethods, XmlParser};
    ///
    /// let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
    /// parser.parse();
    /// for token in &parser.xml_tokens {
    ///    for attribute in parser.xml_tokens.attributes(token) {
    ///    }
    /// }
    /// ```
    fn attributes(&self, token: &XmlToken) -> Vec<&XmlToken>;

    /// Returns references to siblings tokens
    ///
    /// ```
    /// use trashy_xml::{XmlKind, XmlMethods, XmlParser};
    ///
    /// let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
    /// parser.parse();
    /// for token in &parser.xml_tokens {
    ///    for sibling in parser.xml_tokens.siblings(token) {
    ///    }
    /// }
    /// ```
    fn siblings(&self, token: &XmlToken) -> Vec<&XmlToken>;
}

impl XmlMethods for Vec<XmlToken> {
    fn children(&self, token: &XmlToken) -> Vec<&XmlToken> {
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

    fn attributes(&self, token: &XmlToken) -> Vec<&XmlToken> {
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

    fn siblings(&self, token: &XmlToken) -> Vec<&XmlToken> {
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

impl Iterator for XmlParser {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(v) = lexer::next(self) {
            if is_key_char(v) {
                Some(Token {
                    position: self.position,
                    kind: TokenKind::KeyChar(self.index),
                })
            } else {
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
            }
        } else {
            None
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct XmlParserError(String);

impl std::error::Error for XmlParserError {}

impl std::fmt::Display for XmlParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for XmlParser {
    type Err = XmlParserError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let input = s.to_owned();
        Ok(Self {
            index: 0,
            tab_width: 4,
            started_parsing: false,
            ignore_comments: true,
            position: FilePosition::new(),
            buffer: input.into_bytes(),
            raw_tokens: Vec::new(),
            raw_token_index: 0,
            xml_tokens: Vec::new(),
        })
    }
}

impl XmlParser {
    /// Constructs a new `XmlParser`.
    pub fn new<P: AsRef<Path>>(filepath: P) -> Result<Self, XmlParserError> {
        match File::open(filepath) {
            Ok(mut file) => {
                let mut buffer = Vec::new();
                match file.read_to_end(&mut buffer) {
                    Ok(_) => Ok(Self {
                        index: 0,
                        tab_width: 4,
                        started_parsing: false,
                        ignore_comments: true,
                        position: FilePosition::new(),
                        buffer,
                        raw_tokens: Vec::new(),
                        raw_token_index: 0,
                        xml_tokens: Vec::new(),
                    }),
                    Err(e) => Err(XmlParserError(e.to_string())),
                }
            }
            Err(e) => Err(XmlParserError(e.to_string())),
        }
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
        let chars = characters.chars();
        let chars_count = chars.clone().count();
        if index + chars_count < self.raw_tokens.len() {
            for (token, character) in self.raw_tokens[index + 1..=index + chars_count]
                .iter()
                .zip(chars)
            {
                if let TokenKind::KeyChar(kc) = token.kind {
                    if self.buffer[kc] as char != character {
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
                return self.buffer[kc] as char == character;
            }
        }
        false
    }

    pub fn parse(&mut self) {
        use TokenKind::*;
        use XmlKind::*;

        self.raw_tokens = self.collect();

        let mut open_elements = VecDeque::<usize>::new();
        while let Some(raw_token) = self.raw_tokens.get(self.raw_token_index) {
            let parent = open_elements.front().copied();
            match raw_token.kind {
                Text(start_index, end_index) => {
                    self.raw_token_index += 1;
                    if open_elements.is_empty() {
                        self.xml_tokens.push(XmlToken {
                            position: raw_token.position,
                            kind: XmlKind::Error("Document is empty".into()),
                            parent: None,
                        });
                        continue;
                    }
                    while let Some(raw_token) = self.raw_tokens.get(self.raw_token_index) {
                        self.raw_token_index += 1;
                        match raw_token.kind {
                            KeyChar(kc) => {
                                if self.buffer[kc] == b'=' {
                                    break;
                                }
                            }
                            Text(..) => {
                                self.xml_tokens.push(XmlToken {
                                    position: raw_token.position,
                                    kind: XmlKind::Error(format!(
                                        "Specification mandates value for attribute {}",
                                        String::from_utf8_lossy(
                                            &self.buffer[start_index..end_index]
                                        )
                                    )),
                                    parent,
                                });
                                break;
                            }
                            _ => {}
                        }
                    }
                    while let Some(raw_token) = self.raw_tokens.get(self.raw_token_index) {
                        match raw_token.kind {
                            KeyChar(kc) => {
                                if self.buffer[kc] == b'"' || self.buffer[kc] == b'\'' {
                                    break;
                                }
                            }
                            Text(..) => {
                                self.xml_tokens.push(XmlToken {
                                    position: raw_token.position,
                                    kind: XmlKind::Error("\" or \' expected".into()),
                                    parent,
                                });
                                break;
                            }
                            _ => {}
                        }
                        self.raw_token_index += 1;
                    }
                    if let Some(token) = self.raw_tokens.get(self.raw_token_index) {
                        if let KeyChar(attribute_value_start) = token.kind {
                            let boundary_character = self.buffer[attribute_value_start];
                            let attribute_value_start = attribute_value_start + 1;
                            let mut attribute_value_end = attribute_value_start;
                            loop {
                                self.raw_token_index += 1;
                                if let Some(token) = self.raw_tokens.get(self.raw_token_index) {
                                    match token.kind {
                                        KeyChar(key_char_index) => {
                                            if self.buffer[key_char_index] == boundary_character {
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
                                    String::from_utf8_lossy(&self.buffer[start_index..end_index])
                                        .to_string(),
                                    String::from_utf8_lossy(
                                        &self.buffer[attribute_value_start..attribute_value_end],
                                    )
                                    .to_string(),
                                ),
                                position: raw_token.position,
                                parent,
                            };
                            self.xml_tokens.push(token);
                        }
                    }
                }
                KeyChar(kc) => match self.buffer[kc] {
                    b'<' => {
                        if self.match_next_str(self.raw_token_index, "!--") {
                            self.raw_token_index += 4;
                            let position = self.raw_tokens[self.raw_token_index].position;
                            let comment_start = kc + 4;
                            let mut comment_end = comment_start;
                            while let Some(raw_token) =
                                self.raw_tokens.get(self.raw_token_index + 1)
                            {
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
                                if self.match_next_str(self.raw_token_index, "--") {
                                    self.raw_token_index += 2;
                                    if self.match_next_char(self.raw_token_index, '>') {
                                        self.raw_token_index += 1;
                                        break;
                                    }
                                    if !self.ignore_comments {
                                        self.xml_tokens.push(XmlToken {
                                            position,
                                            kind: XmlKind::Error(
                                                "-- is not permitted within comments".into(),
                                            ),
                                            parent,
                                        });
                                    }
                                }
                                self.raw_token_index += 1;
                            }
                            if !self.ignore_comments {
                                let token = XmlToken {
                                    kind: Comment(
                                        String::from_utf8_lossy(
                                            &self.buffer[comment_start..comment_end],
                                        )
                                        .to_string(),
                                    ),
                                    position,
                                    parent,
                                };
                                self.xml_tokens.push(token);
                            }
                        } else if let Some(raw_token) =
                            self.raw_tokens.get(self.raw_token_index + 1)
                        {
                            let position = raw_token.position;
                            match raw_token.kind {
                                Text(start_index, end_index) => {
                                    let token = XmlToken {
                                        kind: OpenElement(
                                            String::from_utf8_lossy(
                                                &self.buffer[start_index..end_index],
                                            )
                                            .to_string(),
                                            self.xml_tokens.len(),
                                        ),
                                        position,
                                        parent,
                                    };
                                    open_elements.push_front(self.xml_tokens.len());
                                    self.xml_tokens.push(token);
                                    self.raw_token_index += 1;
                                }
                                KeyChar(kc) => {
                                    if let b'/' = self.buffer[kc] {
                                        if let Some(raw_token) =
                                            self.raw_tokens.get(self.raw_token_index + 2)
                                        {
                                            self.raw_token_index += 2;
                                            if let Text(start_index, end_index) = raw_token.kind {
                                                if let Some(front) = open_elements.pop_front() {
                                                    if matches!(
                                                        self.xml_tokens[front].kind,
                                                        XmlKind::OpenElement(..)
                                                    ) {
                                                        let open_element = self.xml_tokens[front]
                                                            .as_open_element_unchecked();
                                                        let name = open_element.0.to_owned();
                                                        let index = open_element.1;
                                                        let text = String::from_utf8_lossy(
                                                            &self.buffer[start_index..end_index],
                                                        )
                                                        .to_string();
                                                        if index != front || name != text {
                                                            self.xml_tokens.push(XmlToken {position, kind: XmlKind::Error(format!("Mismatch between closing {} and opening {} elements", text, name)), parent, });
                                                        }
                                                    }
                                                } else {
                                                    self.xml_tokens.push(XmlToken {
                                                        position,
                                                        kind: XmlKind::Error("Mismatch between closing and opening elements".into()),
                                                        parent,
                                                    });
                                                }
                                                let token = XmlToken {
                                                    kind: CloseElement(
                                                        String::from_utf8_lossy(
                                                            &self.buffer[start_index..end_index],
                                                        )
                                                        .to_string(),
                                                    ),
                                                    position,
                                                    parent,
                                                };
                                                self.xml_tokens.push(token);
                                                if (self.raw_token_index + 1)
                                                    >= self.raw_tokens.len()
                                                {
                                                    self.xml_tokens.push(XmlToken {
                                                        position,
                                                        kind: XmlKind::Error("Expected '>'".into()),
                                                        parent,
                                                    });
                                                    break;
                                                }
                                                while let Whitespace(..) =
                                                    self.raw_tokens[self.raw_token_index + 1].kind
                                                {
                                                    self.raw_token_index += 1;
                                                }
                                                match self.raw_tokens[self.raw_token_index + 1].kind
                                                {
                                                    KeyChar(index) => {
                                                        if self.buffer[index] != b'>' {
                                                            self.xml_tokens.push(XmlToken {
                                                                position,
                                                                kind: XmlKind::Error(
                                                                    "Expected '>'".into(),
                                                                ),
                                                                parent: open_elements
                                                                    .front()
                                                                    .copied(),
                                                            });
                                                            self.raw_token_index += 1;
                                                        }
                                                    }
                                                    _ => {
                                                        self.xml_tokens.push(XmlToken {
                                                            position,
                                                            kind: XmlKind::Error(
                                                                "Expected '>'".into(),
                                                            ),
                                                            parent,
                                                        });
                                                        self.raw_token_index += 1;
                                                    }
                                                }
                                            }
                                        }
                                    } else if let b'?' = self.buffer[kc] {
                                        self.raw_token_index += 2;
                                        if self.raw_token_index >= self.raw_tokens.len() {
                                            break;
                                        }
                                        let parent = open_elements.front().copied();
                                        if let Text(start_index, end_index) =
                                            self.raw_tokens[self.raw_token_index].kind
                                        {
                                            let text = String::from_utf8_lossy(
                                                &self.buffer[start_index..end_index],
                                            );
                                            if text == "xml" {
                                                let element_name = format!(
                                                    "?{}",
                                                    String::from_utf8_lossy(
                                                        &self.buffer[start_index..end_index]
                                                    )
                                                );
                                                let parent_index = self.xml_tokens.len();
                                                let token = XmlToken {
                                                    kind: OpenElement(element_name, parent_index),
                                                    position,
                                                    parent,
                                                };
                                                self.xml_tokens.push(token);
                                                open_elements.push_front(parent_index);
                                            }
                                        } else {
                                            self.xml_tokens.push(XmlToken {
                                                position,
                                                kind: XmlKind::Error("Expected 'xml'".into()),
                                                parent,
                                            });
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    b'/' | b'?' => {
                        if self.match_next_char(self.raw_token_index, '>') {
                            if let Some(front) = open_elements.pop_front() {
                                if let OpenElement(parent_name, _) = &self.xml_tokens[front].kind {
                                    let position = self.raw_tokens[self.raw_token_index].position;
                                    let token = XmlToken {
                                        kind: CloseElement(parent_name.into()),
                                        position,
                                        parent,
                                    };
                                    self.xml_tokens.push(token);
                                }
                            }
                        }
                    }
                    b'>' => {
                        let text_start = kc + 1;
                        let mut text_end = text_start;
                        while let Some(raw_token) = self.raw_tokens.get(self.raw_token_index + 1) {
                            match raw_token.kind {
                                Text(start_index, end_index)
                                | Whitespace(start_index, end_index) => {
                                    text_end += end_index - start_index;
                                }
                                KeyChar(kc) => match self.buffer[kc] {
                                    b'<' => {
                                        break;
                                    }
                                    _ => {
                                        text_end += 1;
                                    }
                                },
                            }
                            self.raw_token_index += 1;
                        }
                        let token = XmlToken {
                            kind: InnerText(
                                String::from_utf8_lossy(&self.buffer[text_start..text_end])
                                    .to_string(),
                            ),
                            position: raw_token.position,
                            parent,
                        };
                        self.xml_tokens.push(token);
                    }
                    _ => {}
                },
                _ => {}
            }
            self.raw_token_index += 1;
        }
        if let Some(last) = open_elements.iter().last() {
            self.xml_tokens.push(XmlToken {
                position: self.xml_tokens[*last].position,
                kind: XmlKind::Error(
                    "Mismatch between number of closing and opening elements".into(),
                ),
                parent: self.xml_tokens[*last].parent,
            });
        }
    }
}
