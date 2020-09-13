mod lexer;
pub mod tokens;

use crate::tokens::RawTokens;
use crate::tokens::TokenKind;
use crate::tokens::XmlToken;
use std::char;
use std::collections::VecDeque;
use std::path::Path;
use std::str::FromStr;

use smartstring::alias::String;
use tokens::{FilePosition, StringSpan, Token, XmlKind};

trait BytesToString {
    fn to_string(&self) -> String;
}

impl BytesToString for [u8] {
    fn to_string(&self) -> String {
        std::string::String::from_utf8_lossy(self)
            .to_string()
            .into()
    }
}

struct Settings {
    tab_width: u8,
    ignore_comments: bool,
}

/// # Examples
///
/// ```
/// use trashy_xml::{tokens::XmlKind, XmlParser};
///
/// let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
/// parser.parse();
/// for token in &parser.xml_tokens {
///     if let XmlKind::OpenElement(name, _) = &token.kind {
///         if parser.str(name) == "element" {
///             for attribute in parser.attributes(token) {
///             }
///         }
///     }
/// }
/// ```
pub struct XmlParser {
    settings: Settings,
    started_parsing: bool,
    position: FilePosition,
    index: u32,
    buffer: Vec<u8>,
    raw_tokens: RawTokens,
    pub xml_tokens: Vec<XmlToken>,
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
pub struct XmlParserError(std::string::String);

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
            settings: Settings {
                tab_width: 4,
                ignore_comments: true,
            },
            index: 0,
            started_parsing: false,
            position: FilePosition::new(),
            buffer: input.into_bytes(),
            raw_tokens: RawTokens::new(),
            xml_tokens: Vec::new(),
        })
    }
}

impl XmlParser {
    /// Constructs a new `XmlParser`.
    pub fn new<P: AsRef<Path>>(filepath: P) -> Result<Self, XmlParserError> {
        match std::fs::read(filepath) {
            Ok(buffer) => Ok(Self {
                settings: Settings {
                    tab_width: 4,
                    ignore_comments: true,
                },
                index: 0,
                started_parsing: false,
                position: FilePosition::new(),
                buffer,
                raw_tokens: RawTokens::new(),
                xml_tokens: Vec::new(),
            }),
            Err(e) => Err(XmlParserError(e.to_string())),
        }
    }

    pub fn ignore_comments(&mut self, ignore_comments: bool) -> &mut XmlParser {
        self.settings.ignore_comments = ignore_comments;
        self
    }

    pub fn tab_width(&mut self, tab_width: u8) -> &mut XmlParser {
        self.settings.tab_width = tab_width;
        self
    }

    fn match_next_str(&mut self, characters: &str) -> bool {
        let chars = characters.chars();
        let chars_count = chars.clone().count();
        if self.raw_tokens.index + chars_count < self.raw_tokens.tokens.len() {
            if !self.raw_tokens.tokens
                [self.raw_tokens.index + 1..=self.raw_tokens.index + chars_count]
                .iter()
                .zip(chars)
                .all(|(t, c)| {
                    if let TokenKind::KeyChar(kc) = t.kind {
                        if self.buffer[kc as usize] as char != c {
                            return false;
                        }
                    } else {
                        return false;
                    }
                    true
                })
            {
                return false;
            }
        } else {
            return false;
        }
        self.raw_tokens.index += chars_count;
        true
    }

    fn match_next_char(&mut self, character: char) -> bool {
        if let Some(token) = self.raw_tokens.tokens.get(self.raw_tokens.index + 1) {
            if let TokenKind::KeyChar(kc) = token.kind {
                if self.buffer[kc as usize] as char == character {
                    self.raw_tokens.index += 1;
                    return true;
                }
            }
        }
        false
    }

    /// Returns references to child tokens
    ///
    /// ```
    /// use trashy_xml::{tokens::XmlKind, XmlParser};
    ///
    /// let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
    /// parser.parse();
    /// for token in &parser.xml_tokens {
    ///    for child in parser.children(token) {
    ///    }
    /// }
    /// ```
    pub fn children(&self, token: &XmlToken) -> Vec<&XmlToken> {
        let mut result = Vec::<&XmlToken>::new();
        if let XmlKind::OpenElement(_, i) = &token.kind {
            for token in self.xml_tokens.iter() {
                if let Some(parent) = token.parent {
                    if parent == *i {
                        result.push(token);
                    }
                }
            }
        }
        result
    }

    /// Returns references to attribute tokens
    ///
    /// ```
    /// use trashy_xml::{tokens::XmlKind, XmlParser};
    ///
    /// let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
    /// parser.parse();
    /// for token in &parser.xml_tokens {
    ///    for attribute in parser.attributes(token) {
    ///    }
    /// }
    /// ```
    pub fn attributes(&self, token: &XmlToken) -> Vec<&XmlToken> {
        if let XmlKind::OpenElement(_, i) = &token.kind {
            self.xml_tokens
                .iter()
                .filter(|t| t.parent == Some(*i) && t.is_attribute())
                .collect()
        } else {
            Vec::new()
        }
    }

    /// Returns references to sibling tokens
    ///
    /// ```
    /// use trashy_xml::{tokens::XmlKind, XmlParser};
    ///
    /// let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
    /// parser.parse();
    /// for token in &parser.xml_tokens {
    ///    for sibling in parser.siblings(token) {
    ///    }
    /// }
    /// ```
    pub fn siblings(&self, token: &XmlToken) -> Vec<&XmlToken> {
        let mut result = Vec::<&XmlToken>::new();
        if let Some(token_parent) = &token.parent {
            for token in self.xml_tokens.iter() {
                if let Some(parent) = token.parent {
                    if parent == *token_parent {
                        result.push(token);
                    }
                }
            }
        }
        result
    }

    pub fn str(&self, span: &StringSpan) -> &str {
        std::str::from_utf8(&self.buffer[span.start()..span.end()]).unwrap()
    }

    pub fn parse(&mut self) {
        use TokenKind::*;
        use XmlKind::*;

        self.raw_tokens.tokens = self.collect();

        let mut open_elements = VecDeque::<u32>::new();
        while let Some(raw_token) = self.raw_tokens.tokens.get(self.raw_tokens.index) {
            let parent = open_elements.front().copied();
            match raw_token.kind {
                Text(start_index, end_index) => {
                    self.raw_tokens.index += 1;
                    if open_elements.is_empty() {
                        self.xml_tokens.push(
                            XmlToken::error("Document is empty".into())
                                .position(raw_token.position),
                        );
                        continue;
                    }
                    while let Some(raw_token) = self.raw_tokens.tokens.get(self.raw_tokens.index) {
                        self.raw_tokens.index += 1;
                        match raw_token.kind {
                            KeyChar(kc) => {
                                if self.buffer[kc as usize] == b'=' {
                                    break;
                                }
                            }
                            Text(..) => {
                                self.xml_tokens.push(
                                    XmlToken::error(
                                        format!(
                                            "Specification mandates value for attribute {}",
                                            &self.buffer[start_index as usize..end_index as usize]
                                                .to_string()
                                        )
                                        .into(),
                                    )
                                    .position(raw_token.position)
                                    .parent(parent),
                                );
                                break;
                            }
                            _ => {}
                        }
                    }
                    while let Some(raw_token) = self.raw_tokens.tokens.get(self.raw_tokens.index) {
                        match raw_token.kind {
                            KeyChar(kc) => {
                                if self.buffer[kc as usize] == b'"'
                                    || self.buffer[kc as usize] == b'\''
                                {
                                    break;
                                }
                            }
                            Text(..) => {
                                self.xml_tokens.push(
                                    XmlToken::error("\" or \' expected".into())
                                        .position(raw_token.position)
                                        .parent(parent),
                                );
                                break;
                            }
                            _ => {}
                        }
                        self.raw_tokens.index += 1;
                    }
                    if let Some(token) = self.raw_tokens.tokens.get(self.raw_tokens.index) {
                        if let KeyChar(attribute_value_start) = token.kind {
                            let boundary_character = self.buffer[attribute_value_start as usize];
                            let attribute_value_start = attribute_value_start + 1;
                            let mut attribute_value_end = attribute_value_start;
                            loop {
                                self.raw_tokens.index += 1;
                                if let Some(token) =
                                    self.raw_tokens.tokens.get(self.raw_tokens.index)
                                {
                                    match token.kind {
                                        KeyChar(key_char_index) => {
                                            if self.buffer[key_char_index as usize]
                                                == boundary_character
                                            {
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
                            let token = XmlToken::attribute(
                                StringSpan::new(start_index, end_index),
                                StringSpan::new(attribute_value_start, attribute_value_end),
                            )
                            .position(raw_token.position)
                            .parent(parent);
                            self.xml_tokens.push(token);
                        }
                    }
                }
                KeyChar(kc) => match self.buffer[kc as usize] {
                    b'<' => {
                        if self.match_next_str("!--") {
                            self.raw_tokens.index += 1;
                            let position = self.raw_tokens.tokens[self.raw_tokens.index].position;
                            let comment_start = kc + 4;
                            let mut comment_end = comment_start;
                            while let Some(raw_token) =
                                self.raw_tokens.tokens.get(self.raw_tokens.index + 1)
                            {
                                if !self.settings.ignore_comments {
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
                                if self.match_next_str("--") {
                                    if self.match_next_char('>') {
                                        break;
                                    }
                                    if !self.settings.ignore_comments {
                                        self.xml_tokens.push(
                                            XmlToken::error(
                                                "-- is not permitted within comments".into(),
                                            )
                                            .position(position)
                                            .parent(parent),
                                        );
                                    }
                                }
                                self.raw_tokens.index += 1;
                            }
                            if !self.settings.ignore_comments {
                                let token =
                                    XmlToken::comment(StringSpan::new(comment_start, comment_end))
                                        .position(position)
                                        .parent(parent);
                                self.xml_tokens.push(token);
                            }
                        } else if let Some(raw_token) =
                            self.raw_tokens.tokens.get(self.raw_tokens.index + 1)
                        {
                            let position = raw_token.position;
                            match raw_token.kind {
                                Text(start_index, end_index) => {
                                    let token = XmlToken::open_element(
                                        StringSpan::new(start_index, end_index),
                                        self.xml_tokens.len() as u32,
                                    )
                                    .position(position)
                                    .parent(parent);
                                    open_elements.push_front(self.xml_tokens.len() as u32);
                                    self.xml_tokens.push(token);
                                    self.raw_tokens.index += 1;
                                }
                                KeyChar(kc) => {
                                    if let b'/' = self.buffer[kc as usize] {
                                        if let Some(raw_token) =
                                            self.raw_tokens.tokens.get(self.raw_tokens.index + 2)
                                        {
                                            self.raw_tokens.index += 2;
                                            if let Text(start_index, end_index) = raw_token.kind {
                                                if let Some(front) = open_elements.pop_front() {
                                                    if let XmlKind::OpenElement(name, index) =
                                                        &self.xml_tokens[front as usize].kind
                                                    {
                                                        let text = self.buffer[start_index as usize
                                                            ..end_index as usize]
                                                            .to_string();
                                                        if *index != front as u32
                                                            || self.str(name) != &text
                                                        {
                                                            let token = XmlToken::error(format!("Mismatch between closing {} and opening {} elements", text, self.str(name)).into())
                                                            .position(position)
                                                            .parent(parent);
                                                            self.xml_tokens.push(token);
                                                        }
                                                    }
                                                } else {
                                                    self.xml_tokens.push(XmlToken::error("Mismatch between closing and opening elements".into()).position(position).parent(parent));
                                                }
                                                let token = XmlToken::close_element(
                                                    StringSpan::new(start_index, end_index),
                                                )
                                                .position(position)
                                                .parent(parent);
                                                self.xml_tokens.push(token);
                                                if (self.raw_tokens.index + 1)
                                                    >= self.raw_tokens.tokens.len()
                                                {
                                                    self.xml_tokens.push(
                                                        XmlToken::error("Expected '>'".into())
                                                            .position(position)
                                                            .parent(parent),
                                                    );
                                                    break;
                                                }
                                                while let Whitespace(..) = self.raw_tokens.tokens
                                                    [self.raw_tokens.index + 1]
                                                    .kind
                                                {
                                                    self.raw_tokens.index += 1;
                                                }
                                                match self.raw_tokens.tokens
                                                    [self.raw_tokens.index + 1]
                                                    .kind
                                                {
                                                    KeyChar(index) => {
                                                        if self.buffer[index as usize] != b'>' {
                                                            self.xml_tokens.push(
                                                                XmlToken::error(
                                                                    "Expected '>'".into(),
                                                                )
                                                                .position(position)
                                                                .parent(
                                                                    open_elements.front().copied(),
                                                                ),
                                                            );
                                                            self.raw_tokens.index += 1;
                                                        }
                                                    }
                                                    _ => {
                                                        self.xml_tokens.push(
                                                            XmlToken::error("Expected '>'".into())
                                                                .position(position)
                                                                .parent(parent),
                                                        );
                                                        self.raw_tokens.index += 1;
                                                    }
                                                }
                                            }
                                        }
                                    } else if let b'?' = self.buffer[kc as usize] {
                                        self.raw_tokens.index += 2;
                                        if self.raw_tokens.index >= self.raw_tokens.tokens.len() {
                                            break;
                                        }
                                        let parent = open_elements.front().copied();
                                        if let Text(start_index, end_index) =
                                            self.raw_tokens.tokens[self.raw_tokens.index].kind
                                        {
                                            let text = self.buffer
                                                [start_index as usize..end_index as usize]
                                                .to_string();
                                            if text.as_str() == "xml" {
                                                let parent_index = self.xml_tokens.len() as u32;
                                                let token = XmlToken::open_element(
                                                    StringSpan::new(start_index, end_index),
                                                    parent_index,
                                                )
                                                .position(position)
                                                .parent(parent);
                                                self.xml_tokens.push(token);
                                                open_elements.push_front(parent_index);
                                            }
                                        } else {
                                            self.xml_tokens.push(
                                                XmlToken::error("Expected 'xml'".into())
                                                    .position(position)
                                                    .parent(parent),
                                            );
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    b'/' | b'?' => {
                        if self.match_next_char('>') {
                            self.raw_tokens.index -= 1;
                            if let Some(front) = open_elements.pop_front() {
                                if let OpenElement(parent_name, _) =
                                    &self.xml_tokens[front as usize].kind
                                {
                                    let position =
                                        self.raw_tokens.tokens[self.raw_tokens.index].position;
                                    let token = XmlToken::close_element(parent_name.clone())
                                        .position(position)
                                        .parent(parent);
                                    self.xml_tokens.push(token);
                                }
                            }
                        }
                    }
                    b'>' => {
                        let text_start = kc + 1;
                        let mut text_end = text_start;
                        while let Some(raw_token) =
                            self.raw_tokens.tokens.get(self.raw_tokens.index + 1)
                        {
                            match raw_token.kind {
                                Text(start_index, end_index)
                                | Whitespace(start_index, end_index) => {
                                    text_end += end_index - start_index;
                                }
                                KeyChar(kc) => match self.buffer[kc as usize] {
                                    b'<' => {
                                        break;
                                    }
                                    _ => {
                                        text_end += 1;
                                    }
                                },
                            }
                            self.raw_tokens.index += 1;
                        }

                        self.xml_tokens.push(
                            XmlToken::inner_text(StringSpan::new(text_start, text_end))
                                .position(raw_token.position)
                                .parent(parent),
                        );
                    }
                    _ => {}
                },
                _ => {}
            }
            self.raw_tokens.index += 1;
        }
        if let Some(last) = open_elements.iter().last() {
            self.xml_tokens.push(
                XmlToken::error("Mismatch between number of closing and opening elements".into())
                    .position(self.xml_tokens[*last as usize].position)
                    .parent(self.xml_tokens[*last as usize].parent),
            );
        }
    }
}
