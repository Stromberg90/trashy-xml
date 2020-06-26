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

trait BytesToString {
    fn to_string(&self) -> String;
}

impl BytesToString for [u8] {
    #[inline(always)]
    fn to_string(&self) -> String {
        String::from_utf8_lossy(self).to_string()
    }
}

impl XmlToken {
    fn new_comment(str: String, position: FilePosition, parent: Option<usize>) -> XmlToken {
        XmlToken {
            kind: XmlKind::Comment(str),
            position,
            parent,
        }
    }

    fn new_attribute(
        name: String,
        value: String,
        position: FilePosition,
        parent: Option<usize>,
    ) -> XmlToken {
        XmlToken {
            kind: XmlKind::Attribute(name, value),
            position,
            parent,
        }
    }

    fn new_inner_text(str: String, position: FilePosition, parent: Option<usize>) -> XmlToken {
        XmlToken {
            kind: XmlKind::InnerText(str),
            position,
            parent,
        }
    }

    fn new_open_element(
        str: String,
        id: usize,
        position: FilePosition,
        parent: Option<usize>,
    ) -> XmlToken {
        XmlToken {
            kind: XmlKind::OpenElement(str, id),
            position,
            parent,
        }
    }

    fn new_close_element(str: String, position: FilePosition, parent: Option<usize>) -> XmlToken {
        XmlToken {
            kind: XmlKind::CloseElement(str),
            position,
            parent,
        }
    }

    fn new_error(str: String, position: FilePosition, parent: Option<usize>) -> XmlToken {
        XmlToken {
            kind: XmlKind::Error(str),
            position,
            parent,
        }
    }
}

struct RawTokens {
    tokens: Vec<Token>,
    index: usize,
}

impl RawTokens {
    fn new() -> Self {
        RawTokens {
            tokens: Vec::new(),
            index: 0,
        }
    }
}

struct Settings {
    tab_width: usize,
    ignore_comments: bool,
}

/// # Examples
///
/// ```
/// use trashy_xml::{XmlKind, XmlParser};
///
/// let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
/// parser.parse();
/// for token in &parser.xml_tokens {
///     if let XmlKind::OpenElement(name, _) = &token.kind {
///         if name == "element" {
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
    index: usize,
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

trait TempIter {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
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
        match File::open(filepath) {
            Ok(mut file) => {
                let mut buffer = Vec::new();
                match file.read_to_end(&mut buffer) {
                    Ok(_) => Ok(Self {
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
            Err(e) => Err(XmlParserError(e.to_string())),
        }
    }

    pub fn ignore_comments(&mut self, ignore_comments: bool) -> &mut XmlParser {
        self.settings.ignore_comments = ignore_comments;
        self
    }

    pub fn tab_width(&mut self, tab_width: usize) -> &mut XmlParser {
        self.settings.tab_width = tab_width;
        self
    }

    fn match_next_str(&mut self, characters: &str) -> bool {
        let chars = characters.chars();
        let chars_count = chars.clone().count();
        if self.raw_tokens.index + chars_count < self.raw_tokens.tokens.len() {
            for (token, character) in self.raw_tokens.tokens
                [self.raw_tokens.index + 1..=self.raw_tokens.index + chars_count]
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
        self.raw_tokens.index += chars_count;
        true
    }

    fn match_next_char(&mut self, character: char) -> bool {
        if let Some(token) = self.raw_tokens.tokens.get(self.raw_tokens.index + 1) {
            if let TokenKind::KeyChar(kc) = token.kind {
                if self.buffer[kc] as char == character {
                    self.raw_tokens.index += 1;
                    return true;
                }
            }
        }
        false
    }

    /// Returns references to children tokens
    ///
    /// ```
    /// use trashy_xml::{XmlKind, XmlParser};
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

    /// Returns references to attributes tokens
    ///
    /// ```
    /// use trashy_xml::{XmlKind, XmlParser};
    ///
    /// let mut parser = XmlParser::new("sample_files/small.xml").unwrap();
    /// parser.parse();
    /// for token in &parser.xml_tokens {
    ///    for attribute in parser.attributes(token) {
    ///    }
    /// }
    /// ```
    pub fn attributes(&self, token: &XmlToken) -> Vec<&XmlToken> {
        let mut result = Vec::<&XmlToken>::new();
        if let XmlKind::OpenElement(_, i) = &token.kind {
            for token in self.xml_tokens.iter() {
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

    /// Returns references to siblings tokens
    ///
    /// ```
    /// use trashy_xml::{XmlKind, XmlParser};
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

    pub fn parse(&mut self) {
        use TokenKind::*;
        use XmlKind::*;

        self.raw_tokens.tokens = self.collect();

        let mut open_elements = VecDeque::<usize>::new();
        while let Some(raw_token) = self.raw_tokens.tokens.get(self.raw_tokens.index) {
            let parent = open_elements.front().copied();
            match raw_token.kind {
                Text(start_index, end_index) => {
                    self.raw_tokens.index += 1;
                    if open_elements.is_empty() {
                        self.xml_tokens.push(XmlToken::new_error(
                            "Document is empty".into(),
                            raw_token.position,
                            None,
                        ));
                        continue;
                    }
                    while let Some(raw_token) = self.raw_tokens.tokens.get(self.raw_tokens.index) {
                        self.raw_tokens.index += 1;
                        match raw_token.kind {
                            KeyChar(kc) => {
                                if self.buffer[kc] == b'=' {
                                    break;
                                }
                            }
                            Text(..) => {
                                self.xml_tokens.push(XmlToken::new_error(
                                    format!(
                                        "Specification mandates value for attribute {}",
                                        &self.buffer[start_index..end_index].to_string()
                                    ),
                                    raw_token.position,
                                    parent,
                                ));
                                break;
                            }
                            _ => {}
                        }
                    }
                    while let Some(raw_token) = self.raw_tokens.tokens.get(self.raw_tokens.index) {
                        match raw_token.kind {
                            KeyChar(kc) => {
                                if self.buffer[kc] == b'"' || self.buffer[kc] == b'\'' {
                                    break;
                                }
                            }
                            Text(..) => {
                                self.xml_tokens.push(XmlToken::new_error(
                                    "\" or \' expected".into(),
                                    raw_token.position,
                                    parent,
                                ));
                                break;
                            }
                            _ => {}
                        }
                        self.raw_tokens.index += 1;
                    }
                    if let Some(token) = self.raw_tokens.tokens.get(self.raw_tokens.index) {
                        if let KeyChar(attribute_value_start) = token.kind {
                            let boundary_character = self.buffer[attribute_value_start];
                            let attribute_value_start = attribute_value_start + 1;
                            let mut attribute_value_end = attribute_value_start;
                            loop {
                                self.raw_tokens.index += 1;
                                if let Some(token) =
                                    self.raw_tokens.tokens.get(self.raw_tokens.index)
                                {
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
                            let token = XmlToken::new_attribute(
                                self.buffer[start_index..end_index].to_string(),
                                self.buffer[attribute_value_start..attribute_value_end].to_string(),
                                raw_token.position,
                                parent,
                            );
                            self.xml_tokens.push(token);
                        }
                    }
                }
                KeyChar(kc) => match self.buffer[kc] {
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
                                        self.xml_tokens.push(XmlToken::new_error(
                                            "-- is not permitted within comments".into(),
                                            position,
                                            parent,
                                        ));
                                    }
                                }
                                self.raw_tokens.index += 1;
                            }
                            if !self.settings.ignore_comments {
                                let token = XmlToken::new_comment(
                                    self.buffer[comment_start..comment_end].to_string(),
                                    position,
                                    parent,
                                );
                                self.xml_tokens.push(token);
                            }
                        } else if let Some(raw_token) =
                            self.raw_tokens.tokens.get(self.raw_tokens.index + 1)
                        {
                            let position = raw_token.position;
                            match raw_token.kind {
                                Text(start_index, end_index) => {
                                    let token = XmlToken::new_open_element(
                                        self.buffer[start_index..end_index].to_string(),
                                        self.xml_tokens.len(),
                                        position,
                                        parent,
                                    );
                                    open_elements.push_front(self.xml_tokens.len());
                                    self.xml_tokens.push(token);
                                    self.raw_tokens.index += 1;
                                }
                                KeyChar(kc) => {
                                    if let b'/' = self.buffer[kc] {
                                        if let Some(raw_token) =
                                            self.raw_tokens.tokens.get(self.raw_tokens.index + 2)
                                        {
                                            self.raw_tokens.index += 2;
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
                                                        let text = self.buffer
                                                            [start_index..end_index]
                                                            .to_string();
                                                        if index != front || name != text {
                                                            self.xml_tokens.push(XmlToken::new_error(format!("Mismatch between closing {} and opening {} elements", text, name), position, parent));
                                                        }
                                                    }
                                                } else {
                                                    self.xml_tokens.push(XmlToken::new_error("Mismatch between closing and opening elements".into(), position, parent));
                                                }
                                                let token = XmlToken::new_close_element(
                                                    self.buffer[start_index..end_index].to_string(),
                                                    position,
                                                    parent,
                                                );
                                                self.xml_tokens.push(token);
                                                if (self.raw_tokens.index + 1)
                                                    >= self.raw_tokens.tokens.len()
                                                {
                                                    self.xml_tokens.push(XmlToken::new_error(
                                                        "Expected '>'".into(),
                                                        position,
                                                        parent,
                                                    ));
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
                                                        if self.buffer[index] != b'>' {
                                                            self.xml_tokens.push(
                                                                XmlToken::new_error(
                                                                    "Expected '>'".into(),
                                                                    position,
                                                                    open_elements.front().copied(),
                                                                ),
                                                            );
                                                            self.raw_tokens.index += 1;
                                                        }
                                                    }
                                                    _ => {
                                                        self.xml_tokens.push(XmlToken::new_error(
                                                            "Expected '>'".into(),
                                                            position,
                                                            parent,
                                                        ));
                                                        self.raw_tokens.index += 1;
                                                    }
                                                }
                                            }
                                        }
                                    } else if let b'?' = self.buffer[kc] {
                                        self.raw_tokens.index += 2;
                                        if self.raw_tokens.index >= self.raw_tokens.tokens.len() {
                                            break;
                                        }
                                        let parent = open_elements.front().copied();
                                        if let Text(start_index, end_index) =
                                            self.raw_tokens.tokens[self.raw_tokens.index].kind
                                        {
                                            let text =
                                                self.buffer[start_index..end_index].to_string();
                                            if text == "xml" {
                                                let element_name = format!(
                                                    "?{}",
                                                    self.buffer[start_index..end_index].to_string()
                                                );
                                                let parent_index = self.xml_tokens.len();
                                                let token = XmlToken::new_open_element(
                                                    element_name,
                                                    parent_index,
                                                    position,
                                                    parent,
                                                );
                                                self.xml_tokens.push(token);
                                                open_elements.push_front(parent_index);
                                            }
                                        } else {
                                            self.xml_tokens.push(XmlToken::new_error(
                                                "Expected 'xml'".into(),
                                                position,
                                                parent,
                                            ));
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
                                if let OpenElement(parent_name, _) = &self.xml_tokens[front].kind {
                                    let position =
                                        self.raw_tokens.tokens[self.raw_tokens.index].position;
                                    let token = XmlToken::new_close_element(
                                        parent_name.into(),
                                        position,
                                        parent,
                                    );
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
                                KeyChar(kc) => match self.buffer[kc] {
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
                        let token = XmlToken::new_inner_text(
                            self.buffer[text_start..text_end].to_string(),
                            raw_token.position,
                            parent,
                        );
                        self.xml_tokens.push(token);
                    }
                    _ => {}
                },
                _ => {}
            }
            self.raw_tokens.index += 1;
        }
        if let Some(last) = open_elements.iter().last() {
            self.xml_tokens.push(XmlToken::new_error(
                "Mismatch between number of closing and opening elements".into(),
                self.xml_tokens[*last].position,
                self.xml_tokens[*last].parent,
            ));
        }
    }
}
