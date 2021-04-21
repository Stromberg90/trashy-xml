//! A non-spec compliant xml parser that does not stop parsing when encountering errors.
//! # Examples
//!
//! ```
//! use trashy_xml::XmlParser;
//!
//! // Gets each open element matching "this_element"
//! // then prints the debug representation of its attributes.
//! let parsed = XmlParser::str("<this_element attribute=\"value\" />").parse();
//! for token in parsed.elements_from_name("this_element") {
//!     dbg!(token.attributes());
//! }
//! ```

#![warn(
    missing_debug_implementations,
    rust_2018_idioms,
    missing_doc_code_examples
)]

use lazy_static::lazy_static;
use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

mod lexer;
/// The different types returned by the parser.
pub mod tokens;

use crate::tokens::{TokenKind, XmlToken};
use rustc_hash::FxHashMap;
use std::ops::Range;
use std::{
    borrow::Cow, cell::RefCell, collections::VecDeque, iter::Peekable, path::Path, rc::Rc,
    slice::Iter, str, string::String,
};

use tokens::{FilePosition, OpenElement, Token, XmlError};

/// Struct with settings that's used during parsing.
#[derive(Debug, Clone)]
pub struct Settings {
    /// Sets if the parser will make comment tokens or not.
    pub ignore_comments: bool,
    /// Sets if the parser will put the tokens into a map, where one can get a token from file position.
    pub create_position_map: bool,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            ignore_comments: true,
            create_position_map: false,
        }
    }
}

/// The main parser struct.
#[derive(Debug)]
pub struct XmlParser {
    settings: Settings,
    buffer: Vec<u8>,
    raw_index: usize,
    pub(crate) raw_tokens: Vec<Token>,
}

type RefXmlToken<'a> = Rc<RefCell<XmlToken<'a>>>;

/// Formatted error message token.
#[derive(Debug, Default)]
pub struct FmtXmlError {
    pub error: String,
    pub position: FilePosition,
}

impl FmtXmlError {
    pub(crate) fn new<S>(str: S, position: FilePosition) -> Self
    where
        S: Into<String>,
    {
        FmtXmlError {
            error: str.into(),
            position,
        }
    }
}

/// Struct returned after calling [XmlParser::parse()].
#[derive(Debug, Default)]
pub struct ParsedXml<'a> {
    /// Vector with the tokens.
    pub tokens: Vec<RefXmlToken<'a>>,
    token_map: FxHashMap<usize, FxHashMap<Range<usize>, RefXmlToken<'a>>>,
    open_elements: FxHashMap<String, Vec<RefXmlToken<'a>>>,
    /// Vector with error tokens.
    pub errors: Vec<FmtXmlError>,
    create_position_map: bool,
}

impl<'a> ParsedXml<'a> {
    /// Takes a name and returns all open elements matching that name.
    pub fn elements_from_name(&self, name: &str) -> Vec<OpenElement<'a>> {
        self.open_elements
            .get(name)
            .unwrap_or(&Vec::new())
            .iter()
            .map(|e| e.borrow().as_open_element().clone())
            .collect::<Vec<_>>()
    }

    /// Returns vector with all open elements.
    pub fn elements(&self) -> Vec<OpenElement<'a>> {
        self.open_elements
            .values()
            .into_iter()
            .flatten()
            .map(|e| e.borrow().as_open_element().clone())
            .collect()
    }

    fn insert_into_map(&mut self, position: FilePosition, token: RefXmlToken<'a>, length: usize) {
        assert!(self.create_position_map);
        if let Some(l) = self.token_map.get_mut(&position.line) {
            l.insert(
                Range {
                    start: position.column as usize,
                    end: position.column as usize + length + 1,
                },
                Rc::clone(&token),
            );
        } else {
            self.token_map.insert(position.line, FxHashMap::default());
            self.token_map.get_mut(&position.line).unwrap().insert(
                Range {
                    start: position.column,
                    end: position.column as usize + length + 1,
                },
                Rc::clone(&token),
            );
        }
    }

    fn push_error(&mut self, error: XmlError, settings: &Settings) {
        match error {
            XmlError::EmptyDocument(p) => {
                self.errors.push(FmtXmlError::new("Document is empty", p));
            }
            XmlError::Expected(c, p) => {
                self.errors
                    .push(FmtXmlError::new(format!("Expected {}", c), p));
            }
            XmlError::NotPermittedInComments(p) => {
                self.errors
                    .push(FmtXmlError::new("-- is not permitted within comments", p));
            }
            XmlError::OpenCloseElementsMismatch(p) => {
                self.errors.push(FmtXmlError::new(
                    "Mismatch between closing and opening elements",
                    p,
                ));
            }
            XmlError::OpenCloseElementMismatch(s1, s2, p) => {
                self.errors.push(FmtXmlError::new(
                    format!(
                        "Mismatch between closing {} and opening {} elements",
                        s1, s2
                    ),
                    p,
                ));
            }
            XmlError::Unescaped(c, s, p, pa, ep) => {
                self.errors.push(FmtXmlError::new(
                    format!("Unescaped {} not allowed in attribute values", c),
                    ep,
                ));
                let token = XmlToken::invalid_attribute(
                    s,
                    p,
                    pa.map(|p| Rc::clone(self.tokens.get(p).unwrap())),
                );
                self.push_attribute(token, pa, settings);
            }
            XmlError::MissingValue(s, p, pa) => {
                self.errors.push(FmtXmlError::new(
                    format!("Specification mandates value for attribute {}", s),
                    p,
                ));
                let token = XmlToken::invalid_attribute(
                    s,
                    p,
                    pa.map(|p| Rc::clone(self.tokens.get(p).unwrap())),
                );
                self.push_attribute(token, pa, settings);
            }
            XmlError::QuoteExpected(s, p, pa) => {
                self.errors.push(FmtXmlError::new("\" or \' expected", p));
                let token = XmlToken::invalid_attribute(
                    s,
                    p,
                    pa.map(|p| Rc::clone(self.tokens.get(p).unwrap())),
                );
                self.push_attribute(token, pa, settings);
            }
            XmlError::ElementMustBeFollowedBy(s, p) => {
                self.errors.push(FmtXmlError::new(format!("Element \"{}\" must be followed by either attribute specifications, \">\" or \"/>\"", s), p));
            }
        }
    }

    fn push_open_element(
        &mut self,
        token: XmlToken<'a>,
        parent: Option<usize>,
        settings: &Settings,
    ) {
        let token = new_rc_refcell(token);
        if let Some(parent) = parent {
            if let Some(p) = self.tokens.get_mut(parent) {
                let mut p_token = p.borrow_mut();
                p_token
                    .as_mut_open_element()
                    .children
                    .push(Rc::clone(&token));
            }
        }
        {
            let t = token.borrow();
            let t = t.as_open_element();
            if let Some(vec) = self.open_elements.get_mut(&t.name.to_string()) {
                vec.push(Rc::clone(&token));
            } else {
                self.open_elements
                    .insert(t.name.to_string(), vec![Rc::clone(&token)]);
            }
            if settings.create_position_map {
                self.insert_into_map(t.position, Rc::clone(&token), t.name.len());
            }
        }
        self.tokens.push(token);
    }

    fn push_close_element(
        &mut self,
        token: XmlToken<'a>,
        parent: Option<usize>,
        settings: &Settings,
    ) {
        let token = new_rc_refcell(token);
        if let Some(parent) = parent {
            if let Some(p) = self.tokens.get_mut(parent) {
                let mut p_token = p.borrow_mut();
                p_token
                    .as_mut_open_element()
                    .children
                    .push(Rc::clone(&token));
            }
        }
        {
            let t = token.borrow();
            let t = t.as_close_element();
            if settings.create_position_map {
                self.insert_into_map(
                    t.position,
                    Rc::clone(&token),
                    t.name.as_ref().map_or(0, |t| t.len()),
                );
            }
        }
        self.tokens.push(token);
    }

    fn push_attribute(&mut self, token: XmlToken<'a>, parent: Option<usize>, settings: &Settings) {
        let token = new_rc_refcell(token);
        {
            let token_borrowed = token.borrow();
            let attribute = token_borrowed.as_attribute();
            if let Some(parent) = parent {
                if let Some(p) = self.tokens.get_mut(parent) {
                    let mut p_token = p.borrow_mut();
                    p_token
                        .as_mut_open_element()
                        .children
                        .push(Rc::clone(&token));
                }
            }
            if let Some(parent) = parent {
                if let Some(p) = self.tokens.get_mut(parent) {
                    let mut attributes = p.borrow_mut();
                    if let Some(attrs) = attributes
                        .as_mut_open_element()
                        .attributes
                        .get_mut(&attribute.key.0)
                    {
                        attrs.push(Rc::clone(&token));
                    } else {
                        attributes
                            .as_mut_open_element()
                            .attributes
                            .insert(attribute.key.0.to_string(), vec![Rc::clone(&token)]);
                    }
                }
            }
            if settings.create_position_map {
                self.insert_into_map(attribute.key.1, Rc::clone(&token), attribute.key.0.len());
                if let Some(value) = &attribute.value {
                    self.insert_into_map(value.1, Rc::clone(&token), value.0.len());
                } else {
                    self.insert_into_map(attribute.key.1, Rc::clone(&token), 1);
                }
            }
        }
        self.tokens.push(token);
    }

    fn push_comment(&mut self, token: XmlToken<'a>, parent: Option<usize>, settings: &Settings) {
        let token = new_rc_refcell(token);
        if !settings.ignore_comments {
            if let Some(parent) = parent {
                if let Some(p) = self.tokens.get_mut(parent) {
                    let mut p_token = p.borrow_mut();
                    p_token
                        .as_mut_open_element()
                        .children
                        .push(Rc::clone(&token));
                }
            }
            if settings.create_position_map {
                let t = token.borrow();
                let t = t.as_comment();
                self.insert_into_map(t.position, Rc::clone(&token), t.string.len());
            }
        }
        self.tokens.push(token);
    }

    fn push_inner_text(&mut self, token: XmlToken<'a>, parent: Option<usize>, settings: &Settings) {
        let token = new_rc_refcell(token);
        if let Some(parent) = parent {
            if let Some(p) = self.tokens.get_mut(parent) {
                let mut p_token = p.borrow_mut();
                p_token
                    .as_mut_open_element()
                    .children
                    .push(Rc::clone(&token));
            }
        }
        if settings.create_position_map {
            let t = token.borrow();
            let t = t.as_inner_text();
            self.insert_into_map(t.position, Rc::clone(&token), t.string.len());
        }
        self.tokens.push(token);
    }

    pub fn token_from_position(&self, position: FilePosition) -> Option<XmlToken<'a>> {
        assert!(self.create_position_map);
        if let Some(line) = self.token_map.get(&position.line) {
            for (range, token) in line {
                if range.contains(&position.column) {
                    return Some(token.borrow().clone());
                }
            }
        }
        None
    }
}

lazy_static! {
    static ref KEY_CHARS: Vec<bool> = {
        let mut m = vec![false; u8::MAX as usize];
        m[b'<' as usize] = true;
        m[b'>' as usize] = true;
        m[b'/' as usize] = true;
        m[b'=' as usize] = true;
        m[b'"' as usize] = true;
        m[b'\'' as usize] = true;
        m[b'-' as usize] = true;
        m[b'!' as usize] = true;
        m[b'?' as usize] = true;
        m
    };
}

#[inline]
fn new_rc_refcell<T>(t: T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(t))
}

#[derive(Debug)]
struct Strings {
    strings: Vec<String>,
    map: FxHashMap<String, usize>,
}

impl Default for Strings {
    fn default() -> Self {
        Self {
            strings: Vec::with_capacity(32),
            map: FxHashMap::default(),
        }
    }
}

impl Strings {
    fn get_index_or_insert(&mut self, string: &str) -> usize {
        if let Some(a) = self.map.get(string) {
            *a
        } else {
            let index = self.strings.len();
            self.map.insert(string.to_string(), self.strings.len());
            self.strings.push(string.to_string());
            index
        }
    }

    #[inline]
    fn get(&self, index: usize) -> Cow<'_, str> {
        Cow::Borrowed(&self.strings[index])
    }
}

struct Tokenizer<'a> {
    position: FilePosition,
    strings: Strings,
    buffer: Peekable<Iter<'a, u8>>,
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(v) = lexer::next(self) {
            let position = self.position;
            if KEY_CHARS[v as usize] {
                return Some(Token {
                    position,
                    kind: TokenKind::KeyChar(v),
                });
            }
            let mut text = String::with_capacity(10);
            text.push(v as char);
            if v.is_ascii_whitespace() {
                while lexer::peek(&mut self.buffer)?.is_ascii_whitespace() {
                    text.push(lexer::next(self)? as char);
                }
                let string_index = self.strings.get_index_or_insert(&text);
                return Some(Token {
                    position,
                    kind: TokenKind::Whitespace(string_index),
                });
            }
            while let Some(peeked_character) = lexer::peek(&mut self.buffer) {
                if !peeked_character.is_ascii_whitespace() && !KEY_CHARS[peeked_character as usize]
                {
                    text.push(lexer::next(self)? as char);
                } else {
                    break;
                }
            }
            let string_index = self.strings.get_index_or_insert(&text);
            return Some(Token {
                position,
                kind: TokenKind::Text(string_index),
            });
        }
        None
    }
}

impl<'a> Tokenizer<'a> {
    fn fill(&mut self) -> Vec<Token> {
        self.collect()
    }
}

impl<'a> XmlParser {
    /// Initializes a [XmlParser] by reading the file into a buffer.
    pub fn file<P: AsRef<Path>>(filepath: P) -> Result<Self, Box<dyn std::error::Error + 'static>> {
        Ok(Self {
            settings: Settings::default(),
            buffer: std::fs::read(filepath)?,
            raw_index: 0,
            raw_tokens: Vec::new(),
        })
    }

    /// Same as above, but also takes another argument with [Settings].
    pub fn file_with_settings<P: AsRef<Path>>(
        filepath: P,
        settings: Settings,
    ) -> Result<Self, Box<dyn std::error::Error + 'static>> {
        Ok(Self {
            settings,
            buffer: std::fs::read(filepath)?,
            raw_index: 0,
            raw_tokens: Vec::new(),
        })
    }

    /// Initializes a [XmlParser] by convering the string slice to a vector of bytes.
    pub fn str(s: &str) -> Self {
        Self {
            settings: Settings::default(),
            buffer: s.as_bytes().to_vec(),
            raw_index: 0,
            raw_tokens: Vec::new(),
        }
    }

    /// Same as above, but also takes another argument with [Settings].
    pub fn str_with_settings(s: &str, settings: Settings) -> Self {
        Self {
            settings,
            buffer: s.as_bytes().to_vec(),
            raw_index: 0,
            raw_tokens: Vec::new(),
        }
    }

    #[inline]
    fn char_match(&self, t: &Token, c: u8, string_map: &[String]) -> bool {
        match &t.kind {
            TokenKind::KeyChar(kc) => *kc == c,
            TokenKind::Text(s) => string_map[*s].as_bytes()[0] == c,
            _ => false,
        }
    }

    #[inline]
    fn match_next_str(&self, characters: &str, string_map: &[String]) -> (bool, usize) {
        let chars = characters.as_bytes();
        let chars_count = chars.len();
        if self.raw_index + chars_count < self.raw_tokens.len() {
            if !self.raw_tokens[self.raw_index + 1..=self.raw_index + chars_count]
                .iter()
                .zip(chars)
                .all(|(t, c)| self.char_match(t, *c, string_map))
            {
                return (false, 0);
            }
        } else {
            return (false, 0);
        }
        (true, chars_count)
    }

    fn match_next_char(&self, character: u8, string_map: &[String]) -> bool {
        if let Some(token) = self.raw_tokens.get(self.raw_index + 1) {
            match &token.kind {
                TokenKind::KeyChar(kc) => {
                    if *kc == character {
                        return true;
                    }
                }
                TokenKind::Text(s) => {
                    if string_map[*s].as_bytes()[0] == character {
                        return true;
                    }
                }
                TokenKind::Whitespace(s) => {
                    if string_map[*s].as_bytes()[0] == character {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Takes the settings and does the actual parsing and returning a [ParsedXml] struct.
    pub fn parse(mut self) -> ParsedXml<'a> {
        use TokenKind::*;

        let mut tokenizer = Tokenizer {
            position: FilePosition::default(),
            buffer: self.buffer.iter().peekable(),
            strings: Strings::default(),
        };
        self.raw_tokens = tokenizer.fill();

        let mut open_elements = VecDeque::<usize>::new();
        let mut parsed_xml = ParsedXml {
            create_position_map: self.settings.create_position_map,
            ..Default::default()
        };

        'outer: while let Some(raw_token) = self.raw_tokens.get(self.raw_index) {
            let parent = open_elements.front().copied();
            match &raw_token.kind {
                Text(text) => {
                    let key_token = raw_token;
                    self.raw_index += 1;
                    if open_elements.is_empty() {
                        parsed_xml.push_error(
                            XmlError::EmptyDocument(key_token.position),
                            &self.settings,
                        );
                        continue;
                    }
                    while let Some(token) = self.raw_tokens.get(self.raw_index) {
                        match token.kind {
                            KeyChar(kc) => {
                                if kc == b'=' {
                                    break;
                                }
                            }
                            Text(..) => {
                                parsed_xml.push_error(
                                    XmlError::MissingValue(
                                        tokenizer.strings.get(*text).to_string(),
                                        key_token.position,
                                        parent,
                                    ),
                                    &self.settings,
                                );
                                continue 'outer;
                            }
                            _ => {}
                        }
                        self.raw_index += 1;
                    }
                    while let Some(token) = self.raw_tokens.get(self.raw_index) {
                        match token.kind {
                            KeyChar(kc) => {
                                if KEY_CHARS[kc as usize] {
                                    if kc == b'"' || kc == b'\'' {
                                        break;
                                    } else if kc != b'=' {
                                        parsed_xml.push_error(
                                            XmlError::QuoteExpected(
                                                tokenizer.strings.get(*text).to_string(),
                                                key_token.position,
                                                parent,
                                            ),
                                            &self.settings,
                                        );
                                        continue 'outer;
                                    }
                                }
                            }
                            Text(..) => {
                                parsed_xml.push_error(
                                    XmlError::QuoteExpected(
                                        tokenizer.strings.get(*text).to_string(),
                                        key_token.position,
                                        parent,
                                    ),
                                    &self.settings,
                                );
                                continue 'outer;
                            }
                            _ => {}
                        }
                        self.raw_index += 1;
                    }
                    if let Some(token) = self.raw_tokens.get(self.raw_index) {
                        if let KeyChar(attribute_value_start) = token.kind {
                            let mut found_boundary = false;
                            let attribute = token;
                            let boundary_character = attribute_value_start;
                            let mut value = String::with_capacity(10);
                            while let Some(token) = self.raw_tokens.get(self.raw_index + 1) {
                                match &token.kind {
                                    KeyChar(key_char_index) => {
                                        if *key_char_index == b'<' {
                                            parsed_xml.push_error(
                                                XmlError::Unescaped(
                                                    '<',
                                                    tokenizer.strings.get(*text).to_string(),
                                                    raw_token.position,
                                                    parent,
                                                    token.position,
                                                ),
                                                &self.settings,
                                            );
                                            continue 'outer;
                                        } else if *key_char_index == boundary_character {
                                            let attribute = XmlToken::attribute(
                                                tokenizer.strings.get(*text),
                                                value,
                                                raw_token.position,
                                                attribute.position,
                                                parent.map(|p| {
                                                    Rc::clone(parsed_xml.tokens.get(p).unwrap())
                                                }),
                                            );
                                            parsed_xml.push_attribute(
                                                attribute,
                                                parent,
                                                &self.settings,
                                            );
                                            found_boundary = true;
                                            let mut offset = 2;
                                            while let Some(token) =
                                                self.raw_tokens.get(self.raw_index + offset)
                                            {
                                                offset += 1;
                                                match token.kind {
                                                    KeyChar(kc) => {
                                                        if kc == b'>'
                                                            || (kc == b'?'
                                                                && token.position.line == 1)
                                                        {
                                                            break;
                                                        } else if kc == b'/' {
                                                            if let Some(token) = self
                                                                .raw_tokens
                                                                .get(self.raw_index + offset)
                                                            {
                                                                if let KeyChar(b'>') = token.kind {
                                                                    break;
                                                                }
                                                            }
                                                        }
                                                        parsed_xml.push_error(
                                                            XmlError::ElementMustBeFollowedBy(
                                                                tokenizer
                                                                    .strings
                                                                    .get(*text)
                                                                    .to_string(),
                                                                raw_token.position,
                                                            ),
                                                            &self.settings,
                                                        );
                                                        break;
                                                    }
                                                    Text(_) => {
                                                        break;
                                                    }
                                                    Whitespace(_) => {
                                                        continue;
                                                    }
                                                }
                                            }
                                            break;
                                        }
                                        value.push(*key_char_index as char);
                                    }
                                    Text(text) => {
                                        value.push_str(&tokenizer.strings.get(*text));
                                    }
                                    Whitespace(whitespace) => {
                                        value.push_str(&tokenizer.strings.get(*whitespace));
                                    }
                                }
                                self.raw_index += 1;
                            }
                            if !found_boundary {
                                parsed_xml.push_error(
                                    XmlError::QuoteExpected(
                                        tokenizer.strings.get(*text).to_string(),
                                        raw_token.position,
                                        parent,
                                    ),
                                    &self.settings,
                                );
                                continue 'outer;
                            }
                        }
                    } else {
                        parsed_xml.push_error(
                            XmlError::QuoteExpected(
                                tokenizer.strings.get(*text).to_string(),
                                raw_token.position,
                                parent,
                            ),
                            &self.settings,
                        );
                    }
                }
                KeyChar(kc) => match kc {
                    b'<' => {
                        if let (true, char_num) =
                            self.match_next_str("!--", &tokenizer.strings.strings)
                        {
                            self.raw_index += char_num;
                            let position = self.raw_tokens[self.raw_index].position;
                            let mut comment = String::with_capacity(10);
                            while let Some(raw_token) = self.raw_tokens.get(self.raw_index + 1) {
                                if let (true, char_num) =
                                    self.match_next_str("--", &tokenizer.strings.strings)
                                {
                                    self.raw_index += char_num;
                                    if self.match_next_char(b'>', &tokenizer.strings.strings) {
                                        self.raw_index += 1;
                                        break;
                                    }
                                    if !self.settings.ignore_comments {
                                        parsed_xml.push_error(
                                            XmlError::NotPermittedInComments(position),
                                            &self.settings,
                                        );
                                    }
                                }
                                if !self.settings.ignore_comments {
                                    match &raw_token.kind {
                                        KeyChar(kc) => {
                                            comment.push(*kc as char);
                                        }
                                        Text(text) | Whitespace(text) => {
                                            comment.push_str(&tokenizer.strings.get(*text));
                                        }
                                    }
                                }
                                self.raw_index += 1;
                            }
                            parsed_xml.push_comment(
                                XmlToken::comment(comment, position),
                                parent,
                                &self.settings,
                            );
                        } else if let Some(raw_token) = self.raw_tokens.get(self.raw_index + 1) {
                            let position = raw_token.position;
                            match &raw_token.kind {
                                Text(name) => {
                                    let id = parsed_xml.tokens.len();
                                    let token = XmlToken::open_element(
                                        tokenizer.strings.get(*name),
                                        id,
                                        position,
                                        parent
                                            .map(|p| Rc::clone(parsed_xml.tokens.get(p).unwrap())),
                                    );
                                    parsed_xml.push_open_element(token, parent, &self.settings);
                                    open_elements.push_front(id);
                                    self.raw_index += 1;
                                }
                                KeyChar(kc) => {
                                    if let b'/' = kc {
                                        if let Some(raw_token) =
                                            self.raw_tokens.get(self.raw_index + 2)
                                        {
                                            self.raw_index += 2;
                                            if let Text(text) = &raw_token.kind {
                                                if let Some(front) = open_elements.pop_front() {
                                                    let (name, id) =
                                                        if let XmlToken::OpenElement(e) =
                                                            &*parsed_xml.tokens[front].borrow()
                                                        {
                                                            (Some(e.name.clone()), Some(e.id))
                                                        } else {
                                                            (None, None)
                                                        };
                                                    if let (Some(name), Some(id)) = (name, id) {
                                                        if id != front
                                                            || name != tokenizer.strings.get(*text)
                                                        {
                                                            parsed_xml.push_error(
                                                                XmlError::OpenCloseElementMismatch(
                                                                    tokenizer
                                                                        .strings
                                                                        .get(*text)
                                                                        .to_string(),
                                                                    name,
                                                                    position,
                                                                ),
                                                                &self.settings,
                                                            );
                                                        }
                                                    }
                                                } else {
                                                    parsed_xml.push_error(
                                                        XmlError::OpenCloseElementsMismatch(
                                                            position,
                                                        ),
                                                        &self.settings,
                                                    );
                                                }
                                                let token = XmlToken::close_element(
                                                    tokenizer.strings.get(*text),
                                                    position,
                                                    parent.map(|p| {
                                                        Rc::clone(parsed_xml.tokens.get(p).unwrap())
                                                    }),
                                                );
                                                parsed_xml.push_close_element(
                                                    token,
                                                    parent,
                                                    &self.settings,
                                                );
                                                if (self.raw_index + 1) >= self.raw_tokens.len() {
                                                    parsed_xml.push_error(
                                                        XmlError::Expected(
                                                            '>'.to_string(),
                                                            position,
                                                        ),
                                                        &self.settings,
                                                    );
                                                    break;
                                                }
                                                while let Whitespace(..) =
                                                    self.raw_tokens[self.raw_index + 1].kind
                                                {
                                                    self.raw_index += 1;
                                                }
                                                match self.raw_tokens[self.raw_index + 1].kind {
                                                    KeyChar(index) => {
                                                        if index != b'>' {
                                                            parsed_xml.push_error(
                                                                XmlError::Expected(
                                                                    '>'.to_string(),
                                                                    position,
                                                                ),
                                                                &self.settings,
                                                            );
                                                            self.raw_index += 1;
                                                        }
                                                    }
                                                    _ => {
                                                        parsed_xml.push_error(
                                                            XmlError::Expected(
                                                                '>'.to_string(),
                                                                position,
                                                            ),
                                                            &self.settings,
                                                        );
                                                        self.raw_index += 1;
                                                    }
                                                }
                                            }
                                        }
                                    } else if let b'?' = kc {
                                        self.raw_index += 2;
                                        if self.raw_index >= self.raw_tokens.len() {
                                            break;
                                        }
                                        let parent = open_elements.front().copied();
                                        if let Text(text) = &self.raw_tokens[self.raw_index].kind {
                                            if tokenizer.strings.get(*text) == "xml" {
                                                let id = parsed_xml.tokens.len();
                                                let token = XmlToken::open_element(
                                                    tokenizer.strings.get(*text),
                                                    id,
                                                    position,
                                                    parent.map(|p| {
                                                        Rc::clone(parsed_xml.tokens.get(p).unwrap())
                                                    }),
                                                );
                                                open_elements.push_front(id);
                                                parsed_xml.push_open_element(
                                                    token,
                                                    parent,
                                                    &self.settings,
                                                );
                                            }
                                        } else {
                                            parsed_xml.push_error(
                                                XmlError::Expected("xml".to_string(), position),
                                                &self.settings,
                                            );
                                        }
                                    }
                                }
                                _ => {}
                            }
                        }
                    }
                    b'/' | b'?' => {
                        if self.match_next_char(b'>', &tokenizer.strings.strings) {
                            open_elements.pop_front();
                            let position = self.raw_tokens[self.raw_index].position;
                            let token = XmlToken::close_element_quick(
                                position,
                                parent.map(|p| Rc::clone(parsed_xml.tokens.get(p).unwrap())),
                            );
                            parsed_xml.push_close_element(token, parent, &self.settings);
                        }
                    }
                    b'>' => {
                        let mut inner_text = String::new();
                        while let Some(raw_token) = self.raw_tokens.get(self.raw_index + 1) {
                            match &raw_token.kind {
                                Text(text) | Whitespace(text) => {
                                    inner_text.push_str(&tokenizer.strings.get(*text));
                                }
                                KeyChar(kc) => {
                                    if *kc == b'<' {
                                        break;
                                    }
                                    inner_text.push(*kc as char);
                                }
                            }
                            self.raw_index += 1;
                        }

                        let token = XmlToken::inner_text(
                            inner_text,
                            raw_token.position,
                            parent.map(|p| Rc::clone(parsed_xml.tokens.get(p).unwrap())),
                        );
                        parsed_xml.push_inner_text(token, parent, &self.settings);
                    }
                    _ => {}
                },
                _ => {}
            }
            self.raw_index += 1;
        }
        if let Some(last) = open_elements.iter().last() {
            let position = parsed_xml.tokens[*last].borrow().position();
            parsed_xml.push_error(
                XmlError::OpenCloseElementsMismatch(position),
                &self.settings,
            );
        }
        parsed_xml
    }
}
