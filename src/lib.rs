use smallstr::SmallString;
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

#[derive(Debug, PartialEq, Clone, Copy)]
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
/// let mut parser = XmlParser::new("sample_files/small.xml");
/// parser.parse();
/// for token in &parser.xml_tokens {
///     if let XmlKind::OpenElement(name, _) = &token.kind {
///         if name == "element" {
///             for i in parser.xml_tokens.get_attributes(token) {
///                 dbg!(&parser.xml_tokens[i]);
///             }
///         }
///     }
/// }
/// ```
#[derive(Debug)]
pub struct XmlParser {
    index: usize,
    started_parsing: bool,
    position: FilePosition,
    stream: Vec<u8>,
    raw_tokens: Vec<Token>,
    pub xml_tokens: Vec<XmlToken>,
    pub errors: Vec<XmlError>,
}

/// Helper methods
pub trait XmlMethods {
    /// Gets all children indecies of this token
    ///
    /// ```
    /// use trashy_xml::{XmlKind, XmlMethods, XmlParser};
    ///
    /// let mut parser = XmlParser::new("sample_files/small.xml");
    /// parser.parse();
    /// for token in &parser.xml_tokens {
    ///    for child_index in parser.xml_tokens.get_children(token) {
    ///        dbg!(&parser.xml_tokens[child_index]);
    ///    }
    /// }
    /// ```
    fn get_children(&self, token: &XmlToken) -> Vec<usize>;

    /// Gets all attributes indecies of this token
    ///
    /// ```
    /// use trashy_xml::{XmlKind, XmlMethods, XmlParser};
    ///
    /// let mut parser = XmlParser::new("sample_files/small.xml");
    /// parser.parse();
    /// for token in &parser.xml_tokens {
    ///    for child_index in parser.xml_tokens.get_attributes(token) {
    ///        dbg!(&parser.xml_tokens[child_index]);
    ///    }
    /// }
    /// ```
    fn get_attributes(&self, token: &XmlToken) -> Vec<usize>;

    /// Gets all siblings indecies of this token
    ///
    /// ```
    /// use trashy_xml::{XmlKind, XmlMethods, XmlParser};
    ///
    /// let mut parser = XmlParser::new("sample_files/small.xml");
    /// parser.parse();
    /// for token in &parser.xml_tokens {
    ///    for child_index in parser.xml_tokens.get_siblings(token) {
    ///        dbg!(&parser.xml_tokens[child_index]);
    ///    }
    /// }
    /// ```
    fn get_siblings(&self, token: &XmlToken) -> Vec<usize>;
}

impl XmlMethods for Vec<XmlToken> {
    fn get_children(&self, token: &XmlToken) -> Vec<usize> {
        let mut result = Vec::<usize>::new();
        if let XmlKind::OpenElement(_, i) = &token.kind {
            for (current_index, tk) in self.iter().enumerate() {
                if let Some(parent) = tk.parent {
                    if parent == *i {
                        result.push(current_index);
                    }
                }
            }
        }
        result
    }

    fn get_attributes(&self, token: &XmlToken) -> Vec<usize> {
        let mut result = Vec::<usize>::new();
        if let XmlKind::OpenElement(_, i) = &token.kind {
            for (current_index, tk) in self.iter().enumerate() {
                if let Some(parent) = tk.parent {
                    if let XmlKind::Attribute(_, _) = tk.kind {
                        if parent == *i {
                            result.push(current_index);
                        }
                    }
                }
            }
        }
        result
    }

    fn get_siblings(&self, token: &XmlToken) -> Vec<usize> {
        let mut result = Vec::<usize>::new();
        if let Some(token_parent) = &token.parent {
            for (current_index, tk) in self.iter().enumerate() {
                if let Some(parent) = tk.parent {
                    if parent == *token_parent {
                        result.push(current_index);
                    }
                }
            }
        }
        result
    }
}

fn is_key_char(c: char) -> bool {
    match c {
        '<' | '/' | '!' | '-' | '>' | '"' | '\'' | '=' | '?' | '(' | ')' => true,
        _ => false,
    }
}

mod lexer {
    use super::*;
    pub fn peek(xml_parser: &XmlParser) -> Option<char> {
        if let Some(character) = xml_parser.stream.get(xml_parser.index + 1) {
            return Some(*character as char);
        }
        None
    }

    pub fn next(xml_parser: &mut XmlParser) -> Option<char> {
        if xml_parser.started_parsing {
            xml_parser.index += 1;
        }
        xml_parser.started_parsing = true;
        if let Some(character) = xml_parser.stream.get(xml_parser.index) {
            match *character as char {
                '\r' => {
                    if let Some(v) = peek(xml_parser) {
                        if v as char == '\n' {
                            xml_parser.index += 1;
                            xml_parser.position.line += 1;
                            xml_parser.position.column = 1;
                        }
                    }
                }
                '\n' => {
                    xml_parser.position.line += 1;
                    xml_parser.position.column = 1;
                }
                _ => {
                    xml_parser.position.column += 1;
                }
            }
            return Some(*character as char);
        }
        None
    }
}

impl Iterator for XmlParser {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(v) = lexer::next(self) {
            if !is_key_char(v) {
                if v.is_whitespace() {
                    let start_index = self.index;
                    while lexer::peek(self)?.is_whitespace() {
                        lexer::next(self)?;
                    }
                    return Some(Token {
                        position: self.position,
                        kind: TokenKind::Whitespace(start_index, self.index + 1),
                    });
                }
                let start_index = self.index;
                while let Some(peeked_character) = lexer::peek(self) {
                    if !is_key_char(peeked_character) && !peeked_character.is_whitespace() {
                        lexer::next(self).unwrap();
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
            started_parsing: false,
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
    pub fn new<P: AsRef<Path>>(filepath: P) -> XmlParser {
        let mut buffer = Vec::new();
        File::open(filepath)
            .unwrap()
            .read_to_end(&mut buffer)
            .unwrap();
        XmlParser {
            index: 0,
            started_parsing: false,
            position: FilePosition::new(),
            stream: buffer,
            raw_tokens: Vec::new(),
            xml_tokens: Vec::new(),
            errors: Vec::new(),
        }
    }

    fn formatted_error(position: FilePosition, message: &str) -> String {
        format!(
            "Error({}, {}): {}.",
            position.line, position.column, message
        )
    }
    fn match_next_str(&self, index: usize, characters: &str) -> bool {
        if index + characters.chars().count() < self.raw_tokens.len() {
            for (token, character) in self.raw_tokens
                [index + 1..index + characters.chars().count() + 1]
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
        if index + 1 < self.raw_tokens.len() {
            if let TokenKind::KeyChar(kc) = self.raw_tokens[index + 1].kind {
                return self.stream[kc] as char == character;
            }
        }
        false
    }

    // TODO: I should turn xml tokens into a iterator, then I can call cool iterator methods in it, filter out all attributes and such.
    pub fn parse(&mut self) {
        use TokenKind::*;
        use XmlKind::*;

        // Have raw tokens as a local variable instead of a struct field, I don't need to keep it around right?
        self.raw_tokens = self.collect();

        let mut open_element_index_stack = VecDeque::<usize>::new();
        let mut raw_token_index = 0;
        while let Some(raw_token) = self.raw_tokens.get(raw_token_index) {
            match raw_token.kind {
                Text(start_index, end_index) => {
                    let position = raw_token.position;
                    if open_element_index_stack.is_empty() {
                        self.errors.push(XmlError {
                            position,
                            message: XmlParser::formatted_error(position, "Document is empty"),
                        });
                        raw_token_index += 1;
                        continue;
                    }
                    raw_token_index += 1;

                    let mut skip_loop = false;
                    while let Some(raw_token) = self.raw_tokens.get(raw_token_index) {
                        match raw_token.kind {
                            KeyChar(kc) => {
                                if self.stream[kc] as char == '=' {
                                    break;
                                }
                            }
                            Whitespace(_, _) => {
                                raw_token_index += 1;
                            }
                            _ => {
                                self.errors.push(XmlError {
                                    position,
                                    message: XmlParser::formatted_error(
                                        position,
                                        &format!(
                                            "Specification mandates value for attribute {}",
                                            std::str::from_utf8(
                                                &self.stream[start_index..end_index]
                                            )
                                            .unwrap()
                                        ),
                                    ),
                                });
                                raw_token_index += 1;
                                skip_loop = true;
                                break;
                            }
                        }
                    }
                    raw_token_index += 1;
                    if raw_token_index >= self.raw_tokens.len() {
                        break;
                    }
                    if skip_loop {
                        continue;
                    }
                    while let Some(raw_token) = self.raw_tokens.get(raw_token_index) {
                        match raw_token.kind {
                            KeyChar(kc) => {
                                if self.stream[kc] as char == '"' || self.stream[kc] as char == '\''
                                {
                                    break;
                                }
                            }
                            Whitespace(_, _) => {
                                raw_token_index += 1;
                            }
                            _ => {
                                self.errors.push(XmlError {
                                    position,
                                    message: XmlParser::formatted_error(
                                        position,
                                        "\" or \' expected",
                                    ),
                                });
                                raw_token_index += 1;
                                break;
                            }
                        }
                    }
                    if let KeyChar(boundary_character) = self.raw_tokens[raw_token_index].kind {
                        let boundary_character = self.stream[boundary_character] as char;
                        let mut attribute_value: SmallString<[u8; 16]> = SmallString::new();
                        loop {
                            raw_token_index += 1;
                            match self.raw_tokens[raw_token_index].kind {
                                KeyChar(key_char) => {
                                    let key_char = self.stream[key_char] as char;
                                    if key_char == boundary_character {
                                        break;
                                    }
                                    attribute_value.push(key_char);
                                }
                                Text(start_index, end_index)
                                | Whitespace(start_index, end_index) => {
                                    attribute_value.push_str(
                                        std::str::from_utf8(&self.stream[start_index..end_index])
                                            .unwrap(),
                                    );
                                }
                            }
                        }
                        let token = XmlToken {
                            kind: Attribute(
                                std::str::from_utf8(&self.stream[start_index..end_index])
                                    .unwrap()
                                    .to_owned(),
                                attribute_value.into_string(),
                            ),
                            position,
                            parent: open_element_index_stack.front().copied(),
                        };
                        self.xml_tokens.push(token);
                    }
                }
                KeyChar(kc) => match self.stream[kc] as char {
                    '<' => {
                        if self.match_next_str(raw_token_index, "!--") {
                            raw_token_index += 4;
                            let position = self.raw_tokens[raw_token_index].position;
                            let mut comment: SmallString<[u8; 16]> = SmallString::new();
                            while self.raw_tokens.get(raw_token_index + 1).is_some() {
                                let raw_token = &self.raw_tokens.get(raw_token_index).unwrap();
                                match raw_token.kind {
                                    KeyChar(key_char) => {
                                        comment.push(self.stream[key_char] as char);
                                    }
                                    Whitespace(start_index, end_index)
                                    | Text(start_index, end_index) => {
                                        comment.push_str(
                                            std::str::from_utf8(
                                                &self.stream[start_index..end_index],
                                            )
                                            .unwrap(),
                                        );
                                    }
                                }
                                if self.match_next_str(raw_token_index, "--") {
                                    raw_token_index += 2;
                                    if self.match_next_char(raw_token_index, '>') {
                                        raw_token_index += 1;
                                        break;
                                    } else {
                                        self.errors.push(XmlError {
                                            position,
                                            message: XmlParser::formatted_error(
                                                position,
                                                "-- is not permitted within comments",
                                            ),
                                        });
                                        raw_token_index -= 2;
                                    }
                                }
                                raw_token_index += 1;
                            }
                            let token = XmlToken {
                                kind: Comment(comment.into_string()),
                                position,
                                parent: open_element_index_stack.front().copied(),
                            };
                            self.xml_tokens.push(token);
                        } else if let Some(raw_token) = self.raw_tokens.get(raw_token_index + 1) {
                            let position = raw_token.position;
                            match raw_token.kind {
                                Text(start_index, end_index) => {
                                    let token = XmlToken {
                                        kind: OpenElement(
                                            std::str::from_utf8(
                                                &self.stream[start_index..end_index],
                                            )
                                            .unwrap()
                                            .to_owned(),
                                            self.xml_tokens.len(),
                                        ),
                                        position,
                                        parent: open_element_index_stack.front().copied(),
                                    };
                                    self.xml_tokens.push(token);
                                    open_element_index_stack.push_front(self.xml_tokens.len() - 1);
                                    raw_token_index += 1;
                                }
                                KeyChar(kc) => match self.stream[kc] as char {
                                    '/' => {
                                        if let Some(raw_token) =
                                            self.raw_tokens.get(raw_token_index + 2)
                                        {
                                            if let Text(start_index, end_index) = raw_token.kind {
                                                if open_element_index_stack.is_empty() {
                                                    self.errors.push(XmlError {
                                                                position,
                                                                message: XmlParser::formatted_error(
                                                                    position,
                                                                    "Mismatch between closing and opening elements",
                                                                ),
                                                            });
                                                }
                                                if let Some(front) =
                                                    open_element_index_stack.pop_front()
                                                {
                                                    if let OpenElement(o, i) =
                                                        &self.xml_tokens[front].kind
                                                    {
                                                        let text = std::str::from_utf8(
                                                            &self.stream[start_index..end_index],
                                                        )
                                                        .unwrap();
                                                        if *i != front || o != text {
                                                            self.errors.push(XmlError {
                                                                        position,
                                                                        message: XmlParser::formatted_error(position, &format!("Mismatch between closing {} and opening {} elements",
                                                                        text, o)),
                                                                    });
                                                        }
                                                    }
                                                }
                                                let token = XmlToken {
                                                    kind: CloseElement(
                                                        std::str::from_utf8(
                                                            &self.stream[start_index..end_index],
                                                        )
                                                        .unwrap()
                                                        .to_owned(),
                                                    ),
                                                    position,
                                                    parent: open_element_index_stack
                                                        .front()
                                                        .copied(),
                                                };
                                                self.xml_tokens.push(token);
                                                raw_token_index += 2;
                                            }
                                        }
                                    }
                                    _ => {}
                                },
                                _ => {}
                            }
                        }
                    }
                    '/' => {
                        if self.match_next_char(raw_token_index, '>') {
                            if let Some(front) = open_element_index_stack.pop_front() {
                                if let OpenElement(parent_name, _) = &self.xml_tokens[front].kind {
                                    let position = self.raw_tokens[raw_token_index].position;
                                    let token = XmlToken {
                                        kind: CloseElement(parent_name.clone()),
                                        position,
                                        parent: open_element_index_stack.front().copied(),
                                    };
                                    self.xml_tokens.push(token);
                                }
                            }
                        }
                    }
                    '>' => {
                        let position = self.raw_tokens[raw_token_index].position;
                        raw_token_index += 1;
                        let mut inner_text: SmallString<[u8; 16]> = SmallString::new();
                        while let Some(raw_token) = self.raw_tokens.get(raw_token_index) {
                            match raw_token.kind {
                                Text(start_index, end_index)
                                | Whitespace(start_index, end_index) => {
                                    inner_text.push_str(
                                        std::str::from_utf8(&self.stream[start_index..end_index])
                                            .unwrap(),
                                    );
                                    raw_token_index += 1;
                                }
                                KeyChar(kc) => match self.stream[kc] as char {
                                    '<' => {
                                        raw_token_index -= 1;
                                        break;
                                    }
                                    kc => {
                                        inner_text.push(kc);
                                        raw_token_index += 1;
                                    }
                                },
                            }
                        }
                        let token = XmlToken {
                            kind: InnerText(inner_text.into_string()),
                            position,
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
            let position = self.xml_tokens[*last].position;
            self.errors.push(XmlError {
                position,
                message: XmlParser::formatted_error(
                    position,
                    "Mismatch between number of closing and opening elements",
                ),
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn small_file_len_check() {
        let mut parser = XmlParser::new("sample_files/small.xml");
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
        parser.parse();
        dbg!(&parser.xml_tokens);
        assert_eq!(parser.errors.len(), 1);
    }

    #[test]
    fn missing_closing_bracket_check() {
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
    fn small_file_attributes_len_check() {
        let mut attributes_len = 0;
        let mut parser = XmlParser::new("sample_files/small.xml");
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
    fn large_file_len_check() {
        let mut parser = XmlParser::new("sample_files/large.xml");
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
