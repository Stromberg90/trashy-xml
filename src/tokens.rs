use smartstring::alias::String;

#[derive(PartialEq)]
pub enum TokenKind {
    KeyChar(u32),
    Whitespace(u32, u32),
    Text(u32, u32),
}

#[derive(PartialEq, Debug)]
pub enum XmlKind {
    Comment(String),
    Attribute(String, String),
    InnerText(String),
    OpenElement(String, u32),
    CloseElement(String),
    Error(String),
}

impl XmlToken {
    pub fn comment(str: String) -> XmlToken {
        XmlToken {
            kind: XmlKind::Comment(str),
            position: FilePosition::new(),
            parent: None,
        }
    }

    pub fn attribute(name: String, value: String) -> XmlToken {
        XmlToken {
            kind: XmlKind::Attribute(name, value),
            position: FilePosition::new(),
            parent: None,
        }
    }

    pub fn inner_text(str: String) -> XmlToken {
        XmlToken {
            kind: XmlKind::InnerText(str),
            position: FilePosition::new(),
            parent: None,
        }
    }

    pub fn open_element(str: String, id: u32) -> XmlToken {
        XmlToken {
            kind: XmlKind::OpenElement(str, id),
            position: FilePosition::new(),
            parent: None,
        }
    }

    pub fn close_element(str: String) -> XmlToken {
        XmlToken {
            kind: XmlKind::CloseElement(str),
            position: FilePosition::new(),
            parent: None,
        }
    }

    pub fn error(str: String) -> XmlToken {
        XmlToken {
            kind: XmlKind::Error(str),
            position: FilePosition::new(),
            parent: None,
        }
    }

    pub fn position(mut self, position: FilePosition) -> XmlToken {
        self.position = position;
        self
    }

    pub fn parent(mut self, parent: Option<u32>) -> XmlToken {
        self.parent = parent;
        self
    }

    pub fn is_comment(&self) -> bool {
        matches!(self.kind, XmlKind::Comment(..))
    }

    pub fn is_attribute(&self) -> bool {
        matches!(self.kind, XmlKind::Attribute(..))
    }

    pub fn is_inner_text(&self) -> bool {
        matches!(self.kind, XmlKind::InnerText(..))
    }

    pub fn is_open_element(&self) -> bool {
        matches!(self.kind, XmlKind::OpenElement(..))
    }

    pub fn is_close_element(&self) -> bool {
        matches!(self.kind, XmlKind::CloseElement(..))
    }

    pub fn is_error(&self) -> bool {
        matches!(self.kind, XmlKind::Error(..))
    }

    pub unsafe fn as_attribute_unchecked(&self) -> (&str, &str) {
        use std::hint::unreachable_unchecked;

        match &self.kind {
            XmlKind::Attribute(k, v) => (k, v),
            _ => unreachable_unchecked(),
        }
    }

    pub unsafe fn as_comment_unchecked(&self) -> &str {
        use std::hint::unreachable_unchecked;

        match &self.kind {
            XmlKind::Comment(comment) => comment,
            _ => unreachable_unchecked(),
        }
    }

    pub unsafe fn as_inner_text_unchecked(&self) -> &str {
        use std::hint::unreachable_unchecked;

        match &self.kind {
            XmlKind::InnerText(inner_text) => inner_text,
            _ => unreachable_unchecked(),
        }
    }

    pub unsafe fn as_open_element_unchecked(&self) -> (&str, u32) {
        use std::hint::unreachable_unchecked;

        match &self.kind {
            XmlKind::OpenElement(k, v) => (k, *v),
            _ => unreachable_unchecked(),
        }
    }

    pub unsafe fn as_close_element_unchecked(&self) -> &str {
        use std::hint::unreachable_unchecked;

        match &self.kind {
            XmlKind::CloseElement(k) => k,
            _ => unreachable_unchecked(),
        }
    }
}

#[derive(PartialEq, Copy, Clone)]
pub struct FilePosition {
    pub line: u32,
    pub column: u32,
}

impl FilePosition {
    pub fn new() -> FilePosition {
        FilePosition { line: 1, column: 1 }
    }
}

pub struct Token {
    pub position: FilePosition,
    pub kind: TokenKind,
}

#[derive(PartialEq)]
pub struct XmlToken {
    pub position: FilePosition,
    pub kind: XmlKind,
    pub parent: Option<u32>,
}

pub struct RawTokens {
    pub tokens: Vec<Token>,
    pub index: usize,
}

impl RawTokens {
    pub fn new() -> Self {
        RawTokens {
            tokens: Vec::new(),
            index: 0,
        }
    }
}