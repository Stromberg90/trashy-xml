use smartstring::alias::String;

#[derive(Clone, PartialEq, Debug)]
pub struct StringSpan {
    start: u32,
    end: u32,
}

impl StringSpan {
    pub fn new(start: u32, end: u32) -> Self {
        StringSpan { start, end }
    }

    pub fn start(&self) -> usize {
        self.start as usize
    }

    pub fn end(&self) -> usize {
        self.end as usize
    }

    pub fn length(&self) -> usize {
        (self.end - self.start) as usize
    }
}

#[derive(PartialEq)]
pub enum TokenKind {
    KeyChar(u32),
    Whitespace(u32, u32),
    Text(u32, u32),
}

#[derive(PartialEq, Debug)]
pub enum XmlKind {
    Comment(StringSpan),
    Attribute(StringSpan, StringSpan),
    InnerText(StringSpan),
    OpenElement(StringSpan, u32),
    CloseElement(StringSpan),
    Error(String),
}

impl XmlToken {
    pub fn comment(str: StringSpan) -> XmlToken {
        XmlToken {
            kind: XmlKind::Comment(str),
            position: FilePosition::new(),
            parent: None,
        }
    }

    pub fn attribute(name: StringSpan, value: StringSpan) -> XmlToken {
        XmlToken {
            kind: XmlKind::Attribute(name, value),
            position: FilePosition::new(),
            parent: None,
        }
    }

    pub fn inner_text(str: StringSpan) -> XmlToken {
        XmlToken {
            kind: XmlKind::InnerText(str),
            position: FilePosition::new(),
            parent: None,
        }
    }

    pub fn open_element(str: StringSpan, id: u32) -> XmlToken {
        XmlToken {
            kind: XmlKind::OpenElement(str, id),
            position: FilePosition::new(),
            parent: None,
        }
    }

    pub fn close_element(str: StringSpan) -> XmlToken {
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
}

#[derive(PartialEq, Copy, Clone, Debug)]
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

#[derive(PartialEq, Debug)]
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
