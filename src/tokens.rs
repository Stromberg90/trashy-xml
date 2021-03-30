use rustc_hash::FxHashMap;
use std::{cell::RefCell, fmt, rc::Rc};

/// Trait for getting parent and sibling tokens.
pub trait Family<'a> {
    fn parent(&self) -> Option<XmlToken<'a>>;
    fn siblings(&self) -> Vec<XmlToken<'a>>;
}

// #[derive(Clone, Debug)]
// pub struct StringPositionPair {
//     pub string: String,
//     pub position: FilePosition,
// }

#[derive(PartialEq, Debug)]
pub(crate) enum TokenKind {
    KeyChar(u8),
    Text(usize),
    Whitespace(usize),
}

/// Comment added only if ignore_comments is set to false.
#[derive(Clone, Debug)]
pub struct Comment {
    pub string: String,
    pub position: FilePosition,
}

#[derive(Clone)]
pub struct Attribute<'a> {
    pub key: (String, FilePosition),
    pub value: Option<(String, FilePosition)>,
    pub parent: Option<RefXmlToken<'a>>,
}

impl<'a> fmt::Debug for Attribute<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(
            f,
            "Key: {}\n{:#?}\nValue: {}\n{:#?}",
            self.key.0,
            self.key.1,
            self.value.as_ref().unwrap_or(&("".into(), self.key.1,)).0,
            self.value.as_ref().unwrap_or(&self.key).1
        )
    }
}

impl<'a> Family<'a> for XmlToken<'a> {
    fn parent(&self) -> Option<XmlToken<'a>> {
        match self {
            XmlToken::Comment(_) => None,
            XmlToken::Attribute(t) => t.parent.as_ref().map(|p| p.borrow().clone()),
            XmlToken::InnerText(t) => t.parent.as_ref().map(|p| p.borrow().clone()),
            XmlToken::OpenElement(t) => t.parent.as_ref().map(|p| p.borrow().clone()),
            XmlToken::CloseElement(t) => t.parent.as_ref().map(|p| p.borrow().clone()),
        }
    }

    fn siblings(&self) -> Vec<XmlToken<'a>> {
        self.parent()
            .as_ref()
            .map(|p| {
                p.clone()
                    .as_open_element()
                    .children()
                    .into_iter()
                    .filter(|v| v != self)
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default()
    }
}

impl<'a> Family<'a> for Attribute<'a> {
    fn parent(&self) -> Option<XmlToken<'a>> {
        self.parent.as_ref().map(|p| p.borrow().clone())
    }

    fn siblings(&self) -> Vec<XmlToken<'a>> {
        self.parent
            .as_ref()
            .map(|p| {
                p.borrow()
                    .clone()
                    .as_open_element()
                    .children()
                    .into_iter()
                    .filter(|v| v != self)
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default()
    }
}

#[derive(Clone)]
pub struct InnerText<'a> {
    pub string: String,
    pub position: FilePosition,
    pub parent: Option<RefXmlToken<'a>>,
}

impl<'a> fmt::Debug for InnerText<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "Inner Text: {}\n{:#?}", self.string, self.position)
    }
}

type RefXmlToken<'a> = Rc<RefCell<XmlToken<'a>>>;

#[derive(Clone)]
pub struct OpenElement<'a> {
    pub name: String,
    pub position: FilePosition,
    pub(crate) id: usize,
    pub parent: Option<RefXmlToken<'a>>,
    pub(crate) attributes: FxHashMap<String, Vec<RefXmlToken<'a>>>,
    pub(crate) children: Vec<RefXmlToken<'a>>,
}

impl<'a> fmt::Debug for OpenElement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "Name: {}\n{:#?}", self.name, self.position)
    }
}

impl<'a> OpenElement<'a> {
    pub fn attributes_from_name(&self, name: &str) -> Vec<Attribute<'a>> {
        self.attributes
            .get(name)
            .unwrap_or(&Vec::new())
            .iter()
            .map(|e| e.borrow().as_attribute().clone())
            .collect::<Vec<_>>()
    }

    pub fn attributes(&self) -> Vec<Attribute<'a>> {
        self.attributes
            .values()
            .into_iter()
            .flatten()
            .map(|e| e.borrow().as_attribute().clone())
            .collect()
    }

    pub fn children(&self) -> Vec<XmlToken<'a>> {
        self.children
            .iter()
            .map(|e| e.borrow().clone())
            .collect::<Vec<_>>()
    }
}

#[derive(Clone)]
pub struct CloseElement<'a> {
    pub name: Option<String>,
    pub position: FilePosition,
    pub parent: Option<RefXmlToken<'a>>,
}

impl<'a> fmt::Debug for CloseElement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match &self.name {
            Some(n) => write!(f, "Name: {}\n{:#?}", n, self.position),
            None => write!(f, "{:#?}", self.position),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum XmlError {
    EmptyDocument(FilePosition),
    Expected(String, FilePosition),
    ElementMustBeFollowedBy(String, FilePosition),
    NotPermittedInComments(FilePosition),
    OpenCloseElementsMismatch(FilePosition),
    OpenCloseElementMismatch(String, String, FilePosition),
    Unescaped(char, String, FilePosition, Option<usize>, FilePosition),
    MissingValue(String, FilePosition, Option<usize>),
    QuoteExpected(String, FilePosition, Option<usize>),
}
#[derive(Clone, Debug)]
/// Enum with the different token types
pub enum XmlToken<'a> {
    Comment(Comment),
    Attribute(Attribute<'a>),
    InnerText(InnerText<'a>),
    OpenElement(OpenElement<'a>),
    CloseElement(CloseElement<'a>),
}

impl<'a> PartialEq for XmlToken<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (XmlToken::Comment(s), XmlToken::Comment(o)) => {
                s.string == o.string && s.position == o.position
            }
            (XmlToken::Attribute(s), XmlToken::Attribute(o)) => {
                s.key.0 == o.key.0
                    && s.key.1 == o.key.1
                    && s.value.is_some() == o.value.is_some()
                    && s.value.as_ref().unwrap().0 == o.value.as_ref().unwrap().0
                    && s.value.as_ref().unwrap().1 == o.value.as_ref().unwrap().1
            }
            (XmlToken::InnerText(s), XmlToken::InnerText(o)) => {
                s.string == o.string && s.position == o.position
            }
            (XmlToken::OpenElement(s), XmlToken::OpenElement(o)) => {
                s.name == o.name && s.position == o.position && s.parent == o.parent
            }
            (XmlToken::CloseElement(s), XmlToken::CloseElement(o)) => {
                s.name == o.name && s.position == o.position && s.parent == o.parent
            }
            _ => false,
        }
    }
}
impl<'a> Eq for XmlToken<'a> {}

impl<'a> PartialEq<Attribute<'a>> for XmlToken<'a> {
    fn eq(&self, other: &Attribute<'a>) -> bool {
        match self {
            XmlToken::Attribute(s) => {
                s.key.0 == other.key.0
                    && s.key.1 == other.key.1
                    && s.value.is_some() == other.value.is_some()
                    && s.value.as_ref().unwrap().0 == other.value.as_ref().unwrap().0
                    && s.value.as_ref().unwrap().1 == other.value.as_ref().unwrap().1
            }
            _ => false,
        }
    }
}

impl<'a> XmlToken<'a> {
    pub(crate) fn comment<S>(str: S, position: FilePosition) -> XmlToken<'a>
    where
        S: Into<String>,
    {
        XmlToken::Comment(Comment {
            string: str.into(),
            position,
        })
    }

    pub(crate) fn attribute<S, S2>(
        key: S,
        value: S2,
        key_position: FilePosition,
        value_position: FilePosition,
        parent: Option<RefXmlToken<'a>>,
    ) -> XmlToken<'a>
    where
        S: Into<String>,
        S2: Into<String>,
    {
        assert!({
            if key_position.line == value_position.line {
                key_position.column <= value_position.column
            } else {
                key_position.line <= value_position.line
            }
        });
        XmlToken::Attribute(Attribute {
            key: (key.into(), key_position),
            value: Some((value.into(), value_position)),
            parent,
        })
    }

    pub(crate) fn invalid_attribute<S>(
        key: S,
        key_position: FilePosition,
        parent: Option<RefXmlToken<'a>>,
    ) -> XmlToken<'a>
    where
        S: Into<String>,
    {
        XmlToken::Attribute(Attribute {
            key: (key.into(), key_position),
            value: None,
            parent,
        })
    }

    pub(crate) fn inner_text<S>(
        str: S,
        position: FilePosition,
        parent: Option<RefXmlToken<'a>>,
    ) -> XmlToken<'a>
    where
        S: Into<String>,
    {
        XmlToken::InnerText(InnerText {
            string: str.into(),
            position,
            parent,
        })
    }

    pub(crate) fn open_element<S>(
        str: S,
        id: usize,
        position: FilePosition,
        parent: Option<RefXmlToken<'a>>,
    ) -> XmlToken<'a>
    where
        S: Into<String>,
    {
        XmlToken::OpenElement(OpenElement {
            name: str.into(),
            id,
            position,
            parent,
            attributes: FxHashMap::default(),
            children: Vec::new(),
        })
    }

    pub(crate) fn close_element<S>(
        str: S,
        position: FilePosition,
        parent: Option<RefXmlToken<'a>>,
    ) -> XmlToken<'a>
    where
        S: Into<String>,
    {
        XmlToken::CloseElement(CloseElement {
            name: Some(str.into()),
            position,
            parent,
        })
    }

    pub(crate) fn close_element_quick(
        position: FilePosition,
        parent: Option<RefXmlToken<'a>>,
    ) -> XmlToken<'a> {
        XmlToken::CloseElement(CloseElement {
            name: None,
            position,
            parent,
        })
    }

    pub fn is_comment(&self) -> bool {
        matches!(self, XmlToken::Comment(..))
    }

    pub fn is_attribute(&self) -> bool {
        matches!(self, XmlToken::Attribute(..))
    }

    pub fn is_inner_text(&self) -> bool {
        matches!(self, XmlToken::InnerText(..))
    }

    pub fn is_open_element(&self) -> bool {
        matches!(self, XmlToken::OpenElement(..))
    }

    pub fn is_close_element(&self) -> bool {
        matches!(self, XmlToken::CloseElement(..))
    }

    pub fn as_comment(&self) -> &Comment {
        match self {
            XmlToken::Comment(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn as_attribute(&self) -> &Attribute<'a> {
        match self {
            XmlToken::Attribute(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn as_inner_text(&self) -> &InnerText<'a> {
        match self {
            XmlToken::InnerText(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn as_open_element(&self) -> &OpenElement<'a> {
        match self {
            XmlToken::OpenElement(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn as_close_element(&self) -> &CloseElement<'a> {
        match self {
            XmlToken::CloseElement(v) => v,
            _ => unreachable!(),
        }
    }

    pub(crate) fn as_mut_open_element(&mut self) -> &mut OpenElement<'a> {
        match self {
            XmlToken::OpenElement(v) => v,
            _ => unreachable!(),
        }
    }

    pub fn position(&self) -> FilePosition {
        match self {
            XmlToken::Comment(v) => v.position,
            XmlToken::Attribute(v) => v.key.1,
            XmlToken::InnerText(v) => v.position,
            XmlToken::OpenElement(v) => v.position,
            XmlToken::CloseElement(v) => v.position,
        }
    }
}

#[derive(PartialEq, Copy, Clone, Debug)]
/// Position in the source file or string.
pub struct FilePosition {
    pub line: usize,
    pub column: usize,
}

impl Default for FilePosition {
    fn default() -> Self {
        FilePosition { line: 1, column: 0 }
    }
}

#[derive(PartialEq, Debug)]
pub(crate) struct Token {
    pub position: FilePosition,
    pub kind: TokenKind,
}
