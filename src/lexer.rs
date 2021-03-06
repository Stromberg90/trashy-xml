use super::*;

pub fn peek(xml_parser: &XmlParser) -> Option<u8> {
    xml_parser
        .buffer
        .get((xml_parser.index + 1) as usize)
        .copied()
}

pub fn next(xml_parser: &mut XmlParser) -> Option<u8> {
    if xml_parser.started_parsing {
        xml_parser.index += 1;
    } else {
        xml_parser.started_parsing = true;
    }
    if xml_parser.index as usize >= xml_parser.buffer.len() {
        return None;
    }
    let character = xml_parser.buffer[xml_parser.index as usize];
    match character {
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
            xml_parser.position.column += xml_parser.settings.tab_width as u32;
        }
        b'\n' => {
            xml_parser.position.line += 1;
            xml_parser.position.column = 1;
        }
        _ => {
            xml_parser.position.column += 1;
        }
    }
    return Some(character);
}
