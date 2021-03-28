use super::*;

#[inline]
pub(crate) fn peek(buffer: &mut Peekable<Iter<'_, u8>>) -> Option<u8> {
    buffer.peek().copied().cloned()
}

#[inline]
pub(crate) fn next(tokenizer: &mut Tokenizer<'_>) -> Option<u8> {
    if let Some(character) = tokenizer.buffer.next() {
        if *character == b'\r' && peek(&mut tokenizer.buffer) == Some(b'\n') {
        } else {
            tokenizer.position.column += 1;
        }
        if *character == b'\n' {
            tokenizer.position.line += 1;
            tokenizer.position.column = 0;
        }
        return Some(*character);
    }
    None
}
