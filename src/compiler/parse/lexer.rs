use crate::compiler::parse::syntax::TokenKind;
use logos::Logos;
use num_derive::{FromPrimitive, ToPrimitive};
use std::ops::Range as StdRange;
use text_size::{TextRange, TextSize};
// wrapper for logo's lexer, so we can get inner value of token
pub(crate) struct Lexer<'t> {
    inner: logos::Lexer<'t, TokenKind>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct Token<'t> {
    pub(crate) kind: TokenKind,
    pub(crate) text: &'t str,
    pub(crate) range: TextRange,
}

impl<'t> Lexer<'t> {
    pub(crate) fn new(input: &'t str) -> Self {
        Self {
            inner: TokenKind::lexer(input),
        }
    }
}

impl<'t> Iterator for Lexer<'t> {
    type Item = Token<'t>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = self.inner.next()?.expect("Error: lexer returned None");
        let text = self.inner.slice();
        let range = {
            let StdRange { start, end } = self.inner.span();
            let start = TextSize::try_from(start).unwrap();
            let end = TextSize::try_from(end).unwrap();

            TextRange::new(start, end)
        };
        Some(Self::Item { kind, text, range })
    }
}
