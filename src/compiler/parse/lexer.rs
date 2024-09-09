use crate::compiler::parse::syntax::TokenKind;
use logos::Logos;
use num_derive::{FromPrimitive, ToPrimitive};
use std::ops::Range as StdRange;
use text_size::{TextRange, TextSize};
// wrapper for logo's lexer, so we can get inner value of token
pub(crate) struct Lexer<'t> {
    inner: logos::Lexer<'t, TokenKind>,
    tokens_to_capture: Vec<Token<'t>>, // Store captured tokens
    inside_function: bool,
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
            tokens_to_capture: Vec::new(),
            inside_function: false,
        }
    }
}

impl<'t> Iterator for Lexer<'t> {
    type Item = Token<'t>;

    fn next(&mut self) -> Option<Self::Item> {
        let kind = match self.inner.next()?{
            Ok(kind) => kind,
            Err(_) => panic!("Error: unexpected token at {:?}", self.inner.span()),
        };
        let text = self.inner.slice();
        let range = {
            let StdRange { start, end } = self.inner.span();
            let start = TextSize::try_from(start).unwrap();
            let end = TextSize::try_from(end).unwrap();

            TextRange::new(start, end)
        };
        Some(Self::Item { kind, text, range })
    }

    // /// this function is called when we call next() on Lexer
    // /// it skips tokens that are not in the function call
    // fn next(&mut self) -> Option<Self::Item> {
    //     // If we have captured tokens, return them one-by-one
    //     if let Some(token) = self.tokens_to_capture.pop() {
    //         return Some(token);
    //     }

    //     // Continue lexing and capture tokens we're interested in
    //     'outer: loop {
    //         let kind = self.inner.next()?.expect("Error: lexer returned None");
    //         let text = self.inner.slice();
    //         let range = {
    //             let start = TextSize::try_from(self.inner.span().start).unwrap();
    //             let end = TextSize::try_from(self.inner.span().end).unwrap();
    //             TextRange::new(start, end)
    //         };

    //         match kind {
    //             TokenKind::OpDecorate => {
    //                 // Start capturing the entire instruction
    //                 let mut capture_instruction = vec![Token { kind, text, range }];
    //                 let mut contains_builtin = false;
    //                 self.tokens_to_capture.push(Token { kind, text, range });

    //                 // Continue capturing until we hit a newline or end of input
    //                 'inner: loop  {
    //                     let next_token = self.inner.next()?.expect("Error: lexer returned None");
    //                     let next_text = self.inner.slice();
    //                     let next_range = {
    //                         let start = TextSize::try_from(self.inner.span().start).unwrap();
    //                         let end = TextSize::try_from(self.inner.span().end).unwrap();
    //                         TextRange::new(start, end)
    //                     };

    //                     // Capture each argument token until we hit a newline
    //                     if next_token == TokenKind::Newline {
    //                         break 'inner; // Stop capturing at newline
    //                     }
    //                     if next_token == TokenKind::BuiltIn {
    //                         contains_builtin = true;  // Set flag if BuiltIn is found
    //                     }
    //                     capture_instruction.push(Token { kind: next_token, text: next_text, range: next_range });
    //                 }
    //                 // Only keep the instruction if it contains "BuiltIn"
    //                 if contains_builtin {
    //                     self.tokens_to_capture.extend(capture_instruction);
    //                 }
    //             },

    //             TokenKind::OpFunction => {
    //                 // Start capturing the entire function
    //                 self.tokens_to_capture.push(Token { kind, text, range });
    //                 self.inside_function = true;

    //             },
    //             // Handle OpFunctionEnd and other tokens
    //             TokenKind::OpFunctionEnd => {
    //                 if self.inside_function {
    //                     self.tokens_to_capture.push(Token { kind, text, range });
    //                     self.inside_function = false;
    //                 }
    //             }
    //             _ if self.inside_function => {
    //                 // Capture tokens inside the function as before
    //                 self.tokens_to_capture.push(Token { kind, text, range });
    //             }
    //             _ => break 'outer, // Skip tokens outside of functions
    //         }
    //     }

    //     // Return the next captured token if any, or continue lexing
    //     self.tokens_to_capture.pop()
    // }
}

mod test {
    use super::TokenKind;
    use logos::Logos;

    // #[test]
    // fn test_iterator_next() {
    //     let input = "; SPIR-V
    //     ; Version: 1.0
    //     ; Generator: Khronos Glslang Reference Front End; 11
    //     ; Bound: 61
    //     ; Schema: 0
    //                    OpCapability Shader
    //                    OpCapability GroupNonUniform
    //               %1 = OpExtInstImport \"GLSL.std.450\"
    //                    OpMemoryModel Logical GLSL450
    //                    OpEntryPoint GLCompute %main \"main\" %gl_SubgroupInvocationID
    //                    OpExecutionMode %main LocalSize 256 1 1
    //                    OpSource GLSL 450
    //                    OpSourceExtension \"GL_EXT_shader_atomic_float\"
    //                    OpSourceExtension \"GL_KHR_shader_subgroup_basic\"
    //                    OpDecorate %gl_SubgroupInvocationID RelaxedPrecision
    //                    OpDecorate %gl_SubgroupInvocationID BuiltIn SubgroupLocalInvocationId
    //                    OpDecorate %27 RelaxedPrecision
    //                    OpDecorate %gl_WorkGroupSize BuiltIn WorkgroupSize
    //                    %main = OpFunction %void None %3
    //                    %5 = OpLabel
    //     ";
    // }
}
