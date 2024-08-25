use logos::{Logos, Filter};
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use std::fmt;

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum AsukaLanguage {}

impl rowan::Language for AsukaLanguage {
    type Kind = TokenKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.to_u16().unwrap())
    }
}

pub(crate) type SyntaxNode = rowan::SyntaxNode<AsukaLanguage>;
pub(crate) type SyntaxToken = rowan::SyntaxToken<AsukaLanguage>;
pub(crate) type SyntaxElement = rowan::SyntaxElement<AsukaLanguage>;


#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexingError {
    OpUnsupported(String),
    #[default]
    Other,
}

// impl From<std::string::ParseError> for LexingError {
//     fn from(_: std::string::ParseError) -> Self {
//         LexingError::OpUnsupported
//     }
// }

pub enum BuiltInVariable {
    WorkgroupSize,
    // todo
}


const INSTRUCTION_SET : [&'static str; 19] = [
    "OpLabel",
    "OpReturn",
    "OpKill",
    "OpLoad",
    "OpStore",
    "OpConstant",
    "OpIEqual",
    "OpINotEqual",
    "OpSLessThan",
    "OpSGreaterThan",
    "OpSLessThanEqual",
    "OpSGreaterThanEqual",
    "OpBranch",
    "OpBranchConditional",
    //"OpSwitch",
    "OpLoopMerge",
    "OpSelectionMerge",
    "OpAtomicExchange",
    "OpAtomicCompareExchange",
    "OpGroupAll",
];

fn instruction_not_supported(instruction: &str) -> bool {
    for i in INSTRUCTION_SET.iter() {
        if i == &instruction {
            return false;
        }
    }
    true
}

#[derive(Debug, Hash, Eq, PartialOrd, Ord, Copy, Clone, PartialEq, Logos, FromPrimitive, ToPrimitive)]
#[logos(error = LexingError)]
pub enum TokenKind {
    // unused
    BinaryExpr,
    InfixExpr,
    UnaryExpr,
    PrefixExpr,
    PostfixExpr,
    IndexExpr,
    ParenExpr,
    Literal,
    VariableRef,
    VariableDef,
    IfStatement,
    BlockStatement,
    FuncStatement,

    // Control flow Statement
    BranchConditionalStatement,
    BranchStatement,
    SwitchStatement,

    // Statement    
    ReturnStatement,

    // merge instruction
    LoopMergeStatement,
    SelectionMergeStatement,

    // Expression
    LabelExpr,
    ConstantExpr,
    LoadExpr,
    StoreExpr,
    EqualExpr,
    NotEqualExpr,
    GreaterThanExpr,
    GreaterEqualThanExpr,
    LessThanExpr,
    LessThanEqualExpr,
    AtomicExchangeExpr,
    AtomicCompareExchangeExpr,

    Root,
    Statement,
    Error,
    
    #[regex("OpLabel")]
    OpLabel,
    #[regex("OpReturn")]
    OpReturn,
    #[regex("OpKill")]
    OpKill,
    #[regex("OpLoad")]
    OpLoad,
    #[regex("OpStore")]
    OpStore,
    #[regex("OpConstant")]
    OpConstant,
    #[regex("OpIEqual")]
    OpIEqual,
    #[regex("OpINotEqual")]
    OpINotEqual,
    #[regex("OpSLessThan")]
    OpSLessThan,
    #[regex("OpSGreaterThan")]
    OpSGreaterThan,
    #[regex("OpSLessThanEqual")]
    OpSLessThanEqual,
    #[regex("OpSGreaterThanEqual")]
    OpSGreaterThanEqual,
    #[regex("Aligned")]
    Aligned,
    #[regex("OpBranch")]
    OpBranch,
    #[regex("OpBranchConditional")]
    OpBranchConditional,
    #[regex("OpSwitch")]
    OpSwitch,
    #[regex("OpLoopMerge")]
    OpLoopMerge,
    #[regex("OpSelectionMerge")]
    OpSelectionMerge,
    #[regex("OpAtomicExchange")]
    OpAtomicExchange,
    #[regex("OpAtomicCompareExchange")]
    OpAtomicCompareExchange,
    #[regex("OpGroupAll")]
    OpGroupAll,
    
    // #[regex("Op[A-Za-z]*", |lex|
    //     let inst = lex.slice();
    //     if instruction_not_supported(inst) {
    //         Filter::Emit(())
    //     } else{
    //         Filter::Skip
    //     }
    // )]
    // OpUnsupported,
    


    #[regex("-- .*")]
    Comment,
    #[regex("[_A-Za-z_][_A-Za-z0-9_]*")]
    Ident,
    #[regex("[0-9]+\\.[0-9]+")]
    Float,
    #[regex("[0-9]+")]
    Int,
    #[regex("true|false")]
    Bool,
    #[regex("\"[a-z]+\"")]
    String,
    #[regex("float|uint|bool|void")]
    Type,

    // Single-character tokens.
    #[regex("[ ]+")]
    Whitespace,
    #[regex("\n")]
    Newline,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("[")]
    LeftSquare,
    #[token("]")]
    RightSquare,
    #[token(",")]
    Comma,
    #[token(";")]
    SemiColon,
    #[token(".")]
    Dot,
    #[token("\"")]
    Quote,

    // Operation
    #[token("=")]
    Equal,
    #[token("!")]
    Bang,
    #[token("!=")]
    NotEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterThan,
    #[token("<")]
    Less,
    #[token("<=")]
    LessThan,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("#")]
    Pound,
    #[token("?")]
    Optional,
    

}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Whitespace => "whitespace",
            Self::Newline => "newline",
            Self::Ident => "identifier",
            Self::Int => "int",
            Self::Float => "float",
            Self::String => "string",
            Self::Plus => "‘+’",
            Self::Minus => "‘-’",
            Self::Star => "‘*’",
            Self::Slash => "‘/’",
            Self::Equal => "‘=’",
            Self::Bang => "!",
            Self::LeftParen => "‘(’",
            Self::RightParen => "‘)’",
            Self::LeftBrace => "‘{’",
            Self::RightBrace => "‘}’",
            Self::LeftSquare => "‘[‘",
            Self::RightSquare => "‘]‘",
            Self::Comment => "‘comment‘",
            Self::Comma => "‘,‘",
            Self::Dot => "‘.‘",
            Self::SemiColon => "‘;‘",
            Self::Optional => "‘?‘",
            Self::Percent => "‘%‘",
            _ => unreachable!(),
        })
    }
}

impl TokenKind {
    pub(crate) fn is_trivial(self) -> bool {
        match self {
            Self::Whitespace | Self::Comment => true,
            _ => false,
        }
    }

    // todo: add more keywords
    pub(crate) fn is_keyword(self) -> bool {
        match self {
            Self::Let | Self::Function => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod test {
    use super::TokenKind;
    use logos::Logos;
    #[test]
    fn test_token() {
        let input = "%uint_0 = OpConstant %uint 0
        ";
        let mut lexer = TokenKind::lexer(input);
        assert_eq!(lexer.next().unwrap(), Ok(TokenKind::Percent));
        assert_eq!(lexer.next().unwrap(), Ok(TokenKind::Ident));
        assert_eq!(lexer.next().unwrap(), Ok(TokenKind::Whitespace));
        assert_eq!(lexer.next().unwrap(), Ok(TokenKind::Equal));
        assert_eq!(lexer.next().unwrap(), Ok(TokenKind::Whitespace));
        assert_eq!(lexer.next().unwrap(), Ok(TokenKind::OpConstant));
        assert_eq!(lexer.next().unwrap(), Ok(TokenKind::Whitespace));
        assert_eq!(lexer.next().unwrap(), Ok(TokenKind::Percent));
        assert_eq!(lexer.next().unwrap(), Ok(TokenKind::Type));
        assert_eq!(lexer.next().unwrap(), Ok(TokenKind::Whitespace));
        assert_eq!(lexer.next().unwrap(), Ok(TokenKind::Int));
        assert_eq!(lexer.next().unwrap(), Ok(TokenKind::Newline));
    }
}
