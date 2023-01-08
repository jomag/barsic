use std::str::FromStr;

#[derive(Debug, Clone)]
pub enum DataType {
    BYTE,
    WORD,
    LONG,
    SINGLE,
    DOUBLE,
    STRING,
    RFA,
}

// TODO: Change name of error - it's only used for unknown data types in from_str
#[derive(Debug, PartialEq, Eq)]
pub struct DataTypeError;

impl FromStr for DataType {
    type Err = DataTypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "byte" => Ok(DataType::BYTE),
            "word" => Ok(DataType::WORD),
            "long" => Ok(DataType::LONG),
            "single" => Ok(DataType::SINGLE),
            "double" => Ok(DataType::DOUBLE),
            "string" => Ok(DataType::STRING),
            "rfa" => Ok(DataType::RFA),
            _ => Err(DataTypeError),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Keyword {
    DIM,
    GOTO,
    IF,
    THEN,
    ELSE,
    PRINT,
    RETURN,
    RUN,
    GOSUB,
    LIST,
    END,
    LET,
    ON,
    OTHERWISE,
    FOR,
    TO,
    NEXT,
    INPUT,
    CLOSE,
    LINE,
    OPEN,
    OUTPUT,
    AS,
    FILE,
    RESUME,
    MARGIN,
    QUOTE,
    STOP,
    READ,
    DATA,
    DEF,
    PROGRAM,
    FUNCTION,
    FNEND,
    CHANGE,
    WRITE,
    DEBUG,
}

// TODO: Change name of error - it's only used for unknown keywords in from_str
#[derive(Debug, PartialEq, Eq)]
pub struct KeywordError;

impl FromStr for Keyword {
    type Err = KeywordError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "dim" | "dimension" => Ok(Keyword::DIM),
            "goto" => Ok(Keyword::GOTO),
            "if" => Ok(Keyword::IF),
            "then" => Ok(Keyword::THEN),
            "else" => Ok(Keyword::ELSE),
            "print" => Ok(Keyword::PRINT),
            "return" => Ok(Keyword::RETURN),
            "run" => Ok(Keyword::RUN),
            "gosub" => Ok(Keyword::GOSUB),
            "list" => Ok(Keyword::LIST),
            "end" => Ok(Keyword::END),
            "let" => Ok(Keyword::LET),
            "on" => Ok(Keyword::ON),
            "otherwise" => Ok(Keyword::OTHERWISE),
            "for" => Ok(Keyword::FOR),
            "to" => Ok(Keyword::TO),
            "next" => Ok(Keyword::NEXT),
            "input" => Ok(Keyword::INPUT),
            "close" => Ok(Keyword::CLOSE),
            "line" => Ok(Keyword::LINE),
            "open" => Ok(Keyword::OPEN),
            "output" => Ok(Keyword::OUTPUT),
            "as" => Ok(Keyword::AS),
            "file" => Ok(Keyword::FILE),
            "resume" => Ok(Keyword::RESUME),
            "margin" => Ok(Keyword::MARGIN),
            "quote" => Ok(Keyword::QUOTE),
            "stop" => Ok(Keyword::STOP),
            "read" => Ok(Keyword::READ),
            "data" => Ok(Keyword::DATA),
            "def" => Ok(Keyword::DEF),
            "program" => Ok(Keyword::PROGRAM),
            "function" => Ok(Keyword::FUNCTION),
            "fnend" => Ok(Keyword::FNEND),
            "change" => Ok(Keyword::CHANGE),
            "write" => Ok(Keyword::WRITE),
            "debug" => Ok(Keyword::DEBUG),
            _ => Err(KeywordError),
        }
    }
}
