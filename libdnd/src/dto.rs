use crate::hand::ParseError;
use serde_derive::{Deserialize, Serialize};

#[derive(Serialize)]
#[serde(untagged, rename_all = "snake_case")]
pub enum CommandResult<T, E>
where
    T: serde::Serialize,
    E: serde::Serialize,
{
    Result(T),
    Error(E),
}

#[derive(Serialize)]
#[serde(tag = "command", rename_all = "snake_case")]
pub enum Response {
    CalculateDice(CommandResult<CalculateResponse, ParseError>),
    AnalyzeDice,
    MessageParseError,
}

impl From<Result<CalculateResponse, ParseError>> for Response {
    fn from(res: Result<CalculateResponse, ParseError>) -> Self {
        match res {
            Ok(res) => Response::CalculateDice(CommandResult::Result(res)),
            Err(e) => Response::CalculateDice(CommandResult::Error(e)),
        }
    }
}

#[derive(Serialize)]
pub struct CalculateResponse {
    pub result: i64,
}

#[derive(Deserialize)]
#[serde(tag = "command", rename_all = "snake_case")]
pub enum Request {
    CalculateDice(Dice),
    AnalyzeDice(Dice),
}

#[derive(Deserialize, Serialize)]
pub struct Dice {
    pub expression: String,
}
