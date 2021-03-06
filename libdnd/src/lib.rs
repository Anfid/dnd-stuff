use std::str::FromStr;
use wasm_bindgen::prelude::*;

mod dto;
mod hand;

use dto::{AnalyzeResponse, CalculateResponse, Dice, Request, Response};
use hand::{Hand, ParseError};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = self, js_name = postMessage)]
    fn post_message(msg: String);
}

#[wasm_bindgen]
pub fn setup() {
    todo!()
}

#[wasm_bindgen]
pub fn message_dispatcher(msg: &str) -> JsValue {
    console_error_panic_hook::set_once();

    let response = if let Ok(message) = serde_json::from_str(msg) {
        match message {
            Request::CalculateDice(Dice { expression }) => calculate_dice(expression).into(),
            Request::AnalyzeDice(Dice { expression }) => analyze_dice(expression).into(),
        }
    } else {
        Response::MessageParseError
    };
    serde_json::to_string(&response).unwrap().into()
}

fn calculate_dice(expr: String) -> Result<CalculateResponse, ParseError> {
    Hand::from_str(expr.as_str()).map(|h| CalculateResponse { result: h.throw() })
}

#[allow(unused)]
fn analyze_dice(expr: String) -> Result<AnalyzeResponse, ParseError> {
    Hand::from_str(expr.as_str()).map(|h| AnalyzeResponse {
        result: h.analyze().into(),
    })
}

#[cfg(all(test, target_arch = "wasm32"))]
mod test {
    use wasm_bindgen_test::*;

    use super::*;
    use crate::dto::Request;

    #[wasm_bindgen_test]
    fn de_message_calculate_dice() {
        let json = r#"{"command":"calculate_dice","expression":"d20"}"#;
        let msg: Request =
            serde_json::from_str(json).expect("Unable to parse valid calculate_dice message");
        if let Request::CalculateDice(Dice { expression }) = msg {
            assert_eq!(expression, String::from("d20"))
        } else {
            panic!("Invalid message type parsed")
        }
    }
}
