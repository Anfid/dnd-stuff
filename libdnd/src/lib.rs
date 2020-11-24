use serde_derive::Deserialize;
use std::str::FromStr;
use wasm_bindgen::prelude::*;

mod hand;

use hand::Hand;

#[derive(Deserialize)]
#[serde(tag = "command", rename_all = "snake_case")]
enum Message {
    CalculateDice(Dice),
    AnalyzeDice(Dice),
}

#[derive(Deserialize)]
struct Dice {
    expression: String,
}

#[wasm_bindgen]
pub fn message_dispatcher(msg: &str) -> JsValue {
    console_error_panic_hook::set_once();

    if let Ok(message) = serde_json::from_str(msg) {
        match message {
            Message::CalculateDice(Dice { expression }) => calculate_dice(expression),
            Message::AnalyzeDice(Dice { expression }) => analyze_dice(expression),
        }
    } else {
        String::from("Message parse error")
    }
    .into()
}

fn calculate_dice(expr: String) -> String {
    Hand::from_str(expr.as_str()).unwrap().throw().to_string()
}

fn analyze_dice(_expr: String) -> String {
    format!("AnalyzeDice unimplemented")
}

#[cfg(all(test, target_arch = "wasm32"))]
mod test {
    use wasm_bindgen_test::*;

    use super::*;

    #[wasm_bindgen_test]
    fn de_message_calculate_dice() {
        let json = r#"{"command":"calculate_dice","expression":"d20"}"#;
        let msg: Message =
            serde_json::from_str(json).expect("Unable to parse valid calculate_dice message");
        if let Message::CalculateDice(Dice { expression }) = msg {
            assert_eq!(expression, String::from("d20"))
        } else {
            panic!("Invalid message type parsed")
        }
    }
}
