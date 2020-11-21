use serde_derive::Deserialize;
use wasm_bindgen::prelude::*;

mod utils;

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
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn message_dispatcher(msg: &str) -> JsValue {
    utils::set_panic_hook();

    let message = serde_json::from_str(msg).unwrap();
    match message {
        Message::CalculateDice(Dice { expression }) => calculate_dice(expression),
        Message::AnalyzeDice(Dice { expression: _ }) => todo!(),
    }
    "TODO".into()
}

fn calculate_dice(expr: String) {
    alert(format!("your expr: {:?}", expr).as_str())
}

#[cfg(all(test, target_arch = "wasm32"))]
mod test {
    extern crate wasm_bindgen_test;
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
