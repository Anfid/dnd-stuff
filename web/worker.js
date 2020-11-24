importScripts("libdnd/libdnd.js")

async function setup() {
    await wasm_bindgen("libdnd/libdnd_bg.wasm")
    self.onmessage = message => {
        let resp = wasm_bindgen.message_dispatcher(message.data)
        self.postMessage(resp)
    }
}

setup()
