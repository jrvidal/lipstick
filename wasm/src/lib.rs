mod utils;

use compiler::*;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn initialize() {
    utils::set_panic_hook();
}

/// Compiles the program. `callback` is a Node-style callback that might receive multiple errors
/// as an array of arrays [[msg, syntax, startLine, startColumn, endLine, endColumn]]
#[wasm_bindgen]
pub fn compile(source: String, callback: js_sys::Function) {
    let session = Session::new("file".to_string(), source);

    let err = match session.compile() {
        Ok(program) => callback
            .call2(
                &JsValue::UNDEFINED,
                &JsValue::UNDEFINED,
                &(format!("{}", program).into()),
            )
            .err(),

        Err(err) => {
            let syntax = err.syntax;

            let errors = err.into_ansi().into_iter().map(|(error, span)| {
                let start = span.start();
                let end = span.end();

                [
                    &JsValue::from(&error),
                    &syntax.into(),
                    &(start.line as u32).into(),
                    &(start.column as u32).into(),
                    &(end.line as u32).into(),
                    &(end.column as u32).into(),
                ]
                .into_iter()
                .collect::<js_sys::Array>()
            });

            let errors_array = errors.collect::<js_sys::Array>();

            callback
                .call1(&JsValue::UNDEFINED, &errors_array.into())
                .err()
        }
    };

    if let Some(err) = err {
        wasm_bindgen::throw_val(err);
    }
}
