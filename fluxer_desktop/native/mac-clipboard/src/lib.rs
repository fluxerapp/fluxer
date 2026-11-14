// SPDX-License-Identifier: AGPL-3.0-or-later

use napi::Task;
use napi::bindgen_prelude::{AsyncTask, Env, Error, Result, Status};
use napi_derive::napi;

pub struct WriteFileReferenceTask {
    path: String,
}

#[napi(js_name = "writeFileReferenceToClipboard")]
pub fn write_file_reference_to_clipboard(
    file_path: String,
) -> Result<AsyncTask<WriteFileReferenceTask>> {
    validate_file_path(&file_path)?;
    Ok(AsyncTask::new(WriteFileReferenceTask { path: file_path }))
}

impl Task for WriteFileReferenceTask {
    type Output = ();
    type JsValue = ();

    fn compute(&mut self) -> Result<Self::Output> {
        platform::write_file_reference(&self.path)
    }

    fn resolve(&mut self, _env: Env, _output: Self::Output) -> Result<Self::JsValue> {
        Ok(())
    }
}

fn validate_file_path(path: &str) -> Result<()> {
    if path.is_empty() {
        return Err(Error::new(Status::InvalidArg, "path must be non-empty"));
    }
    if path.as_bytes().contains(&0) {
        return Err(Error::new(
            Status::InvalidArg,
            "path must not contain NUL bytes",
        ));
    }
    Ok(())
}

#[cfg(target_os = "macos")]
mod platform {
    use dispatch2::run_on_main;
    use napi::bindgen_prelude::{Error, Result, Status};
    use objc2::runtime::ProtocolObject;
    use objc2_app_kit::{NSPasteboard, NSPasteboardWriting};
    use objc2_foundation::{NSArray, NSString, NSURL};

    pub(super) fn write_file_reference(path: &str) -> Result<()> {
        run_on_main(|_mtm| write_file_reference_on_main(path))
    }

    fn write_file_reference_on_main(path: &str) -> Result<()> {
        let ns_path = NSString::from_str(path);
        let ns_url = NSURL::fileURLWithPath(&ns_path);
        let writer = ProtocolObject::<dyn NSPasteboardWriting>::from_ref(&*ns_url);
        let objects = NSArray::arrayWithObject(writer);
        let pasteboard = NSPasteboard::generalPasteboard();

        pasteboard.clearContents();
        if pasteboard.writeObjects(&objects) {
            Ok(())
        } else {
            Err(Error::new(
                Status::GenericFailure,
                "NSPasteboard writeObjects returned NO",
            ))
        }
    }
}

#[cfg(not(target_os = "macos"))]
mod platform {
    use napi::bindgen_prelude::{Error, Result, Status};

    pub(super) fn write_file_reference(_path: &str) -> Result<()> {
        Err(Error::new(
            Status::GenericFailure,
            "mac-clipboard called on non-macOS platform",
        ))
    }
}
