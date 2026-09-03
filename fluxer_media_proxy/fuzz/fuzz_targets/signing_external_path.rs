#![no_main]

// SPDX-License-Identifier: AGPL-3.0-or-later

use fluxer_media_proxy::{external_path, signing};
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(text) = std::str::from_utf8(data) {
        if let Ok(path) = external_path::build_external_media_proxy_path(text) {
            let _ = external_path::reconstruct_original_url(&path);
            let sig = signing::create_signature(&path, b"fuzz-secret");
            let _ = signing::verify_signature(&path, &sig, b"fuzz-secret");
            let _ = signing::verify_signature(text, &sig, b"other-secret");
        }
        let opaque = external_path::build_opaque_external_media_proxy_path(text);
        let _ = external_path::reconstruct_original_url(&opaque);
        let opaque_signature = signing::create_signature(&opaque, b"fuzz-secret");
        let _ = signing::verify_signature(&opaque, &opaque_signature, b"fuzz-secret");
        let _ = signing::verify_signature(&opaque, "", b"fuzz-secret");
        let _ = external_path::reconstruct_original_url(text);
        let _ = external_path::percent_decode(text, true);
        let _ = external_path::percent_decode(text, false);
        let _ = external_path::percent_decode_string(text, true);
    }
});
