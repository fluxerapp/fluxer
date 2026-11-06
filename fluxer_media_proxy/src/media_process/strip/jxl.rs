// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::MediaError;
use super::heif::read_box;

const CODESTREAM_SIGNATURE: &[u8] = b"\xff\x0a";

fn is_metadata(kind: &[u8]) -> bool {
    matches!(kind, b"Exif" | b"xml " | b"jumb")
}

pub(super) fn strip_boxes(input: &[u8]) -> Result<Vec<u8>, MediaError> {
    if input.starts_with(CODESTREAM_SIGNATURE) {
        return Ok(input.to_vec());
    }
    let mut output = Vec::with_capacity(input.len());
    let mut offset = 0;
    while offset < input.len() {
        let span = read_box(input, offset, input.len())?;
        offset = span.end;
        let removed = match &span.kind {
            b"jbrd" => true,
            b"brob" => input[span.body..span.end].get(..4).is_some_and(is_metadata),
            kind => is_metadata(kind),
        };
        if !removed {
            output.extend_from_slice(&input[span.start..span.end]);
        }
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn boxed(kind: &[u8; 4], body: &[u8]) -> Vec<u8> {
        let mut output = ((body.len() + 8) as u32).to_be_bytes().to_vec();
        output.extend_from_slice(kind);
        output.extend_from_slice(body);
        output
    }

    #[test]
    fn removes_plain_and_compressed_metadata_boxes() {
        let signature = boxed(b"JXL ", b"\r\n\x87\n");
        let ftyp = boxed(b"ftyp", b"jxl \0\0\0\0jxl ");
        let codestream = boxed(b"jxlc", b"\xff\x0aCODESTREAM");
        let input = [
            signature.clone(),
            ftyp.clone(),
            boxed(b"Exif", b"\0\0\0\0GPS59.33"),
            boxed(b"brob", b"xml COMPRESSED"),
            boxed(b"jbrd", b"RECONSTRUCT"),
            codestream.clone(),
        ]
        .concat();

        let output = strip_boxes(&input).unwrap();

        assert_eq!(output, [signature, ftyp, codestream].concat());
    }
}
