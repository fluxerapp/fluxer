// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    image_quality::ImageQuality,
    image_transform::{ImageOptions, ResizeMode},
    media_limits::MediaLimits,
    media_process::{native_runtime::ensure_vips_init, transform_image},
    metrics::transform::TransformMetrics,
    native::{self, NativeStatus},
    output_format::OutputFormat,
    test_fixtures::{synthetic_png, synthetic_png16},
};
use std::ffi::CString;

fn transformed(input: &[u8], options: ImageOptions) -> Vec<u8> {
    transform_image(
        input,
        &options,
        &MediaLimits::default_from_config(),
        &TransformMetrics::default(),
    )
    .expect("the transform succeeds")
    .bytes
}

#[test]
fn sixteen_bit_png_converts_like_its_eight_bit_twin() {
    let jpeg = ImageOptions {
        format: OutputFormat::JPEG,
        quality: ImageQuality::High,
        ..Default::default()
    };
    assert!(
        transformed(&synthetic_png(64, 48), jpeg) == transformed(&synthetic_png16(64, 48), jpeg),
        "a 16-bit png encodes to the same jpeg as its 8-bit twin"
    );
}

#[test]
fn sixteen_bit_png_cover_resize_encodes_eight_bit_png() {
    let png = transformed(
        &synthetic_png16(800, 600),
        ImageOptions {
            width: Some(96),
            height: Some(72),
            format: OutputFormat::PNG,
            quality: ImageQuality::Lossless,
            resize_mode: ResizeMode::Cover,
            ..Default::default()
        },
    );
    assert_eq!(b"\x89PNG\r\n\x1a\n", &png[..8]);
    assert_eq!(8, png[24]);
}

#[test]
fn blocked_libvips_loaders_cannot_decode_untrusted_input() {
    ensure_vips_init().expect("libvips initialises");
    let ppm = b"P6\n2 2\n255\n\x00\x00\x00\xff\xff\xff\x00\x00\x00\xff\xff\xff".to_vec();
    let options = CString::new("access=sequential").expect("static string has no NUL");
    let raw = unsafe {
        native::fluxer_vips_image_new_from_buffer(ppm.as_ptr().cast(), ppm.len(), options.as_ptr())
    };
    assert!(raw.is_null());
    unsafe { native::fluxer_vips_error_clear() };
}

fn bmff_box(kind: &[u8; 4], body: &[u8]) -> Vec<u8> {
    let size = u32::try_from(body.len() + 8).expect("test box fits in u32");
    let mut output = size.to_be_bytes().to_vec();
    output.extend_from_slice(kind);
    output.extend_from_slice(body);
    output
}

fn heif_with_sample_table(kind: &[u8; 4], count_offset: usize, entry_count: u32) -> Vec<u8> {
    let mut infe = vec![2, 0, 0, 0, 0, 1, 0, 0];
    infe.extend_from_slice(b"hvc1");
    infe.push(0);
    let mut iinf = vec![0, 0, 0, 0, 0, 1];
    iinf.extend_from_slice(&bmff_box(b"infe", &infe));
    let mut iloc = vec![0, 0, 0, 0, 0x44, 0x00, 0, 1, 0, 1, 0, 0, 0, 1];
    iloc.extend_from_slice(&0u32.to_be_bytes());
    iloc.extend_from_slice(&1u32.to_be_bytes());
    let mut meta = vec![0, 0, 0, 0];
    meta.extend_from_slice(&bmff_box(b"iinf", &iinf));
    meta.extend_from_slice(&bmff_box(b"iloc", &iloc));
    let mut table = vec![0; count_offset];
    table.extend_from_slice(&entry_count.to_be_bytes());
    let stbl = bmff_box(b"stbl", &bmff_box(kind, &table));
    let minf = bmff_box(b"minf", &stbl);
    let mdia = bmff_box(b"mdia", &minf);
    let trak = bmff_box(b"trak", &mdia);
    let mut file = bmff_box(b"ftyp", b"heic\0\0\0\0mif1heic");
    file.extend_from_slice(&bmff_box(b"meta", &meta));
    file.extend_from_slice(&bmff_box(b"moov", &trak));
    file
}

fn heif_validate_status(input: &[u8]) -> NativeStatus {
    NativeStatus::from_code(unsafe {
        native::fluxer_heif_validate(input.as_ptr().cast(), input.len(), 0)
    })
}

fn sample_tables_status(input: &[u8]) -> NativeStatus {
    NativeStatus::from_code(unsafe {
        native::fluxer_bmff_sample_tables_validate(input.as_ptr().cast(), input.len())
    })
}

#[test]
fn heif_validation_limits_sample_table_entry_counts() {
    for (kind, count_offset) in [(b"stsc", 4), (b"stco", 4), (b"co64", 4), (b"stsz", 8)] {
        let within = heif_with_sample_table(kind, count_offset, 65_536);
        assert_eq!(heif_validate_status(&within), NativeStatus::Ok);
        assert_eq!(sample_tables_status(&within), NativeStatus::Ok);
        let over = heif_with_sample_table(kind, count_offset, 65_537);
        assert_eq!(heif_validate_status(&over), NativeStatus::CodecFailure);
        assert_eq!(sample_tables_status(&over), NativeStatus::CodecFailure);
    }
}

#[test]
fn sample_table_gate_ignores_non_bmff_input() {
    assert_eq!(
        sample_tables_status(b"P6\n1 1\n255\n\0\0\0"),
        NativeStatus::Ok
    );
}

fn with_truncated_moov(mut file: Vec<u8>) -> Vec<u8> {
    let moov = file
        .windows(4)
        .rposition(|w| w == b"moov")
        .expect("moov box")
        - 4;
    let size = u32::from_be_bytes(file[moov..moov + 4].try_into().expect("size"));
    file[moov..moov + 4].copy_from_slice(&(size + 64).to_be_bytes());
    file
}

#[test]
fn sample_table_gate_checks_truncated_and_padded_files() {
    for (entry_count, expected) in [(16, NativeStatus::Ok), (65_537, NativeStatus::CodecFailure)] {
        let mut padded = heif_with_sample_table(b"stsc", 4, entry_count);
        padded.extend_from_slice(&[0, 0, 0]);
        assert_eq!(sample_tables_status(&padded), expected);
        let truncated = with_truncated_moov(heif_with_sample_table(b"stsc", 4, entry_count));
        assert_eq!(sample_tables_status(&truncated), expected);
    }
    let mut undersized = heif_with_sample_table(b"stsc", 4, 16);
    undersized.extend_from_slice(&[0, 0, 0, 4, b'f', b'r', b'e', b'e']);
    assert_eq!(
        sample_tables_status(&undersized),
        NativeStatus::CodecFailure
    );
}
