// SPDX-License-Identifier: AGPL-3.0-or-later

use super::*;
use crate::test_fixtures::{
    adversarial_media_bytes, animated_gif_fixture, apng_header, minimal_gif,
};

fn vp8x_webp(flags: u8, width_minus_one: u32, height_minus_one: u32) -> Vec<u8> {
    let mut webp = vec![0_u8; 30];
    webp[0..4].copy_from_slice(b"RIFF");
    webp[8..12].copy_from_slice(b"WEBP");
    webp[12..16].copy_from_slice(b"VP8X");
    webp[16..20].copy_from_slice(&10_u32.to_le_bytes());
    webp[20] = flags;
    webp[24..27].copy_from_slice(&width_minus_one.to_le_bytes()[..3]);
    webp[27..30].copy_from_slice(&height_minus_one.to_le_bytes()[..3]);
    webp
}

fn png_chunk(kind: &[u8; 4], payload: &[u8]) -> Vec<u8> {
    let mut chunk = Vec::new();
    chunk.extend_from_slice(
        &u32::try_from(payload.len())
            .expect("test payload fits a PNG chunk")
            .to_be_bytes(),
    );
    chunk.extend_from_slice(kind);
    chunk.extend_from_slice(payload);
    chunk.extend_from_slice(&[0; 4]);
    chunk
}

#[test]
fn sniffs_common_image_formats() {
    assert_eq!("image/png", sniff(b"\x89PNG\r\n\x1a\nxxxx").mime);
    assert_eq!("image/jpeg", sniff(b"\xff\xd8\xff").mime);
    assert_eq!("image/gif", sniff(b"GIF89a\x01\x00\x01\x00").mime);
    assert_eq!("image/tiff", sniff(b"II*\0xxxx").mime);
    assert_eq!("image/tiff", sniff(b"MM\0*xxxx").mime);
    assert_eq!("image/bmp", sniff(b"BMxxxx").mime);
}

#[test]
fn sniffs_apng_via_actl_chunk() {
    let apng = apng_header(2);
    let info = sniff(&apng);
    assert_eq!("image/apng", info.mime);
    assert!(info.animated);
}

#[test]
fn sniffs_animated_webp_via_anim_chunk() {
    let info = sniff(&vp8x_webp(0x02, 0, 0));
    assert_eq!("image/webp", info.mime);
    assert!(info.animated);
}

#[test]
fn sniffs_ftyp_boxes_for_heic_avif_mp4_variants() {
    assert_eq!(
        "image/avif",
        sniff(b"\x00\x00\x00\x20ftypavifsome bytes").mime
    );
    assert_eq!(
        "image/avif",
        sniff(b"\x00\x00\x00\x20ftypavissome bytes").mime
    );
    assert!(sniff(b"\x00\x00\x00\x20ftypavissome bytes").animated);
    assert_eq!(
        "image/heif",
        sniff(b"\x00\x00\x00\x20ftypmif1some bytes").mime
    );
    assert_eq!(
        "image/heic",
        sniff(b"\x00\x00\x00\x20ftypheicsome bytes").mime
    );
    assert_eq!(
        "video/mp4",
        sniff(b"\x00\x00\x00\x20ftypiso5some bytes").mime
    );
    assert_eq!(
        "video/mp4",
        sniff(b"\x00\x00\x00\x20ftypM4V some bytes").mime
    );
    assert_eq!(
        "audio/mp4",
        sniff(b"\x00\x00\x00\x20ftypM4A some bytes").mime
    );
    assert_eq!(
        "video/quicktime",
        sniff(b"\x00\x00\x00\x20ftypqt  some bytes").mime
    );
}

#[test]
fn sniffs_the_literal_heif_ftyp_brand() {
    let heif = b"\x00\x00\x00\x20ftypheifsome bytes";
    assert_eq!("image/heic", sniff(heif).mime);
    assert!(!sniff(heif).animated);
}

#[test]
fn scans_every_compatible_brand_of_a_fully_present_ftyp_box() {
    let compatible_brands = 40;
    let box_size = 16 + compatible_brands * 4;
    let mut ftyp = Vec::new();
    ftyp.extend_from_slice(
        &u32::try_from(box_size)
            .expect("test ftyp box size fits u32")
            .to_be_bytes(),
    );
    ftyp.extend_from_slice(b"ftyp");
    ftyp.extend_from_slice(b"isom");
    ftyp.extend_from_slice(&0x0000_0200_u32.to_be_bytes());
    for _ in 0..compatible_brands - 1 {
        ftyp.extend_from_slice(b"isom");
    }
    ftyp.extend_from_slice(b"avif");
    assert_eq!(box_size, ftyp.len());
    assert!(box_size - 4 > 128);
    assert_eq!("image/avif", sniff(&ftyp).mime);

    let mut truncated_box = ftyp.clone();
    truncated_box[0..4].copy_from_slice(&u32::MAX.to_be_bytes());
    assert_eq!("video/mp4", sniff(&truncated_box).mime);
}

#[test]
fn detect_prefers_m4a_extension_over_generic_mp4_brand() {
    assert_eq!(
        "audio/mp4",
        detect(
            b"\x00\x00\x00\x20ftypisom\x00\x00\x02\x00isomiso2mp41",
            "track.m4a",
            None
        )
    );
}

#[test]
fn sniffs_matroska_vs_webm() {
    let mkv = b"\x1a\x45\xdf\xa3\x9f\x42\x86\x81\x01\x42\xf7\x81\x01\x42\xf2\x81\x04\x42\xf3\x81\x08\x42\x82\x88matroska";
    assert_eq!("video/x-matroska", sniff(mkv).mime);
    assert_eq!(
        "video/webm",
        sniff(b"\x1a\x45\xdf\xa3 here is the webm doctype").mime
    );
}

#[test]
fn sniffs_audio_variants() {
    assert_eq!("audio/mpeg", sniff(b"ID3\x04\x00\x00").mime);
    assert_eq!("audio/mpeg", sniff(b"\xff\xfb\x90\x00").mime);
    assert_eq!("audio/ogg", sniff(b"OggS\x00\x02").mime);
    assert_eq!("video/ogg", sniff(b"OggS\x00\x02xxxx\x80theora").mime);
    assert_eq!("audio/flac", sniff(b"fLaC\x00\x00").mime);
    assert_eq!("audio/wav", sniff(b"RIFF\x00\x00\x00\x00WAVEdata").mime);
}

#[test]
fn extension_mime_covers_common_audio_and_video_containers() {
    assert_eq!(Some("video/ogg"), extension_mime("movie.ogv"));
    assert_eq!(Some("audio/ogg"), extension_mime("voice.opus"));
    assert_eq!(Some("audio/flac"), extension_mime("track.flac"));
    assert_eq!(Some("video/x-matroska"), extension_mime("clip.mkv"));
    assert_eq!(Some("image/tiff"), extension_mime("scan.tiff"));
    assert_eq!(Some("image/bmp"), extension_mime("bitmap.bmp"));
}

#[test]
fn sniffs_pdf() {
    assert_eq!("application/pdf", sniff(b"%PDF-1.7\n").mime);
}

#[test]
fn mime_sniff_recognizes_bounded_container_headers() {
    let png = apng_header(2);
    let sniffed = sniff(&png);
    assert_eq!("image/apng", sniffed.mime);
    assert!(sniffed.animated);
    assert_eq!(2, sniffed.frames);
    assert_eq!((2, 3), (sniffed.width, sniffed.height));
    assert!(sniffed.has_alpha);

    let gif = minimal_gif();
    let sniffed = sniff(&gif);
    assert_eq!("image/gif", sniffed.mime);
    assert!(!sniffed.animated);
    assert_eq!((1, 1), (sniffed.width, sniffed.height));

    let animated = animated_gif_fixture();
    let sniffed = sniff(&animated);
    assert_eq!("image/gif", sniffed.mime);
    assert!(sniffed.animated);
    assert_eq!(2, sniffed.frames);
    assert_eq!((32, 32), (sniffed.width, sniffed.height));

    let sniffed = sniff(&vp8x_webp(0x12, 1, 2));
    assert_eq!("image/webp", sniffed.mime);
    assert!(sniffed.animated);
    assert!(sniffed.has_alpha);
    assert_eq!((2, 3), (sniffed.width, sniffed.height));

    let mut bmff = Vec::new();
    bmff.extend_from_slice(&24_u32.to_be_bytes());
    bmff.extend_from_slice(b"ftyp");
    bmff.extend_from_slice(b"M4A ");
    bmff.extend_from_slice(&0_u32.to_be_bytes());
    bmff.extend_from_slice(b"isom");
    bmff.extend_from_slice(b"M4A ");
    assert_eq!("audio/mp4", sniff(&bmff).mime);
}

#[test]
fn mime_sniffing_stays_bounded_on_adversarial_containers() {
    for bytes in adversarial_media_bytes() {
        let sniffed = sniff(&bytes);
        assert!(sniffed.frames >= 1, "zero frames for {} bytes", bytes.len());

        let complete = sniff_prefix(&bytes, bytes.len());
        assert!(complete.complete, "incomplete for {} bytes", bytes.len());
        assert!(complete.media.frames >= 1);

        let truncated = sniff_prefix(&bytes, bytes.len().saturating_add(1024));
        assert!(truncated.media.frames >= 1);
    }
}

#[test]
fn prefix_sniffing_reports_truncated_containers_as_incomplete() {
    let gif = minimal_gif();
    for prefix_length in 0..gif.len() {
        let result = sniff_prefix(&gif[..prefix_length], gif.len());
        if prefix_length < 6 {
            assert_eq!("application/octet-stream", result.media.mime);
        }
    }
    let complete = sniff_prefix(&gif, gif.len());
    assert!(complete.complete);
    assert_eq!("image/gif", complete.media.mime);
    assert!(!sniff_prefix(&gif[..gif.len() - 1], gif.len()).complete);

    let apng = apng_header(2);
    assert!(sniff_prefix(&apng, apng.len() + 1024).complete);
    assert!(!sniff_prefix(&apng[..20], apng.len()).complete);

    let webp = vp8x_webp(0x02, 0, 0);
    assert!(sniff_prefix(&webp, webp.len() + 1024).complete);
    assert!(!sniff_prefix(&webp[..24], webp.len()).complete);

    assert!(sniff_prefix(b"\xff\xd8\xff\xe0", 4096).complete);
    assert!(!sniff_prefix(b"OggS\x00\x02", 4096).complete);
}

#[test]
fn structural_parsers_reject_the_byte_scan_false_positives() {
    let mut png = b"\x89PNG\r\n\x1a\n".to_vec();
    png.extend_from_slice(&png_chunk(b"IHDR", &[0; 13]));
    png.extend_from_slice(&png_chunk(b"IDAT", b"acTL rides inside the pixel data"));
    let info = sniff(&png);
    assert_eq!("image/png", info.mime);
    assert!(!info.animated);

    let mut gif = minimal_gif();
    gif.extend_from_slice(b"\x21\xffNETSCAPE2.0");
    let info = sniff(&gif);
    assert_eq!("image/gif", info.mime);
    assert!(!info.animated);

    let info = sniff(b"RIFF\x00\x00\x00\x00WEBPVP8XANIMxxxx");
    assert_eq!("image/webp", info.mime);
    assert!(!info.animated);
    assert!(!info.has_alpha);
}

#[test]
fn detect_falls_back_to_the_extension_then_the_declared_header() {
    assert_eq!("audio/mp4", detect(b"", "clip.m4a", None));
    assert_eq!(
        "IMAGE/PNG",
        detect(b"", "unknown.bin", Some("IMAGE/PNG; charset=binary"))
    );
    assert_eq!("application/octet-stream", detect(b"", "unknown.bin", None));
    assert_eq!(
        "image/png",
        detect(b"\x89PNG\r\n\x1a\nxxxx", "audio.m4a", Some("image/gif"))
    );
    assert_eq!("photo.png", filename_for_mime("image/png", "photo"));
    assert_eq!("photo.gif", filename_for_mime("image/png", "photo.gif"));
    assert_eq!("blob.bin", filename_for_mime("application/pdf", "blob"));
}

#[test]
fn sniffs_adts_aac_and_aiff_containers() {
    let mut adts = vec![0xff, 0xf1, 0x50, 0x80, 0x01, 0xa0, 0xfc];
    adts.resize(13, 0);
    assert_eq!("audio/aac", sniff(&adts).mime);
    assert_eq!("audio/aiff", sniff(b"FORM\x00\x00\x00\x12AIFF").mime);
    assert_eq!("audio/aiff", sniff(b"FORM\x00\x00\x00\x12AIFC").mime);
    assert_eq!(
        "application/octet-stream",
        sniff(b"FORM\x00\x00\x00\x12WAVE").mime
    );
}

#[test]
fn content_type_normalization_and_categories_ignore_case_and_reject_control_bytes() {
    assert_eq!(Some("image/png"), normalize(Some("image/png; charset=x")));
    assert_eq!(Some("image/png"), normalize(Some(" \timage/png\t ")));
    assert_eq!(None, normalize(Some("image/p\rng")));
    assert_eq!(None, normalize(Some("   ")));
    assert_eq!(None, normalize(None));

    assert_eq!(Some(Category::Image), category("IMAGE/PNG"));
    assert_eq!(Some(Category::Video), category("Video/MP4"));
    assert_eq!(Some(Category::Audio), category("AUDIO/wav"));
    assert_eq!(None, category("text/css"));
    assert_eq!(None, category("image"));
}

#[test]
fn registry_lookups_canonicalize_extensions_and_passthrough_types() {
    assert_eq!(Some("image/jpeg"), extension_mime("photo.JPG"));
    assert_eq!(Some("text/css; charset=utf-8"), extension_mime("app.css"));
    assert_eq!(None, extension_mime("manual.PDF"));
    assert_eq!(None, extension_mime("archive.zip"));
    assert_eq!(None, extension_mime("noextension"));

    assert_eq!(
        Some("image/png"),
        passthrough_mime(Some("IMAGE/PNG; charset=binary"))
    );
    assert_eq!(
        Some("image/avif"),
        passthrough_mime(Some("image/avif-sequence"))
    );
    assert_eq!(
        Some("application/pdf"),
        passthrough_mime(Some("Application/PDF"))
    );
    assert_eq!(
        Some("text/css; charset=utf-8"),
        passthrough_mime(Some("text/css"))
    );
    assert_eq!(None, passthrough_mime(Some("application/octet-stream")));
    assert_eq!(None, passthrough_mime(None));
}

#[test]
fn supported_media_mimes_gate_metadata_on_the_frozen_allow_list() {
    assert!(is_supported_media_mime("image/heif"));
    assert!(is_supported_media_mime("image/heic"));
    assert!(is_supported_media_mime("video/mp4; codecs=avc1"));
    assert!(!is_supported_media_mime("image/avif-sequence"));
    assert!(!is_supported_media_mime("IMAGE/PNG"));
    assert!(!is_supported_media_mime("application/pdf"));
    assert!(!is_supported_media_mime(""));
}

#[test]
fn every_iso_bmff_brand_matches_the_old_era_table() {
    for (brand, expected) in [
        (&b"heic"[..], "image/heic"),
        (b"heix", "image/heic"),
        (b"heif", "image/heic"),
        (b"heim", "image/heic"),
        (b"heis", "image/heic"),
        (b"hevc", "image/heic"),
        (b"hevx", "image/heic"),
        (b"hevm", "image/heic"),
        (b"hevs", "image/heic"),
        (b"mif1", "image/heif"),
        (b"msf1", "image/heif"),
    ] {
        let mut bytes = b"\x00\x00\x00\x20ftyp".to_vec();
        bytes.extend_from_slice(brand);
        bytes.extend_from_slice(b"some bytes");
        let info = sniff(&bytes);
        assert_eq!(expected, info.mime, "brand {brand:?}");
        assert!(!info.animated, "brand {brand:?} must not be animated");
    }
}
