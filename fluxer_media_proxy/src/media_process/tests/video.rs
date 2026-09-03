// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::{MediaError, extract_video_thumbnail};
use super::fixtures::{decode_rgba, metadata_value, test_media_limits};
use crate::{
    output_format::OutputFormat,
    test_fixtures::{
        ffmpeg_gen_media, ffmpeg_gen_mp4, ffmpeg_gen_rotated_mp4, ffmpeg_mirror_mp4, png_dimensions,
    },
};

fn color_tagged_video(primaries: &str, transfer: &str, matrix: &str) -> Option<Vec<u8>> {
    let params =
        format!("setparams=color_primaries={primaries}:color_trc={transfer}:colorspace={matrix}");
    ffmpeg_gen_media(
        "fixture.mkv",
        &[
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=320x240:rate=10:duration=1",
            "-vf",
            &params,
            "-pix_fmt",
            "yuv420p",
            "-c:v",
            "mpeg4",
            "-f",
            "matroska",
        ],
    )
}

#[test]
fn video_thumbnail_accepts_smpte170m_sd_primaries() {
    let Some(video) = color_tagged_video("smpte170m", "smpte170m", "smpte170m") else {
        eprintln!("skipping: ffmpeg CLI not available");
        return;
    };
    let thumb = extract_video_thumbnail(&video, OutputFormat::PNG, &test_media_limits())
        .expect("ntsc sd video should thumbnail");
    assert_eq!(png_dimensions(&thumb.bytes), Some((320, 240)));
}

#[test]
fn video_thumbnail_accepts_bt470bg_pal_primaries() {
    let Some(video) = color_tagged_video("bt470bg", "bt470bg", "bt470bg") else {
        eprintln!("skipping: ffmpeg CLI not available");
        return;
    };
    let thumb = extract_video_thumbnail(&video, OutputFormat::PNG, &test_media_limits())
        .expect("pal sd video should thumbnail");
    assert_eq!(png_dimensions(&thumb.bytes), Some((320, 240)));

    let film = color_tagged_video("film", "bt470m", "bt470bg").expect("film fixture");
    let thumb = extract_video_thumbnail(&film, OutputFormat::PNG, &test_media_limits())
        .expect("film primaries should thumbnail");
    assert_eq!(png_dimensions(&thumb.bytes), Some((320, 240)));
}

#[test]
fn video_thumbnail_accepts_every_widened_sdr_transfer() {
    let transfers = ["smpte240m", "linear", "iec61966-2-1", "bt470m", "bt470bg"];
    let Some(first) = color_tagged_video("bt709", transfers[0], "bt709") else {
        eprintln!("skipping: ffmpeg CLI not available");
        return;
    };
    for (index, transfer) in transfers.iter().enumerate() {
        let video = if index == 0 {
            first.clone()
        } else {
            color_tagged_video("bt709", transfer, "bt709")
                .unwrap_or_else(|| panic!("{transfer} fixture"))
        };
        let thumb = extract_video_thumbnail(&video, OutputFormat::PNG, &test_media_limits())
            .unwrap_or_else(|error| panic!("{transfer} should thumbnail: {error:?}"));
        assert_eq!(png_dimensions(&thumb.bytes), Some((320, 240)), "{transfer}");
    }
}

#[test]
fn video_thumbnail_still_rejects_pq_bt2020_hdr() {
    let Some(hdr) = color_tagged_video("bt2020", "smpte2084", "bt2020nc") else {
        eprintln!("skipping: ffmpeg CLI not available");
        return;
    };
    assert_eq!(
        Some(MediaError::MediaDecodeFailed),
        extract_video_thumbnail(&hdr, OutputFormat::PNG, &test_media_limits()).err()
    );

    let pq_only = color_tagged_video("bt709", "smpte2084", "bt709").expect("pq fixture");
    assert_eq!(
        Some(MediaError::MediaDecodeFailed),
        extract_video_thumbnail(&pq_only, OutputFormat::PNG, &test_media_limits()).err()
    );

    let hlg = color_tagged_video("bt2020", "arib-std-b67", "bt709").expect("hlg fixture");
    assert_eq!(
        Some(MediaError::MediaDecodeFailed),
        extract_video_thumbnail(&hlg, OutputFormat::PNG, &test_media_limits()).err()
    );

    let wide_primaries = color_tagged_video("bt2020", "bt709", "bt709").expect("bt2020 fixture");
    assert_eq!(
        Some(MediaError::MediaDecodeFailed),
        extract_video_thumbnail(&wide_primaries, OutputFormat::PNG, &test_media_limits()).err()
    );
}

#[test]
fn video_thumbnail_corrects_display_geometry() {
    let Some(plain) = ffmpeg_gen_mp4(&[
        "-f",
        "lavfi",
        "-i",
        "testsrc=size=320x240:rate=10:duration=1",
        "-pix_fmt",
        "yuv420p",
        "-f",
        "mp4",
    ]) else {
        eprintln!("skipping: ffmpeg CLI not available");
        return;
    };
    let thumb = extract_video_thumbnail(&plain, OutputFormat::PNG, &test_media_limits())
        .expect("plain thumbnail");
    assert_eq!(
        png_dimensions(&thumb.bytes),
        Some((320, 240)),
        "square-pixel video should keep its coded dimensions"
    );

    let anamorphic = ffmpeg_gen_mp4(&[
        "-f",
        "lavfi",
        "-i",
        "testsrc=size=1280x720:rate=10:duration=1",
        "-vf",
        "setsar=2/1",
        "-pix_fmt",
        "yuv420p",
        "-f",
        "mp4",
    ])
    .expect("anamorphic fixture");
    let thumb = extract_video_thumbnail(&anamorphic, OutputFormat::PNG, &test_media_limits())
        .expect("anamorphic thumbnail");
    let (w, h) = png_dimensions(&thumb.bytes).expect("anamorphic png dimensions");
    assert_eq!(h, 720, "anamorphic height preserved");
    assert!(
        (i64::from(w) - 2560).abs() <= 2,
        "anamorphic width should expand to the ~2560 display width, got {w}"
    );

    let narrow = ffmpeg_gen_mp4(&[
        "-f",
        "lavfi",
        "-i",
        "testsrc=size=1280x720:rate=10:duration=1",
        "-vf",
        "setsar=1/2",
        "-pix_fmt",
        "yuv420p",
        "-f",
        "mp4",
    ])
    .expect("narrow anamorphic fixture");
    let thumb = extract_video_thumbnail(&narrow, OutputFormat::PNG, &test_media_limits())
        .expect("narrow anamorphic thumbnail");
    assert_eq!(
        png_dimensions(&thumb.bytes),
        Some((640, 720)),
        "sub-square pixel video should shrink width to its display size"
    );

    let rotated = ffmpeg_gen_rotated_mp4(
        "90",
        &[
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=640x480:rate=10:duration=1",
            "-pix_fmt",
            "yuv420p",
            "-f",
            "mp4",
        ],
    )
    .expect("rotated fixture");
    let thumb = extract_video_thumbnail(&rotated, OutputFormat::PNG, &test_media_limits())
        .expect("rotated thumbnail");
    assert_eq!(
        png_dimensions(&thumb.bytes),
        Some((480, 640)),
        "rotation-metadata video should present in its display (portrait) orientation"
    );

    let rotated_counterclockwise = ffmpeg_gen_rotated_mp4(
        "-90",
        &[
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=640x480:rate=10:duration=1",
            "-pix_fmt",
            "yuv420p",
            "-f",
            "mp4",
        ],
    )
    .expect("counterclockwise rotated fixture");
    let thumb = extract_video_thumbnail(
        &rotated_counterclockwise,
        OutputFormat::PNG,
        &test_media_limits(),
    )
    .expect("counterclockwise rotated thumbnail");
    assert_eq!(
        png_dimensions(&thumb.bytes),
        Some((480, 640)),
        "either quarter-turn direction should swap dimensions"
    );

    let rotated_anamorphic = ffmpeg_gen_rotated_mp4(
        "90",
        &[
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=320x180:rate=10:duration=1",
            "-vf",
            "setsar=2/1",
            "-pix_fmt",
            "yuv420p",
            "-f",
            "mp4",
        ],
    )
    .expect("rotated anamorphic fixture");
    let thumb =
        extract_video_thumbnail(&rotated_anamorphic, OutputFormat::PNG, &test_media_limits())
            .expect("rotated anamorphic thumbnail");
    assert_eq!(
        png_dimensions(&thumb.bytes),
        Some((180, 640)),
        "SAR correction should happen in coded space before rotation"
    );

    let single = ffmpeg_gen_mp4(&[
        "-f",
        "lavfi",
        "-i",
        "testsrc=size=200x150:rate=1:duration=1",
        "-frames:v",
        "1",
        "-pix_fmt",
        "yuv420p",
        "-f",
        "mp4",
    ])
    .expect("single-frame fixture");
    let thumb = extract_video_thumbnail(&single, OutputFormat::PNG, &test_media_limits())
        .expect("single-frame thumbnail");
    assert_eq!(
        png_dimensions(&thumb.bytes),
        Some((200, 150)),
        "single-frame clip should still produce a thumbnail"
    );
}

#[test]
fn mirrored_display_matrix_flips_the_video_frame_horizontally() {
    let source_args = [
        "-f",
        "lavfi",
        "-i",
        "testsrc=size=64x48:rate=10:duration=1",
        "-pix_fmt",
        "yuv420p",
        "-f",
        "mp4",
    ];
    let Some(plain) = ffmpeg_gen_mp4(&source_args) else {
        eprintln!("skipping: ffmpeg CLI not available");
        return;
    };
    let mirrored = ffmpeg_mirror_mp4(&plain).expect("mirrored fixture");
    let plain_thumb = extract_video_thumbnail(&plain, OutputFormat::PNG, &test_media_limits())
        .expect("plain thumbnail");
    let mirrored_thumb =
        extract_video_thumbnail(&mirrored, OutputFormat::PNG, &test_media_limits())
            .expect("mirrored thumbnail");
    let (width, height, plain_rgba) = decode_rgba(&plain_thumb.bytes);
    let (mirrored_width, mirrored_height, mirrored_rgba) = decode_rgba(&mirrored_thumb.bytes);
    assert_eq!((64, 48), (width, height));
    assert_eq!((width, height), (mirrored_width, mirrored_height));
    assert_ne!(
        plain_rgba, mirrored_rgba,
        "the mirrored source must not decode to the same pixels"
    );
    let row_stride = width as usize * 4;
    for y in 0..height as usize {
        for x in 0..width as usize {
            let source = y * row_stride + x * 4;
            let mirror = y * row_stride + (width as usize - 1 - x) * 4;
            assert_eq!(
                plain_rgba[source..source + 4],
                mirrored_rgba[mirror..mirror + 4],
                "pixel ({x}, {y}) is not the horizontal mirror of the source"
            );
        }
    }
}

#[test]
fn video_metadata_placeholder_and_dimensions_are_display_corrected() {
    let Some(rotated) = ffmpeg_gen_rotated_mp4(
        "90",
        &[
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=640x480:rate=10:duration=1",
            "-pix_fmt",
            "yuv420p",
            "-f",
            "mp4",
        ],
    ) else {
        eprintln!("skipping: ffmpeg CLI not available");
        return;
    };
    let meta = metadata_value(&rotated, "rotated.mp4");
    assert_eq!(
        meta["width"].as_u64(),
        Some(480),
        "stored width is the display (portrait) width"
    );
    assert_eq!(
        meta["height"].as_u64(),
        Some(640),
        "stored height is the display (portrait) height"
    );
    assert!(
        meta["placeholder"].as_str().is_some_and(|s| !s.is_empty()),
        "placeholder should be generated from the display-corrected frame"
    );

    let anamorphic = ffmpeg_gen_mp4(&[
        "-f",
        "lavfi",
        "-i",
        "testsrc=size=1280x720:rate=10:duration=1",
        "-vf",
        "setsar=2/1",
        "-pix_fmt",
        "yuv420p",
        "-f",
        "mp4",
    ])
    .expect("anamorphic fixture");
    let meta = metadata_value(&anamorphic, "anamorphic.mp4");
    assert_eq!(
        meta["height"].as_u64(),
        Some(720),
        "stored height preserved"
    );
    let w = meta["width"].as_u64().expect("stored width");
    assert!(
        (w as i64 - 2560).abs() <= 2,
        "stored width expands to the display width, got {w}"
    );
}
