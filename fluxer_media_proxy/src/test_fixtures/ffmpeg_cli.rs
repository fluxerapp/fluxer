// SPDX-License-Identifier: AGPL-3.0-or-later

pub const REQUIRE_MEDIA_FIXTURES_ENV: &str = "FLUXER_REQUIRE_MEDIA_FIXTURES";

pub fn media_fixtures_are_required() -> bool {
    std::env::var_os(REQUIRE_MEDIA_FIXTURES_ENV).is_some_and(|value| !value.is_empty())
}

fn ffmpeg_fixture(description: &str, produced: Option<Vec<u8>>) -> Option<Vec<u8>> {
    assert!(
        produced.is_some() || !media_fixtures_are_required(),
        "{REQUIRE_MEDIA_FIXTURES_ENV} is set but the ffmpeg CLI could not produce {description}"
    );
    produced
}

pub fn ffmpeg_gen_media(file_name: &str, args: &[&str]) -> Option<Vec<u8>> {
    ffmpeg_fixture(file_name, run_ffmpeg_gen_media(file_name, args))
}

fn run_ffmpeg_gen_media(file_name: &str, args: &[&str]) -> Option<Vec<u8>> {
    let dir = tempfile::tempdir().ok()?;
    let out = dir.path().join(file_name);
    let status = std::process::Command::new("ffmpeg")
        .args(["-nostdin", "-loglevel", "error", "-y"])
        .args(args)
        .arg(out.to_str()?)
        .status()
        .ok()?;
    if !status.success() {
        return None;
    }
    std::fs::read(&out).ok()
}

pub fn ffmpeg_gen_mp4(args: &[&str]) -> Option<Vec<u8>> {
    ffmpeg_gen_media("fixture.mp4", args)
}

pub fn ffmpeg_gen_rotated_mp4(display_rotation: &str, source_args: &[&str]) -> Option<Vec<u8>> {
    ffmpeg_fixture(
        "rotated.mp4",
        run_ffmpeg_gen_rotated_mp4(display_rotation, source_args),
    )
}

fn run_ffmpeg_gen_rotated_mp4(display_rotation: &str, source_args: &[&str]) -> Option<Vec<u8>> {
    let dir = tempfile::tempdir().ok()?;
    let source = dir.path().join("source.mp4");
    let out = dir.path().join("rotated.mp4");
    let source_status = std::process::Command::new("ffmpeg")
        .args(["-nostdin", "-loglevel", "error", "-y"])
        .args(source_args)
        .arg(source.to_str()?)
        .status()
        .ok()?;
    if !source_status.success() {
        return None;
    }
    let rotate_status = std::process::Command::new("ffmpeg")
        .args(["-nostdin", "-loglevel", "error", "-y", "-noautorotate"])
        .args(["-display_rotation", display_rotation])
        .args(["-i", source.to_str()?])
        .args(["-c", "copy", "-f", "mp4"])
        .arg(out.to_str()?)
        .status()
        .ok()?;
    if !rotate_status.success() {
        return None;
    }
    std::fs::read(&out).ok()
}

pub fn ffmpeg_mirror_mp4(source_mp4: &[u8]) -> Option<Vec<u8>> {
    ffmpeg_fixture("mirrored.mp4", run_ffmpeg_mirror_mp4(source_mp4))
}

fn run_ffmpeg_mirror_mp4(source_mp4: &[u8]) -> Option<Vec<u8>> {
    let dir = tempfile::tempdir().ok()?;
    let source = dir.path().join("source.mp4");
    let out = dir.path().join("mirrored.mp4");
    std::fs::write(&source, source_mp4).ok()?;
    let mirror_status = std::process::Command::new("ffmpeg")
        .args(["-nostdin", "-loglevel", "error", "-y", "-noautorotate"])
        .arg("-display_hflip")
        .args(["-i", source.to_str()?])
        .args(["-c", "copy", "-f", "mp4"])
        .arg(out.to_str()?)
        .status()
        .ok()?;
    if !mirror_status.success() {
        return None;
    }
    std::fs::read(&out).ok()
}
