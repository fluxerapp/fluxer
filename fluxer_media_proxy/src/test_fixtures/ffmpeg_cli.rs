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

#[cfg(test)]
mod tests {
    use super::*;

    const FIXTURE_PROBE_TEST: &str =
        "test_fixtures::ffmpeg_cli::tests::ffmpeg_fixture_probe_reports_its_outcome";

    fn run_fixture_probe(require: bool, empty_path: bool) -> (bool, String) {
        let exe = std::env::current_exe().expect("test binary path");
        let empty = tempfile::tempdir().expect("empty path directory");
        let mut command = std::process::Command::new(exe);
        command.args(["--exact", "--ignored", "--nocapture", FIXTURE_PROBE_TEST]);
        if require {
            command.env(REQUIRE_MEDIA_FIXTURES_ENV, "1");
        } else {
            command.env_remove(REQUIRE_MEDIA_FIXTURES_ENV);
        }
        if empty_path {
            command.env("PATH", empty.path());
        }
        let output = command.output().expect("probe run");
        let mut text = String::from_utf8_lossy(&output.stdout).into_owned();
        text.push_str(&String::from_utf8_lossy(&output.stderr));
        (output.status.success(), text)
    }

    fn ffmpeg_cli_is_on_path() -> bool {
        std::process::Command::new("ffmpeg")
            .arg("-version")
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()
            .is_ok_and(|status| status.success())
    }

    #[test]
    #[ignore]
    fn ffmpeg_fixture_probe_reports_its_outcome() {
        let fixture = ffmpeg_gen_mp4(&[
            "-f",
            "lavfi",
            "-i",
            "testsrc=size=16x16:rate=1:duration=1",
            "-pix_fmt",
            "yuv420p",
            "-f",
            "mp4",
        ]);
        println!(
            "FIXTURE_OUTCOME={}",
            if fixture.is_some() {
                "produced"
            } else {
                "absent"
            }
        );
    }

    #[test]
    fn missing_ffmpeg_fixtures_are_fatal_when_the_requirement_is_set() {
        let (succeeded, output) = run_fixture_probe(true, true);
        assert!(!succeeded, "the probe must fail loudly, got: {output}");
        assert!(
            output.contains(
                "FLUXER_REQUIRE_MEDIA_FIXTURES is set but the ffmpeg CLI could not produce"
            ),
            "the failure must name the requirement, got: {output}"
        );
    }

    #[test]
    fn missing_ffmpeg_fixtures_still_skip_when_the_requirement_is_unset() {
        let (succeeded, output) = run_fixture_probe(false, true);
        assert!(succeeded, "the probe must skip, got: {output}");
        assert!(output.contains("FIXTURE_OUTCOME=absent"), "got: {output}");
    }

    #[test]
    fn the_requirement_lets_fixtures_run_when_the_ffmpeg_cli_is_on_path() {
        let (succeeded, output) = run_fixture_probe(true, false);
        if ffmpeg_cli_is_on_path() {
            assert!(succeeded, "got: {output}");
            assert!(output.contains("FIXTURE_OUTCOME=produced"), "got: {output}");
        } else {
            assert!(!succeeded, "got: {output}");
            assert!(output.contains(REQUIRE_MEDIA_FIXTURES_ENV), "got: {output}");
        }
    }
}
