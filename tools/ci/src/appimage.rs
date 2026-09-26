// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::{CommandSpec, require_any_env, require_env, run_command};
use crate::desktop::file_name_string;
use anyhow::{Context, Result, bail, ensure};
use std::fs::{self, File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};

const PKGS_BASE_URL: &str = "https://pkgs.fluxer.com";
const PKGS_DESKTOP_PREFIX: &str = "desktop";
const APPIMAGE_PLATFORM: &str = "linux";
const APPIMAGE_FORMAT: &str = "appimage";
const APPIMAGE_SUFFIX: &str = ".AppImage";
const ZSYNC_SUFFIX: &str = ".zsync";
const LATEST_ALIAS: &str = "latest";
const UPDATE_INFO_SECTION: &str = ".upd_info";
const PUBLISHED_CHANNELS: &[&str] = &["stable", "canary"];
const PUBLISHED_ARCHES: &[&str] = &["x64", "arm64"];
const ELF64_HEADER_LEN: usize = 64;
const ELF64_SECTION_HEADER_LEN: u64 = 64;
const SHT_NOBITS: u32 = 8;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct UpdateInfoSpan {
    offset: u64,
    size: u64,
}

pub(crate) fn build_update_feed_step() -> Result<()> {
    let channel = published_channel(&require_env("BUILD_CHANNEL")?)?;
    let arch = published_arch(&require_any_env(&["DESKTOP_ARCH", "ARCH"])?)?;
    let staging = Path::new("upload_staging");
    let artifacts = appimage_artifacts(staging)?;
    ensure!(
        !artifacts.is_empty(),
        "No {APPIMAGE_SUFFIX} artifact was staged in {}, so there is nothing to advertise.",
        staging.display()
    );
    for artifact in artifacts {
        let name = file_name_string(&artifact)?;
        let information = update_information(&channel, &arch);
        write_update_information(&artifact, &information)?;
        println!("Stamped {name} with {information}");

        let control = artifact.with_file_name(control_name(&name));
        let url = artifact_url(&channel, &arch, &name);
        run_command(zsyncmake_command(&artifact, &name, &url, &control))?;
        ensure!(
            control.is_file(),
            "zsyncmake did not write {}",
            control.display()
        );
        println!("Built {} against {url}", control_name(&name));
    }
    Ok(())
}

fn published_channel(channel: &str) -> Result<String> {
    ensure!(
        PUBLISHED_CHANNELS.contains(&channel),
        "{channel:?} is not a published desktop channel, so it has no {PKGS_BASE_URL} update feed."
    );
    Ok(channel.to_owned())
}

fn published_arch(arch: &str) -> Result<String> {
    ensure!(
        PUBLISHED_ARCHES.contains(&arch),
        "{arch:?} is not a published Linux architecture, so it has no {PKGS_BASE_URL} update feed."
    );
    Ok(arch.to_owned())
}

fn appimage_artifacts(dir: &Path) -> Result<Vec<PathBuf>> {
    collect_matching(dir, |name| name.ends_with(APPIMAGE_SUFFIX))
}

fn collect_matching(dir: &Path, matches: impl Fn(&str) -> bool) -> Result<Vec<PathBuf>> {
    let mut paths = Vec::new();
    for entry in fs::read_dir(dir).with_context(|| format!("Failed to read {}", dir.display()))? {
        let path = entry?.path();
        if path.is_file() && matches(&file_name_string(&path)?) {
            paths.push(path);
        }
    }
    paths.sort();
    Ok(paths)
}

fn control_name(artifact_name: &str) -> String {
    format!("{artifact_name}{ZSYNC_SUFFIX}")
}

fn published_arch_url(channel: &str, arch: &str) -> String {
    format!("{PKGS_BASE_URL}/{PKGS_DESKTOP_PREFIX}/{channel}/{APPIMAGE_PLATFORM}/{arch}")
}

fn update_information(channel: &str, arch: &str) -> String {
    format!(
        "zsync|{}/{LATEST_ALIAS}/{APPIMAGE_FORMAT}{ZSYNC_SUFFIX}",
        published_arch_url(channel, arch)
    )
}

fn artifact_url(channel: &str, arch: &str, name: &str) -> String {
    format!("{}/{name}", published_arch_url(channel, arch))
}

fn zsyncmake_command(artifact: &Path, name: &str, url: &str, control: &Path) -> CommandSpec {
    CommandSpec::new("zsyncmake")
        .arg("-u")
        .arg(url)
        .arg("-f")
        .arg(name)
        .arg("-o")
        .arg(control)
        .arg(artifact)
}

fn write_update_information(path: &Path, information: &str) -> Result<()> {
    let span = update_info_span(path)?;
    let bytes = information.as_bytes();
    ensure!(
        (bytes.len() as u64) < span.size,
        "Update information is {} bytes but {UPDATE_INFO_SECTION} only holds {} including its terminator.",
        bytes.len(),
        span.size
    );
    let before = fs::metadata(path)
        .with_context(|| format!("Failed to stat {}", path.display()))?
        .len();
    let mut section = vec![0u8; span.size as usize];
    section[..bytes.len()].copy_from_slice(bytes);
    let mut file = OpenOptions::new()
        .write(true)
        .open(path)
        .with_context(|| format!("Failed to open {} for writing", path.display()))?;
    file.seek(SeekFrom::Start(span.offset)).with_context(|| {
        format!(
            "Failed to seek to {UPDATE_INFO_SECTION} in {}",
            path.display()
        )
    })?;
    file.write_all(&section).with_context(|| {
        format!(
            "Failed to write {UPDATE_INFO_SECTION} in {}",
            path.display()
        )
    })?;
    file.flush()?;
    drop(file);
    let after = fs::metadata(path)
        .with_context(|| format!("Failed to stat {}", path.display()))?
        .len();
    ensure!(
        before == after,
        "{} changed size from {before} to {after} while stamping {UPDATE_INFO_SECTION}. The squashfs payload follows the ELF, so the update information must be written in place.",
        path.display()
    );
    let written = read_update_information(path)?;
    ensure!(
        written == information,
        "{} reports {written:?} after being stamped with {information:?}",
        path.display()
    );
    Ok(())
}

fn read_update_information(path: &Path) -> Result<String> {
    let span = update_info_span(path)?;
    let mut file =
        File::open(path).with_context(|| format!("Failed to open {}", path.display()))?;
    file.seek(SeekFrom::Start(span.offset))?;
    let mut section = vec![0u8; span.size as usize];
    file.read_exact(&mut section).with_context(|| {
        format!(
            "Failed to read {UPDATE_INFO_SECTION} from {}",
            path.display()
        )
    })?;
    let end = section
        .iter()
        .position(|byte| *byte == 0)
        .unwrap_or(section.len());
    String::from_utf8(section[..end].to_vec())
        .with_context(|| format!("{UPDATE_INFO_SECTION} in {} is not UTF-8", path.display()))
}

fn update_info_span(path: &Path) -> Result<UpdateInfoSpan> {
    let mut file =
        File::open(path).with_context(|| format!("Failed to open {}", path.display()))?;
    let mut header = [0u8; ELF64_HEADER_LEN];
    file.read_exact(&mut header)
        .with_context(|| format!("Failed to read the ELF header of {}", path.display()))?;
    ensure!(
        header[..4] == *b"\x7fELF",
        "{} is not an ELF file",
        path.display()
    );
    ensure!(
        header[4] == 2 && header[5] == 1,
        "{} is not a little-endian 64-bit ELF file",
        path.display()
    );
    let section_offset = read_u64(&header, 0x28);
    let entry_size = u64::from(read_u16(&header, 0x3a));
    let count = u64::from(read_u16(&header, 0x3c));
    let names_index = u64::from(read_u16(&header, 0x3e));
    ensure!(
        section_offset != 0 && count != 0 && entry_size >= ELF64_SECTION_HEADER_LEN,
        "{} has no usable section header table",
        path.display()
    );
    ensure!(
        names_index < count,
        "{} has an out of range section name table index",
        path.display()
    );

    let names = read_section_header(&mut file, section_offset, entry_size, names_index)?;
    let mut names_table = vec![0u8; names.size as usize];
    file.seek(SeekFrom::Start(names.offset))?;
    file.read_exact(&mut names_table).with_context(|| {
        format!(
            "Failed to read the section name table of {}",
            path.display()
        )
    })?;

    for index in 0..count {
        let section = read_section_header(&mut file, section_offset, entry_size, index)?;
        if section_name(&names_table, section.name_offset) != UPDATE_INFO_SECTION {
            continue;
        }
        ensure!(
            section.kind != SHT_NOBITS,
            "{UPDATE_INFO_SECTION} in {} occupies no file space",
            path.display()
        );
        ensure!(
            section.size != 0,
            "{UPDATE_INFO_SECTION} in {} is empty",
            path.display()
        );
        return Ok(UpdateInfoSpan {
            offset: section.offset,
            size: section.size,
        });
    }
    bail!(
        "{} has no {UPDATE_INFO_SECTION} section, so it cannot advertise update information",
        path.display()
    )
}

#[derive(Debug, Clone, Copy)]
struct SectionHeader {
    name_offset: usize,
    kind: u32,
    offset: u64,
    size: u64,
}

fn read_section_header(
    file: &mut File,
    table_offset: u64,
    entry_size: u64,
    index: u64,
) -> Result<SectionHeader> {
    let mut entry = vec![0u8; entry_size as usize];
    file.seek(SeekFrom::Start(table_offset + index * entry_size))?;
    file.read_exact(&mut entry)
        .with_context(|| format!("Failed to read ELF section header {index}"))?;
    Ok(SectionHeader {
        name_offset: read_u32(&entry, 0) as usize,
        kind: read_u32(&entry, 4),
        offset: read_u64(&entry, 0x18),
        size: read_u64(&entry, 0x20),
    })
}

fn section_name(names_table: &[u8], offset: usize) -> &str {
    let Some(tail) = names_table.get(offset..) else {
        return "";
    };
    let end = tail
        .iter()
        .position(|byte| *byte == 0)
        .unwrap_or(tail.len());
    std::str::from_utf8(&tail[..end]).unwrap_or_default()
}

fn read_u16(bytes: &[u8], offset: usize) -> u16 {
    u16::from_le_bytes([bytes[offset], bytes[offset + 1]])
}

fn read_u32(bytes: &[u8], offset: usize) -> u32 {
    u32::from_le_bytes([
        bytes[offset],
        bytes[offset + 1],
        bytes[offset + 2],
        bytes[offset + 3],
    ])
}

fn read_u64(bytes: &[u8], offset: usize) -> u64 {
    let mut value = [0u8; 8];
    value.copy_from_slice(&bytes[offset..offset + 8]);
    u64::from_le_bytes(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    const SECTION_SIZE: u64 = 1024;

    fn write_elf(path: &Path, section_kind: u32, section_size: u64, payload: &[u8]) {
        let header_len = ELF64_HEADER_LEN as u64;
        let section_offset = header_len;
        let names_table = b"\0.shstrtab\0.upd_info\0";
        let names_offset = section_offset + section_size;
        let table_offset = names_offset + names_table.len() as u64;
        let total = table_offset + 3 * ELF64_SECTION_HEADER_LEN;

        let mut bytes = vec![0u8; total as usize];
        bytes[..4].copy_from_slice(b"\x7fELF");
        bytes[4] = 2;
        bytes[5] = 1;
        bytes[0x28..0x30].copy_from_slice(&table_offset.to_le_bytes());
        bytes[0x3a..0x3c].copy_from_slice(&(ELF64_SECTION_HEADER_LEN as u16).to_le_bytes());
        bytes[0x3c..0x3e].copy_from_slice(&3u16.to_le_bytes());
        bytes[0x3e..0x40].copy_from_slice(&1u16.to_le_bytes());

        let names_start = names_offset as usize;
        bytes[names_start..names_start + names_table.len()].copy_from_slice(names_table);

        let mut put = |index: u64, name_offset: u32, kind: u32, offset: u64, size: u64| {
            let start = (table_offset + index * ELF64_SECTION_HEADER_LEN) as usize;
            bytes[start..start + 4].copy_from_slice(&name_offset.to_le_bytes());
            bytes[start + 4..start + 8].copy_from_slice(&kind.to_le_bytes());
            bytes[start + 0x18..start + 0x20].copy_from_slice(&offset.to_le_bytes());
            bytes[start + 0x20..start + 0x28].copy_from_slice(&size.to_le_bytes());
        };
        put(0, 0, 0, 0, 0);
        put(1, 1, 3, names_offset, names_table.len() as u64);
        put(2, 11, section_kind, section_offset, section_size);

        let mut file = bytes;
        file.extend_from_slice(payload);
        fs::write(path, file).unwrap();
    }

    #[test]
    fn stamping_rewrites_only_the_update_information_section() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("Fluxer.AppImage");
        let payload = b"squashfs-payload-that-must-survive".to_vec();
        write_elf(&path, 1, SECTION_SIZE, &payload);
        let before = fs::read(&path).unwrap();

        let information = update_information("canary", "x64");
        write_update_information(&path, &information).unwrap();

        let after = fs::read(&path).unwrap();
        assert_eq!(before.len(), after.len());
        assert_eq!(read_update_information(&path).unwrap(), information);
        assert_eq!(
            &after[after.len() - payload.len()..],
            payload.as_slice(),
            "the appended squashfs payload must be untouched"
        );
        let span = update_info_span(&path).unwrap();
        let mut untouched_before = before.clone();
        let mut untouched_after = after.clone();
        let range = span.offset as usize..(span.offset + span.size) as usize;
        untouched_before.splice(range.clone(), std::iter::empty());
        untouched_after.splice(range, std::iter::empty());
        assert_eq!(untouched_before, untouched_after);
    }

    #[test]
    fn stamping_refuses_update_information_that_does_not_fit() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("Fluxer.AppImage");
        write_elf(&path, 1, 32, b"payload");

        let error = write_update_information(&path, &"z".repeat(64))
            .unwrap_err()
            .to_string();
        assert!(error.contains("only holds"), "{error}");
        assert_eq!(read_update_information(&path).unwrap(), "");
    }

    #[test]
    fn stamping_refuses_an_appimage_without_an_update_information_section() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("Fluxer.AppImage");
        write_elf(&path, 1, SECTION_SIZE, b"payload");
        let mut bytes = fs::read(&path).unwrap();
        let names_start = (ELF64_HEADER_LEN as u64 + SECTION_SIZE) as usize;
        bytes[names_start + 11] = b'x';
        fs::write(&path, bytes).unwrap();

        let error = update_info_span(&path).unwrap_err().to_string();
        assert!(error.contains("no .upd_info section"), "{error}");
    }

    #[test]
    fn stamping_refuses_a_section_that_occupies_no_file_space() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("Fluxer.AppImage");
        write_elf(&path, SHT_NOBITS, SECTION_SIZE, b"payload");

        let error = update_info_span(&path).unwrap_err().to_string();
        assert!(error.contains("occupies no file space"), "{error}");
    }

    #[test]
    fn update_information_points_at_the_mutable_latest_alias_on_the_packages_box() {
        assert_eq!(
            update_information("canary", "x64"),
            "zsync|https://pkgs.fluxer.com/desktop/canary/linux/x64/latest/appimage.zsync"
        );
        assert_eq!(
            update_information("stable", "arm64"),
            "zsync|https://pkgs.fluxer.com/desktop/stable/linux/arm64/latest/appimage.zsync"
        );
    }

    #[test]
    fn the_control_file_downloads_the_immutable_versioned_artifact() {
        assert_eq!(
            artifact_url(
                "stable",
                "arm64",
                "Fluxer-2026.904.135113-linux-arm64.AppImage"
            ),
            "https://pkgs.fluxer.com/desktop/stable/linux/arm64/Fluxer-2026.904.135113-linux-arm64.AppImage"
        );
    }

    #[test]
    fn only_published_channels_and_arches_are_stamped() {
        assert_eq!(published_channel("canary").unwrap(), "canary");
        assert_eq!(published_arch("arm64").unwrap(), "arm64");
        let channel = published_channel("nightly").unwrap_err().to_string();
        assert!(
            channel.contains("not a published desktop channel"),
            "{channel}"
        );
        let arch = published_arch("x86_64").unwrap_err().to_string();
        assert!(
            arch.contains("not a published Linux architecture"),
            "{arch}"
        );
    }

    #[test]
    fn zsyncmake_writes_the_control_file_beside_the_stamped_artifact() {
        let command = zsyncmake_command(
            Path::new("upload_staging/Fluxer-2026.904.135113-linux-x86_64.AppImage"),
            "Fluxer-2026.904.135113-linux-x86_64.AppImage",
            &artifact_url(
                "stable",
                "x64",
                "Fluxer-2026.904.135113-linux-x86_64.AppImage",
            ),
            Path::new("upload_staging/Fluxer-2026.904.135113-linux-x86_64.AppImage.zsync"),
        );

        assert_eq!(command.program, std::ffi::OsString::from("zsyncmake"));
        assert_eq!(
            command.args,
            vec![
                std::ffi::OsString::from("-u"),
                std::ffi::OsString::from(
                    "https://pkgs.fluxer.com/desktop/stable/linux/x64/Fluxer-2026.904.135113-linux-x86_64.AppImage"
                ),
                std::ffi::OsString::from("-f"),
                std::ffi::OsString::from("Fluxer-2026.904.135113-linux-x86_64.AppImage"),
                std::ffi::OsString::from("-o"),
                std::ffi::OsString::from(
                    "upload_staging/Fluxer-2026.904.135113-linux-x86_64.AppImage.zsync"
                ),
                std::ffi::OsString::from(
                    "upload_staging/Fluxer-2026.904.135113-linux-x86_64.AppImage"
                ),
            ]
        );
    }
}
