#![allow(clippy::four_forward_slashes)]

/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

use regex::Regex;
use std::fs;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

const TS_LICENSE_HEADER: &str = r"/*
 * Copyright (C) {year} Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */";

const ERLANG_LICENSE_HEADER: &str = r"%% Copyright (C) {year} Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.";

const GLEAM_LICENSE_HEADER: &str = r"//// Copyright (C) {year} Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.";

const SHELL_LICENSE_HEADER: &str = r"# Copyright (C) {year} Fluxer Contributors
#
# This file is part of Fluxer.
#
# Fluxer is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Fluxer is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with Fluxer. If not, see <https://www.gnu.org/licenses/>.";

const BLOCK_COMMENT_EXTS: &[&str] = &[
    "ts", "tsx", "js", "jsx", "mjs", "cjs", "css", "go", "rs", "c", "cc", "cpp", "cxx", "h", "hh",
    "hpp", "hxx", "mm", "m", "java", "kt", "kts", "swift", "scala", "dart", "cs", "fs",
];

const HASH_LINE_EXTS: &[&str] = &[
    "sh", "bash", "zsh", "py", "rb", "ps1", "psm1", "psd1", "ksh", "fish",
];

#[derive(Clone, Copy)]
enum HeaderStyle {
    Block,
    Line(&'static str),
}

#[derive(Clone, Copy)]
struct FileTemplate {
    header: &'static str,
    style: HeaderStyle,
}

impl FileTemplate {
    const fn new(header: &'static str, style: HeaderStyle) -> Self {
        Self { header, style }
    }
}

struct Processor {
    current_year: i32,
    updated: usize,
    ignore_patterns: Vec<String>,
}

impl Processor {
    fn new() -> Self {
        let current_year = chrono::Datelike::year(&chrono::Utc::now());
        let mut processor = Processor {
            current_year,
            updated: 0,
            ignore_patterns: Vec::new(),
        };
        processor.load_gitignore();
        processor
    }

    fn load_gitignore(&mut self) {
        if let Ok(file) = fs::File::open(".gitignore") {
            let reader = BufReader::new(file);
            for line in reader.lines().map_while(Result::ok) {
                let trimmed = line.trim();
                if !trimmed.is_empty() && !trimmed.starts_with('#') {
                    self.ignore_patterns.push(trimmed.to_string());
                }
            }
        }
    }

    fn should_ignore(&self, path: &str) -> bool {
        if path.contains("fluxer_static") {
            return true;
        }
        for pattern in &self.ignore_patterns {
            if self.match_pattern(pattern, path) {
                return true;
            }
        }
        false
    }

    fn match_pattern(&self, pattern: &str, path: &str) -> bool {
        if let Some(sub_pattern) = pattern.strip_prefix("**/") {
            if sub_pattern.ends_with('/') {
                let dir_name = sub_pattern.trim_end_matches('/');
                return path
                    .split(std::path::MAIN_SEPARATOR)
                    .any(|part| part == dir_name);
            }
            return path
                .split(std::path::MAIN_SEPARATOR)
                .any(|part| part == sub_pattern);
        }

        if pattern.ends_with('/') {
            let dir_pattern = pattern.trim_end_matches('/');
            return path
                .split(std::path::MAIN_SEPARATOR)
                .any(|part| part == dir_pattern)
                || path.starts_with(&format!("{dir_pattern}/"));
        }

        if let Some(p) = pattern.strip_prefix('/') {
            return path == p;
        }

        path.split(std::path::MAIN_SEPARATOR)
            .any(|part| part == pattern)
            || Path::new(path).file_name().and_then(|f| f.to_str()) == Some(pattern)
    }

    fn is_target_file(&self, path: &Path) -> bool {
        self.get_template(path).is_some()
    }

    fn get_template(&self, path: &Path) -> Option<FileTemplate> {
        path.extension()
            .and_then(|ext| ext.to_str())
            .and_then(Self::template_for_extension)
    }

    fn template_for_extension(ext: &str) -> Option<FileTemplate> {
        let normalized = ext.to_ascii_lowercase();
        if BLOCK_COMMENT_EXTS.contains(&normalized.as_str()) {
            Some(FileTemplate::new(TS_LICENSE_HEADER, HeaderStyle::Block))
        } else if HASH_LINE_EXTS.contains(&normalized.as_str()) {
            Some(FileTemplate::new(
                SHELL_LICENSE_HEADER,
                HeaderStyle::Line("#"),
            ))
        } else {
            match normalized.as_str() {
                "gleam" => Some(FileTemplate::new(
                    GLEAM_LICENSE_HEADER,
                    HeaderStyle::Line("////"),
                )),
                "erl" | "hrl" => Some(FileTemplate::new(
                    ERLANG_LICENSE_HEADER,
                    HeaderStyle::Line("%%"),
                )),
                _ => None,
            }
        }
    }

    fn detect_license(&self, content: &str) -> (bool, Option<i32>) {
        let lines: Vec<&str> = content.lines().take(25).collect();
        let mut has_agpl = false;
        let mut has_fluxer = false;
        let mut detected_year = None;

        let year_regex = Regex::new(r"\b(20\d{2})\b").unwrap();

        for line in lines {
            let lower = line.to_lowercase();
            if lower.contains("gnu affero general public license") || lower.contains("agpl") {
                has_agpl = true;
            }
            if lower.contains("fluxer") {
                has_fluxer = true;
            }
            if lower.contains("copyright")
                && lower.contains("fluxer")
                && detected_year.is_none()
                && let Some(cap) = year_regex.captures(line)
                && let Ok(year) = cap[1].parse::<i32>()
                && (1900..3000).contains(&year)
            {
                detected_year = Some(year);
            }
        }

        (has_agpl && has_fluxer, detected_year)
    }

    fn update_year(&self, content: &str, old_year: i32) -> String {
        content.replacen(&old_year.to_string(), &self.current_year.to_string(), 1)
    }

    fn strip_license_header(&self, content: &str, style: HeaderStyle) -> (String, bool) {
        let lines: Vec<&str> = content.split('\n').collect();
        if lines.is_empty() {
            return (content.to_string(), false);
        }

        let mut prefix_end = 0;
        if let Some(first) = lines.get(0) {
            if first.starts_with("#!") {
                prefix_end = 1;
            }
        }

        let mut header_start = prefix_end;
        while header_start < lines.len() && lines[header_start].trim().is_empty() {
            header_start += 1;
        }

        if header_start >= lines.len() {
            return (content.to_string(), false);
        }

        let original_ending = content.ends_with('\n');

        let after_idx = match style {
            HeaderStyle::Block => {
                let first = lines[header_start].trim_start();
                if !first.starts_with("/*") {
                    return (content.to_string(), false);
                }
                let mut header_end = header_start;
                let mut found_end = false;
                for i in header_start..lines.len() {
                    if lines[i].contains("*/") {
                        header_end = i;
                        found_end = true;
                        break;
                    }
                }
                if !found_end {
                    return (content.to_string(), false);
                }
                let mut after = header_end + 1;
                while after < lines.len() && lines[after].trim().is_empty() {
                    after += 1;
                }
                after
            }
            HeaderStyle::Line(prefix) => {
                let first = lines[header_start].trim_start();
                if !first.starts_with(prefix) {
                    return (content.to_string(), false);
                }
                let mut header_end = header_start;
                while header_end < lines.len() {
                    let trimmed = lines[header_end].trim_start();
                    if trimmed.is_empty() {
                        break;
                    }
                    if trimmed.starts_with(prefix) {
                        header_end += 1;
                        continue;
                    }
                    break;
                }
                let mut after = header_end;
                while after < lines.len() && lines[after].trim().is_empty() {
                    after += 1;
                }
                after
            }
        };

        let mut new_lines = Vec::new();
        new_lines.extend_from_slice(&lines[..prefix_end]);
        new_lines.extend_from_slice(&lines[after_idx..]);

        let mut result = new_lines.join("\n");
        if original_ending && !result.ends_with('\n') {
            result.push('\n');
        }

        (result, true)
    }

    fn add_header(&self, content: &str, template: FileTemplate) -> String {
        let header = template
            .header
            .replace("{year}", &self.current_year.to_string());

        if let Some(first_line) = content.lines().next()
            && first_line.starts_with("#!")
        {
            let rest = content.lines().skip(1).collect::<Vec<_>>().join("\n");
            return format!("{first_line}\n\n{header}\n\n{rest}");
        }

        format!("{header}\n\n{content}")
    }

    fn process_file(&mut self, path: &Path) -> Result<(), Box<dyn std::error::Error>> {
        let content = fs::read_to_string(path)?;
        let template = self.get_template(path).ok_or("Unknown file type")?;
        let (has_header, detected_year) = self.detect_license(&content);

        let (new_content, action) = if !has_header {
            (self.add_header(&content, template), "Added header")
        } else {
            let (stripped, stripped_ok) = self.strip_license_header(&content, template.style);
            if stripped_ok {
                (self.add_header(&stripped, template), "Normalized header")
            } else if let Some(old_year) = detected_year {
                if old_year == self.current_year {
                    return Ok(());
                }
                (
                    self.update_year(&content, old_year),
                    &format!("Updated year {} → {}", old_year, self.current_year) as &str,
                )
            } else {
                return Ok(());
            }
        };

        fs::write(path, new_content)?;
        self.updated += 1;
        println!("{}: {}", action, path.display());

        Ok(())
    }

    fn walk(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        let paths: Vec<PathBuf> = WalkDir::new(".")
            .into_iter()
            .filter_map(std::result::Result::ok)
            .filter(|e| {
                let path = e.path();
                let path_str = path.to_string_lossy();
                !self.should_ignore(&path_str)
                    && e.file_type().is_file()
                    && self.is_target_file(path)
            })
            .map(|e| e.path().to_path_buf())
            .collect();

        for path in paths {
            if let Err(e) = self.process_file(&path) {
                eprintln!("Error processing {path:?}: {e}");
            }
        }
        Ok(())
    }
}

fn main() {
    let mut processor = Processor::new();
    if let Err(e) = processor.walk() {
        eprintln!("Error: {e}");
        std::process::exit(1);
    }
    println!("Updated {} files", processor.updated);
}