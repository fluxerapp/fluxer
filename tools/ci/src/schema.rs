// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::common::collect_files;
use anyhow::{Context, Result};
use clap::Args;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Args, Clone)]
pub struct CleanSchemaGeneratedFilesArgs {
    #[arg(long, default_value = "packages/schema/src/gen")]
    root: PathBuf,
}

pub fn run_clean_generated_files(args: CleanSchemaGeneratedFilesArgs) -> Result<()> {
    clean_generated_files(&args.root)
}

fn clean_generated_files(root: &Path) -> Result<()> {
    for file in collect_files(root)?
        .into_iter()
        .filter(|path| path.extension().and_then(|value| value.to_str()) == Some("ts"))
    {
        let source = fs::read_to_string(&file)
            .with_context(|| format!("Failed to read {}", file.display()))?;
        let Some(import_index) = find_import_start(&source) else {
            continue;
        };
        let content = format!(
            "{}\n",
            collapse_extra_blank_lines(source[import_index..].trim_end())
        );
        if content != source {
            fs::write(&file, content)
                .with_context(|| format!("Failed to write {}", file.display()))?;
        }
    }
    Ok(())
}

fn find_import_start(source: &str) -> Option<usize> {
    let mut offset = 0usize;
    for line in source.split_inclusive('\n') {
        if line.starts_with("import ") {
            return Some(offset);
        }
        offset += line.len();
    }
    None
}

fn collapse_extra_blank_lines(source: &str) -> String {
    let mut output = String::with_capacity(source.len());
    let mut consecutive_newlines = 0usize;
    for ch in source.chars() {
        if ch == '\n' {
            consecutive_newlines += 1;
            if consecutive_newlines <= 2 {
                output.push(ch);
            }
        } else {
            consecutive_newlines = 0;
            output.push(ch);
        }
    }
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_file(path: &Path, contents: &str) {
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(path, contents).unwrap();
    }

    #[test]
    fn find_import_start_only_matches_line_starts() {
        assert_eq!(
            find_import_start("// import nope\nimport {x} from 'x';\n"),
            Some(15)
        );
        assert_eq!(find_import_start("const value = 'import nope';\n"), None);
    }

    #[test]
    fn collapse_extra_blank_lines_keeps_at_most_one_blank_line() {
        assert_eq!(collapse_extra_blank_lines("a\n\n\n\nb\n\nc"), "a\n\nb\n\nc");
    }

    #[test]
    fn clean_generated_files_removes_prelude_and_compacts_blank_lines() {
        let temp = tempfile::tempdir().unwrap();
        let root = temp.path();
        let file = root.join("generated.ts");
        write_file(
            &file,
            "// header\n\n\nimport {x} from 'x';\n\n\n\nexport const y = x;\n\n",
        );
        write_file(&root.join("keep.txt"), "// header\n");

        clean_generated_files(root).unwrap();

        assert_eq!(
            fs::read_to_string(file).unwrap(),
            "import {x} from 'x';\n\nexport const y = x;\n"
        );
    }
}
