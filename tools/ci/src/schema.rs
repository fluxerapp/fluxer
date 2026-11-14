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
