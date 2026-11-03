// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::desktop::build_desktop_shell;
use crate::proc::{RunOptions, run_command};
use anyhow::Result;

fn task_run(args: &[&str]) -> Result<()> {
    run_command(
        args,
        RunOptions {
            load_default_env: false,
            ..RunOptions::default()
        },
    )
    .map(drop)
}

pub fn run_build() -> Result<i32> {
    task_run(&["pnpm", "--filter", "@fluxer/schema", "generate"])?;
    task_run(&["pnpm", "--filter", "fluxer_app", "i18n:compile"])?;
    build_desktop_shell()?;
    task_run(&["pnpm", "--filter", "fluxer_app", "build"])?;
    Ok(0)
}
