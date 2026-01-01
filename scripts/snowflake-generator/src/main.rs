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

use clap::Parser;
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

const FLUXER_EPOCH: i64 = 1420070400000;

#[derive(Parser)]
#[command(name = "snowflake-generator")]
#[command(about = "Generates unique Fluxer snowflake IDs", long_about = None)]
struct Cli {
    /// Number of snowflakes to generate
    #[arg(long, short, default_value = "1")]
    count: usize,

    /// Optional Unix timestamp in milliseconds (defaults to current time)
    #[arg(long, short)]
    timestamp: Option<i64>,
}

fn generate_snowflake(timestamp: Option<i64>) -> i64 {
    let ts = match timestamp {
        Some(t) => t,
        None => SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as i64,
    };

    (ts - FLUXER_EPOCH) << 22
}

fn main() {
    let cli = Cli::parse();

    if cli.count < 1 {
        eprintln!("Error: count must be at least 1");
        std::process::exit(1);
    }

    if cli.count == 1 {
        let snowflake = generate_snowflake(cli.timestamp);
        println!("{snowflake}");
    } else {
        for _ in 0..cli.count {
            let snowflake = generate_snowflake(cli.timestamp);
            println!("{snowflake}");

            if cli.timestamp.is_none() {
                thread::sleep(Duration::from_millis(1));
            }
        }
    }
}
