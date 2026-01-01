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

use anyhow::Result;
use clap::{Parser, Subcommand};
use std::env;

mod migrate;

fn get_env_or_default(key: &str, default: &str) -> String {
    env::var(key).unwrap_or_else(|_| default.to_string())
}

#[derive(Parser)]
#[command(name = "cassandra-migrate")]
#[command(about = "Forward-only Cassandra migration tool for Fluxer", long_about = Some("A simple, forward-only migration tool for Cassandra.\nMigrations are stored in fluxer_devops/cassandra/migrations.\nMigration metadata is stored in the 'fluxer' keyspace."))]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    #[arg(long, default_value_t = get_env_or_default("CASSANDRA_HOST", "localhost"))]
    host: String,

    #[arg(long, default_value = "9042")]
    port: u16,

    #[arg(long, default_value_t = get_env_or_default("CASSANDRA_USERNAME", "cassandra"))]
    username: String,

    #[arg(long, default_value_t = get_env_or_default("CASSANDRA_PASSWORD", "cassandra"))]
    password: String,
}

#[derive(Subcommand)]
enum Commands {
    /// Create a new migration file
    Create {
        /// Name of the migration
        name: String,
    },
    /// Validate all migration files
    Check,
    /// Run pending migrations
    Up,
    /// Acknowledge a failed migration to skip it
    Ack {
        /// Filename of the migration to acknowledge
        filename: String,
    },
    /// Show migration status
    Status,
    /// Test Cassandra connection
    Test,
    /// Debug Cassandra connection
    Debug,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Create { name } => {
            migrate::create_migration(&name)?;
        }
        Commands::Check => {
            migrate::check_migrations()?;
        }
        Commands::Up => {
            migrate::run_migrations(&cli.host, cli.port, &cli.username, &cli.password).await?;
        }
        Commands::Ack { filename } => {
            migrate::acknowledge_migration(
                &cli.host,
                cli.port,
                &cli.username,
                &cli.password,
                &filename,
            )
            .await?;
        }
        Commands::Status => {
            migrate::show_status(&cli.host, cli.port, &cli.username, &cli.password).await?;
        }
        Commands::Test => {
            migrate::test_connection(&cli.host, cli.port, &cli.username, &cli.password).await?;
        }
        Commands::Debug => {
            migrate::debug_connection(&cli.host, cli.port, &cli.username, &cli.password).await?;
        }
    }

    Ok(())
}
