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
use clickhouse::Client;
use tracing::info;

const MIGRATIONS: &[(&str, &str)] = &[
    (
        "001_initial_schema",
        include_str!("migrations/001_initial_schema.sql"),
    ),
    (
        "002_materialized_views",
        include_str!("migrations/002_materialized_views.sql"),
    ),
];

pub async fn run_migrations(client: &Client, database: &str) -> Result<()> {
    info!("Running database migrations...");

    client
        .query(&format!("CREATE DATABASE IF NOT EXISTS `{database}`"))
        .execute()
        .await?;

    let client = client.clone().with_database(database);

    client
        .query(
            r#"
            CREATE TABLE IF NOT EXISTS _migrations (
                name String,
                applied_at DateTime64(3, 'UTC') DEFAULT now64(3)
            )
            ENGINE = MergeTree()
            ORDER BY name
            "#,
        )
        .execute()
        .await?;

    for (name, sql) in MIGRATIONS {
        if migration_applied(&client, name).await? {
            info!("Migration {} already applied, skipping", name);
            continue;
        }

        info!("Applying migration: {}", name);

        for statement in sql.split(';').filter(|s| !s.trim().is_empty()) {
            let statement: String = statement
                .lines()
                .filter(|line| !line.trim_start().starts_with("--"))
                .collect::<Vec<_>>()
                .join("\n");
            let statement = statement.trim();
            if statement.is_empty() {
                continue;
            }
            client.query(statement).execute().await?;
        }

        mark_applied(&client, name).await?;
        info!("Migration {} applied successfully", name);
    }

    info!("All migrations completed");
    Ok(())
}

async fn migration_applied(client: &Client, name: &str) -> Result<bool> {
    let count: u64 = client
        .query("SELECT count() FROM _migrations WHERE name = ?")
        .bind(name)
        .fetch_one()
        .await?;
    Ok(count > 0)
}

async fn mark_applied(client: &Client, name: &str) -> Result<()> {
    client
        .query("INSERT INTO _migrations (name) VALUES (?)")
        .bind(name)
        .execute()
        .await?;
    Ok(())
}
