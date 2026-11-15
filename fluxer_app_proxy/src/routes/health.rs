// SPDX-License-Identifier: AGPL-3.0-or-later

pub async fn health() -> &'static str {
    "OK"
}

pub async fn ready() -> &'static str {
    "OK"
}
