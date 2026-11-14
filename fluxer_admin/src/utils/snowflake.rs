// SPDX-License-Identifier: AGPL-3.0-or-later

const MAX_SNOWFLAKE_DIGITS: usize = 20;

pub(crate) fn is_snowflake(value: &str) -> bool {
    (1..=MAX_SNOWFLAKE_DIGITS).contains(&value.len())
        && value.bytes().all(|byte| byte.is_ascii_digit())
}
