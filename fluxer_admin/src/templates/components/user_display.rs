// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::utils::user_tag::user_tag;

pub fn format_user_display(
    global_name: Option<&str>,
    username: Option<&str>,
    discriminator: Option<&str>,
    is_bot: bool,
) -> String {
    match (global_name, username, discriminator) {
        (Some(gn), Some(un), Some("0")) => format!("{gn} (@{un})"),
        (Some(gn), _, _) => gn.to_owned(),
        (None, Some(un), Some(d)) if d != "0" => user_tag(un, d, is_bot),
        (None, Some(un), _) => format!("@{un}"),
        _ => "Unknown".to_owned(),
    }
}
