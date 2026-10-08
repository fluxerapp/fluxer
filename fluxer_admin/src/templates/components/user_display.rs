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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::user_tag::sync_with_unique_usernames;

    #[test]
    fn username_instances_show_bare_human_names_and_keep_bot_tags() {
        sync_with_unique_usernames(true, || {
            assert_eq!(
                format_user_display(None, Some("alice"), Some("0000"), false),
                "alice"
            );
            assert_eq!(
                format_user_display(None, Some("helper"), Some("4363"), true),
                "helper#4363"
            );
        });
    }

    #[test]
    fn email_instances_keep_the_zero_tag() {
        assert_eq!(
            format_user_display(None, Some("alice"), Some("0000"), false),
            "alice#0000"
        );
    }
}
