// SPDX-License-Identifier: AGPL-3.0-or-later

use std::future::Future;

tokio::task_local! {
    static UNIQUE_USERNAMES: bool;
}

pub async fn with_unique_usernames<F: Future>(unique_usernames: bool, future: F) -> F::Output {
    UNIQUE_USERNAMES.scope(unique_usernames, future).await
}

pub fn sync_with_unique_usernames<R>(unique_usernames: bool, f: impl FnOnce() -> R) -> R {
    UNIQUE_USERNAMES.sync_scope(unique_usernames, f)
}

pub fn unique_usernames() -> bool {
    UNIQUE_USERNAMES.try_with(|value| *value).unwrap_or(false)
}

pub fn shows_discriminator(discriminator: &str, is_bot: bool) -> bool {
    is_bot || !unique_usernames() || discriminator.trim().parse::<u16>() != Ok(0)
}

pub fn user_tag(username: &str, discriminator: &str, is_bot: bool) -> String {
    if shows_discriminator(discriminator, is_bot) {
        format!("{username}#{discriminator}")
    } else {
        username.to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn email_mode_keeps_every_tag() {
        assert_eq!(user_tag("alice", "0000", false), "alice#0000");
        assert_eq!(user_tag("alice", "0042", false), "alice#0042");
        sync_with_unique_usernames(false, || {
            assert_eq!(user_tag("alice", "0000", false), "alice#0000");
            assert_eq!(user_tag("bot", "0000", true), "bot#0000");
        });
    }

    #[test]
    fn username_mode_hides_zero_tag_for_humans() {
        sync_with_unique_usernames(true, || {
            assert_eq!(user_tag("alice", "0000", false), "alice");
            assert_eq!(user_tag("alice", "0", false), "alice");
            assert!(!shows_discriminator("0000", false));
        });
    }

    #[test]
    fn username_mode_keeps_bot_and_non_zero_tags() {
        sync_with_unique_usernames(true, || {
            assert_eq!(user_tag("helper", "4363", true), "helper#4363");
            assert_eq!(user_tag("helper", "0000", true), "helper#0000");
            assert_eq!(user_tag("legacy", "0042", false), "legacy#0042");
        });
    }

    #[test]
    fn mode_defaults_to_email_outside_a_request() {
        assert!(!unique_usernames());
    }
}
