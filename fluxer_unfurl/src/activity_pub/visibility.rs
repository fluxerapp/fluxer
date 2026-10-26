// SPDX-License-Identifier: AGPL-3.0-or-later

use super::types::{ActivityPubActor, ActivityPubPost};

const PUBLIC_URIS: [&str; 3] = [
    "https://www.w3.org/ns/activitystreams#Public",
    "as:Public",
    "Public",
];

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum WebVisibility {
    Public,
    Unlocked,
    Restricted,
}

pub fn is_hidden_from_unauthed_web(
    post: &ActivityPubPost,
    author: Option<&ActivityPubActor>,
) -> bool {
    let Some(author) = author else {
        return false;
    };
    if author.hides_to_public_from_unauthed_web.is_none()
        && author.hides_cc_public_from_unauthed_web.is_none()
    {
        return false;
    }
    let hides_to = author.hides_to_public_from_unauthed_web.unwrap_or(false);
    let hides_cc = author.hides_cc_public_from_unauthed_web.unwrap_or(true);
    match web_visibility(post) {
        WebVisibility::Public => hides_to,
        WebVisibility::Unlocked => hides_cc,
        WebVisibility::Restricted => true,
    }
}

fn web_visibility(post: &ActivityPubPost) -> WebVisibility {
    if addresses_public(post.to.as_ref()) {
        WebVisibility::Public
    } else if addresses_public(post.cc.as_ref()) {
        WebVisibility::Unlocked
    } else {
        WebVisibility::Restricted
    }
}

fn addresses_public(value: Option<&serde_json::Value>) -> bool {
    match value {
        Some(serde_json::Value::String(s)) => is_public_uri(s),
        Some(serde_json::Value::Array(items)) => items
            .iter()
            .filter_map(|item| item.as_str())
            .any(is_public_uri),
        _ => false,
    }
}

fn is_public_uri(value: &str) -> bool {
    PUBLIC_URIS.contains(&value)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn post(value: serde_json::Value) -> ActivityPubPost {
        serde_json::from_value(value).expect("valid post")
    }

    fn actor(value: serde_json::Value) -> ActivityPubActor {
        serde_json::from_value(value).expect("valid actor")
    }

    const PUBLIC: &str = "https://www.w3.org/ns/activitystreams#Public";

    #[test]
    fn public_post_from_an_unhidden_actor_is_shown() {
        let post = post(json!({
            "to": [PUBLIC],
            "cc": ["https://gts.test/users/alice/followers"],
        }));
        let actor = actor(json!({
            "hidesToPublicFromUnauthedWeb": false,
            "hidesCcPublicFromUnauthedWeb": false,
        }));
        assert!(!is_hidden_from_unauthed_web(&post, Some(&actor)));
    }

    #[test]
    fn public_post_is_hidden_when_actor_hides_public_from_web() {
        let post = post(json!({ "to": [PUBLIC] }));
        let actor = actor(json!({
            "hidesToPublicFromUnauthedWeb": true,
            "hidesCcPublicFromUnauthedWeb": false,
        }));
        assert!(is_hidden_from_unauthed_web(&post, Some(&actor)));
    }

    #[test]
    fn unlisted_post_is_hidden_by_gotosocial_default() {
        let post = post(json!({
            "to": ["https://gts.test/users/alice/followers"],
            "cc": [PUBLIC],
        }));
        let actor = actor(json!({ "hidesToPublicFromUnauthedWeb": false }));
        assert!(is_hidden_from_unauthed_web(&post, Some(&actor)));
    }

    #[test]
    fn unlisted_post_is_shown_when_actor_allows_cc_on_web() {
        let post = post(json!({
            "to": ["https://gts.test/users/alice/followers"],
            "cc": [PUBLIC],
        }));
        let actor = actor(json!({
            "hidesToPublicFromUnauthedWeb": false,
            "hidesCcPublicFromUnauthedWeb": false,
        }));
        assert!(!is_hidden_from_unauthed_web(&post, Some(&actor)));
    }

    #[test]
    fn followers_only_post_is_always_hidden_for_a_gotosocial_actor() {
        let post = post(json!({
            "to": ["https://gts.test/users/alice/followers"],
        }));
        let actor = actor(json!({
            "hidesToPublicFromUnauthedWeb": false,
            "hidesCcPublicFromUnauthedWeb": false,
        }));
        assert!(is_hidden_from_unauthed_web(&post, Some(&actor)));
    }

    #[test]
    fn non_gotosocial_actor_without_flags_is_never_filtered() {
        let post = post(json!({
            "to": ["https://mastodon.test/users/bob/followers"],
            "cc": [PUBLIC],
        }));
        let actor = actor(json!({ "preferredUsername": "bob" }));
        assert!(!is_hidden_from_unauthed_web(&post, Some(&actor)));
    }

    #[test]
    fn missing_author_actor_is_never_filtered() {
        let post = post(json!({ "to": ["https://gts.test/users/alice/followers"] }));
        assert!(!is_hidden_from_unauthed_web(&post, None));
    }

    #[test]
    fn single_string_and_alias_public_addresses_are_recognised() {
        let string_public = post(json!({ "to": PUBLIC }));
        let hides_to = actor(json!({ "hidesToPublicFromUnauthedWeb": true }));
        assert!(is_hidden_from_unauthed_web(&string_public, Some(&hides_to)));

        let aliased = post(json!({ "cc": "as:Public" }));
        let hides_cc = actor(json!({ "hidesCcPublicFromUnauthedWeb": true }));
        assert!(is_hidden_from_unauthed_web(&aliased, Some(&hides_cc)));
    }
}
