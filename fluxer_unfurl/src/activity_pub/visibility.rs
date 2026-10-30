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
