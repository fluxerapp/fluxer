// SPDX-License-Identifier: AGPL-3.0-or-later

use super::limits;
use super::signature::RequestSigner;
use super::types::{
    ActivityPubActor, ActivityPubPost, InstanceInfo, MastodonInstance, MastodonPost,
};
use crate::http_fetch;
use reqwest::header::{ACCEPT, HeaderMap, HeaderValue};
use std::time::{Duration, SystemTime};
use tracing::debug;
use url::Url;

const ACCEPT_ACTIVITY_PUB: &str = concat!(
    "application/json, ",
    "application/activity+json, ",
    "application/ld+json; ",
    "profile=\"https://www.w3.org/ns/activitystreams\""
);

#[derive(Clone, Copy)]
pub struct ActivityPubClient<'a> {
    pub http: &'a reqwest::Client,
    pub signer: Option<&'a RequestSigner>,
}

async fn fetch_activity_pub_object(
    client: ActivityPubClient<'_>,
    url: &str,
) -> Option<http_fetch::FetchResult> {
    http_fetch::fetch_url_with_headers(
        client.http,
        url,
        |hop| {
            let mut headers = match client.signer {
                Some(signer) => signer.headers(hop, SystemTime::now())?,
                None => HeaderMap::new(),
            };
            headers.insert(ACCEPT, HeaderValue::from_static(ACCEPT_ACTIVITY_PUB));
            Ok(headers)
        },
        limits::OBJECT_MAX_BYTES,
        Duration::from_secs(5),
    )
    .await
    .ok()
}

pub async fn fetch_activity_pub(
    client: ActivityPubClient<'_>,
    url: &str,
) -> Option<ActivityPubPost> {
    let result = fetch_activity_pub_object(client, url).await?;

    if !(200..=299).contains(&result.status) {
        debug!(url, status = result.status, "ActivityPub fetch failed");
        return None;
    }

    let post: ActivityPubPost = serde_json::from_slice(&result.bytes).ok()?;

    if post.url.is_none() || post.published.is_none() || post.attributed_to.is_none() {
        debug!(url, "response is not a valid ActivityPub post");
        return None;
    }

    Some(post)
}

pub async fn fetch_activity_pub_actor(
    client: ActivityPubClient<'_>,
    url: &str,
) -> Option<ActivityPubActor> {
    let result = fetch_activity_pub_object(client, url).await?;

    if !(200..=299).contains(&result.status) {
        return None;
    }

    let actor: ActivityPubActor = serde_json::from_slice(&result.bytes).ok()?;
    if actor.name.is_none()
        && actor.preferred_username.is_none()
        && actor.url.is_none()
        && actor.icon.is_none()
    {
        return None;
    }
    Some(actor)
}

pub async fn fetch_mastodon_status(
    client: &reqwest::Client,
    url: &Url,
    post_id: &str,
) -> Option<MastodonPost> {
    let api_url = format!(
        "{}/api/v1/statuses/{post_id}",
        url.origin().ascii_serialization()
    );

    let result = http_fetch::fetch_url(
        client,
        &api_url,
        limits::MASTODON_STATUS_MAX_BYTES,
        Duration::from_secs(5),
    )
    .await
    .ok()?;

    if result.status != 200 {
        return None;
    }

    serde_json::from_slice(&result.bytes).ok()
}

pub async fn fetch_instance_info(client: &reqwest::Client, url: &Url) -> Option<InstanceInfo> {
    let api_url = format!("{}/api/v2/instance", url.origin().ascii_serialization());

    let result = http_fetch::fetch_url(
        client,
        &api_url,
        limits::INSTANCE_INFO_MAX_BYTES,
        Duration::from_secs(5),
    )
    .await
    .ok()?;

    if result.status != 200 {
        return None;
    }

    let instance: MastodonInstance = serde_json::from_slice(&result.bytes).ok()?;
    Some(instance.into())
}
