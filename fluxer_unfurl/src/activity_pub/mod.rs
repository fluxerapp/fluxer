// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod fetcher;
pub mod formatter;
pub mod limits;
pub mod signature;
pub mod types;
pub mod utils;
pub mod visibility;

use crate::html_parser;
use crate::media_proxy::MediaProxyClient;
use crate::types::MessageEmbed;
use crate::types::NsfwMode;
use fetcher::ActivityPubClient;
use url::Url;

pub async fn try_resolve(
    client: ActivityPubClient<'_>,
    media_proxy: &MediaProxyClient,
    nsfw_mode: NsfwMode,
    url: &Url,
    html: &str,
) -> Option<Vec<MessageEmbed>> {
    let Some(ap_link) = html_parser::find_activity_pub_link(html) else {
        if client.signer.is_some()
            && utils::extract_post_id(url).is_some()
            && html_parser::has_fediverse_profile_username(html)
        {
            return resolve_activity_pub_post(
                client,
                media_proxy,
                nsfw_mode,
                url,
                html,
                url.as_str(),
            )
            .await;
        }
        return None;
    };

    let ap_url = utils::resolve_relative_url(url, &ap_link)?;

    if !utils::is_http_url(&ap_url) {
        return None;
    }

    if let Some(embeds) =
        resolve_activity_pub_post(client, media_proxy, nsfw_mode, url, html, &ap_url).await
    {
        return Some(embeds);
    }

    let post_id = utils::extract_post_id(url)?;
    let mastodon_post = fetcher::fetch_mastodon_status(client.http, url, &post_id).await?;
    let mut context = build_context(client, url, Some(html)).await;
    if mastodon_post.in_reply_to_id.is_some() && mastodon_post.in_reply_to_account_id.is_some() {
        context.in_reply_to = fetch_mastodon_reply_context(client.http, url, &mastodon_post).await;
    }
    let embeds =
        formatter::format_mastodon_post(&mastodon_post, url, &context, media_proxy, nsfw_mode)
            .await;

    if embeds.is_empty() {
        None
    } else {
        Some(embeds)
    }
}

async fn resolve_activity_pub_post(
    client: ActivityPubClient<'_>,
    media_proxy: &MediaProxyClient,
    nsfw_mode: NsfwMode,
    url: &Url,
    html: &str,
    ap_url: &str,
) -> Option<Vec<MessageEmbed>> {
    let post = fetcher::fetch_activity_pub(client, ap_url).await?;
    let author_actor = fetch_post_author_actor(client, &post).await;
    if visibility::is_hidden_from_unauthed_web(&post, author_actor.as_ref()) {
        return None;
    }
    let mut context = build_context(client, url, Some(html)).await;
    if let Some(parent_url) = post.in_reply_to.as_deref() {
        context.in_reply_to = fetch_activity_pub_reply_context(client, parent_url).await;
    }
    let quote_child = fetch_activity_pub_quote_child(client, media_proxy, nsfw_mode, &post).await;
    let embeds = formatter::format_activity_pub_post(
        &post,
        url,
        &context,
        media_proxy,
        nsfw_mode,
        formatter::ActivityPubFormatOptions {
            author_actor: author_actor.as_ref(),
            quote_child,
            is_nested: false,
        },
    )
    .await;
    if embeds.is_empty() {
        None
    } else {
        Some(embeds)
    }
}

pub(crate) async fn build_context(
    client: ActivityPubClient<'_>,
    url: &Url,
    html: Option<&str>,
) -> types::ActivityPubContext {
    let instance = fetcher::fetch_instance_info(client.http, url).await;
    let apple_touch_icon = html.and_then(|html| html_parser::find_apple_touch_icon(html, url));
    let clean_host = url
        .host_str()
        .unwrap_or_default()
        .trim_start_matches("www.")
        .trim_start_matches("social.")
        .trim_start_matches("mstdn.")
        .to_owned();

    types::ActivityPubContext {
        server_domain: instance
            .as_ref()
            .and_then(|i| i.domain.clone())
            .unwrap_or_else(|| clean_host.clone()),
        server_title: instance
            .as_ref()
            .and_then(|i| i.title.clone())
            .unwrap_or_else(|| format!("{clean_host} Mastodon")),
        server_icon: instance.and_then(|i| i.thumbnail_url).or(apple_touch_icon),
        in_reply_to: None,
    }
}

pub(crate) async fn fetch_post_author_actor(
    client: ActivityPubClient<'_>,
    post: &types::ActivityPubPost,
) -> Option<types::ActivityPubActor> {
    let actor_url = post.attributed_to.as_ref()?.as_str()?;
    fetcher::fetch_activity_pub_actor(client, actor_url).await
}

async fn fetch_activity_pub_reply_context(
    client: ActivityPubClient<'_>,
    parent_url: &str,
) -> Option<types::ActivityPubReplyContext> {
    let parent = fetcher::fetch_activity_pub(client, parent_url).await?;
    let author_actor = fetch_post_author_actor(client, &parent).await;
    if visibility::is_hidden_from_unauthed_web(&parent, author_actor.as_ref()) {
        return None;
    }
    let author = match author_actor.as_ref() {
        Some(actor) => activity_pub_handle_from_actor(actor, parent_url),
        None => parent
            .attributed_to
            .as_ref()
            .and_then(|value| value.as_str())
            .and_then(activity_pub_handle_from_actor_url)
            .unwrap_or_default(),
    };
    let url = author_actor
        .as_ref()
        .and_then(|actor| actor.url.clone().or(actor.id.clone()))
        .or(parent.url)
        .unwrap_or_else(|| parent_url.to_owned());
    Some(types::ActivityPubReplyContext { author, url })
}

async fn fetch_mastodon_reply_context(
    client: &reqwest::Client,
    url: &Url,
    post: &types::MastodonPost,
) -> Option<types::ActivityPubReplyContext> {
    let parent_id = post.in_reply_to_id.as_deref()?;
    let parent = fetcher::fetch_mastodon_status(client, url, parent_id).await?;
    let account = parent.account.as_ref()?;
    let account_url = account.url.clone().or(parent.url)?;
    let author = mastodon_parent_author(account);
    Some(types::ActivityPubReplyContext {
        author,
        url: account_url,
    })
}

async fn fetch_activity_pub_quote_child(
    client: ActivityPubClient<'_>,
    media_proxy: &MediaProxyClient,
    nsfw_mode: NsfwMode,
    post: &types::ActivityPubPost,
) -> Option<MessageEmbed> {
    let quote_url = post
        .quote
        .as_deref()
        .or(post.quote_uri.as_deref())
        .or(post.misskey_quote.as_deref())?;
    let quote = fetcher::fetch_activity_pub(client, quote_url).await?;
    let author_actor = fetch_post_author_actor(client, &quote).await;
    if visibility::is_hidden_from_unauthed_web(&quote, author_actor.as_ref()) {
        return None;
    }
    let fallback = Url::parse(quote_url).ok()?;
    let quote_url = quote
        .url
        .as_deref()
        .and_then(|url| Url::parse(url).ok())
        .unwrap_or(fallback);
    let mut quote_context = build_context(client, &quote_url, None).await;
    if let Some(parent_url) = quote.in_reply_to.as_deref() {
        quote_context.in_reply_to = fetch_activity_pub_reply_context(client, parent_url).await;
    }
    let embeds = formatter::format_activity_pub_post(
        &quote,
        &quote_url,
        &quote_context,
        media_proxy,
        nsfw_mode,
        formatter::ActivityPubFormatOptions {
            author_actor: author_actor.as_ref(),
            quote_child: None,
            is_nested: true,
        },
    )
    .await;
    embeds.into_iter().next()
}

fn activity_pub_handle_from_actor(actor: &types::ActivityPubActor, fallback_url: &str) -> String {
    let actor_url = actor
        .url
        .as_deref()
        .or(actor.id.as_deref())
        .unwrap_or(fallback_url);
    let host = Url::parse(actor_url)
        .ok()
        .and_then(|url| url.host_str().map(str::to_owned))
        .unwrap_or_default();
    let username_from_url = utils::extract_username_from_url(actor_url);
    let username = actor
        .preferred_username
        .as_deref()
        .or(username_from_url.as_deref())
        .or(actor.name.as_deref())
        .unwrap_or_default()
        .trim_start_matches('@')
        .to_owned();
    if host.is_empty() {
        username
    } else {
        format!("@{username}@{host}")
    }
}

fn activity_pub_handle_from_actor_url(actor_url: &str) -> Option<String> {
    let parsed = Url::parse(actor_url).ok()?;
    let host = parsed.host_str()?;
    let username = utils::extract_username_from_url(actor_url)?;
    Some(format!("@{}@{host}", username.trim_start_matches('@')))
}

fn mastodon_parent_author(account: &types::MastodonAccount) -> String {
    let acct = account
        .acct
        .as_deref()
        .or(account.username.as_deref())
        .unwrap_or_default();
    if acct.contains('@') {
        format!("@{}", acct.trim_start_matches('@'))
    } else if let Some(host) = account
        .url
        .as_deref()
        .and_then(|url| Url::parse(url).ok())
        .and_then(|url| url.host_str().map(str::to_owned))
    {
        format!("@{acct}@{host}")
    } else {
        format!("@{acct}")
    }
}
