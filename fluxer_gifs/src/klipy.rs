// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::media_proxy::MediaProxyUrlBuilder;
use crate::types::{GifCategoryTag, GifItem, GifMediaFormat};
use anyhow::Context;
use reqwest::Url;
use serde::Deserialize;
use serde_json::Value;
use std::collections::{BTreeMap, HashSet};
use std::time::Duration;
use tokio::time::sleep;

const KLIPY_BASE_URL: &str = "https://api.klipy.com/v2";
const KLIPY_DIRECT_BASE_URL: &str = "https://api.klipy.com/api/v1";
const DEFAULT_CONTENT_FILTER: &str = "low";
const CLIENT_KEY: &str = "fluxer";
const MAX_RETRIES: usize = 3;
const BACKOFF_BASE_DELAY: Duration = Duration::from_secs(1);
const KLIPY_RESPONSE_LIMIT_BYTES: usize = 512 * 1024;
const FLUXER_USER_AGENT: &str = "Fluxerbot/1.0 (+https://fluxer.app)";
const KLIPY_PROVIDER_NAME: &str = "klipy";
const KLIPY_FEATURED_CATEGORY_REFRESH_COUNTRY: &str = "US";

const SIZE_PREFERENCE: [&str; 4] = ["hd", "md", "sm", "xs"];
const FILE_FORMAT_PREFERENCE: [&str; 4] = ["webm", "mp4", "webp", "gif"];
const MEDIA_FORMAT_PREFERENCE: [&str; 11] = [
    "webm",
    "mp4",
    "webp",
    "gif",
    "mediumgif",
    "tinywebm",
    "tinymp4",
    "tinygif",
    "nanowebm",
    "nanomp4",
    "nanogif",
];
const MEDIA_FILTER: &str =
    "webm,mp4,webp,gif,mediumgif,tinywebm,tinymp4,tinygif,nanowebm,nanomp4,nanogif";

#[derive(Clone)]
pub struct KlipyClient {
    http_client: reqwest::Client,
    media_proxy: MediaProxyUrlBuilder,
}

#[derive(Debug, Deserialize)]
struct ResultsResponse {
    results: Vec<Value>,
}

#[derive(Debug, Deserialize)]
struct TagsResponse {
    tags: Vec<Value>,
}

#[derive(Debug, Deserialize)]
struct KlipyGif {
    id: Value,
    #[serde(default)]
    slug: Option<String>,
    #[serde(default)]
    title: String,
    #[serde(default)]
    itemurl: Option<String>,
    #[serde(default)]
    file: Option<BTreeMap<String, BTreeMap<String, KlipyMediaEntry>>>,
    #[serde(default)]
    media_formats: Option<BTreeMap<String, KlipyMediaEntry>>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum KlipyMediaEntry {
    Url(String),
    Object {
        #[serde(default)]
        url: Option<String>,
        #[serde(default)]
        width: Option<i32>,
        #[serde(default)]
        height: Option<i32>,
        #[serde(default)]
        dims: Option<[i32; 2]>,
    },
}

#[derive(Debug, Deserialize)]
struct KlipyCategoryTag {
    searchterm: String,
}

#[derive(Debug, Deserialize)]
struct DirectGifResponse {
    #[serde(default)]
    data: Option<KlipyGif>,
}

enum KlipyJsonFetch<T> {
    Found(T),
    NotFound,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct KlipyPath {
    path_type: KlipyPathType,
    slug: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum KlipyPathType {
    Gif,
    Clip,
}

impl KlipyClient {
    pub fn new(media_proxy: MediaProxyUrlBuilder) -> anyhow::Result<Self> {
        let http_client = reqwest::Client::builder()
            .user_agent(FLUXER_USER_AGENT)
            .timeout(Duration::from_secs(30))
            .build()
            .context("failed to build KLIPY HTTP client")?;
        Ok(Self {
            http_client,
            media_proxy,
        })
    }

    pub async fn search(
        &self,
        api_key: &str,
        q: &str,
        locale: &str,
        country: &str,
        limit: u32,
    ) -> anyhow::Result<Vec<GifItem>> {
        let locale = normalize_locale(locale);
        let limit = limit.to_string();
        self.fetch_gifs(
            "search",
            &[
                ("key", api_key),
                ("q", q),
                ("country", country),
                ("locale", &locale),
                ("limit", &limit),
                ("media_filter", MEDIA_FILTER),
            ],
        )
        .await
    }

    pub async fn featured_gifs(
        &self,
        api_key: &str,
        locale: &str,
        country: &str,
    ) -> anyhow::Result<Vec<GifItem>> {
        let locale = normalize_locale(locale);
        self.fetch_gifs(
            "featured",
            &[
                ("key", api_key),
                ("country", country),
                ("locale", &locale),
                ("limit", "1"),
                ("media_filter", MEDIA_FILTER),
            ],
        )
        .await
    }

    pub async fn trending_gifs(
        &self,
        api_key: &str,
        locale: &str,
        country: &str,
    ) -> anyhow::Result<Vec<GifItem>> {
        let locale = normalize_locale(locale);
        self.fetch_gifs(
            "featured",
            &[
                ("key", api_key),
                ("country", country),
                ("locale", &locale),
                ("limit", "50"),
                ("media_filter", MEDIA_FILTER),
            ],
        )
        .await
    }

    pub async fn suggestions(
        &self,
        api_key: &str,
        q: &str,
        locale: &str,
    ) -> anyhow::Result<Vec<String>> {
        let locale = normalize_locale(locale);
        let response: ResultsResponse = self
            .fetch_json(
                "autocomplete",
                &[("key", api_key), ("q", q), ("locale", &locale)],
            )
            .await?;
        Ok(response
            .results
            .into_iter()
            .filter_map(|value| value.as_str().map(ToOwned::to_owned))
            .collect())
    }

    pub async fn register_share(
        &self,
        api_key: &str,
        id: &str,
        q: &str,
        locale: &str,
        country: &str,
    ) -> anyhow::Result<()> {
        let locale = normalize_locale(locale);
        let url = self.create_url(
            "registershare",
            &[
                ("key", api_key),
                ("id", id),
                ("country", country),
                ("locale", &locale),
                ("q", q),
            ],
        )?;
        let response = self.http_client.get(url).send().await?;
        if !response.status().is_success() {
            anyhow::bail!(
                "KLIPY registershare failed with status {}",
                response.status()
            );
        }
        Ok(())
    }

    pub async fn resolve_by_url(
        &self,
        api_key: &str,
        url: &str,
        _locale: &str,
        _country: &str,
    ) -> anyhow::Result<Option<GifItem>> {
        let Some(path) = parse_klipy_path(url) else {
            return Ok(None);
        };
        self.fetch_direct_gif(api_key, &path).await
    }

    pub async fn featured_categories(
        &self,
        api_key: &str,
        locale: &str,
    ) -> anyhow::Result<Vec<GifCategoryTag>> {
        let normalized_locale = normalize_locale(locale);
        let response: TagsResponse = self
            .fetch_json(
                "categories",
                &[
                    ("key", api_key),
                    ("country", KLIPY_FEATURED_CATEGORY_REFRESH_COUNTRY),
                    ("locale", &normalized_locale),
                    ("type", "featured"),
                ],
            )
            .await?;

        let mut seen = HashSet::new();
        let search_terms = response
            .tags
            .into_iter()
            .filter_map(|value| serde_json::from_value::<KlipyCategoryTag>(value).ok())
            .map(|tag| tag.searchterm.trim().to_owned())
            .filter(|term| !term.is_empty())
            .filter(|term| seen.insert(term.clone()))
            .collect::<Vec<_>>();

        let mut categories = Vec::with_capacity(search_terms.len());
        for search_term in search_terms {
            let gif = match self
                .search(
                    api_key,
                    &search_term,
                    &normalized_locale,
                    KLIPY_FEATURED_CATEGORY_REFRESH_COUNTRY,
                    1,
                )
                .await
            {
                Ok(mut gifs) => gifs.drain(..).next(),
                Err(err) => {
                    tracing::debug!(
                        error = %err,
                        search_term = %search_term,
                        locale = %normalized_locale,
                        "failed to fetch KLIPY category preview GIF"
                    );
                    None
                }
            };
            categories.push(category_response(search_term, gif));
        }

        Ok(categories)
    }

    async fn fetch_gifs(
        &self,
        endpoint: &str,
        params: &[(&str, &str)],
    ) -> anyhow::Result<Vec<GifItem>> {
        let response: ResultsResponse = self.fetch_json(endpoint, params).await?;
        Ok(response
            .results
            .into_iter()
            .filter_map(|value| serde_json::from_value::<KlipyGif>(value).ok())
            .filter_map(|gif| self.transform_gif(gif))
            .collect())
    }

    async fn fetch_direct_gif(
        &self,
        api_key: &str,
        path: &KlipyPath,
    ) -> anyhow::Result<Option<GifItem>> {
        let url = self.create_direct_url(api_key, path)?;
        let mut last_error = None;
        for attempt in 0..MAX_RETRIES {
            match self.fetch_direct_gif_once(url.clone(), path).await {
                Ok(value) => return Ok(value),
                Err(error) if attempt + 1 < MAX_RETRIES => {
                    last_error = Some(error);
                    sleep(BACKOFF_BASE_DELAY * 2_u32.pow(attempt as u32)).await;
                }
                Err(error) => return Err(error),
            }
        }
        Err(last_error.unwrap_or_else(|| anyhow::anyhow!("exceeded KLIPY retry limit")))
    }

    async fn fetch_direct_gif_once(
        &self,
        url: Url,
        path: &KlipyPath,
    ) -> anyhow::Result<Option<GifItem>> {
        match self.fetch_json_response::<DirectGifResponse>(url).await? {
            KlipyJsonFetch::NotFound => Ok(None),
            KlipyJsonFetch::Found(response) => Ok(response
                .data
                .and_then(|gif| self.transform_gif_with_path(gif, Some(path)))),
        }
    }

    async fn fetch_json<T>(&self, endpoint: &str, params: &[(&str, &str)]) -> anyhow::Result<T>
    where
        T: serde::de::DeserializeOwned,
    {
        let url = self.create_url(endpoint, params)?;
        let mut last_error = None;
        for attempt in 0..MAX_RETRIES {
            match self.fetch_json_once(url.clone()).await {
                Ok(value) => return Ok(value),
                Err(error) if attempt + 1 < MAX_RETRIES => {
                    last_error = Some(error);
                    sleep(BACKOFF_BASE_DELAY * 2_u32.pow(attempt as u32)).await;
                }
                Err(error) => return Err(error),
            }
        }
        Err(last_error.unwrap_or_else(|| anyhow::anyhow!("exceeded KLIPY retry limit")))
    }

    async fn fetch_json_once<T>(&self, url: Url) -> anyhow::Result<T>
    where
        T: serde::de::DeserializeOwned,
    {
        match self.fetch_json_response(url).await? {
            KlipyJsonFetch::Found(value) => Ok(value),
            KlipyJsonFetch::NotFound => anyhow::bail!("KLIPY request returned not found"),
        }
    }

    async fn fetch_json_response<T>(&self, url: Url) -> anyhow::Result<KlipyJsonFetch<T>>
    where
        T: serde::de::DeserializeOwned,
    {
        let response = self.http_client.get(url.clone()).send().await?;
        if response.status() == reqwest::StatusCode::NOT_FOUND {
            return Ok(KlipyJsonFetch::NotFound);
        }
        if !response.status().is_success() {
            anyhow::bail!("KLIPY request failed with status {}", response.status());
        }
        if response
            .content_length()
            .is_some_and(|len| len > KLIPY_RESPONSE_LIMIT_BYTES as u64)
        {
            anyhow::bail!("KLIPY response declared more than {KLIPY_RESPONSE_LIMIT_BYTES} bytes");
        }
        let bytes = response.bytes().await?;
        if bytes.len() > KLIPY_RESPONSE_LIMIT_BYTES {
            anyhow::bail!("KLIPY response exceeded {KLIPY_RESPONSE_LIMIT_BYTES} bytes");
        }
        serde_json::from_slice(&bytes)
            .with_context(|| format!("failed to parse KLIPY response from {url}"))
            .map(KlipyJsonFetch::Found)
    }

    fn create_url(&self, endpoint: &str, params: &[(&str, &str)]) -> anyhow::Result<Url> {
        let mut url = Url::parse(&format!("{KLIPY_BASE_URL}/{endpoint}"))?;
        {
            let mut query = url.query_pairs_mut();
            query.append_pair("client_key", CLIENT_KEY);
            query.append_pair("contentfilter", DEFAULT_CONTENT_FILTER);
            for (key, value) in params {
                query.append_pair(key, value);
            }
        }
        Ok(url)
    }

    fn create_direct_url(&self, api_key: &str, path: &KlipyPath) -> anyhow::Result<Url> {
        let mut url = Url::parse(&format!("{KLIPY_DIRECT_BASE_URL}/"))?;
        {
            let mut segments = url
                .path_segments_mut()
                .map_err(|_| anyhow::anyhow!("KLIPY direct base URL cannot be a base"))?;
            segments
                .push(api_key)
                .push(klipy_resource(path.path_type))
                .push(&path.slug);
        }
        Ok(url)
    }

    fn transform_gif(&self, input: KlipyGif) -> Option<GifItem> {
        self.transform_gif_with_path(input, None)
    }

    fn transform_gif_with_path(
        &self,
        input: KlipyGif,
        fallback_path: Option<&KlipyPath>,
    ) -> Option<GifItem> {
        let parsed_path = input.itemurl.as_deref().and_then(parse_klipy_path);
        let resolved_path = parsed_path.as_ref().or(fallback_path);
        let explicit_slug = input
            .slug
            .as_deref()
            .map(str::trim)
            .filter(|slug| !slug.is_empty());
        let fallback_id = klipy_id_as_string(&input.id)?;
        let normalized_slug = explicit_slug
            .or_else(|| resolved_path.map(|path| path.slug.as_str()))
            .unwrap_or(fallback_id.as_str())
            .to_owned();
        let normalized_type = resolved_path
            .map(|path| path.path_type)
            .unwrap_or(KlipyPathType::Gif);
        let normalized_url = if resolved_path.is_some() || explicit_slug.is_some() {
            build_share_url_with_type(normalized_type, &normalized_slug)
        } else {
            input
                .itemurl
                .clone()
                .unwrap_or_else(|| build_share_url_with_type(normalized_type, &normalized_slug))
        };
        let (media, preferred) = self.collect_media(&input);
        let top = media.get("webm").cloned().or(preferred)?;
        Some(GifItem {
            id: normalized_slug.clone(),
            slug: normalized_slug,
            provider: KLIPY_PROVIDER_NAME.to_owned(),
            title: input.title,
            url: normalized_url,
            src: top.src.clone(),
            proxy_src: top.proxy_src.clone(),
            width: top.width,
            height: top.height,
            media,
            placeholder: None,
        })
    }

    fn collect_media(
        &self,
        input: &KlipyGif,
    ) -> (BTreeMap<String, GifMediaFormat>, Option<GifMediaFormat>) {
        let mut media = BTreeMap::new();
        let mut preferred = None;
        for size in SIZE_PREFERENCE {
            let Some(bucket) = input.file.as_ref().and_then(|files| files.get(size)) else {
                continue;
            };
            for format in FILE_FORMAT_PREFERENCE {
                let Some(entry) = bucket.get(format) else {
                    continue;
                };
                let Some(media_format) = self.to_media_format(entry) else {
                    continue;
                };
                let public_key = public_format_key(size, format);
                media.insert(public_key, media_format.clone());
                if preferred.is_none() {
                    preferred = Some(media_format);
                }
            }
        }
        if let Some(formats) = input.media_formats.as_ref() {
            for format in MEDIA_FORMAT_PREFERENCE {
                let Some(entry) = formats.get(format) else {
                    continue;
                };
                let Some(media_format) = self.to_media_format(entry) else {
                    continue;
                };
                media
                    .entry(format.to_owned())
                    .or_insert_with(|| media_format.clone());
                if preferred.is_none() {
                    preferred = Some(media_format);
                }
            }

            for (format, entry) in formats {
                if MEDIA_FORMAT_PREFERENCE.contains(&format.as_str())
                    || !is_supported_media_format_key(format)
                {
                    continue;
                }
                let Some(media_format) = self.to_media_format(entry) else {
                    continue;
                };
                media
                    .entry(format.clone())
                    .or_insert_with(|| media_format.clone());
                if preferred.is_none() {
                    preferred = Some(media_format);
                }
            }
        }
        (media, preferred)
    }

    fn to_media_format(&self, entry: &KlipyMediaEntry) -> Option<GifMediaFormat> {
        let (src, width, height) = entry.media_parts()?;
        let proxy_src = self.media_proxy.external_proxy_url(src)?;
        Some(GifMediaFormat {
            src: src.to_owned(),
            proxy_src,
            width,
            height,
        })
    }
}

impl KlipyMediaEntry {
    fn media_parts(&self) -> Option<(&str, i32, i32)> {
        match self {
            KlipyMediaEntry::Url(url) => {
                let _ = url;
                None
            }
            KlipyMediaEntry::Object {
                url,
                width,
                height,
                dims,
            } => {
                let src = url.as_deref().filter(|url| !url.trim().is_empty())?;
                let size_from_dims =
                    (*dims).and_then(|[width, height]| valid_dimensions(width, height));
                let size_from_fields = (*width)
                    .zip(*height)
                    .and_then(|(width, height)| valid_dimensions(width, height));
                let (width, height) = size_from_dims.or(size_from_fields)?;
                Some((src, width, height))
            }
        }
    }
}

pub fn normalize_locale(locale: &str) -> String {
    // KLIPY v2 rejects numeric-region tags such as es-419 on categories and autocomplete.
    if let Some((language, region)) = locale.split_once(['-', '_'])
        && region.chars().all(|ch| ch.is_ascii_digit())
    {
        return language.to_owned();
    }

    locale.replace('-', "_")
}

pub fn build_share_url(slug: &str) -> String {
    let trimmed = slug.trim();
    if trimmed.is_empty() {
        return "https://klipy.com/gifs".to_owned();
    }
    build_share_url_with_type(KlipyPathType::Gif, trimmed)
}

pub fn extract_slug_from_url(url: &str) -> Option<String> {
    parse_klipy_path(url).map(|path| path.slug)
}

fn klipy_id_as_string(value: &Value) -> Option<String> {
    match value {
        Value::String(value) => {
            let trimmed = value.trim();
            (!trimmed.is_empty()).then(|| trimmed.to_owned())
        }
        Value::Number(value) => Some(value.to_string()),
        _ => None,
    }
}

fn parse_klipy_path(raw_url: &str) -> Option<KlipyPath> {
    let parsed = Url::parse(raw_url).ok()?;
    let hostname = parsed.host_str()?.to_ascii_lowercase();
    if hostname != "klipy.com" && hostname != "www.klipy.com" {
        return None;
    }
    let mut segments = parsed.path_segments()?;
    let kind = segments.next()?.to_ascii_lowercase();
    let slug = segments.next()?.trim().to_owned();
    if slug.is_empty() {
        return None;
    }
    let path_type = match kind.as_str() {
        "gif" | "gifs" => KlipyPathType::Gif,
        "clip" | "clips" => KlipyPathType::Clip,
        _ => return None,
    };
    Some(KlipyPath { path_type, slug })
}

fn build_share_url_with_type(path_type: KlipyPathType, slug: &str) -> String {
    let base_path = match path_type {
        KlipyPathType::Gif => "gifs",
        KlipyPathType::Clip => "clips",
    };
    let encoded_slug = urlencoding::encode(slug);
    format!("https://klipy.com/{base_path}/{encoded_slug}")
}

fn klipy_resource(path_type: KlipyPathType) -> &'static str {
    match path_type {
        KlipyPathType::Gif => "gifs",
        KlipyPathType::Clip => "clips",
    }
}

fn public_format_key(size: &str, format: &str) -> String {
    match (size, format) {
        ("hd", "webm") => "webm",
        ("hd", "mp4") => "mp4",
        ("hd", "webp") => "webp",
        ("hd", "gif") => "gif",
        ("md", "webm") => "mediumwebm",
        ("md", "mp4") => "mediummp4",
        ("md", "webp") => "mediumwebp",
        ("md", "gif") => "mediumgif",
        ("sm", "webm") => "tinywebm",
        ("sm", "mp4") => "tinymp4",
        ("sm", "webp") => "tinywebp",
        ("sm", "gif") => "tinygif",
        ("xs", "webm") => "nanowebm",
        ("xs", "mp4") => "nanomp4",
        ("xs", "webp") => "nanowebp",
        ("xs", "gif") => "nanogif",
        _ => format,
    }
    .to_owned()
}

fn valid_dimensions(width: i32, height: i32) -> Option<(i32, i32)> {
    (width > 0 && height > 0).then_some((width, height))
}

fn is_supported_media_format_key(format: &str) -> bool {
    matches!(
        format,
        "webm"
            | "tinywebm"
            | "nanowebm"
            | "mp4"
            | "loopedmp4"
            | "tinymp4"
            | "nanomp4"
            | "webp"
            | "gif"
            | "mediumgif"
            | "tinygif"
            | "nanogif"
    )
}

fn category_response(name: String, gif: Option<GifItem>) -> GifCategoryTag {
    GifCategoryTag {
        src: gif.as_ref().map(|gif| gif.src.clone()).unwrap_or_default(),
        proxy_src: gif
            .as_ref()
            .map(|gif| gif.proxy_src.clone())
            .unwrap_or_default(),
        gif,
        name,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn locale_uses_klipy_supported_form() {
        assert_eq!(normalize_locale("en-US"), "en_US");
        assert_eq!(normalize_locale("pt-BR"), "pt_BR");
        assert_eq!(normalize_locale("zh-CN"), "zh_CN");
        assert_eq!(normalize_locale("sv_SE"), "sv_SE");
        assert_eq!(normalize_locale("es-419"), "es");
        assert_eq!(normalize_locale("es_419"), "es");
        assert_eq!(normalize_locale("fr"), "fr");
    }

    #[test]
    fn extracts_klipy_slug_only_from_klipy_hosts() {
        assert_eq!(
            extract_slug_from_url("https://klipy.com/gifs/funny-123").as_deref(),
            Some("funny-123")
        );
        assert_eq!(
            extract_slug_from_url("https://www.klipy.com/clip/abc").as_deref(),
            Some("abc")
        );
        assert_eq!(
            extract_slug_from_url("https://notklipy.com/gifs/funny"),
            None
        );
    }

    #[test]
    fn build_share_url_uses_gifs_path() {
        assert_eq!(build_share_url("hello"), "https://klipy.com/gifs/hello");
        assert_eq!(build_share_url("  "), "https://klipy.com/gifs");
        assert_eq!(
            build_share_url_with_type(KlipyPathType::Clip, "hello"),
            "https://klipy.com/clips/hello"
        );
    }

    #[test]
    fn stringifies_numeric_klipy_ids() {
        assert_eq!(
            klipy_id_as_string(&serde_json::json!(2484942301552561_i64)).as_deref(),
            Some("2484942301552561")
        );
        assert_eq!(
            klipy_id_as_string(&serde_json::json!("  abc  ")).as_deref(),
            Some("abc")
        );
        assert_eq!(klipy_id_as_string(&serde_json::json!("   ")), None);
    }

    #[test]
    fn maps_provider_format_keys() {
        assert_eq!(public_format_key("hd", "webm"), "webm");
        assert_eq!(public_format_key("sm", "gif"), "tinygif");
        assert_eq!(public_format_key("xs", "webp"), "nanowebp");
    }

    #[test]
    fn transforms_v2_media_formats_into_animated_variants() {
        let client = KlipyClient::new(MediaProxyUrlBuilder::for_test(
            "https://media.example.test",
            "secret",
        ))
        .expect("client");
        let input = serde_json::from_value::<KlipyGif>(serde_json::json!({
            "id": "1373787462859498",
            "title": "Move Faster",
            "itemurl": "https://klipy.com/gifs/move-faster",
            "media_formats": {
                "webp": {
                    "url": "https://static.klipy.com/media/move.webp",
                    "dims": [498, 327]
                },
                "gif": {
                    "url": "https://static.klipy.com/media/move.gif",
                    "dims": [498, 326]
                },
                "mediumgif": {
                    "url": "https://static.klipy.com/media/move-medium.gif",
                    "dims": [640, 420]
                },
                "tinygif": {
                    "url": "https://static.klipy.com/media/move-tiny.gif",
                    "dims": [220, 144]
                },
                "nanogif": {
                    "url": "https://static.klipy.com/media/move-nano.gif",
                    "dims": [137, 90]
                },
                "webm": {
                    "url": "https://static.klipy.com/media/move.webm",
                    "dims": [640, 420]
                },
                "tinywebm": {
                    "url": "https://static.klipy.com/media/move-tiny.webm",
                    "dims": [320, 210]
                },
                "mp4": {
                    "url": "https://static.klipy.com/media/move.mp4",
                    "dims": [640, 420]
                },
                "preview": {
                    "url": "https://static.klipy.com/media/move.jpg",
                    "dims": [220, 144]
                }
            }
        }))
        .expect("fixture");

        let gif = client.transform_gif(input).expect("transformed gif");

        assert_eq!(gif.id, "move-faster");
        assert_eq!(gif.url, "https://klipy.com/gifs/move-faster");
        assert_eq!(gif.src, "https://static.klipy.com/media/move.webm");
        assert_eq!(gif.width, 640);
        assert_eq!(gif.height, 420);
        assert_eq!(
            gif.media.get("webp").map(|format| format.src.as_str()),
            Some("https://static.klipy.com/media/move.webp")
        );
        assert_eq!(
            gif.media.get("gif").map(|format| format.src.as_str()),
            Some("https://static.klipy.com/media/move.gif")
        );
        assert_eq!(
            gif.media
                .get("tinygif")
                .map(|format| (format.width, format.height)),
            Some((220, 144))
        );
        assert!(gif.media.contains_key("mediumgif"));
        assert!(gif.media.contains_key("nanogif"));
        assert!(gif.media.contains_key("tinywebm"));
        assert!(!gif.media.contains_key("preview"));
        assert!(
            gif.media
                .get("webp")
                .expect("webp format")
                .proxy_src
                .starts_with("https://media.example.test/external/")
        );
    }

    #[test]
    fn media_formats_without_video_choose_webp_as_top_level() {
        let client = KlipyClient::new(MediaProxyUrlBuilder::for_test(
            "https://media.example.test",
            "secret",
        ))
        .expect("client");
        let input = serde_json::from_value::<KlipyGif>(serde_json::json!({
            "id": "cat-cone",
            "title": "Cat Cone",
            "media_formats": {
                "webp": {
                    "url": "https://static.klipy.com/media/cat.webp",
                    "dims": [320, 180]
                },
                "gif": {
                    "url": "https://static.klipy.com/media/cat.gif",
                    "dims": [320, 180]
                }
            }
        }))
        .expect("fixture");

        let gif = client.transform_gif(input).expect("transformed gif");

        assert_eq!(gif.src, "https://static.klipy.com/media/cat.webp");
        assert_eq!(gif.proxy_src, gif.media["webp"].proxy_src);
        assert_eq!(gif.media["gif"].width, 320);
    }

    #[tokio::test]
    #[ignore]
    async fn live_search_exposes_animated_image_variants() {
        let api_key = std::env::var("FLUXER_KLIPY_API_KEY")
            .or_else(|_| std::env::var("KLIPY_API_KEY"))
            .expect("FLUXER_KLIPY_API_KEY or KLIPY_API_KEY set");
        let client =
            KlipyClient::new(MediaProxyUrlBuilder::from_env().expect("media proxy env configured"))
                .expect("KLIPY client");

        let gifs = client
            .search(&api_key, "move faster", "en_US", "US", 1)
            .await
            .expect("KLIPY search");
        let gif = gifs.first().expect("at least one GIF");

        assert!(gif.media.contains_key("webm"));
        assert!(gif.media.contains_key("webp"));
        assert!(gif.media.contains_key("gif"));
        assert!(gif.media.contains_key("tinygif") || gif.media.contains_key("nanogif"));
    }

    #[tokio::test]
    #[ignore]
    async fn live_accepts_es_419_locale_for_picker_endpoints() {
        let api_key = std::env::var("FLUXER_KLIPY_API_KEY")
            .or_else(|_| std::env::var("KLIPY_API_KEY"))
            .expect("FLUXER_KLIPY_API_KEY or KLIPY_API_KEY set");
        let locale = normalize_locale("es-419");
        assert_eq!(locale, "es");

        for (endpoint, params) in [
            (
                "featured",
                vec![
                    ("country", "US"),
                    ("locale", locale.as_str()),
                    ("limit", "1"),
                ],
            ),
            (
                "categories",
                vec![
                    ("country", "US"),
                    ("locale", locale.as_str()),
                    ("type", "featured"),
                ],
            ),
            (
                "search",
                vec![
                    ("country", "US"),
                    ("locale", locale.as_str()),
                    ("q", "cat"),
                    ("limit", "1"),
                ],
            ),
            (
                "autocomplete",
                vec![("locale", locale.as_str()), ("q", "cat")],
            ),
        ] {
            assert_live_klipy_endpoint_accepts(endpoint, &api_key, &params).await;
        }
    }

    async fn assert_live_klipy_endpoint_accepts(
        endpoint: &str,
        api_key: &str,
        params: &[(&str, &str)],
    ) {
        let mut url = Url::parse(&format!("{KLIPY_BASE_URL}/{endpoint}")).expect("KLIPY URL");
        {
            let mut query = url.query_pairs_mut();
            query.append_pair("client_key", CLIENT_KEY);
            query.append_pair("contentfilter", DEFAULT_CONTENT_FILTER);
            query.append_pair("key", api_key);
            for (key, value) in params {
                query.append_pair(key, value);
            }
        }

        let status = reqwest::Client::builder()
            .user_agent(FLUXER_USER_AGENT)
            .build()
            .expect("KLIPY HTTP client")
            .get(url)
            .send()
            .await
            .expect("KLIPY live request")
            .status();
        assert!(
            status.is_success(),
            "KLIPY {endpoint} request failed with status {status}"
        );
    }

    #[tokio::test]
    #[ignore]
    async fn live_resolves_klipy_url_with_direct_lookup() {
        let api_key = std::env::var("FLUXER_KLIPY_API_KEY")
            .or_else(|_| std::env::var("KLIPY_API_KEY"))
            .expect("FLUXER_KLIPY_API_KEY or KLIPY_API_KEY set");
        let client =
            KlipyClient::new(MediaProxyUrlBuilder::from_env().expect("media proxy env configured"))
                .expect("KLIPY client");

        let gif = client
            .resolve_by_url(
                &api_key,
                "https://klipy.com/gifs/goatplaybanjo-chat-4",
                "en-US",
                "US",
            )
            .await
            .expect("KLIPY direct lookup")
            .expect("resolved GIF");

        assert_eq!(gif.slug, "goatplaybanjo-chat-4");
        assert_eq!(gif.provider, KLIPY_PROVIDER_NAME);
        assert_eq!(gif.url, "https://klipy.com/gifs/goatplaybanjo-chat-4");
        assert!(gif.width > 0);
        assert!(gif.height > 0);
        assert!(gif.media.contains_key("webm") || gif.media.contains_key("mp4"));
        assert!(gif.proxy_src.starts_with("http"));
    }
}
