// SPDX-License-Identifier: AGPL-3.0-or-later

use url::Url;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShareKind {
    Gif,
    Clip,
}

impl ShareKind {
    pub fn resource(self) -> &'static str {
        match self {
            Self::Gif => "gifs",
            Self::Clip => "clips",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SharePath {
    pub kind: ShareKind,
    pub slug: String,
}

pub fn parse_share_path(url: &Url) -> Option<SharePath> {
    let host = url.host_str()?;
    if !host.eq_ignore_ascii_case("klipy.com") && !host.eq_ignore_ascii_case("www.klipy.com") {
        return None;
    }
    let mut segments = url.path_segments()?;
    let kind = match segments.next()?.to_ascii_lowercase().as_str() {
        "gif" | "gifs" => ShareKind::Gif,
        "clip" | "clips" => ShareKind::Clip,
        _ => return None,
    };
    let slug = segments.next()?.trim();
    if slug.is_empty() {
        return None;
    }
    Some(SharePath {
        kind,
        slug: slug.to_owned(),
    })
}

pub fn direct_items_url(api_key: &str, kind: ShareKind, slug: &str) -> anyhow::Result<Url> {
    let mut url = Url::parse("https://api.klipy.com/api/v1")?;
    url.path_segments_mut()
        .map_err(|_| anyhow::anyhow!("KLIPY API base URL cannot be a base"))?
        .push(api_key)
        .push(kind.resource())
        .push("items");
    url.query_pairs_mut().append_pair("slugs", slug);
    Ok(url)
}
