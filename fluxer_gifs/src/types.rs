// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "op", rename_all_fields = "snake_case")]
pub enum GifRequest {
    IsAvailable {
        api_key: Option<String>,
    },
    Search {
        api_key: String,
        q: String,
        locale: String,
        country: String,
    },
    GetFeatured {
        api_key: String,
        locale: String,
        country: String,
    },
    GetTrendingGifs {
        api_key: String,
        locale: String,
        country: String,
    },
    Suggest {
        api_key: String,
        q: String,
        locale: String,
    },
    RegisterShare {
        api_key: String,
        id: String,
        q: String,
        locale: String,
        country: String,
    },
    ResolveByUrl {
        api_key: String,
        url: String,
        locale: String,
        country: String,
    },
    BuildShareUrl {
        slug: String,
    },
    ExtractSlugFromUrl {
        url: String,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GifServiceResponse {
    Available {
        available: bool,
    },
    SearchResults(Vec<GifItem>),
    Featured {
        gifs: Vec<GifItem>,
        categories: Vec<GifCategoryTag>,
    },
    TrendingResults(Vec<GifItem>),
    Suggestions(Vec<String>),
    Registered,
    Resolved {
        gif: Option<GifItem>,
    },
    ShareUrl {
        url: String,
    },
    ExtractedSlug {
        slug: Option<String>,
    },
    Failed {
        message: String,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GifMediaFormat {
    pub src: String,
    pub proxy_src: String,
    pub width: i32,
    pub height: i32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GifItem {
    pub id: String,
    pub slug: String,
    pub provider: String,
    pub title: String,
    pub url: String,
    pub src: String,
    pub proxy_src: String,
    pub width: i32,
    pub height: i32,
    pub media: BTreeMap<String, GifMediaFormat>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub placeholder: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GifCategoryTag {
    pub name: String,
    pub src: String,
    pub proxy_src: String,
    pub gif: Option<GifItem>,
}
