// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::types::{GifRequest, GifServiceResponse};
use fluxer_svc::router::RouterService;
use moka::sync::Cache;
use std::time::Duration;

pub struct GifsRouter {
    l1: Cache<String, GifServiceResponse>,
}

impl GifsRouter {
    pub fn new(max_entries: u64, ttl: Duration) -> Self {
        Self {
            l1: Cache::builder()
                .max_capacity(max_entries)
                .time_to_live(ttl)
                .build(),
        }
    }
}

impl RouterService for GifsRouter {
    type Request = GifRequest;
    type Response = GifServiceResponse;

    fn service_name(&self) -> &str {
        "gifs"
    }

    fn route_key(req: &GifRequest) -> String {
        request_key(req).unwrap_or_else(|| "uncached".to_owned())
    }

    fn coalesce_key(req: &GifRequest) -> Option<String> {
        request_key(req)
    }

    fn l1_lookup(&self, req: &GifRequest) -> Option<GifServiceResponse> {
        request_l1_key(req).and_then(|key| self.l1.get(&key))
    }

    fn l1_insert(&self, req: &GifRequest, resp: &GifServiceResponse) {
        if response_is_cacheable(resp)
            && let Some(key) = request_l1_key(req)
        {
            self.l1.insert(key, resp.clone());
        }
    }

    fn l1_invalidate(&self, key: &str) {
        self.l1.invalidate(key);
    }
}

fn request_l1_key(req: &GifRequest) -> Option<String> {
    match req {
        GifRequest::Search { .. }
        | GifRequest::GetFeatured { .. }
        | GifRequest::GetTrendingGifs { .. }
        | GifRequest::Suggest { .. }
        | GifRequest::ResolveByUrl { .. }
        | GifRequest::BuildShareUrl { .. }
        | GifRequest::ExtractSlugFromUrl { .. } => request_key(req),
        GifRequest::IsAvailable { .. } | GifRequest::RegisterShare { .. } => None,
    }
}

fn request_key(req: &GifRequest) -> Option<String> {
    match req {
        GifRequest::IsAvailable { .. } => None,
        GifRequest::Search {
            q, locale, country, ..
        } => Some(format!("search:{locale}:{country}:{q}")),
        GifRequest::GetFeatured {
            locale, country, ..
        } => Some(format!("featured:{locale}:{country}")),
        GifRequest::GetTrendingGifs {
            locale, country, ..
        } => Some(format!("trending:{locale}:{country}")),
        GifRequest::Suggest { q, locale, .. } => Some(format!("suggest:{locale}:{q}")),
        GifRequest::RegisterShare { .. } => None,
        GifRequest::ResolveByUrl {
            url,
            locale,
            country,
            ..
        } => Some(format!("resolve:{locale}:{country}:{url}")),
        GifRequest::BuildShareUrl { slug } => Some(format!("share-url:{slug}")),
        GifRequest::ExtractSlugFromUrl { url } => Some(format!("extract-slug:{url}")),
    }
}

fn response_is_cacheable(resp: &GifServiceResponse) -> bool {
    !matches!(
        resp,
        GifServiceResponse::Available { .. }
            | GifServiceResponse::Registered
            | GifServiceResponse::Failed { .. }
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn coalesce_key_excludes_api_key() {
        let first = GifRequest::Search {
            api_key: "a".to_owned(),
            q: "wave".to_owned(),
            locale: "en_US".to_owned(),
            country: "US".to_owned(),
        };
        let second = GifRequest::Search {
            api_key: "b".to_owned(),
            q: "wave".to_owned(),
            locale: "en_US".to_owned(),
            country: "US".to_owned(),
        };

        assert_eq!(
            GifsRouter::coalesce_key(&first),
            GifsRouter::coalesce_key(&second)
        );
    }

    #[test]
    fn trending_key_is_locale_country_scoped_and_api_key_free() {
        let base = GifRequest::GetTrendingGifs {
            api_key: "a".to_owned(),
            locale: "en_US".to_owned(),
            country: "US".to_owned(),
        };
        let different_api_key = GifRequest::GetTrendingGifs {
            api_key: "b".to_owned(),
            locale: "en_US".to_owned(),
            country: "US".to_owned(),
        };
        let different_locale = GifRequest::GetTrendingGifs {
            api_key: "a".to_owned(),
            locale: "sv_SE".to_owned(),
            country: "US".to_owned(),
        };
        let different_country = GifRequest::GetTrendingGifs {
            api_key: "a".to_owned(),
            locale: "en_US".to_owned(),
            country: "SE".to_owned(),
        };

        assert_eq!(
            GifsRouter::coalesce_key(&base),
            Some("trending:en_US:US".to_owned())
        );
        assert_eq!(
            GifsRouter::coalesce_key(&base),
            GifsRouter::coalesce_key(&different_api_key)
        );
        assert_ne!(
            GifsRouter::coalesce_key(&base),
            GifsRouter::coalesce_key(&different_locale)
        );
        assert_ne!(
            GifsRouter::coalesce_key(&base),
            GifsRouter::coalesce_key(&different_country)
        );
        assert_eq!(request_l1_key(&base), Some("trending:en_US:US".to_owned()));
    }

    #[test]
    fn register_share_is_not_cached_or_coalesced() {
        let request = GifRequest::RegisterShare {
            api_key: "key".to_owned(),
            id: "gif".to_owned(),
            q: "wave".to_owned(),
            locale: "en_US".to_owned(),
            country: "US".to_owned(),
        };

        assert_eq!(GifsRouter::coalesce_key(&request), None);
        assert_eq!(request_l1_key(&request), None);
    }
}
