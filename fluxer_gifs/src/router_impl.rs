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
}

fn request_l1_key(req: &GifRequest) -> Option<String> {
    match req {
        GifRequest::Search { .. }
        | GifRequest::GetFeatured { .. }
        | GifRequest::GetTrendingGifs { .. }
        | GifRequest::Suggest { .. }
        | GifRequest::ResolveByUrl { .. } => request_key(req),
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
        GifRequest::ResolveByUrl { url, .. } => {
            Some(format!("resolve:{}", crate::klipy::resolve_cache_key(url)))
        }
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
