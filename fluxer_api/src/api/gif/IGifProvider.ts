// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GifCategoryTagResponse, GifResponse} from '@fluxer/schema/src/domains/gif/GifSchemas';

export interface GifProviderMeta {
	readonly name: string;
	readonly displayName: string;
	readonly attributionRequired: boolean;
}

export interface IGifProvider {
	readonly meta: GifProviderMeta;
	isAvailable(): Promise<boolean>;
	search(params: {q: string; locale: string; country: string}): Promise<Array<GifResponse>>;
	registerShare(params: {id: string; q: string; locale: string; country: string}): Promise<void>;
	getFeatured(params: {locale: string; country: string}): Promise<{
		gifs: Array<GifResponse>;
		categories: Array<GifCategoryTagResponse>;
	}>;
	getTrendingGifs(params: {locale: string; country: string}): Promise<Array<GifResponse>>;
	suggest(params: {q: string; locale: string}): Promise<Array<string>>;
	resolveByUrl(params: {url: string; locale: string; country: string}): Promise<GifResponse | null>;
	buildShareUrl(slug: string): string;
	extractSlugFromUrl(url: string): string | null;
}
