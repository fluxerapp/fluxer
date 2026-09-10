// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type FavoriteGifEntry,
	type FavoriteGifMediaFormat,
	slimFavoriteGifEntry,
} from '@app/features/channel/components/pickers/gif/FavoriteGifTypes';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {FAVORITE_GIF_MAX_ENCODED_BYTES} from '@app/features/user/state/SyncedFieldBudget';
import type {FavoriteGifMediaFormat as FavoriteGifMediaFormatProto} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/pickers_pb';
import {FavoriteGifSettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/pickers_pb';
import {makeAutoObservable} from 'mobx';

type FavoriteGifMediaFormatInit = Pick<FavoriteGifMediaFormatProto, 'src' | 'proxySrc' | 'width' | 'height'>;

function mediaToProto(media: Record<string, FavoriteGifMediaFormat>): {
	[key: string]: FavoriteGifMediaFormatInit;
} {
	const out: {
		[key: string]: FavoriteGifMediaFormatInit;
	} = {};
	for (const [key, value] of Object.entries(media)) {
		out[key] = {
			src: value.src,
			proxySrc: value.proxy_src,
			width: value.width,
			height: value.height,
		};
	}
	return out;
}

function mediaFromProto(media: {[key: string]: FavoriteGifMediaFormatProto}): Record<string, FavoriteGifMediaFormat> {
	const out: Record<string, FavoriteGifMediaFormat> = {};
	for (const [key, value] of Object.entries(media)) {
		out[key] = {
			src: value.src,
			proxy_src: value.proxySrc,
			width: value.width,
			height: value.height,
		};
	}
	return out;
}

class FavoriteGif {
	favoriteGifs: Array<FavoriteGifEntry> = [];
	saveGifFavoritesAsSavedMedia = false;
	hasSeenFavoriteGifFirstTimePrompt = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void makeSyncedField(this, {
			field: 'favoriteGifs',
			schema: FavoriteGifSettingsSchema,
			persist: ['favoriteGifs', 'saveGifFavoritesAsSavedMedia', 'hasSeenFavoriteGifFirstTimePrompt'],
			maxEncodedBytes: FAVORITE_GIF_MAX_ENCODED_BYTES,
			toMessage: (s) => ({
				entries: s.favoriteGifs.map(slimFavoriteGifEntry).map((entry) => ({
					url: entry.url,
					proxyUrl: entry.proxy_url,
					width: entry.width,
					height: entry.height,
					media: mediaToProto(entry.media),
					contentType: entry.content_type,
					placeholder: entry.placeholder ?? '',
				})),
				saveAsSavedMedia: s.saveGifFavoritesAsSavedMedia,
				seenFirstTimePrompt: s.hasSeenFavoriteGifFirstTimePrompt,
			}),
			applyMessage: (s, m) => {
				s.favoriteGifs = m.entries.map((entry) =>
					slimFavoriteGifEntry({
						url: entry.url,
						proxy_url: entry.proxyUrl,
						width: entry.width,
						height: entry.height,
						media: mediaFromProto(entry.media),
						content_type: entry.contentType,
						placeholder: entry.placeholder ? entry.placeholder : null,
					}),
				);
				s.saveGifFavoritesAsSavedMedia = m.saveAsSavedMedia;
				s.hasSeenFavoriteGifFirstTimePrompt = m.seenFirstTimePrompt;
			},
		});
	}

	get totalCount(): number {
		return this.favoriteGifs.length;
	}

	hasUrl(url: string): boolean {
		return this.favoriteGifs.some((entry) => entry.url === url);
	}

	findByUrl(url: string): FavoriteGifEntry | null {
		return this.favoriteGifs.find((entry) => entry.url === url) ?? null;
	}

	addEntry(entry: FavoriteGifEntry): void {
		if (this.hasUrl(entry.url)) return;
		this.favoriteGifs = [...this.favoriteGifs, entry];
	}

	removeByUrl(url: string): void {
		if (!this.hasUrl(url)) return;
		this.favoriteGifs = this.favoriteGifs.filter((entry) => entry.url !== url);
	}

	replaceAll(entries: ReadonlyArray<FavoriteGifEntry>): void {
		this.favoriteGifs = [...entries];
	}

	setSaveGifFavoritesAsSavedMedia(value: boolean): void {
		this.saveGifFavoritesAsSavedMedia = value;
	}
}

export default new FavoriteGif();
