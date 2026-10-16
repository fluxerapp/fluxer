// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type FavoriteGifEntry,
	type FavoriteGifMediaFormat,
	slimFavoriteGifEntry,
	stripFavoriteGifEntrySignatures,
} from '@app/features/channel/components/pickers/gif/FavoriteGifTypes';
import {stripAttachmentSignature} from '@app/features/messaging/utils/AttachmentCdnUrl';
import {unwrapDesktopLocalResourceURL} from '@app/features/messaging/utils/DesktopResourceUrl';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {FAVORITE_GIF_MAX_ENCODED_BYTES} from '@app/features/user/state/SyncedFieldBudget';
import type {FavoriteGifMediaFormat as FavoriteGifMediaFormatProto} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/pickers_pb';
import {FavoriteGifSettingsSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/pickers_pb';
import {makeAutoObservable} from 'mobx';

type FavoriteGifMediaFormatInit = Pick<FavoriteGifMediaFormatProto, 'src' | 'proxySrc' | 'width' | 'height'>;

function portableUrl(value: unknown): string | null {
	if (typeof value !== 'string' || value.trim().length === 0) {
		return null;
	}
	try {
		const portable = unwrapDesktopLocalResourceURL(value.trim()).trim();
		const parsed = new URL(portable);
		if (
			(parsed.protocol !== 'http:' && parsed.protocol !== 'https:') ||
			parsed.username.length > 0 ||
			parsed.password.length > 0
		) {
			return null;
		}
		return portable;
	} catch {
		return null;
	}
}

function dimension(value: unknown): number {
	return typeof value === 'number' && Number.isSafeInteger(value) && value >= 0 && value <= 0xffffffff ? value : 0;
}

function portableMedia(
	media: Record<string, FavoriteGifMediaFormat> | null | undefined,
): Record<string, FavoriteGifMediaFormat> {
	const out: Record<string, FavoriteGifMediaFormat> = {};
	for (const [key, value] of Object.entries(media ?? {})) {
		const src = portableUrl(value?.src);
		const proxySrc = portableUrl(value?.proxy_src);
		const width = dimension(value?.width);
		const height = dimension(value?.height);
		if (src === null || proxySrc === null || width === 0 || height === 0) {
			continue;
		}
		out[key] = {...value, src, proxy_src: proxySrc, width, height};
	}
	return out;
}

function syncedEntry(entry: FavoriteGifEntry): FavoriteGifEntry | null {
	const url = portableUrl(entry.url);
	if (url === null) {
		return null;
	}
	return slimFavoriteGifEntry({
		url,
		proxy_url: portableUrl(entry.proxy_url) ?? url,
		width: dimension(entry.width),
		height: dimension(entry.height),
		media: portableMedia(entry.media),
		content_type: typeof entry.content_type === 'string' ? entry.content_type : '',
		placeholder: typeof entry.placeholder === 'string' && entry.placeholder.length > 0 ? entry.placeholder : null,
	});
}

function syncedEntries(entries: ReadonlyArray<FavoriteGifEntry>): Array<FavoriteGifEntry> {
	const out: Array<FavoriteGifEntry> = [];
	const seen = new Set<string>();
	for (const entry of entries) {
		const synced = syncedEntry(entry);
		if (synced === null || seen.has(synced.url)) {
			continue;
		}
		seen.add(synced.url);
		out.push(synced);
	}
	return out;
}

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
		initializeStore(this, () =>
			makeSyncedField(this, {
				field: 'favoriteGifs',
				schema: FavoriteGifSettingsSchema,
				persist: ['favoriteGifs', 'saveGifFavoritesAsSavedMedia', 'hasSeenFavoriteGifFirstTimePrompt'],
				maxEncodedBytes: FAVORITE_GIF_MAX_ENCODED_BYTES,
				toMessage: (s) => ({
					entries: syncedEntries(s.favoriteGifs).map((entry) => ({
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
					s.favoriteGifs = syncedEntries(
						m.entries.map((entry) => ({
							url: entry.url,
							proxy_url: entry.proxyUrl,
							width: entry.width,
							height: entry.height,
							media: mediaFromProto(entry.media),
							content_type: entry.contentType,
							placeholder: entry.placeholder ? entry.placeholder : null,
						})),
					);
					s.saveGifFavoritesAsSavedMedia = m.saveAsSavedMedia;
					s.hasSeenFavoriteGifFirstTimePrompt = m.seenFirstTimePrompt;
				},
			}),
		);
	}

	get totalCount(): number {
		return this.favoriteGifs.length;
	}

	hasUrl(url: string): boolean {
		const target = stripAttachmentSignature(url);
		return this.favoriteGifs.some((entry) => stripAttachmentSignature(entry.url) === target);
	}

	findByUrl(url: string): FavoriteGifEntry | null {
		const target = stripAttachmentSignature(url);
		return this.favoriteGifs.find((entry) => stripAttachmentSignature(entry.url) === target) ?? null;
	}

	addEntry(entry: FavoriteGifEntry): void {
		const stored = stripFavoriteGifEntrySignatures(entry);
		if (this.hasUrl(stored.url)) return;
		this.favoriteGifs = [...this.favoriteGifs, stored];
	}

	removeByUrl(url: string): void {
		if (!this.hasUrl(url)) return;
		const target = stripAttachmentSignature(url);
		this.favoriteGifs = this.favoriteGifs.filter((entry) => stripAttachmentSignature(entry.url) !== target);
	}

	replaceAll(entries: ReadonlyArray<FavoriteGifEntry>): void {
		this.favoriteGifs = entries.map(stripFavoriteGifEntrySignatures);
	}

	setSaveGifFavoritesAsSavedMedia(value: boolean): void {
		this.saveGifFavoritesAsSavedMedia = value;
	}
}

export default new FavoriteGif();
