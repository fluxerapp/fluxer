// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {
	type FavoriteGifEntry,
	inferFormatContentType,
	pickCanonicalPreviewFormat,
} from '@app/features/channel/components/pickers/gif/FavoriteGifTypes';
import FavoriteGif from '@app/features/expressions/state/FavoriteGif';
import {FAVORITE_GIF_LIMIT_REACHED_DESCRIPTOR} from '@app/features/expressions/utils/FavoriteGifMessageDescriptors';
import * as GifSlugUtils from '@app/features/expressions/utils/GifSlugUtils';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {ME} from '@fluxer/constants/src/AppConstants';
import {MAX_FAVORITE_GIFS} from '@fluxer/constants/src/LimitConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {createElement} from 'react';

const ADDED_TO_FAVORITE_GIFS_DESCRIPTOR = msg({
	message: 'Added to favorite GIFs',
	comment: 'Toast confirming a GIF was added to favorites.',
});
const REMOVED_FROM_FAVORITE_GIFS_DESCRIPTOR = msg({
	message: 'Removed from favorite GIFs',
	comment: 'Toast confirming a GIF was removed from favorites.',
});
const FAVORITE_GIF_LIMIT_REACHED_TITLE_DESCRIPTOR = msg({
	message: 'Favorite GIF limit reached',
	comment: 'Title of the error modal shown when no more favorite GIFs can be added.',
});
const logger = new Logger('FavoriteGifs');

let lastPreviewRefreshSignature = '';
let previewRefreshPromise: Promise<void> | null = null;

interface ResolveGifUrlsResponse {
	entries: Array<Omit<FavoriteGifEntry, 'placeholder'> & {placeholder?: string | null}>;
}

interface FavoriteGifFromMediaParams {
	url: string;
	proxyUrl?: string;
	width?: number;
	height?: number;
	media?: FavoriteGifEntry['media'];
	placeholder?: string | null;
}

function favoriteGifLimitMessage(i18n: I18n): string {
	return i18n._(FAVORITE_GIF_LIMIT_REACHED_DESCRIPTOR);
}

function showFavoriteGifToast(children: string): void {
	ToastCommands.createToast({type: 'success', children});
}

function showFavoriteGifLimitModal(i18n: I18n): void {
	const title = i18n._(FAVORITE_GIF_LIMIT_REACHED_TITLE_DESCRIPTOR);
	const message = favoriteGifLimitMessage(i18n);
	ModalCommands.push(modal(() => createElement(GenericErrorModal, {title, message})));
}

function canAddFavoriteGif(entry: FavoriteGifEntry): boolean {
	return !FavoriteGif.hasUrl(entry.url) && FavoriteGif.totalCount < MAX_FAVORITE_GIFS;
}

function requestFavoriteGifResolution(urls: Array<string>): Promise<ResolveGifUrlsResponse> {
	return http
		.post<ResolveGifUrlsResponse>(Endpoints.USER_FAVORITE_GIFS_RESOLVE(ME), {body: {urls}})
		.then((response) => response.body);
}

export function addFavoriteGif(i18n: I18n, entry: FavoriteGifEntry): void {
	if (FavoriteGif.hasUrl(entry.url)) return;
	if (FavoriteGif.totalCount >= MAX_FAVORITE_GIFS) {
		showFavoriteGifLimitModal(i18n);
		return;
	}
	if (!canAddFavoriteGif(entry)) return;
	FavoriteGif.addEntry(entry);
	showFavoriteGifToast(i18n._(ADDED_TO_FAVORITE_GIFS_DESCRIPTOR));
	logger.debug(`Added favorite GIF ${entry.url}`);
}

export function addFavoriteGifFromMedia(i18n: I18n, params: FavoriteGifFromMediaParams): void {
	const media = params.media ?? {};
	const bestPreview = pickCanonicalPreviewFormat(media);
	addFavoriteGif(i18n, {
		url: params.url,
		proxy_url: bestPreview?.format.proxy_src ?? params.proxyUrl ?? '',
		width: bestPreview?.format.width ?? params.width ?? 0,
		height: bestPreview?.format.height ?? params.height ?? 0,
		media,
		content_type: bestPreview ? inferFormatContentType(bestPreview.key) : '',
		placeholder: params.placeholder ?? null,
	});
}

export function removeFavoriteGifByUrl(i18n: I18n, url: string): void {
	if (!FavoriteGif.hasUrl(url)) return;
	FavoriteGif.removeByUrl(url);
	showFavoriteGifToast(i18n._(REMOVED_FROM_FAVORITE_GIFS_DESCRIPTOR));
	logger.debug(`Removed favorite GIF ${url}`);
}

export async function resolveGifUrls(urls: Array<string>): Promise<Array<FavoriteGifEntry>> {
	if (urls.length === 0) return [];
	const response = await requestFavoriteGifResolution(urls);
	return response.entries.map((entry) => ({...entry, placeholder: entry.placeholder ?? null}));
}

export async function refreshFavoriteGifPreviews(): Promise<void> {
	if (previewRefreshPromise) return previewRefreshPromise;
	const candidates = FavoriteGif.favoriteGifs.filter(needsPreviewRefresh);
	const signature = candidates.map((entry) => entry.url).join('\n');
	if (!signature || signature === lastPreviewRefreshSignature) return;
	previewRefreshPromise = (async () => {
		try {
			const resolved = await resolveGifUrls(candidates.map((entry) => entry.url));
			if (resolved.length === 0) return;
			const replacements = new Map<string, FavoriteGifEntry>();
			candidates.forEach((entry, index) => {
				const replacement = resolved[index];
				if (replacement) replacements.set(entry.url, replacement);
			});
			const deduped: Array<FavoriteGifEntry> = [];
			const seen = new Set<string>();
			for (const entry of FavoriteGif.favoriteGifs) {
				const nextEntry = replacements.get(entry.url) ?? entry;
				if (seen.has(nextEntry.url)) continue;
				seen.add(nextEntry.url);
				deduped.push(nextEntry);
			}
			FavoriteGif.replaceAll(deduped);
			lastPreviewRefreshSignature = signature;
		} catch (error) {
			logger.warn({error}, 'Failed to refresh favorite GIF previews');
		} finally {
			previewRefreshPromise = null;
		}
	})();
	return previewRefreshPromise;
}

function needsPreviewRefresh(entry: FavoriteGifEntry): boolean {
	if (pickCanonicalPreviewFormat(entry.media)) return false;
	if (!entry.proxy_url || GifSlugUtils.isProviderPageUrl(entry.proxy_url)) return true;
	if (entry.width <= 0 || entry.height <= 0) return true;
	return !entry.content_type.startsWith('image/') && !entry.content_type.startsWith('video/');
}
