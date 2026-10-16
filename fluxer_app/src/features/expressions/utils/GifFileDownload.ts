// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Gif} from '@app/features/expressions/commands/GifCommands';
import {wrapDesktopLocalResourceURL} from '@app/features/messaging/utils/DesktopResourceUrl';

const GIF_IMAGE_FORMAT_KEYS = ['gif', 'tinygif', 'nanogif'] as const;
const GIF_VIDEO_FORMAT_KEYS = ['mp4', 'tinymp4'] as const;
const DOWNLOAD_CANDIDATES_MAX = 8;

interface GifDownloadTarget {
	url: string;
	contentType: string;
	extension: string;
}

function sanitizeGifFileBaseName(gif: Gif): string {
	const base = (gif.id || gif.slug || 'gif').toLowerCase().replace(/[^a-z0-9_-]+/gu, '-');
	return base.length > 0 ? base.slice(0, 64) : 'gif';
}

function collectFormatTargets(
	gif: Gif,
	keys: ReadonlyArray<string>,
	contentType: string,
	extension: string,
): Array<GifDownloadTarget> {
	const targets: Array<GifDownloadTarget> = [];
	for (const key of keys) {
		const format = gif.media?.[key];
		if (!format) continue;
		if (format.proxy_src) {
			targets.push({url: format.proxy_src, contentType, extension});
		}
		if (format.src) {
			targets.push({url: format.src, contentType, extension});
		}
	}
	return targets;
}

function collectImageTargets(gif: Gif): Array<GifDownloadTarget> {
	const targets = collectFormatTargets(gif, GIF_IMAGE_FORMAT_KEYS, 'image/gif', 'gif');
	for (const url of [gif.proxy_src, gif.src]) {
		if (url && /\.gif(?:$|\?)/iu.test(url)) {
			targets.push({url, contentType: 'image/gif', extension: 'gif'});
		}
	}
	return targets;
}

function collectVideoTargets(gif: Gif): Array<GifDownloadTarget> {
	return collectFormatTargets(gif, GIF_VIDEO_FORMAT_KEYS, 'video/mp4', 'mp4');
}

async function downloadFirstAvailableTarget(targets: Array<GifDownloadTarget>, baseName: string): Promise<File> {
	const seenUrls = new Set<string>();
	let attempts = 0;
	for (const target of targets) {
		if (attempts >= DOWNLOAD_CANDIDATES_MAX) break;
		if (seenUrls.has(target.url)) continue;
		seenUrls.add(target.url);
		attempts += 1;
		try {
			const response = await fetch(wrapDesktopLocalResourceURL(target.url));
			if (!response.ok) continue;
			const blob = await response.blob();
			if (blob.size === 0) continue;
			return new File([blob], `${baseName}.${target.extension}`, {type: target.contentType});
		} catch {}
	}
	throw new Error('Failed to download GIF media');
}

export async function downloadGifAsImageFile(gif: Gif): Promise<File> {
	return downloadFirstAvailableTarget(collectImageTargets(gif), sanitizeGifFileBaseName(gif));
}

export async function downloadGifAsVideoOrImageFile(gif: Gif): Promise<File> {
	const targets = [...collectVideoTargets(gif), ...collectImageTargets(gif)];
	return downloadFirstAvailableTarget(targets, sanitizeGifFileBaseName(gif));
}
