// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {convertToCodePoints} from '@app/features/expressions/utils/EmojiCodepointUtils';
import {MODE} from '@app/features/platform/types/Env';

const TWEMOJI_VERSION = '2';
const TWEMOJI_URL_CACHE_LIMIT = 8192;

const TWEMOJI_URL_CACHE = new Map<string, string>();

let normalizedEndpointSource = '';
let normalizedEndpoint = '';

function getStaticOrigin(): string {
	const endpoint = RuntimeConfig.staticCdnEndpoint;
	if (endpoint !== normalizedEndpointSource) {
		normalizedEndpointSource = endpoint;
		normalizedEndpoint = endpoint.replace(/\/+$/, '');
	}
	return normalizedEndpoint;
}

export function fromHexCodePoint(hex: string): string {
	return String.fromCodePoint(Number.parseInt(hex, 16));
}

export function getTwemojiURL(codePoints: string): string | null {
	if (MODE === 'test' || !codePoints) {
		return null;
	}
	const origin = getStaticOrigin();
	const key = `${origin}:${codePoints}`;
	const cached = TWEMOJI_URL_CACHE.get(key);
	if (cached !== undefined) {
		return cached;
	}
	const url = `${origin}/emoji/${codePoints}.svg?v=${TWEMOJI_VERSION}`;
	if (TWEMOJI_URL_CACHE.size >= TWEMOJI_URL_CACHE_LIMIT) {
		const oldest = TWEMOJI_URL_CACHE.keys().next();
		if (!oldest.done) {
			TWEMOJI_URL_CACHE.delete(oldest.value);
		}
	}
	TWEMOJI_URL_CACHE.set(key, url);
	return url;
}

export function getEmojiURL(unicode: string): string | null {
	return getTwemojiURL(convertToCodePoints(unicode));
}
