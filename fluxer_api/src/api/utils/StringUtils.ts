// SPDX-License-Identifier: AGPL-3.0-or-later

import {URLType} from '@fluxer/schema/src/primitives/UrlValidators';
import {decode} from 'html-entities';

const graphemeSegmenter = new Intl.Segmenter('en', {granularity: 'grapheme'});

export function parseString(value: string, maxLength: number) {
	const decoded = decode(value).trim();
	const graphemes = Array.from(graphemeSegmenter.segment(decoded), ({segment}) => segment);
	return graphemes.length > maxLength ? `${graphemes.slice(0, Math.max(0, maxLength - 3)).join('')}...` : decoded;
}

export function safeUrl(value: unknown): string | undefined {
	if (typeof value !== 'string' || value.length === 0) return undefined;
	if (!value.startsWith('http://') && !value.startsWith('https://')) return undefined;
	const parsed = URLType.safeParse(value);
	return parsed.success ? parsed.data : undefined;
}
