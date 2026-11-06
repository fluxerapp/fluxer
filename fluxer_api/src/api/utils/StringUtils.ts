// SPDX-License-Identifier: AGPL-3.0-or-later

import {URLType} from '@fluxer/schema/src/primitives/UrlValidators';
import {decode} from 'html-entities';

const graphemeSegmenter = new Intl.Segmenter('en', {granularity: 'grapheme'});

export function parseString(value: string, maxLength: number) {
	const decoded = decode(value).trim();
	const keep = Math.max(0, maxLength - 3);
	let cut = 0;
	let count = 0;
	for (const {index} of graphemeSegmenter.segment(decoded)) {
		if (count === keep) cut = index;
		count++;
		if (count > maxLength) return `${decoded.slice(0, cut)}...`;
	}
	return decoded;
}

export function safeUrl(value: unknown): string | undefined {
	if (typeof value !== 'string' || value.length === 0) return undefined;
	if (!value.startsWith('http://') && !value.startsWith('https://')) return undefined;
	const parsed = URLType.safeParse(value);
	return parsed.success ? parsed.data : undefined;
}
