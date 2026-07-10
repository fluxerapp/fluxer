// SPDX-License-Identifier: AGPL-3.0-or-later

import {URLType} from '@fluxer/schema/src/primitives/UrlValidators';
import {decode} from 'html-entities';
import _ from 'lodash';

function isInvisibleContentCodePoint(codePoint: number): boolean {
	return (
		codePoint === 0x0000 ||
		codePoint === 0x00ad ||
		codePoint === 0x034f ||
		codePoint === 0x061c ||
		codePoint === 0x115f ||
		codePoint === 0x1160 ||
		codePoint === 0x17b4 ||
		codePoint === 0x17b5 ||
		codePoint === 0x180e ||
		(codePoint >= 0x200b && codePoint <= 0x200f) ||
		(codePoint >= 0x202a && codePoint <= 0x202e) ||
		(codePoint >= 0x2060 && codePoint <= 0x2069) ||
		codePoint === 0x2800 ||
		codePoint === 0x3164 ||
		(codePoint >= 0xfe00 && codePoint <= 0xfe0f) ||
		codePoint === 0xfeff ||
		codePoint === 0xffa0 ||
		(codePoint >= 0xe0100 && codePoint <= 0xe01ef)
	);
}

export function hasVisibleContent(value: string): boolean {
	for (const char of value) {
		if (char.trim().length === 0) {
			continue;
		}
		const codePoint = char.codePointAt(0);
		if (codePoint == null || isInvisibleContentCodePoint(codePoint)) {
			continue;
		}
		return true;
	}
	return false;
}

export function parseString(value: string, maxLength: number) {
	return _.truncate(decode(value).trim(), {length: maxLength});
}

export function safeUrl(value: unknown): string | undefined {
	if (typeof value !== 'string' || value.length === 0) return undefined;
	if (!value.startsWith('http://') && !value.startsWith('https://')) return undefined;
	const parsed = URLType.safeParse(value);
	return parsed.success ? parsed.data : undefined;
}
