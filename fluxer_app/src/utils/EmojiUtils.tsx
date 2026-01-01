/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {FC, SVGProps} from 'react';
import {MODE} from '~/lib/env';
import {Platform} from '~/lib/Platform';

export type TwemojiComponent = FC<SVGProps<SVGSVGElement>>;

const TWEMOJI_CDN = 'https://fluxerstatic.com/emoji';

export const shouldUseNativeEmoji = Platform.isAppleDevice;

export const convertToCodePoints = (emoji: string): string => {
	const containsZWJ = emoji.includes('\u200D');
	const processedEmoji = containsZWJ ? emoji : emoji.replace(/\uFE0F/g, '');
	return Array.from(processedEmoji)
		.map((char) => char.codePointAt(0)?.toString(16).replace(/^0+/, '') || '')
		.join('-');
};

export const fromHexCodePoint = (hex: string): string => String.fromCodePoint(Number.parseInt(hex, 16));

export const getTwemojiURL = (codePoints: string): string | null => {
	if (shouldUseNativeEmoji || MODE === 'test' || !codePoints) {
		return null;
	}

	return `${TWEMOJI_CDN}/${codePoints}.svg`;
};

export const getEmojiURL = (unicode: string): string | null => getTwemojiURL(convertToCodePoints(unicode));

export const getTwemojiSvg = (_codePoints: string): TwemojiComponent | null => null;
export const getEmojiSvg = (_unicode: string): TwemojiComponent | null => null;
