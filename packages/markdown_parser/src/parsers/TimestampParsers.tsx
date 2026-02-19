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

import {NodeType, TimestampStyle} from '@fluxer/markdown_parser/src/types/Enums';
import type {ParserResult} from '@fluxer/markdown_parser/src/types/Nodes';

const LESS_THAN = 60;
const LETTER_T = 116;
const COLON = 58;

export function parseTimestamp(text: string): ParserResult | null {
	if (
		text.length < 4 ||
		text.charCodeAt(0) !== LESS_THAN ||
		text.charCodeAt(1) !== LETTER_T ||
		text.charCodeAt(2) !== COLON
	) {
		return null;
	}

	const end = text.indexOf('>');
	if (end === -1) {
		return null;
	}

	const inner = text.slice(3, end);

	const allParts = inner.split(':');
	if (allParts.length > 2) {
		return null;
	}

	const [timestampPart, stylePart] = allParts;

	if (!/^\d+$/.test(timestampPart)) {
		return null;
	}

	const timestamp = Number(timestampPart);

	if (timestamp === 0) {
		return null;
	}

	const timestampMillis = timestamp * 1000;
	if (!Number.isFinite(timestampMillis)) {
		return null;
	}

	if (Number.isNaN(new Date(timestampMillis).getTime())) {
		return null;
	}

	let style: TimestampStyle;
	if (stylePart !== undefined) {
		if (stylePart === '') {
			return null;
		}

		const styleChar = stylePart[0];

		const parsedStyle = getTimestampStyle(styleChar);

		if (!parsedStyle) {
			return null;
		}

		style = parsedStyle;
	} else {
		style = TimestampStyle.ShortDateTime;
	}

	return {
		node: {
			type: NodeType.Timestamp,
			timestamp,
			style,
		},
		advance: end + 1,
	};
}

function getTimestampStyle(char: string): TimestampStyle | null {
	switch (char) {
		case 't':
			return TimestampStyle.ShortTime;
		case 'T':
			return TimestampStyle.LongTime;
		case 'd':
			return TimestampStyle.ShortDate;
		case 'D':
			return TimestampStyle.LongDate;
		case 'f':
			return TimestampStyle.ShortDateTime;
		case 'F':
			return TimestampStyle.LongDateTime;
		case 's':
			return TimestampStyle.ShortDateShortTime;
		case 'S':
			return TimestampStyle.ShortDateMediumTime;
		case 'R':
			return TimestampStyle.RelativeTime;
		default:
			return null;
	}
}
