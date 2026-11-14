// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import emojiRegex from 'emoji-regex';
import {z} from 'zod';

const EMOJI_REGEX = emojiRegex();
const REGIONAL_INDICATOR_START = 0x1f1e6;
const REGIONAL_INDICATOR_END = 0x1f1ff;

function isSingleRegionalIndicator(value: string): boolean {
	const codePoints = [...value];
	if (codePoints.length !== 1) {
		return false;
	}
	const codePoint = codePoints[0].codePointAt(0);
	return codePoint !== undefined && codePoint >= REGIONAL_INDICATOR_START && codePoint <= REGIONAL_INDICATOR_END;
}

export function isValidSingleUnicodeEmoji(value: string): boolean {
	if (!value || value.length === 0) {
		return false;
	}
	EMOJI_REGEX.lastIndex = 0;
	const match = EMOJI_REGEX.exec(value);
	if (match && match.index === 0 && match[0] === value) {
		return true;
	}
	return isSingleRegionalIndicator(value);
}

export const UnicodeRoleEmojiType = z
	.string()
	.max(64, ValidationErrorCodes.NOT_A_VALID_UNICODE_EMOJI)
	.refine(isValidSingleUnicodeEmoji, ValidationErrorCodes.NOT_A_VALID_UNICODE_EMOJI);
