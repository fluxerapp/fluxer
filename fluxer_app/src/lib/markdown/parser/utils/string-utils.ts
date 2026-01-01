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

import {APP_PROTOCOL_PREFIX} from '~/utils/appProtocol';

const HTTP_PREFIX = 'http://';
const HTTPS_PREFIX = 'https://';

const WORD_CHARS = new Set('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_');

const ESCAPABLE_CHARS = new Set('[]()\\*_~`@!#$%^&+={}|:;"\'<>,.?/');

const URL_TERMINATION_CHARS = new Set(' \t\n\r)\'"');

function isWordCharacter(char: string): boolean {
	return char.length === 1 && WORD_CHARS.has(char);
}

export function isEscapableCharacter(char: string): boolean {
	return char.length === 1 && ESCAPABLE_CHARS.has(char);
}

export function isUrlTerminationChar(char: string): boolean {
	return char.length === 1 && URL_TERMINATION_CHARS.has(char);
}

export function isWordUnderscore(chars: Array<string>, pos: number): boolean {
	if (chars[pos] !== '_') return false;

	const prevChar = pos > 0 ? chars[pos - 1] : '';
	const nextChar = pos + 1 < chars.length ? chars[pos + 1] : '';

	return isWordCharacter(prevChar) && isWordCharacter(nextChar);
}

export function isAlphaNumeric(charCode: number): boolean {
	return (
		(charCode >= 48 && charCode <= 57) || (charCode >= 65 && charCode <= 90) || (charCode >= 97 && charCode <= 122)
	);
}

export function isAlphaNumericChar(char: string): boolean {
	return char.length === 1 && isAlphaNumeric(char.charCodeAt(0));
}

export function startsWithUrl(text: string): boolean {
	if (text.length < 8) return false;

	if (text.startsWith(HTTP_PREFIX)) {
		const prefixEnd = 7;
		return !text.substring(0, prefixEnd).includes('"') && !text.substring(0, prefixEnd).includes("'");
	}

	if (text.startsWith(HTTPS_PREFIX)) {
		const prefixEnd = 8;
		return !text.substring(0, prefixEnd).includes('"') && !text.substring(0, prefixEnd).includes("'");
	}

	if (text.startsWith(APP_PROTOCOL_PREFIX)) {
		const prefixEnd = APP_PROTOCOL_PREFIX.length;
		return !text.substring(0, prefixEnd).includes('"') && !text.substring(0, prefixEnd).includes("'");
	}

	return false;
}

export function matchMarker(chars: Array<string>, pos: number, marker: string): boolean {
	if (pos + marker.length > chars.length) return false;

	if (marker.length === 1) {
		return chars[pos] === marker;
	}

	if (marker.length === 2) {
		return chars[pos] === marker[0] && chars[pos + 1] === marker[1];
	}

	for (let i = 0; i < marker.length; i++) {
		if (chars[pos + i] !== marker[i]) return false;
	}

	return true;
}
