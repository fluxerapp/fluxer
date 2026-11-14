// SPDX-License-Identifier: AGPL-3.0-or-later

const NON_ASCII_JSON_CHAR = /[\u007f-\uffff]/g;

function stringifyArchiveJson(value: unknown, space: string | number = 2): string {
	const json = JSON.stringify(value, null, space);
	if (json === undefined) {
		throw new Error('Archive JSON value is not serializable');
	}
	return json.replace(NON_ASCII_JSON_CHAR, (char) => `\\u${char.charCodeAt(0).toString(16).padStart(4, '0')}`);
}

export function createArchiveJsonBuffer(value: unknown, space: string | number = 2): Buffer {
	return Buffer.from(stringifyArchiveJson(value, space), 'utf8');
}
