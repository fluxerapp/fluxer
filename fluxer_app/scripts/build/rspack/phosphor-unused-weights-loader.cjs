// SPDX-License-Identifier: AGPL-3.0-or-later

const UNUSED_WEIGHTS = ['thin', 'light'];
const UNUSED_WEIGHT_ENTRY = /\[\s*"(?:thin|light)"\s*,/g;
const REQUIRED_WEIGHTS = ['"bold"', '"duotone"', '"fill"', '"regular"'];

class UnreadablePhosphorWeightMapError extends Error {
	constructor(resource, detail) {
		super(`${resource} does not match the expected @phosphor-icons weight map shape (${detail})`);
		this.name = 'UnreadablePhosphorWeightMapError';
	}
}

function findEntryEnd(source, start) {
	let depth = 0;
	let quote = null;
	for (let i = start; i < source.length; i++) {
		const char = source[i];
		if (quote != null) {
			if (char === '\\') {
				i++;
			} else if (char === quote) {
				quote = null;
			}
			continue;
		}
		if (char === '"' || char === "'" || char === '`') {
			quote = char;
			continue;
		}
		if (char === '[' || char === '(' || char === '{') {
			depth++;
			continue;
		}
		if (char === ']' || char === ')' || char === '}') {
			depth--;
			if (depth === 0) {
				return i + 1;
			}
		}
	}
	return -1;
}

module.exports = function phosphorUnusedWeightsLoader(source) {
	this.cacheable(true);
	for (const weight of REQUIRED_WEIGHTS) {
		if (!source.includes(weight)) {
			throw new UnreadablePhosphorWeightMapError(this.resourcePath, `missing ${weight}`);
		}
	}
	const spans = [];
	UNUSED_WEIGHT_ENTRY.lastIndex = 0;
	for (let match = UNUSED_WEIGHT_ENTRY.exec(source); match != null; match = UNUSED_WEIGHT_ENTRY.exec(source)) {
		const end = findEntryEnd(source, match.index);
		if (end < 0) {
			throw new UnreadablePhosphorWeightMapError(this.resourcePath, `unterminated entry at ${match.index}`);
		}
		spans.push([match.index, end]);
		UNUSED_WEIGHT_ENTRY.lastIndex = end;
	}
	if (spans.length !== UNUSED_WEIGHTS.length) {
		throw new UnreadablePhosphorWeightMapError(this.resourcePath, `found ${spans.length} unused weight entries`);
	}
	let out = '';
	let cursor = 0;
	for (const [start, end] of spans) {
		out += source.slice(cursor, start);
		cursor = end;
		while (cursor < source.length && /[\s,]/.test(source[cursor])) {
			cursor++;
		}
	}
	return out + source.slice(cursor);
};
