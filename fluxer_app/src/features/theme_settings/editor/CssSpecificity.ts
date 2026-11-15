// SPDX-License-Identifier: AGPL-3.0-or-later

export type Specificity = [number, number, number];

const LEGACY_PSEUDO_ELEMENTS = new Set(['before', 'after', 'first-line', 'first-letter']);
const MAX_OF_ARGUMENTS = new Set(['is', 'not', 'has', 'matches', '-webkit-any', '-moz-any']);
const NTH_WITH_SELECTOR = new Set(['nth-child', 'nth-last-child']);

function add(left: Specificity, right: Specificity): Specificity {
	return [left[0] + right[0], left[1] + right[1], left[2] + right[2]];
}

function compare(left: Specificity, right: Specificity): number {
	return left[0] - right[0] || left[1] - right[1] || left[2] - right[2];
}

export function splitSelectorList(selector: string): Array<string> {
	const parts: Array<string> = [];
	let depth = 0;
	let quote: string | null = null;
	let start = 0;
	for (let index = 0; index < selector.length; index += 1) {
		const char = selector[index];
		if (quote) {
			if (char === '\\') index += 1;
			else if (char === quote) quote = null;
			continue;
		}
		if (char === '"' || char === "'") quote = char;
		else if (char === '(' || char === '[') depth += 1;
		else if (char === ')' || char === ']') depth -= 1;
		else if (char === ',' && depth === 0) {
			parts.push(selector.slice(start, index).trim());
			start = index + 1;
		}
	}
	parts.push(selector.slice(start).trim());
	return parts.filter(Boolean);
}

function readBalanced(selector: string, open: number): number {
	let depth = 0;
	let quote: string | null = null;
	for (let index = open; index < selector.length; index += 1) {
		const char = selector[index];
		if (quote) {
			if (char === '\\') index += 1;
			else if (char === quote) quote = null;
			continue;
		}
		if (char === '"' || char === "'") quote = char;
		else if (char === '(') depth += 1;
		else if (char === ')') {
			depth -= 1;
			if (depth === 0) return index;
		}
	}
	return selector.length - 1;
}

function readName(selector: string, start: number): number {
	let index = start;
	while (index < selector.length && /[\w\-\\ -￿]/u.test(selector[index])) {
		index += selector[index] === '\\' ? 2 : 1;
	}
	return index;
}

function maxOfList(list: string): Specificity {
	let best: Specificity = [0, 0, 0];
	for (const part of splitSelectorList(list)) {
		const value = complexSpecificity(part);
		if (compare(value, best) > 0) best = value;
	}
	return best;
}

function complexSpecificity(selector: string): Specificity {
	let total: Specificity = [0, 0, 0];
	let index = 0;
	while (index < selector.length) {
		const char = selector[index];
		if (char === '#') {
			total = add(total, [1, 0, 0]);
			index = readName(selector, index + 1);
		} else if (char === '.') {
			total = add(total, [0, 1, 0]);
			index = readName(selector, index + 1);
		} else if (char === '[') {
			total = add(total, [0, 1, 0]);
			let depth = 0;
			let quote: string | null = null;
			for (; index < selector.length; index += 1) {
				const inner = selector[index];
				if (quote) {
					if (inner === '\\') index += 1;
					else if (inner === quote) quote = null;
					continue;
				}
				if (inner === '"' || inner === "'") quote = inner;
				else if (inner === '[') depth += 1;
				else if (inner === ']') {
					depth -= 1;
					if (depth === 0) break;
				}
			}
			index += 1;
		} else if (char === ':') {
			const isElement = selector[index + 1] === ':';
			const nameStart = index + (isElement ? 2 : 1);
			const nameEnd = readName(selector, nameStart);
			const name = selector.slice(nameStart, nameEnd).toLowerCase();
			let argument: string | null = null;
			index = nameEnd;
			if (selector[index] === '(') {
				const close = readBalanced(selector, index);
				argument = selector.slice(index + 1, close);
				index = close + 1;
			}
			if (isElement || LEGACY_PSEUDO_ELEMENTS.has(name)) {
				total = add(total, [0, 0, 1]);
			} else if (name === 'where') {
			} else if (MAX_OF_ARGUMENTS.has(name) && argument !== null) {
				total = add(total, maxOfList(argument));
			} else if (NTH_WITH_SELECTOR.has(name) && argument !== null && /\sof\s/u.test(argument)) {
				total = add(add(total, [0, 1, 0]), maxOfList(argument.slice(argument.search(/\sof\s/u) + 4)));
			} else {
				total = add(total, [0, 1, 0]);
			}
		} else if (char === '*' || char === '&') {
			index += 1;
		} else if (/[\w\-\\ -￿]/u.test(char)) {
			total = add(total, [0, 0, 1]);
			index = readName(selector, index);
		} else {
			index += 1;
		}
	}
	return total;
}

export function selectorSpecificity(selector: string): Specificity {
	return maxOfList(selector);
}
