// SPDX-License-Identifier: AGPL-3.0-or-later

import {readFileSync} from 'node:fs';
import {fileURLToPath} from 'node:url';
import {describe, expect, it} from 'vitest';

const MESSAGE_CSS = readFileSync(fileURLToPath(new URL('./Message.module.css', import.meta.url)), 'utf8');

const ISOLATED_NAMES = ['.messageUsername', '.repliedUsername'];
const ISOLATED_TIMESTAMPS = [
	'.messageTimestamp',
	'.messageTimestampCompact',
	'.messageTimestampHover',
	'.messageTimestampCompactHover',
];

function rules(css: string): Array<{selector: string; declarations: string}> {
	const source = css.replace(/\/\*[\s\S]*?\*\//g, '');
	const out: Array<{selector: string; declarations: string}> = [];
	const chain: Array<string> = [];
	let buffer = '';
	for (const char of source) {
		if (char === '{') {
			chain.push(buffer.split(/[;}]/).pop()?.trim().replace(/\s+/g, ' ') ?? '');
			buffer = '';
		} else if (char === '}') {
			if (chain.length > 0) {
				out.push({selector: chain.join(' '), declarations: buffer});
				chain.pop();
			}
			buffer = '';
		} else {
			buffer += char;
		}
	}
	return out;
}

function declaredBidi(className: string): Array<string> {
	const selectorPattern = new RegExp(`\\${className}(?![\\w-])`);
	const out: Array<string> = [];
	for (const rule of rules(MESSAGE_CSS)) {
		if (!rule.selector.split(',').some((selector) => selectorPattern.test(selector))) continue;
		const match = rule.declarations.match(/unicode-bidi\s*:\s*([^;]+)/);
		if (match) out.push(match[1].trim());
	}
	return out;
}

describe('message bidi isolation', () => {
	it.each(ISOLATED_NAMES)('resolves %s in its own bidi paragraph', (className) => {
		expect(declaredBidi(className)).toContain('plaintext');
	});

	it.each(ISOLATED_TIMESTAMPS)('keeps %s out of a neighbouring right-to-left run', (className) => {
		expect(declaredBidi(className)).toContain('isolate');
	});
});
