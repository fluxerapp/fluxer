// SPDX-License-Identifier: AGPL-3.0-or-later

import {initializeEmojiParser} from '@app/features/messaging/utils/markdown/EmojiProviderSetup';
import {ParserFlags} from '@app/features/messaging/utils/markdown/parser/Enums';
import {parseMarkdownAstWithWasm} from '@app/features/messaging/utils/markdown/parser/MarkdownParserWasm';
import {beforeAll, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/expressions/utils/EmojiUtils', () => ({
	getEmojiURL: () => null,
}));

vi.mock('@lingui/core/macro', () => ({
	msg: (descriptor: unknown) => descriptor,
}));

describe('markdown parser wasm blockquotes', () => {
	it('preserves consecutive empty blockquote lines', () => {
		expect(parseMarkdownAstWithWasm('> \n>  \nsome text', ParserFlags.ALLOW_BLOCKQUOTES)).toEqual({
			nodes: [
				{
					type: 'Blockquote',
					children: [],
					blankLines: 2,
				},
				{
					type: 'Text',
					content: 'some text',
				},
			],
		});
	});

	it('keeps a bare greater-than line as text', () => {
		expect(parseMarkdownAstWithWasm('> \n>', ParserFlags.ALLOW_BLOCKQUOTES)).toEqual({
			nodes: [
				{
					type: 'Blockquote',
					children: [],
					blankLines: 1,
				},
				{
					type: 'Text',
					content: '>',
				},
			],
		});
	});
});

describe('markdown parser wasm headings', () => {
	it('preserves presentation-only heading lines as block headings', () => {
		expect(parseMarkdownAstWithWasm('# ‎ \n# ‎', ParserFlags.ALLOW_HEADINGS)).toEqual({
			nodes: [
				{
					type: 'Heading',
					level: 1,
					children: [{type: 'Text', content: '‎ '}],
				},
				{
					type: 'Heading',
					level: 1,
					children: [{type: 'Text', content: '‎'}],
				},
			],
		});
	});

	it('keeps ordinary whitespace-only heading syntax as text', () => {
		expect(parseMarkdownAstWithWasm('# ', ParserFlags.ALLOW_HEADINGS)).toEqual({
			nodes: [{type: 'Text', content: '# '}],
		});
	});
});

describe('markdown parser wasm default-presentation emoji', () => {
	beforeAll(() => {
		initializeEmojiParser();
	});

	it.each([
		['⌚', 'watch', '231a'],
		['⌛', 'hourglass', '231b'],
		['⏩', 'fast_forward', '23e9'],
		['⏪', 'rewind', '23ea'],
		['⏫', 'arrow_double_up', '23eb'],
		['⏬', 'arrow_double_down', '23ec'],
		['⏰', 'alarm_clock', '23f0'],
		['⏳', 'hourglass_flowing_sand', '23f3'],
	])('renders %s as an emoji without a variation selector', (raw, name, codepoints) => {
		expect(parseMarkdownAstWithWasm(raw, 0)).toEqual({
			nodes: [{type: 'Emoji', kind: {kind: 'Standard', raw, codepoints, name}}],
		});
	});

	it('keeps text-presentation symbols as plain text when typed bare', () => {
		expect(parseMarkdownAstWithWasm('❤', 0)).toEqual({
			nodes: [{type: 'Text', content: '❤'}],
		});
	});

	it('still renders text-presentation symbols carrying a variation selector', () => {
		expect(parseMarkdownAstWithWasm('❤️', 0)).toEqual({
			nodes: [{type: 'Emoji', kind: {kind: 'Standard', raw: '❤️', codepoints: '2764', name: 'heart'}}],
		});
	});
});
