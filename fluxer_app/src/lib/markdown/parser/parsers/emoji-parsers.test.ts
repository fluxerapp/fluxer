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

import {describe, expect, test} from 'vitest';
import {Parser} from '../parser/parser';
import {EmojiKind, NodeType, ParserFlags} from '../types/enums';

describe('Fluxer Markdown Parser', () => {
	test('standard emoji', () => {
		const input = 'Hello 🦶 World!';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'Hello '},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '🦶',
					codepoints: '1f9b6',
					name: expect.any(String),
				},
			},
			{type: NodeType.Text, content: ' World!'},
		]);
	});

	test('custom emoji static', () => {
		const input = 'Check this <:mmLol:216154654256398347> emoji!';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'Check this '},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Custom,
					name: 'mmLol',
					id: '216154654256398347',
					animated: false,
				},
			},
			{type: NodeType.Text, content: ' emoji!'},
		]);
	});

	test('custom emoji animated', () => {
		const input = 'Animated: <a:b1nzy:392938283556143104>';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'Animated: '},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Custom,
					name: 'b1nzy',
					id: '392938283556143104',
					animated: true,
				},
			},
		]);
	});

	test('generate codepoints with vs16 and zwj', () => {
		const input = '👨‍👩‍👧‍👦❤️😊';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '👨‍👩‍👧‍👦',
					codepoints: '1f468-200d-1f469-200d-1f467-200d-1f466',
					name: 'family_mwgb',
				},
			},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '❤️',
					codepoints: '2764',
					name: 'heart',
				},
			},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '😊',
					codepoints: '1f60a',
					name: 'blush',
				},
			},
		]);
	});

	test('multiple consecutive emojis', () => {
		const input = '😀😃😄😁';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '😀',
					codepoints: '1f600',
					name: expect.any(String),
				},
			},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '😃',
					codepoints: '1f603',
					name: expect.any(String),
				},
			},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '😄',
					codepoints: '1f604',
					name: expect.any(String),
				},
			},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '😁',
					codepoints: '1f601',
					name: expect.any(String),
				},
			},
		]);
	});

	test('special plaintext symbols should be rendered as text', () => {
		const input = '™ ™️ © ©️ ® ®️';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '™ ™ © © ® ®'}]);
	});

	test('copyright shortcode converts to text symbol', () => {
		const input = ':copyright: normal text';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '© normal text'}]);
	});

	test('trademark shortcode converts to text symbol', () => {
		const input = ':tm: normal text';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '™ normal text'}]);
	});

	test('registered shortcode converts to text symbol', () => {
		const input = ':registered: normal text';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '® normal text'}]);
	});

	test('mixed shortcodes with regular emojis', () => {
		const input = ':copyright: and :smile: with :registered:';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: '© and '},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '😄',
					codepoints: '1f604',
					name: 'smile',
				},
			},
			{type: NodeType.Text, content: ' with ®'},
		]);
	});

	test('shortcodes in formatted text', () => {
		const input = '**Bold :tm:** *Italic :copyright:* __:registered:__';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Strong,
				children: [{type: NodeType.Text, content: 'Bold ™'}],
			},
			{type: NodeType.Text, content: ' '},
			{
				type: NodeType.Emphasis,
				children: [{type: NodeType.Text, content: 'Italic ©'}],
			},
			{type: NodeType.Text, content: ' '},
			{
				type: NodeType.Underline,
				children: [{type: NodeType.Text, content: '®'}],
			},
		]);
	});

	test('emoji data loaded', () => {
		const input = ':smile: :wave: :heart:';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast.length).toBeGreaterThan(0);
		expect(ast.some((node) => node.type === NodeType.Emoji)).toBe(true);
	});

	test('emoji cache initialization', () => {
		const input = ':smile: :face_holding_back_tears: :face-holding-back-tears:';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '😄',
					codepoints: '1f604',
					name: 'smile',
				},
			},
			{
				type: NodeType.Text,
				content: ' ',
			},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '🥹',
					codepoints: '1f979',
					name: 'face_holding_back_tears',
				},
			},
			{
				type: NodeType.Text,
				content: ' :face-holding-back-tears:',
			},
		]);
	});

	test('case sensitive emoji lookup', () => {
		const validVariants = [':smile:', ':face_holding_back_tears:'];

		for (const emoji of validVariants) {
			const parser = new Parser(emoji, 0);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Emoji,
					kind: {
						kind: EmojiKind.Standard,
						raw: expect.any(String),
						codepoints: expect.any(String),
						name: expect.any(String),
					},
				},
			]);
		}
	});

	test('invalid case emoji lookup', () => {
		const invalidVariants = [
			':SMILE:',
			':Smile:',
			':FACE_HOLDING_BACK_TEARS:',
			':Face_Holding_Back_Tears:',
			':face-holding-back-tears:',
		];

		for (const emoji of invalidVariants) {
			const parser = new Parser(emoji, 0);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: emoji}]);
		}
	});

	test('separator variants', () => {
		const input = ':face_holding_back_tears: :face-holding-back-tears:';
		const parser = new Parser(input, 0);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '🥹',
					codepoints: '1f979',
					name: 'face_holding_back_tears',
				},
			},
			{
				type: NodeType.Text,
				content: ' :face-holding-back-tears:',
			},
		]);
	});

	test('basic emoji shortcode', () => {
		const input = 'Hello :face_holding_back_tears: world!';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'Hello '},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: expect.any(String),
					codepoints: expect.any(String),
					name: 'face_holding_back_tears',
				},
			},
			{type: NodeType.Text, content: ' world!'},
		]);
	});

	test('emoji shortcode in code', () => {
		const input = "`print(':face_holding_back_tears:')`";
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.InlineCode, content: "print(':face_holding_back_tears:')"}]);
	});

	test('emoji shortcode in code block', () => {
		const input = '```\n:face_holding_back_tears:\n```';
		const flags = ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.CodeBlock,
				language: undefined,
				content: ':face_holding_back_tears:\n',
			},
		]);
	});

	test('distinguishes between plaintext and emoji versions with variation selectors', () => {
		const inputs = [
			{text: '↩', shouldBeEmoji: false},
			{text: '↩️', shouldBeEmoji: true},
			{text: '↪', shouldBeEmoji: false},
			{text: '↪️', shouldBeEmoji: true},
			{text: '⤴', shouldBeEmoji: false},
			{text: '⤴️', shouldBeEmoji: true},
		];

		for (const {text, shouldBeEmoji} of inputs) {
			const parser = new Parser(text, 0);
			const {nodes: ast} = parser.parse();

			if (shouldBeEmoji) {
				expect(ast[0].type).toBe(NodeType.Emoji);
				expect((ast[0] as any).kind.kind).toBe(EmojiKind.Standard);
				expect((ast[0] as any).kind.name).not.toBe('');
			} else {
				expect(ast[0].type).toBe(NodeType.Text);
				expect((ast[0] as any).content).toBe(text);
			}
		}
	});

	test('renders mixed text with both plaintext and emoji versions', () => {
		const input = '↩ is plaintext, ↩️ is emoji';
		const parser = new Parser(input, 0);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: '↩ is plaintext, '},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '↩️',
					codepoints: '21a9',
					name: 'leftwards_arrow_with_hook',
				},
			},
			{type: NodeType.Text, content: ' is emoji'},
		]);
	});

	test('correctly parses dingbat emojis', () => {
		const inputs = [
			{emoji: '✅', name: 'white_check_mark', codepoint: '2705'},
			{emoji: '❌', name: 'x', codepoint: '274c'},
		];

		for (const {emoji, name, codepoint} of inputs) {
			const parser = new Parser(emoji, 0);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Emoji,
					kind: {
						kind: EmojiKind.Standard,
						raw: emoji,
						codepoints: codepoint,
						name,
					},
				},
			]);
		}
	});

	test('dingbat emojis in text context', () => {
		const input = 'Task complete ✅ but error ❌';
		const parser = new Parser(input, 0);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'Task complete '},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '✅',
					codepoints: '2705',
					name: 'white_check_mark',
				},
			},
			{type: NodeType.Text, content: ' but error '},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '❌',
					codepoints: '274c',
					name: 'x',
				},
			},
		]);
	});

	test('malformed custom emoji edge cases', () => {
		const malformedCases = [
			'<:ab>',
			'<:abc>',
			'<:name:>',
			'<:name:abc>',
			'<:name:123abc>',
			'<:name:12ab34>',
			'<::123>',
		];

		for (const input of malformedCases) {
			const parser = new Parser(input, 0);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Text,
					content: input,
				},
			]);
		}
	});

	test('empty custom emoji cases', () => {
		const emptyCases = ['<::>', '<:name:>', '<::123>'];

		for (const input of emptyCases) {
			const parser = new Parser(input, 0);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Text,
					content: input,
				},
			]);
		}
	});

	test('custom emoji with invalid ID characters', () => {
		const invalidIdCases = ['<:test:123a>', '<:name:12b34>', '<:emoji:abc123>', '<:custom:123-456>', '<:sample:12_34>'];

		for (const input of invalidIdCases) {
			const parser = new Parser(input, 0);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Text,
					content: input,
				},
			]);
		}
	});

	test('custom emoji with edge case names and IDs', () => {
		const edgeCases = ['<::123>', '<: :123>'];

		for (const input of edgeCases) {
			const parser = new Parser(input, 0);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Text,
					content: input,
				},
			]);
		}
	});
});

describe('Special Symbols Plaintext Rendering', () => {
	test('trademark symbol should render as text without variation selector', () => {
		const inputs = ['™', '™️'];

		for (const input of inputs) {
			const parser = new Parser(input, 0);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '™'}]);
		}
	});

	test('copyright symbol should render as text without variation selector', () => {
		const inputs = ['©', '©️'];

		for (const input of inputs) {
			const parser = new Parser(input, 0);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '©'}]);
		}
	});

	test('registered symbol should render as text without variation selector', () => {
		const inputs = ['®', '®️'];

		for (const input of inputs) {
			const parser = new Parser(input, 0);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '®'}]);
		}
	});

	test('mixed emoji and special symbols', () => {
		const input = '™️ ©️ ®️ 👍 ❤️';
		const parser = new Parser(input, 0);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: '™ © ® '},
			{
				type: NodeType.Emoji,
				kind: {
					kind: 'Standard',
					raw: '👍',
					codepoints: '1f44d',
					name: 'thumbsup',
				},
			},
			{type: NodeType.Text, content: ' '},
			{
				type: NodeType.Emoji,
				kind: {
					kind: 'Standard',
					raw: '❤️',
					codepoints: '2764',
					name: 'heart',
				},
			},
		]);
	});

	test('special symbols in formatted text', () => {
		const input = '**™️** *©️* __®️__';
		const parser = new Parser(input, 0);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Strong,
				children: [{type: NodeType.Text, content: '™'}],
			},
			{type: NodeType.Text, content: ' '},
			{
				type: NodeType.Emphasis,
				children: [{type: NodeType.Text, content: '©'}],
			},
			{type: NodeType.Text, content: ' '},
			{
				type: NodeType.Underline,
				children: [{type: NodeType.Text, content: '®'}],
			},
		]);
	});

	test('special symbols interspersed with text', () => {
		const input = 'This product™️ is copyright©️ and registered®️';
		const parser = new Parser(input, 0);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: 'This product™ is copyright© and registered®'}]);
	});
});
