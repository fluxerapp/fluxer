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

import {clearTestEmojiProvider, setupTestEmojiProvider} from '@fluxer/markdown_parser/src/__tests__/TestEmojiSetup';
import {Parser} from '@fluxer/markdown_parser/src/parser/Parser';
import {EmojiKind, NodeType, ParserFlags, TimestampStyle} from '@fluxer/markdown_parser/src/types/Enums';
import {afterAll, beforeAll, describe, expect, test} from 'vitest';

beforeAll(() => {
	setupTestEmojiProvider();
});

afterAll(() => {
	clearTestEmojiProvider();
});

describe('Fluxer Markdown Parser', () => {
	test('timestamp default', () => {
		const input = 'Current time: <t:1618953630>';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'Current time: '},
			{
				type: NodeType.Timestamp,
				timestamp: 1618953630,
				style: TimestampStyle.ShortDateTime,
			},
		]);
	});

	test('timestamp with style', () => {
		const input = '<t:1618953630:d> is the date.';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Timestamp,
				timestamp: 1618953630,
				style: TimestampStyle.ShortDate,
			},
			{type: NodeType.Text, content: ' is the date.'},
		]);
	});

	test('timestamp with short date & short time style', () => {
		const input = '<t:1618953630:s>';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Timestamp,
				timestamp: 1618953630,
				style: TimestampStyle.ShortDateShortTime,
			},
		]);
	});

	test('timestamp with short date & medium time style', () => {
		const input = '<t:1618953630:S>';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Timestamp,
				timestamp: 1618953630,
				style: TimestampStyle.ShortDateMediumTime,
			},
		]);
	});

	test('timestamp invalid style', () => {
		const input = 'Check this <t:1618953630:z> time.';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: 'Check this <t:1618953630:z> time.'}]);
	});

	test('timestamp non numeric', () => {
		const input = 'Check <t:abc123> time.';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: 'Check <t:abc123> time.'}]);
	});

	test('timestamp with milliseconds should not parse', () => {
		const input = 'Time: <t:1618953630.123>';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: 'Time: <t:1618953630.123>'}]);
	});

	test('timestamp with partial milliseconds should not parse', () => {
		const input = '<t:1618953630.1>';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '<t:1618953630.1>'}]);
	});

	test('timestamp with excess millisecond precision should not parse', () => {
		const input = '<t:1618953630.123456>';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '<t:1618953630.123456>'}]);
	});

	test('timestamp with milliseconds and style should not parse', () => {
		const input = '<t:1618953630.123:R>';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '<t:1618953630.123:R>'}]);
	});

	test('timestamp in mixed content', () => {
		const input = 'Hello <a:wave:12345> 🦶 <t:1618953630:d> <:smile:9876>';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'Hello '},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Custom,
					name: 'wave',
					id: '12345',
					animated: true,
				},
			},
			{type: NodeType.Text, content: ' '},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Standard,
					raw: '🦶',
					codepoints: '1f9b6',
					name: expect.any(String),
				},
			},
			{type: NodeType.Text, content: ' '},
			{
				type: NodeType.Timestamp,
				timestamp: 1618953630,
				style: TimestampStyle.ShortDate,
			},
			{type: NodeType.Text, content: ' '},
			{
				type: NodeType.Emoji,
				kind: {
					kind: EmojiKind.Custom,
					name: 'smile',
					id: '9876',
					animated: false,
				},
			},
		]);
	});

	test('timestamp edge cases', () => {
		const inputs = ['<t:>', '<t:1618953630:>', '<t::d>', '<t:1618953630::d>', '<t:1618953630:d:R>'];

		for (const input of inputs) {
			const parser = new Parser(input, 0);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: input}]);
		}
	});

	test('very large valid timestamp', () => {
		const input = '<t:9999999999>';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Timestamp,
				timestamp: 9999999999,
				style: TimestampStyle.ShortDateTime,
			},
		]);
	});

	test('timestamp at max js date boundary parses', () => {
		const input = '<t:8640000000000>';
		const parser = new Parser(input, 0);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Timestamp,
				timestamp: 8640000000000,
				style: TimestampStyle.ShortDateTime,
			},
		]);
	});

	test('timestamp above max js date boundary does not parse', () => {
		const input = '<t:8640000000001>';
		const parser = new Parser(input, 0);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: input}]);
	});

	test('timestamp that overflows to infinity does not parse', () => {
		const largeDigits = '9'.repeat(400);
		const input = `<t:${largeDigits}>`;
		const parser = new Parser(input, 0);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: input}]);
	});

	test('timestamp with leading zeros', () => {
		const input = '<t:0001618953630>';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Timestamp,
				timestamp: 1618953630,
				style: TimestampStyle.ShortDateTime,
			},
		]);
	});

	test('timestamp in code block should not be parsed', () => {
		const input = '```\n<t:1618953630>\n```';
		const flags = ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.CodeBlock,
				language: undefined,
				content: '<t:1618953630>\n',
			},
		]);
	});

	test('timestamp in inline code should not be parsed', () => {
		const input = '`<t:1618953630>`';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.InlineCode,
				content: '<t:1618953630>',
			},
		]);
	});

	test('timestamp with non-digit characters should not parse', () => {
		const inputs = [
			'<t:12a34>',
			'<t:12.34>',
			'<t:12,34>',
			'<t:1234e5>',
			'<t:+1234>',
			'<t:-1234>',
			'<t:1234 >',
			'<t: 1234>',
			'<t:1_234>',
			'<t:0x1234>',
			'<t:0b1010>',
			'<t:123.456.789>',
		];

		for (const input of inputs) {
			const parser = new Parser(input, 0);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: input}]);
		}
	});

	test('timestamp with valid integer formats', () => {
		const inputs = ['<t:1234>', '<t:01234>', '<t:9999999999>'];

		for (const input of inputs) {
			const parser = new Parser(input, 0);
			const {nodes: ast} = parser.parse();

			expect(ast[0].type).toBe(NodeType.Timestamp);
		}
	});

	test('timestamp with zero value should not parse', () => {
		const parser = new Parser('<t:0>', 0);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '<t:0>'}]);
	});
});
