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
import {NodeType, ParserFlags} from '../types/enums';
import type {TextNode} from '../types/nodes';

describe('Fluxer Markdown Parser', () => {
	test('inline nodes', () => {
		const input =
			'This is **strong**, *emphasis*, __underline__, ~~strikethrough~~, `inline code`, and a [link](https://example.com). Also, visit https://rust-lang.org.';
		const flags = ParserFlags.ALLOW_MASKED_LINKS | ParserFlags.ALLOW_AUTOLINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'This is '},
			{type: NodeType.Strong, children: [{type: NodeType.Text, content: 'strong'}]},
			{type: NodeType.Text, content: ', '},
			{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'emphasis'}]},
			{type: NodeType.Text, content: ', '},
			{type: NodeType.Underline, children: [{type: NodeType.Text, content: 'underline'}]},
			{type: NodeType.Text, content: ', '},
			{type: NodeType.Strikethrough, children: [{type: NodeType.Text, content: 'strikethrough'}]},
			{type: NodeType.Text, content: ', '},
			{type: NodeType.InlineCode, content: 'inline code'},
			{type: NodeType.Text, content: ', and a '},
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'link'},
				url: 'https://example.com/',
				escaped: false,
			},
			{type: NodeType.Text, content: '. Also, visit '},
			{
				type: NodeType.Link,
				text: undefined,
				url: 'https://rust-lang.org/',
				escaped: false,
			},
			{type: NodeType.Text, content: '.'},
		]);
	});

	test('incomplete formatting', () => {
		const input = '**incomplete strong *incomplete emphasis `incomplete code';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: '*'},
			{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'incomplete strong '}]},
			{type: NodeType.Text, content: 'incomplete emphasis `incomplete code'},
		]);
	});

	test('underscore emphasis', () => {
		const input = 'This is _emphasized_ and *also emphasized*';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'This is '},
			{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'emphasized'}]},
			{type: NodeType.Text, content: ' and '},
			{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'also emphasized'}]},
		]);
	});

	test('alternate delimiters', () => {
		const input = '__underscore *asterisk* mix__ and _single *with* under_';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Underline,
				children: [
					{type: NodeType.Text, content: 'underscore '},
					{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'asterisk'}]},
					{type: NodeType.Text, content: ' mix'},
				],
			},
			{type: NodeType.Text, content: ' and '},
			{
				type: NodeType.Emphasis,
				children: [{type: NodeType.Text, content: 'single with under'}],
			},
		]);
	});

	test('inline spoiler', () => {
		const input = 'This is a ||secret|| message';
		const flags = ParserFlags.ALLOW_SPOILERS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'This is a '},
			{type: NodeType.Spoiler, isBlock: false, children: [{type: NodeType.Text, content: 'secret'}]},
			{type: NodeType.Text, content: ' message'},
		]);
	});

	test('formatted spoiler', () => {
		const input = '||This is *emphasized* and **strong**||';
		const flags = ParserFlags.ALLOW_MASKED_LINKS | ParserFlags.ALLOW_SPOILERS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Spoiler,
				isBlock: false,
				children: [
					{type: NodeType.Text, content: 'This is '},
					{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'emphasized'}]},
					{type: NodeType.Text, content: ' and '},
					{type: NodeType.Strong, children: [{type: NodeType.Text, content: 'strong'}]},
				],
			},
		]);
	});

	test('adjacent spoilers', () => {
		const input = '||a|| ||b|| ||c||';
		const flags = ParserFlags.ALLOW_SPOILERS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Spoiler, isBlock: false, children: [{type: NodeType.Text, content: 'a'}]},
			{type: NodeType.Text, content: ' '},
			{type: NodeType.Spoiler, isBlock: false, children: [{type: NodeType.Text, content: 'b'}]},
			{type: NodeType.Text, content: ' '},
			{type: NodeType.Spoiler, isBlock: false, children: [{type: NodeType.Text, content: 'c'}]},
		]);
	});

	test('unclosed spoiler', () => {
		const input = '||This spoiler never ends';
		const flags = ParserFlags.ALLOW_SPOILERS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '||This spoiler never ends'}]);
	});

	test('consecutive pipes should create spoilers with pipe content', () => {
		const input = '|||||||||||||||';
		const flags = ParserFlags.ALLOW_SPOILERS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Spoiler, isBlock: false, children: [{type: NodeType.Text, content: '|'}]},
			{type: NodeType.Spoiler, isBlock: false, children: [{type: NodeType.Text, content: '|'}]},
			{type: NodeType.Spoiler, isBlock: false, children: [{type: NodeType.Text, content: '|'}]},
		]);

		const fivePipes = '|||||';
		const fiveParser = new Parser(fivePipes, ParserFlags.ALLOW_SPOILERS);
		const {nodes: fiveAst} = fiveParser.parse();
		expect(fiveAst).toEqual([
			{type: NodeType.Spoiler, isBlock: false, children: [{type: NodeType.Text, content: '|'}]},
		]);

		const sixPipes = '||||||';
		const sixParser = new Parser(sixPipes, ParserFlags.ALLOW_SPOILERS);
		const {nodes: sixAst} = sixParser.parse();
		expect(sixAst).toEqual([
			{type: NodeType.Spoiler, isBlock: false, children: [{type: NodeType.Text, content: '|'}]},
			{type: NodeType.Text, content: '|'},
		]);
	});

	test('bold italics', () => {
		const input = '***bolditalics***';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Emphasis,
				children: [
					{
						type: NodeType.Strong,
						children: [{type: NodeType.Text, content: 'bolditalics'}],
					},
				],
			},
		]);
	});

	test('complex nested formatting combinations', () => {
		const input =
			'***__bold italic underline__***\n**_bold and italic_**\n__***underline, bold, italic***__\n**_nested __underline inside italic__ text_**';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Emphasis,
				children: [
					{
						type: NodeType.Strong,
						children: [
							{
								type: NodeType.Underline,
								children: [{type: NodeType.Text, content: 'bold italic underline'}],
							},
						],
					},
				],
			},
			{type: NodeType.Text, content: '\n'},
			{
				type: NodeType.Strong,
				children: [
					{
						type: NodeType.Emphasis,
						children: [{type: NodeType.Text, content: 'bold and italic'}],
					},
				],
			},
			{type: NodeType.Text, content: '\n'},
			{
				type: NodeType.Underline,
				children: [
					{
						type: NodeType.Emphasis,
						children: [
							{
								type: NodeType.Strong,
								children: [{type: NodeType.Text, content: 'underline, bold, italic'}],
							},
						],
					},
				],
			},
			{type: NodeType.Text, content: '\n'},
			{
				type: NodeType.Strong,
				children: [
					{
						type: NodeType.Emphasis,
						children: [
							{type: NodeType.Text, content: 'nested '},
							{
								type: NodeType.Underline,
								children: [{type: NodeType.Text, content: 'underline inside italic'}],
							},
							{type: NodeType.Text, content: ' text'},
						],
					},
				],
			},
		]);
	});

	test('spoiler with various formatting combinations', () => {
		const input =
			'||**spoiler bold**||\n**||bold spoiler||**\n_||italic spoiler||_\n`||spoiler code||`\n||`spoiler code inside spoiler`||';
		const flags = ParserFlags.ALLOW_SPOILERS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Spoiler,
				isBlock: false,
				children: [
					{
						type: NodeType.Strong,
						children: [{type: NodeType.Text, content: 'spoiler bold'}],
					},
				],
			},
			{type: NodeType.Text, content: '\n'},
			{
				type: NodeType.Strong,
				children: [
					{
						type: NodeType.Spoiler,
						isBlock: false,
						children: [{type: NodeType.Text, content: 'bold spoiler'}],
					},
				],
			},
			{type: NodeType.Text, content: '\n'},
			{
				type: NodeType.Emphasis,
				children: [
					{
						type: NodeType.Spoiler,
						isBlock: false,
						children: [{type: NodeType.Text, content: 'italic spoiler'}],
					},
				],
			},
			{type: NodeType.Text, content: '\n'},
			{type: NodeType.InlineCode, content: '||spoiler code||'},
			{type: NodeType.Text, content: '\n'},
			{
				type: NodeType.Spoiler,
				isBlock: false,
				children: [{type: NodeType.InlineCode, content: 'spoiler code inside spoiler'}],
			},
		]);
	});

	test('spoiler with broken nesting and mixed formatting', () => {
		const input =
			'||**spoiler bold**||\n**||bold spoiler||**\n_||italic spoiler||_\n`||spoiler code||`\n||`spoiler code inside spoiler`||\n||_**mixed || nesting madness**_||';
		const flags = ParserFlags.ALLOW_SPOILERS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Spoiler,
				isBlock: false,
				children: [
					{
						type: NodeType.Strong,
						children: [{type: NodeType.Text, content: 'spoiler bold'}],
					},
				],
			},
			{type: NodeType.Text, content: '\n'},
			{
				type: NodeType.Strong,
				children: [
					{
						type: NodeType.Spoiler,
						isBlock: false,
						children: [{type: NodeType.Text, content: 'bold spoiler'}],
					},
				],
			},
			{type: NodeType.Text, content: '\n'},
			{
				type: NodeType.Emphasis,
				children: [
					{
						type: NodeType.Spoiler,
						isBlock: false,
						children: [{type: NodeType.Text, content: 'italic spoiler'}],
					},
				],
			},
			{type: NodeType.Text, content: '\n'},
			{type: NodeType.InlineCode, content: '||spoiler code||'},
			{type: NodeType.Text, content: '\n'},
			{
				type: NodeType.Spoiler,
				isBlock: false,
				children: [{type: NodeType.InlineCode, content: 'spoiler code inside spoiler'}],
			},
			{type: NodeType.Text, content: '\n'},
			{type: NodeType.Spoiler, isBlock: false, children: [{type: NodeType.Text, content: '_**mixed '}]},
			{type: NodeType.Text, content: ' nesting madness**_||'},
		]);
	});

	test('text node splitting', () => {
		const input = 'This is a link: [Rust](https://www.rust-lang.org).';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'This is a link: '},
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'Rust'},
				url: 'https://www.rust-lang.org/',
				escaped: false,
			},
			{type: NodeType.Text, content: '.'},
		]);
	});

	test('newline text handling', () => {
		const input = 'First line.\nSecond line with `code`.\nThird line.';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'First line.\nSecond line with '},
			{type: NodeType.InlineCode, content: 'code'},
			{type: NodeType.Text, content: '.\nThird line.'},
		]);
	});

	test('underscore emphasis with constants', () => {
		const input = 'THIS_IS_A_CONSTANT THIS _IS_ A_CONSTANT THIS _IS_. A CONSTANT';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'THIS_IS_A_CONSTANT THIS '},
			{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'IS'}]},
			{type: NodeType.Text, content: ' A_CONSTANT THIS '},
			{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'IS'}]},
			{type: NodeType.Text, content: '. A CONSTANT'},
		]);
	});

	test('incomplete formatting in code', () => {
		const input = '`function() { /* ** {{ __unclosed__ }} ** */ }`';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.InlineCode, content: 'function() { /* ** {{ __unclosed__ }} ** */ }'}]);
	});

	test('link with inline code', () => {
		const input = '[`f38932b`](https://github.com/test/test/commit/f38932ba169e863c6693d0edf3d1d1b10609cf13)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Link,
				text: {type: NodeType.InlineCode, content: 'f38932b'},
				url: 'https://github.com/test/test/commit/f38932ba169e863c6693d0edf3d1d1b10609cf13',
				escaped: false,
			},
		]);
	});

	test('link with all inline formatting', () => {
		const input = '[**Bold**, *Italic*, ~~Strikethrough~~, and `Code`](https://example.com)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Link,
				text: {
					type: NodeType.Sequence,
					children: [
						{type: NodeType.Strong, children: [{type: NodeType.Text, content: 'Bold'}]},
						{type: NodeType.Text, content: ', '},
						{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'Italic'}]},
						{type: NodeType.Text, content: ', '},
						{type: NodeType.Strikethrough, children: [{type: NodeType.Text, content: 'Strikethrough'}]},
						{type: NodeType.Text, content: ', and '},
						{type: NodeType.InlineCode, content: 'Code'},
					],
				},
				url: 'https://example.com/',
				escaped: false,
			},
		]);
	});

	test('shrug emoticon should preserve backslash before underscore', () => {
		const input = 'Check out this shrug: ¯\\_(ツ)_/¯';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: 'Check out this shrug: ¯\\_(ツ)_/¯'}]);

		expect((ast[0] as TextNode).content).toContain('¯\\_(');
	});

	test('regular escaped underscore should be handled correctly', () => {
		const input = 'This is not \\_emphasized\\_ text';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: 'This is not _emphasized_ text'}]);
	});

	describe('Edge cases and complex nesting', () => {
		test('double-space line breaks in formatted text', () => {
			const input = '**bold  \nacross  \nmultiple lines**\n__underline  \nwith breaks__';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(3);
			expect(ast[0].type).toBe(NodeType.Strong);
			expect(ast[1].type).toBe(NodeType.Text);
			expect(ast[1]).toEqual({type: NodeType.Text, content: '\n'});
			expect(ast[2].type).toBe(NodeType.Underline);

			// TODO: Check for line break nodes within the formatted sections
		});

		test('escaped characters in formatting', () => {
			const input = '**bold \\*with\\* escaped**';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.Strong);
		});

		test('nested formatting behavior', () => {
			const input = '**outer **inner** content**';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBeGreaterThan(0);
		});

		test('empty double markers', () => {
			const input = '****';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '****'}]);
		});

		test('incomplete formatting marker', () => {
			const input = '**';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '**'}]);
		});

		test('complex nested formatting behavior', () => {
			const input = '**outer *middle **inner** content* end**';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBeGreaterThan(0);
		});

		test('escaped backslash handling', () => {
			const input = '**test \\\\escaped backslash**';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.Strong);
		});

		test('extremely complex nested formatting with weird edge cases', () => {
			const input =
				'***bold _italic __underline bold italic__ italic_ bold***\n**_nested *italic inside bold inside italic*_**\n__**_underline bold italic **still going_**__\n**_this ends weirdly__**\n***bold *italic `code` inside***';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Emphasis,
					children: [
						{
							type: NodeType.Strong,
							children: [
								{type: NodeType.Text, content: 'bold '},
								{
									type: NodeType.Emphasis,
									children: [
										{type: NodeType.Text, content: 'italic '},
										{
											type: NodeType.Underline,
											children: [{type: NodeType.Text, content: 'underline bold italic'}],
										},
										{type: NodeType.Text, content: ' italic'},
									],
								},
								{type: NodeType.Text, content: ' bold'},
							],
						},
					],
				},
				{type: NodeType.Text, content: '\n'},
				{
					type: NodeType.Strong,
					children: [
						{
							type: NodeType.Emphasis,
							children: [
								{type: NodeType.Text, content: 'nested '},
								{
									type: NodeType.Emphasis,
									children: [{type: NodeType.Text, content: 'italic inside bold inside italic'}],
								},
							],
						},
					],
				},
				{type: NodeType.Text, content: '\n'},
				{
					type: NodeType.Underline,
					children: [
						{type: NodeType.Strong, children: [{type: NodeType.Text, content: '_underline bold italic '}]},
						{type: NodeType.Text, content: 'still going_**'},
					],
				},
				{type: NodeType.Text, content: '\n'},
				{type: NodeType.Strong, children: [{type: NodeType.Text, content: '_this ends weirdly__'}]},
				{type: NodeType.Text, content: '\n'},
				{
					type: NodeType.Emphasis,
					children: [
						{
							type: NodeType.Strong,
							children: [
								{type: NodeType.Text, content: 'bold *italic '},
								{type: NodeType.InlineCode, content: 'code'},
								{type: NodeType.Text, content: ' inside'},
							],
						},
					],
				},
			]);
		});

		test('complex mismatched formatting markers', () => {
			const input =
				'**bold *italic*\n__underline **bold__\n~~strike *italic~~ text*\n**__mixed but only one end__\n_italics __underline_';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Strong,
					children: [
						{type: NodeType.Text, content: 'bold '},
						{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'italic'}]},
						{type: NodeType.Text, content: '\n__underline '},
					],
				},
				{type: NodeType.Text, content: 'bold'},
				{
					type: NodeType.Underline,
					children: [
						{type: NodeType.Text, content: '\n'},
						{type: NodeType.Strikethrough, children: [{type: NodeType.Text, content: 'strike *italic'}]},
						{type: NodeType.Text, content: ' text'},
						{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: '\n'}]},
						{type: NodeType.Text, content: '*'},
					],
				},
				{type: NodeType.Text, content: 'mixed but only one end'},
				{type: NodeType.Underline, children: [{type: NodeType.Text, content: '\n_italics '}]},
				{type: NodeType.Text, content: 'underline_'},
			]);
		});
	});
});
