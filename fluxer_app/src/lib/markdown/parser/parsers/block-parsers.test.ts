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
import {AlertType, NodeType, ParserFlags, TableAlignment} from '../types/enums';
import type {BlockquoteNode, TextNode} from '../types/nodes';

describe('Fluxer Markdown Parser', () => {
	test('heading without newlines', () => {
		const input = '# Heading 1\n## Heading 2\n### Heading 3';
		const flags = ParserFlags.ALLOW_HEADINGS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toHaveLength(3);
		expect(ast[0]).toEqual({
			type: NodeType.Heading,
			level: 1,
			children: [{type: NodeType.Text, content: 'Heading 1'}],
		});
		expect(ast[1]).toEqual({
			type: NodeType.Heading,
			level: 2,
			children: [{type: NodeType.Text, content: 'Heading 2'}],
		});
		expect(ast[2]).toEqual({
			type: NodeType.Heading,
			level: 3,
			children: [{type: NodeType.Text, content: 'Heading 3'}],
		});
	});

	test('heading with paragraph', () => {
		const input = '# Heading 1\nParagraph text\n## Heading 2';
		const flags = ParserFlags.ALLOW_HEADINGS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toHaveLength(3);
		expect(ast[0]).toEqual({
			type: NodeType.Heading,
			level: 1,
			children: [{type: NodeType.Text, content: 'Heading 1'}],
		});
		expect(ast[1]).toEqual({type: NodeType.Text, content: 'Paragraph text'});
		expect(ast[2]).toEqual({
			type: NodeType.Heading,
			level: 2,
			children: [{type: NodeType.Text, content: 'Heading 2'}],
		});
	});

	test('multiple headings with blank lines', () => {
		const input = '# Heading 1\n\n## Heading 2\n\n### Heading 3\n\n#### Heading 4';
		const flags = ParserFlags.ALLOW_HEADINGS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Heading,
				level: 1,
				children: [{type: NodeType.Text, content: 'Heading 1'}],
			},
			{
				type: NodeType.Heading,
				level: 2,
				children: [{type: NodeType.Text, content: 'Heading 2'}],
			},
			{
				type: NodeType.Heading,
				level: 3,
				children: [{type: NodeType.Text, content: 'Heading 3'}],
			},
			{
				type: NodeType.Heading,
				level: 4,
				children: [{type: NodeType.Text, content: 'Heading 4'}],
			},
		]);
	});

	test('malformed headings', () => {
		const input = '#Not a heading\n###### Too many hashes\n###No space';
		const flags = ParserFlags.ALLOW_HEADINGS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: '#Not a heading\n'},
			{type: NodeType.Text, content: '###### Too many hashes\n'},
			{type: NodeType.Text, content: '###No space'},
		]);
	});

	test('heading after blank line', () => {
		const input = 'test\n\n# Test';
		const flags = ParserFlags.ALLOW_HEADINGS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'test\n'},
			{
				type: NodeType.Heading,
				level: 1,
				children: [{type: NodeType.Text, content: 'Test'}],
			},
		]);
	});

	test('heading before blank line', () => {
		const input = '# Test\n\ntest';
		const flags = ParserFlags.ALLOW_HEADINGS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Heading,
				level: 1,
				children: [{type: NodeType.Text, content: 'Test'}],
			},
			{type: NodeType.Text, content: 'test'},
		]);
	});

	test('heading list spacing', () => {
		const input = '# Heading\n\n- Item 1\n- Item 2';
		const flags = ParserFlags.ALLOW_LISTS | ParserFlags.ALLOW_HEADINGS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Heading,
				level: 1,
				children: [{type: NodeType.Text, content: 'Heading'}],
			},
			{
				type: NodeType.List,
				ordered: false,
				items: [
					{children: [{type: NodeType.Text, content: 'Item 1'}]},
					{children: [{type: NodeType.Text, content: 'Item 2'}]},
				],
			},
		]);
	});

	test('list heading spacing', () => {
		const input = '- Item 1\n- Item 2\n\n# Heading';
		const flags = ParserFlags.ALLOW_LISTS | ParserFlags.ALLOW_HEADINGS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.List,
				ordered: false,
				items: [
					{children: [{type: NodeType.Text, content: 'Item 1'}]},
					{children: [{type: NodeType.Text, content: 'Item 2'}]},
				],
			},
			{
				type: NodeType.Heading,
				level: 1,
				children: [{type: NodeType.Text, content: 'Heading'}],
			},
		]);
	});

	test('blockquote', () => {
		const input = '> This is a blockquote.\n> It has two lines.';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: 'This is a blockquote.\nIt has two lines.'}],
			},
		]);
	});

	test('blockquote with preserved > character and newline', () => {
		const input = '> fsdff\n> > fdfs';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: 'fsdff\n> fdfs'}],
			},
		]);
	});

	test('blockquote disabled', () => {
		const input = '> This is a blockquote.\n> It has two lines.';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '> This is a blockquote.\n> It has two lines.'}]);
	});

	test('multiline blockquote', () => {
		const input = ">>> This is an multiline blockquote.\nIt continues without '> ' prefix.";
		const flags = ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [
					{type: NodeType.Text, content: "This is an multiline blockquote.\nIt continues without '> ' prefix."},
				],
			},
		]);
	});

	test('multiline blockquote disabled', () => {
		const input = ">>> This is an multiline blockquote.\nIt continues without '> ' prefix.";
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: ">>> This is an multiline blockquote.\nIt continues without '> ' prefix."},
		]);
	});

	test('code block', () => {
		const input = '```rust\nfn main() {\n    println!("Hello, world!");\n}\n```';
		const flags = ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toHaveLength(1);
		expect(ast[0]).toEqual({
			type: NodeType.CodeBlock,
			language: 'rust',
			content: 'fn main() {\n    println!("Hello, world!");\n}\n',
		});
	});

	test('code block edge cases', () => {
		const input =
			'```\nNo language specified\n```\n```invalid\nInvalid language\n```\n```rust no build\nWith extra tokens\n```';
		const flags = ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toHaveLength(3);

		expect(ast[0]).toEqual({
			type: NodeType.CodeBlock,
			language: undefined,
			content: 'No language specified\n',
		});

		expect(ast[1]).toEqual({
			type: NodeType.CodeBlock,
			language: 'invalid',
			content: 'Invalid language\n',
		});

		expect(ast[2]).toEqual({
			type: NodeType.CodeBlock,
			language: 'rust no build',
			content: 'With extra tokens\n',
		});
	});

	test('inline fenced block after text', () => {
		const input = 'a```b```';
		const flags = ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'a'},
			{type: NodeType.CodeBlock, language: undefined, content: 'b'},
		]);
	});

	test('inline fenced block with trailing text after closing fence', () => {
		const input = 'a```b```c';
		const flags = ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'a'},
			{type: NodeType.CodeBlock, language: undefined, content: 'b'},
			{type: NodeType.Text, content: 'c'},
		]);
	});

	test('fenced block following inline text with multiline content', () => {
		const input = 'before ```\ncode line\n``` after';
		const flags = ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'before '},
			{type: NodeType.CodeBlock, language: undefined, content: 'code line\n'},
			{type: NodeType.Text, content: ' after'},
		]);
	});

	test('captures trailing text after closing fence on its own line', () => {
		const input = '```\ncode line\n``` trailing';
		const flags = ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.CodeBlock, language: undefined, content: 'code line\n'},
			{type: NodeType.Text, content: ' trailing'},
		]);
	});

	test('multiple blockquotes', () => {
		const input = '> First blockquote\n\n> Second blockquote\n> With multiple lines.';
		const flags = ParserFlags.ALLOW_HEADINGS | ParserFlags.ALLOW_LISTS | ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: 'First blockquote'}],
			},
			{type: NodeType.Text, content: '\n'},
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: 'Second blockquote\nWith multiple lines.'}],
			},
		]);
	});

	test('list followed by blank line and text', () => {
		const input = '- Test\n\nTest';
		const flags = ParserFlags.ALLOW_LISTS | ParserFlags.ALLOW_HEADINGS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.List,
				ordered: false,
				items: [{children: [{type: NodeType.Text, content: 'Test'}]}],
			},
			{type: NodeType.Text, content: '\nTest'},
		]);
	});

	test('blockquote without continuation line prefix', () => {
		const input = '> **[Author](https://example.com)**\n> First line of quote.\nContinuation line without prefix.';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [
					{
						type: NodeType.Strong,
						children: [
							{
								type: NodeType.Link,
								text: {type: NodeType.Text, content: 'Author'},
								url: 'https://example.com/',
								escaped: false,
							},
						],
					},
					{type: NodeType.Text, content: '\nFirst line of quote.'},
				],
			},
			{type: NodeType.Text, content: 'Continuation line without prefix.'},
		]);
	});

	test('blockquote infinite loop', () => {
		const input = '> >>';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toBeDefined();
		expect(ast.length).toBeGreaterThan(0);
		expect(ast.length).toBeLessThan(50);
	});

	test('blockquote infinite loop alternate', () => {
		const input = '>> >';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toBeDefined();
		expect(ast.length).toBeGreaterThan(0);
		expect(ast.length).toBeLessThan(50);
	});

	test('blockquote list', () => {
		const input = '> - test';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_LISTS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [
					{
						type: NodeType.List,
						ordered: false,
						items: [{children: [{type: NodeType.Text, content: 'test'}]}],
					},
				],
			},
		]);
	});

	test('multiline blockquote list', () => {
		const input = '>>> - test';
		const flags = ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES | ParserFlags.ALLOW_LISTS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [
					{
						type: NodeType.List,
						ordered: false,
						items: [{children: [{type: NodeType.Text, content: 'test'}]}],
					},
				],
			},
		]);
	});

	test('blockquote with multiple lists', () => {
		const input = '> - test1\n> - test2';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_LISTS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [
					{
						type: NodeType.List,
						ordered: false,
						items: [
							{children: [{type: NodeType.Text, content: 'test1'}]},
							{children: [{type: NodeType.Text, content: 'test2'}]},
						],
					},
				],
			},
		]);
	});

	test('blockquote with paragraph and list', () => {
		const input = '> This is a paragraph.\n>\n> - Item 1\n> - Item 2';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_LISTS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: 'This is a paragraph.'}],
			},
			{type: NodeType.Text, content: '>'},
			{
				type: NodeType.Blockquote,
				children: [
					{
						type: NodeType.List,
						ordered: false,
						items: [
							{children: [{type: NodeType.Text, content: 'Item 1'}]},
							{children: [{type: NodeType.Text, content: 'Item 2'}]},
						],
					},
				],
			},
		]);
	});

	test('blockquote without continuation line prefix', () => {
		const input = '> This is in a blockquote.\n>\n> This is in another blockquote.';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: 'This is in a blockquote.'}],
			},
			{type: NodeType.Text, content: '>'},
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: 'This is in another blockquote.'}],
			},
		]);
	});

	test('blockquote with blank lines', () => {
		const input = '> First paragraph\n> \n> Second paragraph';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: 'First paragraph\n\nSecond paragraph'}],
			},
		]);
	});

	test('multiline blockquote with blank lines', () => {
		const input = '>>> First paragraph\n\nSecond paragraph';
		const flags = ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: 'First paragraph\n\nSecond paragraph'}],
			},
		]);
	});

	test('blockquote with multiple blank lines should create consistent newlines', () => {
		const input = '> Line one\n> \n> \n> Line two';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: 'Line one\n\n\nLine two'}],
			},
		]);
	});

	test('blockquote vs multiline blockquote with blank lines should behave the same', () => {
		const blockquoteInput = '> Test\n> \n> test';
		const blockquoteFlags = ParserFlags.ALLOW_BLOCKQUOTES;
		const blockquoteParser = new Parser(blockquoteInput, blockquoteFlags);
		const {nodes: blockquoteAst} = blockquoteParser.parse();

		const multilineInput = '>>> Test\n\ntest';
		const multilineFlags = ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES;
		const multilineParser = new Parser(multilineInput, multilineFlags);
		const {nodes: multilineAst} = multilineParser.parse();

		const blockquoteContent = (blockquoteAst[0] as BlockquoteNode).children[0] as TextNode;
		const multilineContent = (multilineAst[0] as BlockquoteNode).children[0] as TextNode;

		expect(blockquoteContent.content).toBe('Test\n\ntest');
		expect(multilineContent.content).toBe('Test\n\ntest');
	});

	test('blockquote with > without space should not be parsed as part of blockquote', () => {
		const input = '> First line\n>\n> Second line';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: 'First line'}],
			},
			{type: NodeType.Text, content: '>'},
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: 'Second line'}],
			},
		]);
	});

	test('blockquote with multiple > spaces', () => {
		const input = '> First line\n>  \n>   \n> Second line';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: 'First line\n\n\nSecond line'}],
			},
		]);
	});

	test('blockquote nested list', () => {
		const input = '> - Item 1\n>   - Subitem 1\n>   - Subitem 2\n> - Item 2';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_LISTS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [
					{
						type: NodeType.List,
						ordered: false,
						items: [
							{
								children: [
									{type: NodeType.Text, content: 'Item 1'},
									{
										type: NodeType.List,
										ordered: false,
										items: [
											{children: [{type: NodeType.Text, content: 'Subitem 1'}]},
											{children: [{type: NodeType.Text, content: 'Subitem 2'}]},
										],
									},
								],
							},
							{children: [{type: NodeType.Text, content: 'Item 2'}]},
						],
					},
				],
			},
		]);
	});

	test('double arrow blockquote', () => {
		const input = '>> test';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '>> test'}]);
	});

	test('double spaced arrow blockquote', () => {
		const input = '> > test';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: '> test'}],
			},
		]);
	});

	test('multiple arrows blockquote', () => {
		const input = '> > > > deeply nested';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [{type: NodeType.Text, content: '> > > deeply nested'}],
			},
		]);
	});

	test('multiline blockquote with blockquote', () => {
		const input = 'test\n>>> test\ntest\n\ntest\n> test';
		const flags = ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'test'},
			{
				type: NodeType.Blockquote,
				children: [
					{type: NodeType.Text, content: 'test\ntest\n\ntest'},
					{
						type: NodeType.Blockquote,
						children: [{type: NodeType.Text, content: 'test'}],
					},
				],
			},
		]);
	});

	test('multiline blockquote with double nested blockquote', () => {
		const input = 'test\n>>> test\ntest\n\ntest\n> test\n> > should not nest';
		const flags = ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'test'},
			{
				type: NodeType.Blockquote,
				children: [
					{type: NodeType.Text, content: 'test\ntest\n\ntest'},
					{
						type: NodeType.Blockquote,
						children: [{type: NodeType.Text, content: 'test\n> should not nest'}],
					},
				],
			},
		]);
	});

	test('basic subtext', () => {
		const input = '-# This is subtext';
		const flags = ParserFlags.ALLOW_SUBTEXT;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Subtext,
				children: [{type: NodeType.Text, content: 'This is subtext'}],
			},
		]);
	});

	test('subtext disabled', () => {
		const input = '-# This is subtext';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '-# This is subtext'}]);
	});

	test('subtext with formatting', () => {
		const input = '-# This is *formatted* and **bold** subtext';
		const flags = ParserFlags.ALLOW_SUBTEXT;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Subtext,
				children: [
					{type: NodeType.Text, content: 'This is '},
					{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'formatted'}]},
					{type: NodeType.Text, content: ' and '},
					{type: NodeType.Strong, children: [{type: NodeType.Text, content: 'bold'}]},
					{type: NodeType.Text, content: ' subtext'},
				],
			},
		]);
	});

	test('multiple subtexts', () => {
		const input = '-# First subtext\n-# Second subtext';
		const flags = ParserFlags.ALLOW_SUBTEXT;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Subtext,
				children: [{type: NodeType.Text, content: 'First subtext'}],
			},
			{
				type: NodeType.Subtext,
				children: [{type: NodeType.Text, content: 'Second subtext'}],
			},
		]);
	});

	test('subtext with mixed content', () => {
		const input = '# Heading\n-# Subtext below heading\nNormal text\n-# Another subtext';
		const flags = ParserFlags.ALLOW_SUBTEXT | ParserFlags.ALLOW_HEADINGS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Heading,
				level: 1,
				children: [{type: NodeType.Text, content: 'Heading'}],
			},
			{
				type: NodeType.Subtext,
				children: [{type: NodeType.Text, content: 'Subtext below heading'}],
			},
			{type: NodeType.Text, content: 'Normal text\n'},
			{
				type: NodeType.Subtext,
				children: [{type: NodeType.Text, content: 'Another subtext'}],
			},
		]);
	});

	test('malformed subtext', () => {
		const input = '-#No space\n-#  Two spaces\n-## Extra hash';
		const flags = ParserFlags.ALLOW_SUBTEXT;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: '-#No space\n'},
			{type: NodeType.Text, content: '-#  Two spaces\n'},
			{type: NodeType.Text, content: '-## Extra hash'},
		]);
	});

	test('spoiler with block elements', () => {
		const input = '||\n# Heading\n- List item\n> Quote\n||';
		const flags =
			ParserFlags.ALLOW_SPOILERS | ParserFlags.ALLOW_HEADINGS | ParserFlags.ALLOW_LISTS | ParserFlags.ALLOW_BLOCKQUOTES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Spoiler,
				isBlock: true,
				children: [
					{
						type: NodeType.Heading,
						level: 1,
						children: [{type: NodeType.Text, content: 'Heading'}],
					},
					{
						type: NodeType.List,
						ordered: false,
						items: [
							{
								children: [{type: NodeType.Text, content: 'List item'}],
							},
						],
					},
					{
						type: NodeType.Blockquote,
						children: [{type: NodeType.Text, content: 'Quote'}],
					},
				],
			},
		]);
	});

	test('spoiler with code blocks', () => {
		const input = '||\n```rust\nfn main() {}\n```\n||';
		const flags = ParserFlags.ALLOW_SPOILERS | ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Spoiler,
				isBlock: true,
				children: [
					{
						type: NodeType.CodeBlock,
						language: 'rust',
						content: 'fn main() {}\n',
					},
				],
			},
		]);
	});

	test('spoiler with tables', () => {
		const input = '||\n| Header |\n|--------|\n| Cell |\n||';
		const flags = ParserFlags.ALLOW_SPOILERS | ParserFlags.ALLOW_TABLES;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Spoiler,
				isBlock: true,
				children: [
					{
						type: NodeType.Table,
						header: {
							type: NodeType.TableRow,
							cells: [
								{
									type: NodeType.TableCell,
									children: [{type: NodeType.Text, content: 'Header'}],
								},
							],
						},
						alignments: [TableAlignment.None],
						rows: [
							{
								type: NodeType.TableRow,
								cells: [
									{
										type: NodeType.TableCell,
										children: [{type: NodeType.Text, content: 'Cell'}],
									},
								],
							},
						],
					},
				],
			},
		]);
	});

	test('spoiler with alerts', () => {
		const input = '||\n> [!NOTE]\n> This is a note\n||';
		const flags = ParserFlags.ALLOW_SPOILERS | ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_ALERTS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Spoiler,
				isBlock: true,
				children: [
					{
						type: NodeType.Alert,
						alertType: AlertType.Note,
						children: [{type: NodeType.Text, content: 'This is a note'}],
					},
				],
			},
		]);
	});

	test('malformed spoiler blocks', () => {
		const inputs = ['|| incomplete spoiler\n# heading', '||\n# heading\n| incomplete', '|| # not a heading ||'];

		const flags = ParserFlags.ALLOW_SPOILERS | ParserFlags.ALLOW_HEADINGS;

		for (const input of inputs) {
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(
				ast.every(
					(node) => node.type === NodeType.Text || node.type === NodeType.Heading || node.type === NodeType.Spoiler,
				),
			).toBe(true);
		}
	});

	test('subtext with bracketed text followed by link', () => {
		const input =
			'-# [1] TL;DR: We use the [GNU Affero General Public License v3 (AGPLv3)](https://www.gnu.org/licenses/agpl-3.0.en.html) for our code.';
		const flags = ParserFlags.ALLOW_SUBTEXT | ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Subtext,
				children: [
					{type: NodeType.Text, content: '[1] TL;DR: We use the '},
					{
						type: NodeType.Link,
						text: {type: NodeType.Text, content: 'GNU Affero General Public License v3 (AGPLv3)'},
						url: 'https://www.gnu.org/licenses/agpl-3.0.en.html',
						escaped: false,
					},
					{type: NodeType.Text, content: ' for our code.'},
				],
			},
		]);
	});

	describe('Alert Parser', () => {
		test('basic note alert', () => {
			const input = '> [!NOTE]\n> This is a note';
			const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_ALERTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Alert,
					alertType: AlertType.Note,
					children: [{type: NodeType.Text, content: 'This is a note'}],
				},
			]);
		});

		test('alert with formatted content', () => {
			const input = '> [!WARNING]\n> This is **important** and *emphasized*';
			const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_ALERTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Alert,
					alertType: AlertType.Warning,
					children: [
						{type: NodeType.Text, content: 'This is '},
						{type: NodeType.Strong, children: [{type: NodeType.Text, content: 'important'}]},
						{type: NodeType.Text, content: ' and '},
						{type: NodeType.Emphasis, children: [{type: NodeType.Text, content: 'emphasized'}]},
					],
				},
			]);
		});

		test('multiline alert', () => {
			const input = '> [!IMPORTANT]\n> First line\n> Second line';
			const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_ALERTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Alert,
					alertType: AlertType.Important,
					children: [{type: NodeType.Text, content: 'First line\nSecond line'}],
				},
			]);
		});

		test('alert with nested content', () => {
			const input = '> [!TIP]\n> - List item 1\n> - List item 2\n> \n> Additional text';
			const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_ALERTS | ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Alert,
					alertType: AlertType.Tip,
					children: [
						{
							type: NodeType.List,
							ordered: false,
							items: [
								{children: [{type: NodeType.Text, content: 'List item 1'}]},
								{children: [{type: NodeType.Text, content: 'List item 2'}]},
							],
						},
						{type: NodeType.Text, content: '\nAdditional text'},
					],
				},
			]);
		});

		test('invalid alert types', () => {
			const input = '> [!INVALID]\n> This should be a regular blockquote';
			const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_ALERTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Blockquote,
					children: [{type: NodeType.Text, content: '[!INVALID]\nThis should be a regular blockquote'}],
				},
			]);
		});

		test('alert with blank lines', () => {
			const input = '> [!CAUTION]\n> First paragraph\n>\n> Second paragraph';
			const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_ALERTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Alert,
					alertType: AlertType.Caution,
					children: [{type: NodeType.Text, content: 'First paragraph'}],
				},
				{type: NodeType.Text, content: '>'},
				{
					type: NodeType.Blockquote,
					children: [{type: NodeType.Text, content: 'Second paragraph'}],
				},
			]);
		});

		test('alerts disabled with blockquotes', () => {
			const input = '> [!NOTE]\n> This should be a regular blockquote';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '> [!NOTE]\n> This should be a regular blockquote'}]);
		});

		test('remove empty space between alerts', () => {
			const input = '> [!NOTE]\n> This is a note\n\n> [!WARNING]\n> This is a warning';
			const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_ALERTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Alert,
					alertType: AlertType.Note,
					children: [{type: NodeType.Text, content: 'This is a note'}],
				},
				{
					type: NodeType.Alert,
					alertType: AlertType.Warning,
					children: [{type: NodeType.Text, content: 'This is a warning'}],
				},
			]);
		});
	});

	test('blockquote with bold, italic, nested quote and code block', () => {
		const input = '> **bold in quote**\n> *italic in quote*\n> > nested quote **inside**\n> ```code inside quote```';
		const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Blockquote,
				children: [
					{
						type: NodeType.Strong,
						children: [{type: NodeType.Text, content: 'bold in quote'}],
					},
					{type: NodeType.Text, content: '\n'},
					{
						type: NodeType.Emphasis,
						children: [{type: NodeType.Text, content: 'italic in quote'}],
					},
					{type: NodeType.Text, content: '\n> nested quote '},
					{
						type: NodeType.Strong,
						children: [{type: NodeType.Text, content: 'inside'}],
					},
					{type: NodeType.Text, content: '\n'},
					{
						type: NodeType.CodeBlock,
						language: undefined,
						content: 'code inside quote',
					},
				],
			},
		]);
	});

	test('inline code with backtick followed by code block', () => {
		const input = '`single backtick with ` inside`\n````md\nnested ```';
		const flags = ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.InlineCode, content: 'single backtick with '},
			{type: NodeType.Text, content: ' inside`\n'},
			{
				type: NodeType.CodeBlock,
				language: 'md',
				content: 'nested ```\n',
			},
		]);
	});

	test('single-line code block with backticks inside and inline code with triple backticks', () => {
		const input = '```code `inline inside` block```\n`inline ```block inside``` inline`';
		const flags = ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.CodeBlock,
				language: undefined,
				content: 'code `inline inside` block',
			},
			{type: NodeType.InlineCode, content: 'inline ```block inside``` inline'},
		]);
	});

	test('code block containing triple backticks with extra backtick', () => {
		const input = '```multi\n```multi\n````';
		const flags = ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toHaveLength(2);
		expect(ast[0]).toEqual({
			type: NodeType.CodeBlock,
			language: 'multi',
			content: '```multi\n',
		});
		expect(ast[1]).toEqual({type: NodeType.Text, content: '`'});
	});

	describe('Edge cases and error handling', () => {
		test('invalid heading levels', () => {
			const input = '##### Too many hashes';
			const flags = ParserFlags.ALLOW_HEADINGS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '##### Too many hashes'}]);
		});

		test('multiline blockquote content handling', () => {
			const input = '>>> First line\nSecond line without prefix';
			const flags = ParserFlags.ALLOW_BLOCKQUOTES;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.Text);
		});

		test('invalid multiline blockquote start', () => {
			const input = '>> Not a multiline blockquote';
			const flags = ParserFlags.ALLOW_BLOCKQUOTES;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '>> Not a multiline blockquote'}]);
		});

		test('string cache performance optimization', () => {
			const repeatedInput = '# Same heading\n# Same heading\n# Same heading';
			const flags = ParserFlags.ALLOW_HEADINGS;
			const parser = new Parser(repeatedInput, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(3);
			expect(ast[0].type).toBe(NodeType.Heading);
			expect(ast[1].type).toBe(NodeType.Heading);
			expect(ast[2].type).toBe(NodeType.Heading);
		});

		test('code block without closing fence', () => {
			const input = '```js\nconst x = 1;\n// no closing fence';
			const flags = ParserFlags.ALLOW_CODE_BLOCKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.CodeBlock);
		});
	});
});
