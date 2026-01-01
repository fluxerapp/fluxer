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
import {APP_PROTOCOL_PREFIX} from '~/utils/appProtocol';
import {Parser} from '../parser/parser';
import {NodeType, ParserFlags} from '../types/enums';

describe('Fluxer Markdown Parser', () => {
	test('obfuscated URLs with Unicode confusables', () => {
		const testCases = [
			{
				input: 'https://ｅｘａｍｐｌｅ.com',
				expected: 'https://example.com/',
			},
			{
				input: 'http://ｇ০০ｇｌｅ.com',
				expected: 'http://xn--ggle-02ja.com/',
			},
			{
				input: 'http://googlｅ.com',
				expected: 'http://google.com/',
			},
			{
				input: 'http://xn--80ak6aa92e.com/',
				expected: 'http://xn--80ak6aa92e.com/',
			},
		];

		for (const {input, expected} of testCases) {
			const parser = new Parser(input, ParserFlags.ALLOW_AUTOLINKS);
			const {nodes: ast} = parser.parse();

			expect(ast[0].type).toBe(NodeType.Link);
			expect((ast[0] as any).url).toBe(expected);
			expect(ast.length).toBe(1);
		}
	});

	test('crazy URL with Unicode confusables', () => {
		const input =
			'http://\\ｃaｎaｒｙ%2e𝑑𝓲Ｓ𝐜𝑜ｒᵈＡ%50Ｐ。𝐜𝑜ｍ\\google.com⁂⌘∮/%2e.\\../invite\\youtube.com‖∠∇\\../\\../\\../white_check_marktwitter.com]「＠\\../\\../\\../\\../hK5He8z8Ge';
		const parser = new Parser(input, ParserFlags.ALLOW_AUTOLINKS);
		const {nodes: ast} = parser.parse();

		expect(ast[0].type).toBe(NodeType.Link);
		expect(ast).toEqual([
			{
				type: NodeType.Link,
				text: undefined,
				url: 'http://canary.discordapp.com/invite/hK5He8z8Ge',
				escaped: false,
			},
		]);
	});

	test('URL path traversal attempts', () => {
		const input = 'https://example.com/folder/../../../secret';
		const parser = new Parser(input, ParserFlags.ALLOW_AUTOLINKS);
		const {nodes: ast} = parser.parse();

		expect(ast[0].type).toBe(NodeType.Link);
		expect((ast[0] as any).url).toBe('https://example.com/secret');
	});

	test('URL with encoded periods in domain', () => {
		const input = 'http://example%2ecom/test';
		const parser = new Parser(input, ParserFlags.ALLOW_AUTOLINKS);
		const {nodes: ast} = parser.parse();

		expect(ast[0].type).toBe(NodeType.Link);
		expect((ast[0] as any).url).toBe('http://example.com/test');
	});

	test('url edge cases', () => {
		const input = 'Visit http://example.com., https://test.com! http:// https:// http://incomplete';
		const flags = ParserFlags.ALLOW_AUTOLINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'Visit '},
			{type: NodeType.Link, text: undefined, url: 'http://example.com/', escaped: false},
			{type: NodeType.Text, content: '., '},
			{type: NodeType.Link, text: undefined, url: 'https://test.com/', escaped: false},
			{type: NodeType.Text, content: '! http:// https:// '},
			{type: NodeType.Link, text: undefined, url: 'http://incomplete/', escaped: false},
		]);
	});

	test('url edge cases with autolinks disabled', () => {
		const input = 'Visit http://example.com., https://test.com! http:// https:// http://incomplete';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'Visit http://example.com., https://test.com! http:// https:// http://incomplete'},
		]);
	});

	test('spoofed links', () => {
		const input = '[http://good.com](http://evil.com)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '[http://good.com](http://evil.com)'}]);
	});

	test('relative urls in markdown links', () => {
		const input = '[this should not parse as a URL](/localhost) [this should](http://localhost)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: '[this should not parse as a URL](/localhost) '},
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'this should'},
				url: 'http://localhost/',
				escaped: false,
			},
		]);
	});

	test('absolute urls in markdown links', () => {
		const input = '[Google](https://www.google.com) [Rust](http://www.rust-lang.org)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'Google'},
				url: 'https://www.google.com/',
				escaped: false,
			},
			{type: NodeType.Text, content: ' '},
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'Rust'},
				url: 'http://www.rust-lang.org/',
				escaped: false,
			},
		]);
	});

	test('domain names without protocol should not be treated as links', () => {
		const input = '[free plutonium](cakey.bot)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '[free plutonium](cakey.bot)'}]);
	});

	test('domain-like link text matching target url domain should parse', () => {
		const input =
			'[www.businessinsider.com/carl-rinsch-legal-2025-12](https://www.businessinsider.com/carl-rinsch-legal-2025-12)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'www.businessinsider.com/carl-rinsch-legal-2025-12'},
				url: 'https://www.businessinsider.com/carl-rinsch-legal-2025-12',
				escaped: false,
			},
		]);
	});

	test('truncated domain-like text should still parse when protocol is provided', () => {
		const input =
			'[www.businessinsider.com/carl-rinsch-...](https://www.businessinsider.com/carl-rinsch-netflix-fraud-trial-hermes-hastens-mattress-2025-12)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'www.businessinsider.com/carl-rinsch-...'},
				url: 'https://www.businessinsider.com/carl-rinsch-netflix-fraud-trial-hermes-hastens-mattress-2025-12',
				escaped: false,
			},
		]);
	});

	test('single domain names without protocol should not be treated as links', () => {
		const input = '[dfgdfg](getblocked)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '[dfgdfg](getblocked)'}]);
	});

	test('complex nested formatting with domain-like strings should not create links', () => {
		const input = '# __**||5345345|| > gfgd ||[dfgdfg](getblocked)||**__';
		const flags = ParserFlags.ALLOW_HEADINGS | ParserFlags.ALLOW_SPOILERS | ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Heading,
				level: 1,
				children: [
					{
						type: NodeType.Underline,
						children: [
							{
								type: NodeType.Strong,
								children: [
									{
										type: NodeType.Spoiler,
										children: [{type: NodeType.Text, content: '5345345'}],
										isBlock: false,
									},
									{type: NodeType.Text, content: ' > gfgd '},
									{
										type: NodeType.Spoiler,
										children: [{type: NodeType.Text, content: '[dfgdfg](getblocked)'}],
										isBlock: false,
									},
								],
							},
						],
					},
				],
			},
		]);
	});

	test('nested links as plain text', () => {
		const input = '[Outer [Inner](https://inner.com)](https://outer.com)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: '[Outer [Inner](https://inner.com)](https://outer.com)'}]);
	});

	test('links with escaped characters', () => {
		const input = '\\[Not a link\\](https://example.com) [Valid Link](https://valid.com)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS | ParserFlags.ALLOW_AUTOLINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: '[Not a link]('},
			{type: NodeType.Link, text: undefined, url: 'https://example.com/', escaped: false},
			{type: NodeType.Text, content: ') '},
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'Valid Link'},
				url: 'https://valid.com/',
				escaped: false,
			},
		]);
	});

	test('links with escaped characters with autolinks disabled', () => {
		const input = '\\[Not a link\\](https://example.com) [Valid Link](https://valid.com)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: '[Not a link](https://example.com) '},
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'Valid Link'},
				url: 'https://valid.com/',
				escaped: false,
			},
		]);
	});

	test('link with nested parentheses in url', () => {
		const input = '[Link with parentheses](https://example.com/path_(with_parentheses))';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'Link with parentheses'},
				url: 'https://example.com/path_(with_parentheses)',
				escaped: false,
			},
		]);
	});

	test('punycode links', () => {
		const input = 'Check out [Münich Site](https://münchen.de) and https://漢字.com';
		const flags = ParserFlags.ALLOW_MASKED_LINKS | ParserFlags.ALLOW_AUTOLINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'Check out '},
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'Münich Site'},
				url: 'https://xn--mnchen-3ya.de/',
				escaped: false,
			},
			{type: NodeType.Text, content: ' and '},
			{
				type: NodeType.Link,
				text: undefined,
				url: 'https://xn--p8s937b.com/',
				escaped: false,
			},
		]);
	});

	test('punycode with autolinks disabled', () => {
		const input = 'Check out [Münich Site](https://münchen.de) and https://漢字.com';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'Check out '},
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'Münich Site'},
				url: 'https://xn--mnchen-3ya.de/',
				escaped: false,
			},
			{type: NodeType.Text, content: ' and https://漢字.com'},
		]);
	});

	test('punycode in code blocks', () => {
		const input = '```\nVisit https://münchen.de\n```';
		const flags = ParserFlags.ALLOW_CODE_BLOCKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.CodeBlock,
				language: undefined,
				content: 'Visit https://münchen.de\n',
			},
		]);
	});

	test('punycode in inline code', () => {
		const input = '`https://münchen.de`';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.InlineCode, content: 'https://münchen.de'}]);
	});

	test('multiple punycode domains', () => {
		const input = '[Test](https://sub.münchen.example.com)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'Test'},
				url: 'https://sub.xn--mnchen-3ya.example.com/',
				escaped: false,
			},
		]);
	});

	test('punycode with path and query', () => {
		const input = '[Test](https://münchen.de/über/test?param=währung)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'Test'},
				url: 'https://xn--mnchen-3ya.de/%C3%BCber/test?param=w%C3%A4hrung',
				escaped: false,
			},
		]);
	});

	test('invalid punycode urls', () => {
		const input = `[Bad URL](https://münchen${String.fromCharCode(0xffff)}.de)`;
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: `[Bad URL](https://münchen${String.fromCharCode(0xffff)}.de)`},
		]);
	});

	test('autolinks with punycode with flag enabled', () => {
		const input = 'Visit https://münchen.de. And https://test.münich.de!';
		const flags = ParserFlags.ALLOW_AUTOLINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'Visit '},
			{type: NodeType.Link, text: undefined, url: 'https://xn--mnchen-3ya.de/', escaped: false},
			{type: NodeType.Text, content: '. And '},
			{type: NodeType.Link, text: undefined, url: 'https://test.xn--mnich-kva.de/', escaped: false},
			{type: NodeType.Text, content: '!'},
		]);
	});

	test('autolinks with punycode with flag disabled', () => {
		const input = 'Visit https://münchen.de. And https://test.münich.de!';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: 'Visit https://münchen.de. And https://test.münich.de!'}]);
	});

	test('nested content with punycode', () => {
		const input = '**[Test](https://münchen.de)** and *https://漢字.com*';
		const flags = ParserFlags.ALLOW_MASKED_LINKS | ParserFlags.ALLOW_AUTOLINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Strong,
				children: [
					{
						type: NodeType.Link,
						text: {type: NodeType.Text, content: 'Test'},
						url: 'https://xn--mnchen-3ya.de/',
						escaped: false,
					},
				],
			},
			{type: NodeType.Text, content: ' and '},
			{
				type: NodeType.Emphasis,
				children: [
					{
						type: NodeType.Link,
						text: undefined,
						url: 'https://xn--p8s937b.com/',
						escaped: false,
					},
				],
			},
		]);
	});

	test('nested content with punycode with autolinks disabled', () => {
		const input = '**[Test](https://münchen.de)** and *https://漢字.com*';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Strong,
				children: [
					{
						type: NodeType.Link,
						text: {type: NodeType.Text, content: 'Test'},
						url: 'https://xn--mnchen-3ya.de/',
						escaped: false,
					},
				],
			},
			{type: NodeType.Text, content: ' and '},
			{
				type: NodeType.Emphasis,
				children: [{type: NodeType.Text, content: 'https://漢字.com'}],
			},
		]);
	});

	test('link parser should handle bracketed text followed by link', () => {
		const input = '[1] This is a [link](https://example.com)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: '[1] This is a '},
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'link'},
				url: 'https://example.com/',
				escaped: false,
			},
		]);
	});

	test('link parser should handle adjacent bracketed text and link', () => {
		const input = '[1][link](https://example.com)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: '[1]'},
			{
				type: NodeType.Link,
				text: {type: NodeType.Text, content: 'link'},
				url: 'https://example.com/',
				escaped: false,
			},
		]);
	});

	test('domain-like link text without protocol should parse as a link', () => {
		const testCases = [
			{input: '[google.com](https://evil.com)', url: 'https://evil.com/'},
			{input: '[mail.example.com](https://phishing-site.com)', url: 'https://phishing-site.com/'},
			{input: '[my-domain.org](https://attack.com)', url: 'https://attack.com/'},
		];

		for (const {input, url} of testCases) {
			const parser = new Parser(input, ParserFlags.ALLOW_MASKED_LINKS);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: input.slice(1, input.indexOf(']'))},
					url,
					escaped: false,
				},
			]);
		}
	});

	test('various domain-like link text without protocol should still parse', () => {
		const testCases = [
			{input: '[example.com](https://example.com)', url: 'https://example.com/'},
			{input: '[example.com](https://example.com/path)', url: 'https://example.com/path'},
			{input: '[example.com/path](https://example.com/path)', url: 'https://example.com/path'},
			{input: '[www.example.com](https://example.com)', url: 'https://example.com/'},
			{input: '[example.com](https://www.example.com)', url: 'https://www.example.com/'},
		];

		for (const {input, url} of testCases) {
			const parser = new Parser(input, ParserFlags.ALLOW_MASKED_LINKS);
			const {nodes: ast} = parser.parse();
			expect(ast).toEqual([
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: input.slice(1, input.indexOf(']'))},
					url,
					escaped: false,
				},
			]);
		}
	});

	test('protocol-prefixed domain-like text with mismatched domains should be treated as text', () => {
		const inputs = [
			'[https://example.com](https://evil.com)',
			'[https://sub.example.com](https://example.com)',
			'[https://example.org](https://example.com)',
		];

		for (const input of inputs) {
			const parser = new Parser(input, ParserFlags.ALLOW_MASKED_LINKS);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: input}]);
		}
	});

	test('link with special characters', () => {
		const input = '[Aparna Nair (@disabilitystor1.bsky.social)](https://bsky.app/profile/disabilitystor1.bsky.social)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{
				type: NodeType.Link,
				text: {
					type: NodeType.Text,
					content: 'Aparna Nair (@disabilitystor1.bsky.social)',
				},
				url: 'https://bsky.app/profile/disabilitystor1.bsky.social',
				escaped: false,
			},
		]);
	});

	test('auto link with autolinks enabled', () => {
		const input = 'Check out <https://example.com>!';
		const flags = ParserFlags.ALLOW_AUTOLINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: 'Check out '},
			{
				type: NodeType.Link,
				text: undefined,
				url: 'https://example.com/',
				escaped: true,
			},
			{type: NodeType.Text, content: '!'},
		]);
	});

	test('auto link with autolinks disabled', () => {
		const input = 'Check out <https://example.com>!';
		const flags = 0;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([{type: NodeType.Text, content: 'Check out <https://example.com>!'}]);
	});

	test('nested link syntax should be treated as plain text', () => {
		const input = '[Text with [nested link](https://nested.com) content](https://example.com)';
		const flags = ParserFlags.ALLOW_MASKED_LINKS;
		const parser = new Parser(input, flags);
		const {nodes: ast} = parser.parse();

		expect(ast).toEqual([
			{type: NodeType.Text, content: '[Text with [nested link](https://nested.com) content](https://example.com)'},
		]);
	});

	describe('Autolinks Flag', () => {
		test('standard URLs with autolinks enabled', () => {
			const input = 'Visit https://example.com and http://test.org';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'Visit '},
				{type: NodeType.Link, text: undefined, url: 'https://example.com/', escaped: false},
				{type: NodeType.Text, content: ' and '},
				{type: NodeType.Link, text: undefined, url: 'http://test.org/', escaped: false},
			]);
		});

		test('standard URLs with autolinks disabled', () => {
			const input = 'Visit https://example.com and http://test.org';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: 'Visit https://example.com and http://test.org'}]);
		});

		test('angle bracket URLs with autolinks enabled', () => {
			const input = 'Visit <https://example.com> and <http://test.org>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'Visit '},
				{type: NodeType.Link, text: undefined, url: 'https://example.com/', escaped: true},
				{type: NodeType.Text, content: ' and '},
				{type: NodeType.Link, text: undefined, url: 'http://test.org/', escaped: true},
			]);
		});

		test('angle bracket URLs with autolinks disabled', () => {
			const input = 'Visit <https://example.com> and <http://test.org>';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: 'Visit <https://example.com> and <http://test.org>'}]);
		});

		test('masked link with angle brackets', () => {
			const input = '[test](<https://example.com>)';
			const flags = ParserFlags.ALLOW_MASKED_LINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'test'},
					url: 'https://example.com/',
					escaped: true,
				},
			]);
		});

		test('masked link with angle brackets and title', () => {
			const input = '[test](<https://example.com> "title")';
			const flags = ParserFlags.ALLOW_MASKED_LINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'test'},
					url: 'https://example.com/',
					escaped: true,
				},
			]);
		});

		test('escaped url parsing with quotes with autolinks enabled', () => {
			const input = 'URL parsing for both "https://google.com" and "https://google.com/"';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'URL parsing for both "'},
				{
					type: NodeType.Link,
					text: undefined,
					url: 'https://google.com/',
					escaped: true,
				},
				{type: NodeType.Text, content: '" and "'},
				{
					type: NodeType.Link,
					text: undefined,
					url: 'https://google.com/',
					escaped: true,
				},
				{type: NodeType.Text, content: '"'},
			]);
		});

		test('escaped url parsing with quotes with autolinks disabled', () => {
			const input = 'URL parsing for both "https://google.com" and "https://google.com/"';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'URL parsing for both "https://google.com" and "https://google.com/"'},
			]);
		});

		test('does NOT create masked links with quoted urls', () => {
			const input = '[Link 1]("https://google.com") and [Link 2]("https://example.org/")';
			const flags = ParserFlags.ALLOW_MASKED_LINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: '[Link 1]("https://google.com") and [Link 2]("https://example.org/")'},
			]);
		});

		test('does NOT autolink with quoted urls', () => {
			const input = 'Check <"https://google.com"> and <"https://example.org/">!';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'Check <"https://google.com"> and <"https://example.org/">!'},
			]);
		});

		test('email autolinks with autolinks enabled', () => {
			const input = 'Contact us at <info@example.com>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'Contact us at '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'info@example.com'},
					url: 'mailto:info@example.com',
					escaped: true,
				},
			]);
		});

		test('email autolinks with autolinks disabled', () => {
			const input = 'Contact us at <info@example.com>';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: 'Contact us at <info@example.com>'}]);
		});

		test('autolinks mixed with other features', () => {
			const input = '# Heading\n**Bold text** https://example.com';
			const flags = ParserFlags.ALLOW_HEADINGS | ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Heading,
					level: 1,
					children: [{type: NodeType.Text, content: 'Heading'}],
				},
				{
					type: NodeType.Strong,
					children: [{type: NodeType.Text, content: 'Bold text'}],
				},
				{type: NodeType.Text, content: ' '},
				{type: NodeType.Link, text: undefined, url: 'https://example.com/', escaped: false},
			]);
		});
	});

	describe('Email Link Parser', () => {
		test('basic email link with autolinks enabled', () => {
			const input = 'Contact me at <user@example.com>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'Contact me at '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'user@example.com'},
					url: 'mailto:user@example.com',
					escaped: true,
				},
			]);
		});

		test('basic email link with autolinks disabled', () => {
			const input = 'Contact me at <user@example.com>';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: 'Contact me at <user@example.com>'}]);
		});

		test('multiple email links with autolinks enabled', () => {
			const input = 'Contact <support@example.com> or <admin@example.com>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'Contact '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'support@example.com'},
					url: 'mailto:support@example.com',
					escaped: true,
				},
				{type: NodeType.Text, content: ' or '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'admin@example.com'},
					url: 'mailto:admin@example.com',
					escaped: true,
				},
			]);
		});

		test('multiple email links with autolinks disabled', () => {
			const input = 'Contact <support@example.com> or <admin@example.com>';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: 'Contact <support@example.com> or <admin@example.com>'}]);
		});

		test('email with complex domain with autolinks enabled', () => {
			const input = '<user@subdomain.example.co.uk>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'user@subdomain.example.co.uk'},
					url: 'mailto:user@subdomain.example.co.uk',
					escaped: true,
				},
			]);
		});

		test('email with complex domain with autolinks disabled', () => {
			const input = '<user@subdomain.example.co.uk>';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '<user@subdomain.example.co.uk>'}]);
		});

		test('invalid email format should not be parsed as email link', () => {
			const input = '<user@example>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '<user@example>'}]);
		});

		test('email inside code blocks should not be parsed', () => {
			const input = '```\n<user@example.com>\n```';
			const flags = ParserFlags.ALLOW_CODE_BLOCKS | ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.CodeBlock,
					language: undefined,
					content: '<user@example.com>\n',
				},
			]);
		});

		test('email inside inline code should not be parsed', () => {
			const input = '`<user@example.com>`';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.InlineCode, content: '<user@example.com>'}]);
		});

		test('angle brackets with non-email content', () => {
			const input = '<this is not an email>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '<this is not an email>'}]);
		});

		test('masked link for emails should be parsed with masked links flag', () => {
			const input = '[Contact me](mailto:user@example.com)';
			const flags = ParserFlags.ALLOW_MASKED_LINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'Contact me'},
					url: 'mailto:user@example.com',
					escaped: false,
				},
			]);
		});

		test('masked email links with mismatch should not parse', () => {
			const input = '[user@good.com](<user@evil.com>)';
			const flags = ParserFlags.ALLOW_MASKED_LINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '[user@good.com](<user@evil.com>)'}]);
		});

		test('email with special characters in local part with autolinks enabled', () => {
			const input = '<firstname.lastname+tag@example.com>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'firstname.lastname+tag@example.com'},
					url: 'mailto:firstname.lastname+tag@example.com',
					escaped: true,
				},
			]);
		});

		test('mixed email links and regular text with autolinks enabled', () => {
			const input = 'Contact us at <info@example.com> or visit https://example.com';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'Contact us at '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'info@example.com'},
					url: 'mailto:info@example.com',
					escaped: true,
				},
				{type: NodeType.Text, content: ' or visit '},
				{
					type: NodeType.Link,
					text: undefined,
					url: 'https://example.com/',
					escaped: false,
				},
			]);
		});

		test('email with quotes should not be parsed as email link', () => {
			const input = '<"user"@example.com>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '<"user"@example.com>'}]);
		});

		test('email with unicode characters in domain', () => {
			const input = '<user@例子.测试>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '<user@例子.测试>'}]);
		});

		test('edge case email formats with autolinks enabled', () => {
			const validInputs = [
				'<user.name@example.com>',
				'<user-name@example.co.uk>',
				'<user_name@example.io>',
				'<user123@example.dev>',
				'<12345@example.com>',
				'<user+tag@example.org>',
			];

			for (const input of validInputs) {
				const parser = new Parser(input, ParserFlags.ALLOW_AUTOLINKS);
				const {nodes: ast} = parser.parse();

				const emailWithoutBrackets = input.slice(1, -1);

				expect(ast).toEqual([
					{
						type: NodeType.Link,
						text: {type: NodeType.Text, content: emailWithoutBrackets},
						url: `mailto:${emailWithoutBrackets}`,
						escaped: true,
					},
				]);
			}
		});

		test('raw email addresses should not be auto-linked', () => {
			const input = 'Contact us at user@example.com';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: 'Contact us at user@example.com'}]);
		});

		test('mailto URLs in angle brackets should not be parsed', () => {
			const input = '<mailto:user@example.com>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '<mailto:user@example.com>'}]);
		});

		test('raw mailto URLs should not be auto-linked', () => {
			const input = 'Visit mailto:user@example.com for support';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: 'Visit mailto:user@example.com for support'}]);
		});
	});

	describe('Phone Link Parser', () => {
		test('basic phone link with autolinks enabled', () => {
			const input = 'Call <+12345678901>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'Call '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: '+12345678901'},
					url: 'tel:+12345678901',
					escaped: true,
				},
			]);
		});

		test('basic phone link with autolinks disabled', () => {
			const input = 'Call <+12345678901>';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: 'Call <+12345678901>'}]);
		});

		test('multiple phone links with autolinks enabled', () => {
			const input = 'Call <+12345678901> or <+9876543210>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'Call '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: '+12345678901'},
					url: 'tel:+12345678901',
					escaped: true,
				},
				{type: NodeType.Text, content: ' or '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: '+9876543210'},
					url: 'tel:+9876543210',
					escaped: true,
				},
			]);
		});

		test('phone with various country codes', () => {
			const validInputs = ['<+1234567890>', '<+441234567890>', '<+61234567890>', '<+8612345678901>', '<+911234567890>'];

			for (const input of validInputs) {
				const parser = new Parser(input, ParserFlags.ALLOW_AUTOLINKS);
				const {nodes: ast} = parser.parse();

				const phoneWithoutBrackets = input.slice(1, -1);

				expect(ast).toEqual([
					{
						type: NodeType.Link,
						text: {type: NodeType.Text, content: phoneWithoutBrackets},
						url: `tel:${phoneWithoutBrackets}`,
						escaped: true,
					},
				]);
			}
		});

		test('invalid phone formats should not be parsed', () => {
			const invalidInputs = ['<12345678901>', '<+abcdefghij>', '<+>'];

			for (const input of invalidInputs) {
				const parser = new Parser(input, ParserFlags.ALLOW_AUTOLINKS);
				const {nodes: ast} = parser.parse();

				expect(ast).toEqual([{type: NodeType.Text, content: input}]);
			}
		});

		test('short phone numbers (less than 7 digits) should not be parsed', () => {
			const input = '<+123>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: '<+123>'}]);
		});

		test('phone with spaces and hyphens should be parsed', () => {
			const validInputs = ['<+1-800-555-1234>', '<+44 20 1234 5678>', '<+61 4 1234 5678>', '<+1 (555) 123-4567>'];

			for (const input of validInputs) {
				const parser = new Parser(input, ParserFlags.ALLOW_AUTOLINKS);
				const {nodes: ast} = parser.parse();

				const phoneWithoutBrackets = input.slice(1, -1);
				const normalizedPhone = phoneWithoutBrackets.replace(/[\s\-()]/g, '');

				expect(ast).toEqual([
					{
						type: NodeType.Link,
						text: {type: NodeType.Text, content: phoneWithoutBrackets},
						url: `tel:${normalizedPhone}`,
						escaped: true,
					},
				]);
			}
		});

		test('phone inside code blocks should not be parsed', () => {
			const input = '```\n<+12345678901>\n```';
			const flags = ParserFlags.ALLOW_CODE_BLOCKS | ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.CodeBlock,
					language: undefined,
					content: '<+12345678901>\n',
				},
			]);
		});

		test('phone inside inline code should not be parsed', () => {
			const input = '`<+12345678901>`';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.InlineCode, content: '<+12345678901>'}]);
		});

		test('masked link for phone with formatting should normalize', () => {
			const input = '[Call me](tel:+1 (555) 123-4567)';
			const flags = ParserFlags.ALLOW_MASKED_LINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'Call me'},
					url: 'tel:+15551234567',
					escaped: false,
				},
			]);
		});

		test('raw tel: URLs should not be auto-linked', () => {
			const input = 'Call tel:+12345678901';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: 'Call tel:+12345678901'}]);
		});
	});

	describe('SMS Link Parser', () => {
		test('basic SMS link with autolinks enabled', () => {
			const input = 'Text <sms:+12345678901>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'Text '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: '+12345678901'},
					url: 'sms:+12345678901',
					escaped: true,
				},
			]);
		});

		test('basic SMS link with autolinks disabled', () => {
			const input = 'Text <sms:+12345678901>';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: 'Text <sms:+12345678901>'}]);
		});

		test('multiple SMS links with autolinks enabled', () => {
			const input = 'Text <sms:+12345678901> or <sms:+9876543210>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'Text '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: '+12345678901'},
					url: 'sms:+12345678901',
					escaped: true,
				},
				{type: NodeType.Text, content: ' or '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: '+9876543210'},
					url: 'sms:+9876543210',
					escaped: true,
				},
			]);
		});

		test('SMS with invalid phone format should not be parsed', () => {
			const invalidInputs = ['<sms:12345678901>', '<sms:+abcdefghij>', '<sms:+>'];

			for (const input of invalidInputs) {
				const parser = new Parser(input, ParserFlags.ALLOW_AUTOLINKS);
				const {nodes: ast} = parser.parse();

				expect(ast).toEqual([{type: NodeType.Text, content: input}]);
			}
		});

		test('SMS with spaces and hyphens should be parsed', () => {
			const validInputs = ['<sms:+1-800-555-1234>', '<sms:+44 20 1234 5678>', '<sms:+61 4 1234 5678>'];

			for (const input of validInputs) {
				const parser = new Parser(input, ParserFlags.ALLOW_AUTOLINKS);
				const {nodes: ast} = parser.parse();

				const fullContent = input.slice(1, -1);
				const phoneNumber = fullContent.slice(4);
				const normalizedPhone = phoneNumber.replace(/[\s-]/g, '');

				expect(ast).toEqual([
					{
						type: NodeType.Link,
						text: {type: NodeType.Text, content: phoneNumber},
						url: `sms:${normalizedPhone}`,
						escaped: true,
					},
				]);
			}
		});

		test('SMS inside code blocks should not be parsed', () => {
			const input = '```\n<sms:+12345678901>\n```';
			const flags = ParserFlags.ALLOW_CODE_BLOCKS | ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.CodeBlock,
					language: undefined,
					content: '<sms:+12345678901>\n',
				},
			]);
		});

		test('masked link for SMS with formatting should normalize', () => {
			const input = '[Text me](sms:+1 (555) 123-4567)';
			const flags = ParserFlags.ALLOW_MASKED_LINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'Text me'},
					url: 'sms:+15551234567',
					escaped: false,
				},
			]);
		});

		test('raw sms: URLs should not be auto-linked', () => {
			const input = 'Text sms:+12345678901';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([{type: NodeType.Text, content: 'Text sms:+12345678901'}]);
		});
	});

	describe('Mixed Protocol Links', () => {
		test('fluxer protocol links', () => {
			const input = `Check out <${APP_PROTOCOL_PREFIX}dev>`;
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'Check out '},
				{
					type: NodeType.Link,
					text: undefined,
					url: `${APP_PROTOCOL_PREFIX}dev`,
					escaped: true,
				},
			]);
		});

		test('multiple protocol types in one text', () => {
			const input = 'Contact me: <info@example.com>, <+12345678901>, or <sms:+12345678901>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'Contact me: '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'info@example.com'},
					url: 'mailto:info@example.com',
					escaped: true,
				},
				{type: NodeType.Text, content: ', '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: '+12345678901'},
					url: 'tel:+12345678901',
					escaped: true,
				},
				{type: NodeType.Text, content: ', or '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: '+12345678901'},
					url: 'sms:+12345678901',
					escaped: true,
				},
			]);
		});

		test('mixed masked link protocols', () => {
			const input = '[Email](mailto:user@example.com) | [Call](tel:+12345678901) | [Text](sms:+12345678901)';
			const flags = ParserFlags.ALLOW_MASKED_LINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'Email'},
					url: 'mailto:user@example.com',
					escaped: false,
				},
				{type: NodeType.Text, content: ' | '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'Call'},
					url: 'tel:+12345678901',
					escaped: false,
				},
				{type: NodeType.Text, content: ' | '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'Text'},
					url: 'sms:+12345678901',
					escaped: false,
				},
			]);
		});

		test('protocol conflicts should resolve correctly', () => {
			const input = 'See <+12345678901> and <user@example.com>';
			const flags = ParserFlags.ALLOW_AUTOLINKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toEqual([
				{type: NodeType.Text, content: 'See '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: '+12345678901'},
					url: 'tel:+12345678901',
					escaped: true,
				},
				{type: NodeType.Text, content: ' and '},
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: 'user@example.com'},
					url: 'mailto:user@example.com',
					escaped: true,
				},
			]);
		});
	});

	test('long international phone numbers', () => {
		const inputs = [
			'<+44 118 999 881 999 119 7253>',
			'<+44 118 999 881 999 119 725 3>',
			'<+1-800-555-123-456-789-0123>',
			'<+123 456 789 012 345>',
		];

		for (const input of inputs) {
			const parser = new Parser(input, ParserFlags.ALLOW_AUTOLINKS);
			const {nodes: ast} = parser.parse();

			const phoneWithoutBrackets = input.slice(1, -1);
			const normalizedPhone = phoneWithoutBrackets.replace(/[\s\-()]/g, '');

			expect(ast).toEqual([
				{
					type: NodeType.Link,
					text: {type: NodeType.Text, content: phoneWithoutBrackets},
					url: `tel:${normalizedPhone}`,
					escaped: true,
				},
			]);
		}
	});
});
