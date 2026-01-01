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
import type {CodeBlockNode, ListNode, TextNode} from '../types/nodes';

describe('Fluxer Markdown Parser - Lists', () => {
	describe('Basic list functionality', () => {
		test('unordered list', () => {
			const input = '- Item 1\n- Item 2\n- Item 3';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.List);
			expect(ast[0]).toEqual({
				type: NodeType.List,
				ordered: false,
				items: [
					{children: [{type: NodeType.Text, content: 'Item 1'}]},
					{children: [{type: NodeType.Text, content: 'Item 2'}]},
					{children: [{type: NodeType.Text, content: 'Item 3'}]},
				],
			});
		});

		test('ordered list', () => {
			const input = '1. Item 1\n2. Item 2\n3. Item 3';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.List);
			expect(ast[0]).toEqual({
				type: NodeType.List,
				ordered: true,
				items: [
					{children: [{type: NodeType.Text, content: 'Item 1'}], ordinal: 1},
					{children: [{type: NodeType.Text, content: 'Item 2'}], ordinal: 2},
					{children: [{type: NodeType.Text, content: 'Item 3'}], ordinal: 3},
				],
			});
		});

		test('mixed list types should create separate lists', () => {
			const input = '1. First\n- Unordered\n2. Second';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(3);
			expect(ast[0].type).toBe(NodeType.List);
			expect(ast[0]).toEqual({
				type: NodeType.List,
				ordered: true,
				items: [{children: [{type: NodeType.Text, content: 'First'}], ordinal: 1}],
			});
			expect(ast[1].type).toBe(NodeType.List);
			expect(ast[1]).toEqual({
				type: NodeType.List,
				ordered: false,
				items: [{children: [{type: NodeType.Text, content: 'Unordered'}]}],
			});
			expect(ast[2].type).toBe(NodeType.List);
			expect(ast[2]).toEqual({
				type: NodeType.List,
				ordered: true,
				items: [{children: [{type: NodeType.Text, content: 'Second'}], ordinal: 2}],
			});
		});

		test('list with asterisks', () => {
			const input = '* Item 1\n* Item 2\n* Item 3';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.List);
			expect(ast[0]).toEqual({
				type: NodeType.List,
				ordered: false,
				items: [
					{children: [{type: NodeType.Text, content: 'Item 1'}]},
					{children: [{type: NodeType.Text, content: 'Item 2'}]},
					{children: [{type: NodeType.Text, content: 'Item 3'}]},
				],
			});
		});
	});

	describe('Custom ordering in lists', () => {
		test('custom ordered list numbering', () => {
			const input = '1. First item\n3. Third item\n5. Fifth item';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0]).toEqual({
				type: NodeType.List,
				ordered: true,
				items: [
					{children: [{type: NodeType.Text, content: 'First item'}], ordinal: 1},
					{children: [{type: NodeType.Text, content: 'Third item'}], ordinal: 3},
					{children: [{type: NodeType.Text, content: 'Fifth item'}], ordinal: 5},
				],
			});
		});

		test('list with all same number', () => {
			const input = '1. Item one\n1. Item two\n1. Item three';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0]).toEqual({
				type: NodeType.List,
				ordered: true,
				items: [
					{children: [{type: NodeType.Text, content: 'Item one'}], ordinal: 1},
					{children: [{type: NodeType.Text, content: 'Item two'}], ordinal: 1},
					{children: [{type: NodeType.Text, content: 'Item three'}], ordinal: 1},
				],
			});
		});

		test('list starting with non-1', () => {
			const input = '5. First item\n6. Second item\n7. Third item';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0]).toEqual({
				type: NodeType.List,
				ordered: true,
				items: [
					{children: [{type: NodeType.Text, content: 'First item'}], ordinal: 5},
					{children: [{type: NodeType.Text, content: 'Second item'}], ordinal: 6},
					{children: [{type: NodeType.Text, content: 'Third item'}], ordinal: 7},
				],
			});
		});

		test('mixed pattern ordered list', () => {
			const input = '1. a\n1. b\n3. c\n4. d';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0]).toEqual({
				type: NodeType.List,
				ordered: true,
				items: [
					{children: [{type: NodeType.Text, content: 'a'}], ordinal: 1},
					{children: [{type: NodeType.Text, content: 'b'}], ordinal: 1},
					{children: [{type: NodeType.Text, content: 'c'}], ordinal: 3},
					{children: [{type: NodeType.Text, content: 'd'}], ordinal: 4},
				],
			});
		});
	});

	describe('Nested lists', () => {
		test('simple nested list', () => {
			const input = '- Parent 1\n  - Child 1\n  - Child 2\n- Parent 2';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.List);

			const listNode = ast[0] as ListNode;
			expect(listNode.items.length).toBe(2);

			expect(listNode.items[0].children.length).toBe(2);
			expect(listNode.items[0].children[0].type).toBe(NodeType.Text);
			expect(listNode.items[0].children[1].type).toBe(NodeType.List);

			const nestedList = listNode.items[0].children[1] as ListNode;
			expect(nestedList.items.length).toBe(2);

			expect(listNode.items[1].children.length).toBe(1);
			expect(listNode.items[1].children[0].type).toBe(NodeType.Text);
		});

		test('ordered list with nested unordered list', () => {
			const input = '1. First item\n   - Nested unordered 1\n   - Nested unordered 2\n2. Second item';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);

			const listNode = ast[0] as ListNode;
			expect(listNode.ordered).toBe(true);
			expect(listNode.items.length).toBe(2);

			expect(listNode.items[0].children.length).toBe(2);
			expect(listNode.items[0].children[0].type).toBe(NodeType.Text);
			expect(listNode.items[0].children[1].type).toBe(NodeType.List);
			expect(listNode.items[0].ordinal).toBe(1);

			const nestedList = listNode.items[0].children[1] as ListNode;
			expect(nestedList.ordered).toBe(false);

			expect(listNode.items[1].children.length).toBe(1);
			expect(listNode.items[1].ordinal).toBe(2);
		});

		test('unordered list with nested ordered list', () => {
			const input = '- First item\n  1. Nested ordered 1\n  2. Nested ordered 2\n- Second item';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);

			const listNode = ast[0] as ListNode;
			expect(listNode.ordered).toBe(false);

			const nestedList = listNode.items[0].children[1] as ListNode;
			expect(nestedList.ordered).toBe(true);
			expect(nestedList.items[0].ordinal).toBe(1);
			expect(nestedList.items[1].ordinal).toBe(2);
		});

		test('multi-level nesting', () => {
			const input =
				'1. Level 1\n   - Level 2\n     - Level 3\n       1. Level 4\n   - Back to level 2\n2. Back to level 1';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.List);

			const listNode = ast[0] as ListNode;

			expect(listNode.items[0].ordinal).toBe(1);

			const level2List = listNode.items[0].children[1] as ListNode;
			expect(level2List.ordered).toBe(false);

			const level3List = level2List.items[0].children[1] as ListNode;
			expect(level3List.ordered).toBe(false);

			const level4List = level3List.items[0].children[1] as ListNode;
			expect(level4List.ordered).toBe(true);
			expect(level4List.items[0].ordinal).toBe(1);

			expect(listNode.items[1].ordinal).toBe(2);
		});
	});

	describe('Lists with other content', () => {
		test('list with formatted text', () => {
			const input = '- **Bold item**\n- *Italic item*\n- `Code item`';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);

			const listNode = ast[0] as ListNode;
			expect(listNode.items[0].children[0].type).toBe(NodeType.Strong);

			expect(listNode.items[1].children[0].type).toBe(NodeType.Emphasis);

			expect(listNode.items[2].children[0].type).toBe(NodeType.InlineCode);
		});

		test('list with blank lines between paragraphs', () => {
			const input = '1. First paragraph\n\n   Second paragraph\n2. Another item';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(3);

			expect(ast[0].type).toBe(NodeType.List);
			const firstList = ast[0] as ListNode;
			expect(firstList.items.length).toBe(1);
			expect(firstList.items[0].children[0].type).toBe(NodeType.Text);
			expect((firstList.items[0].children[0] as TextNode).content).toBe('First paragraph');

			expect(ast[1].type).toBe(NodeType.Text);

			expect(ast[2].type).toBe(NodeType.List);
			const secondList = ast[2] as ListNode;
			expect(secondList.items.length).toBe(1);
			expect(secondList.items[0].children[0].type).toBe(NodeType.Text);
			expect((secondList.items[0].children[0] as TextNode).content).toBe('Another item');
		});

		test('list before and after paragraph', () => {
			const input = '- List item 1\n- List item 2\n\nParagraph text\n\n- List item 3\n- List item 4';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(3);
			expect(ast[0].type).toBe(NodeType.List);
			expect(ast[1].type).toBe(NodeType.Text);
			expect(ast[2].type).toBe(NodeType.List);

			expect((ast[1] as TextNode).content).toBe('\nParagraph text\n\n');
		});
	});

	describe('Lists with code blocks', () => {
		test('list with code block', () => {
			const input =
				'1. Item with code block:\n' +
				'    ```\n' +
				'    function example() {\n' +
				'      return "test";\n' +
				'    }\n' +
				'    ```\n' +
				'2. Next item';

			const flags = ParserFlags.ALLOW_LISTS | ParserFlags.ALLOW_CODE_BLOCKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);

			const listNode = ast[0] as ListNode;
			expect(listNode.items.length).toBe(2);

			expect(listNode.items[0].children.length).toBe(2);
			expect(listNode.items[0].children[0].type).toBe(NodeType.Text);
			expect(listNode.items[0].children[1].type).toBe(NodeType.CodeBlock);

			const codeBlock = listNode.items[0].children[1] as CodeBlockNode;
			expect(codeBlock.content).toContain('function example()');

			expect(listNode.items[1].children.length).toBe(1);
			expect(listNode.items[1].children[0].type).toBe(NodeType.Text);
		});

		test('code block with language specified', () => {
			const input =
				'- Item with JavaScript:\n' + '  ```javascript\n' + '  const x = 42;\n' + '  console.log(x);\n' + '  ```';

			const flags = ParserFlags.ALLOW_LISTS | ParserFlags.ALLOW_CODE_BLOCKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			const listNode = ast[0] as ListNode;
			const codeBlock = listNode.items[0].children[1] as CodeBlockNode;

			expect(codeBlock.language).toBe('javascript');
			expect(codeBlock.content).toContain('const x = 42;');
		});

		test('list with multiple code blocks', () => {
			const input =
				'1. First code block:\n' +
				'   ```\n' +
				'   Block 1\n' +
				'   ```\n' +
				'2. Second code block:\n' +
				'   ```\n' +
				'   Block 2\n' +
				'   ```';

			const flags = ParserFlags.ALLOW_LISTS | ParserFlags.ALLOW_CODE_BLOCKS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			const listNode = ast[0] as ListNode;

			expect(listNode.items.length).toBe(2);
			expect(listNode.items[0].children[1].type).toBe(NodeType.CodeBlock);
			expect(listNode.items[1].children[1].type).toBe(NodeType.CodeBlock);

			const firstCodeBlock = listNode.items[0].children[1] as CodeBlockNode;
			const secondCodeBlock = listNode.items[1].children[1] as CodeBlockNode;

			expect(firstCodeBlock.content).toContain('Block 1');
			expect(secondCodeBlock.content).toContain('Block 2');
		});
	});

	describe('Edge cases and special scenarios', () => {
		test('deeply nested list beyond max depth (9 levels)', () => {
			const input =
				'* fdf\n  * dffsdf\n    * dfsdfs\n      * fdfsf\n        * test\n          * test2\n            * test3\n              * test4\n                * test5';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.List);

			const listNode = ast[0] as ListNode;
			expect(listNode.ordered).toBe(false);
			expect(listNode.items.length).toBe(1);

			let currentLevel: ListNode = listNode;
			const expectedContents = ['fdf', 'dffsdf', 'dfsdfs', 'fdfsf', 'test', 'test2', 'test3', 'test4', 'test5'];

			for (let i = 0; i < expectedContents.length; i++) {
				expect(currentLevel.items.length).toBeGreaterThan(0);
				const item = currentLevel.items[0];
				expect(item.children.length).toBeGreaterThan(0);

				const textNode = item.children[0] as TextNode;
				expect(textNode.type).toBe(NodeType.Text);
				expect(textNode.content).toBe(expectedContents[i]);

				if (i < expectedContents.length - 1) {
					expect(item.children.length).toBe(2);
					expect(item.children[1].type).toBe(NodeType.List);
					currentLevel = item.children[1] as ListNode;
				} else {
					expect(item.children.length).toBe(1);
				}
			}
		});

		test('list with invalid indentation', () => {
			const input = '1. First\n - Invalid subitem with 1 space\n2. Second';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.List);

			const listNode = ast[0] as ListNode;
			expect(listNode.items[0].children.length).toBe(2);
			expect(listNode.items[0].children[1].type).toBe(NodeType.Text);
			expect((listNode.items[0].children[1] as TextNode).content).toContain('Invalid subitem');
		});

		test('empty list items', () => {
			const input = '- \n- Item 2\n- ';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);

			const listNode = ast[0] as ListNode;
			expect(listNode.items.length).toBe(3);

			expect(listNode.items[1].children.length).toBeGreaterThan(0);
			expect(listNode.items[1].children[0].type).toBe(NodeType.Text);
			expect((listNode.items[1].children[0] as TextNode).content).toBe('Item 2');
		});

		test('list with very large numbers', () => {
			const input = '9999999. First item\n10000000. Second item';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);

			const listNode = ast[0] as ListNode;
			expect(listNode.items[0].ordinal).toBe(9999999);
			expect(listNode.items[1].ordinal).toBe(10000000);
		});

		test('lists disabled by parser flags', () => {
			const input = '- Item 1\n- Item 2';
			const flags = 0;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.Text);

			expect((ast[0] as TextNode).content).toBe('- Item 1- Item 2');
		});
	});

	describe('Interaction with other block elements', () => {
		test('list adjacent to heading', () => {
			const input = '# Heading\n- List item\n## Next heading';
			const flags = ParserFlags.ALLOW_HEADINGS | ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(3);
			expect(ast[0].type).toBe(NodeType.Heading);
			expect(ast[1].type).toBe(NodeType.List);
			expect(ast[2].type).toBe(NodeType.Heading);
		});

		test('list adjacent to blockquote', () => {
			const input = '> Blockquote\n- List item\n> Another blockquote';
			const flags = ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(3);
			expect(ast[0].type).toBe(NodeType.Blockquote);
			expect(ast[1].type).toBe(NodeType.List);
			expect(ast[2].type).toBe(NodeType.Blockquote);
		});

		test('nested list with blank lines between items', () => {
			const input = '- Parent item\n\n  - Child item 1\n\n  - Child item 2';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBeGreaterThan(0);
			expect(ast[0].type).toBe(NodeType.List);
		});

		test('empty list continuation handling', () => {
			const input = '- Item 1\n  Continuation text\n- Item 2';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.List);

			const listNode = ast[0] as ListNode;
			expect(listNode.items).toHaveLength(2);
			expect(listNode.items[0].children).toHaveLength(2);
			expect(listNode.items[0].children[1]).toEqual({
				type: NodeType.Text,
				content: 'Continuation text',
			});
		});

		test('list with multiple continuation lines', () => {
			const input = '- Item 1\n  Line 2\n  Line 3\n- Item 2';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.List);

			const listNode = ast[0] as ListNode;
			expect(listNode.items[0].children).toHaveLength(3);
		});

		test('deeply nested list structure', () => {
			const input = '- Level 1\n  - Level 2\n    - Level 3\n      Text continuation';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.List);
		});

		test('list continuation with empty items array', () => {
			const input = 'Some text\nContinuation line\n- Actual list item';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(2);
			expect(ast[0].type).toBe(NodeType.Text);
			expect(ast[1].type).toBe(NodeType.List);
		});

		test('invalid list marker patterns', () => {
			const invalidPatterns = [
				'1 Not a list item',
				'1.Not a list item',
				'- ',
				'1. ',
				'  -Not a list',
				'  1.Not a list',
			];

			for (const pattern of invalidPatterns) {
				const flags = ParserFlags.ALLOW_LISTS;
				const parser = new Parser(pattern, flags);
				const {nodes: ast} = parser.parse();

				if (pattern.trim() === '-' || pattern.trim() === '1.') {
					continue;
				}
				expect(ast[0].type).toBe(NodeType.Text);
			}
		});

		test('complex list with nested bullets and formatting', () => {
			const input =
				'1. **Bold list**\n2. *Italic list*\n3. - Nested bullet\n   * **Bold nested**\n4. Text with\n   line breaks  \n   **and bold**';
			const flags = ParserFlags.ALLOW_LISTS;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast).toHaveLength(1);
			expect(ast[0].type).toBe(NodeType.List);

			const listNode = ast[0] as ListNode;
			expect(listNode.ordered).toBe(true);
			expect(listNode.items.length).toBe(4);

			expect(listNode.items[0].ordinal).toBe(1);
			expect(listNode.items[0].children.length).toBe(1);
			expect(listNode.items[0].children[0].type).toBe(NodeType.Strong);

			expect(listNode.items[1].ordinal).toBe(2);
			expect(listNode.items[1].children.length).toBe(1);
			expect(listNode.items[1].children[0].type).toBe(NodeType.Emphasis);

			expect(listNode.items[2].ordinal).toBe(3);
			expect(listNode.items[2].children.length).toBe(1);
			expect(listNode.items[2].children[0].type).toBe(NodeType.List);

			const nestedList = listNode.items[2].children[0] as ListNode;
			expect(nestedList.ordered).toBe(false);
			expect(nestedList.items.length).toBe(2);

			expect(nestedList.items[0].children.length).toBe(1);
			expect(nestedList.items[0].children[0].type).toBe(NodeType.Text);
			expect((nestedList.items[0].children[0] as TextNode).content).toBe('Nested bullet');

			expect(nestedList.items[1].children.length).toBe(1);
			expect(nestedList.items[1].children[0].type).toBe(NodeType.Strong);

			expect(listNode.items[3].ordinal).toBe(4);
			expect(listNode.items[3].children.length).toBeGreaterThan(1);

			const item4Children = listNode.items[3].children;
			expect(item4Children.some((child) => child.type === NodeType.Text)).toBe(true);
			expect(item4Children.some((child) => child.type === NodeType.Strong)).toBe(true);
		});
	});
});
