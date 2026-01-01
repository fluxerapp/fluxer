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
import {NodeType, ParserFlags, TableAlignment} from '../types/enums';
import type {InlineCodeNode, TableNode, TextNode} from '../types/nodes';

describe('Fluxer Markdown Parser', () => {
	describe('Table Parser', () => {
		test('basic table', () => {
			const input = `| Header 1 | Header 2 |
|----------|----------|
| Cell 1   | Cell 2   |
| Cell 3   | Cell 4   |`;

			const parser = new Parser(input, ParserFlags.ALLOW_TABLES);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBe(1);
			expect(ast[0].type).toBe(NodeType.Table);

			const tableNode = ast[0] as TableNode;
			expect(tableNode.header.cells.length).toBe(2);
			expect(tableNode.rows.length).toBe(2);

			expect(tableNode.header.cells[0].children[0].type).toBe(NodeType.Text);
			expect((tableNode.header.cells[0].children[0] as TextNode).content).toBe('Header 1');

			expect(tableNode.rows[0].cells[0].children[0].type).toBe(NodeType.Text);
			expect((tableNode.rows[0].cells[0].children[0] as TextNode).content).toBe('Cell 1');
		});

		test('table with alignments', () => {
			const input = `| Left | Center | Right |
|:-----|:------:|------:|
| 1    | 2      | 3     |`;

			const parser = new Parser(input, ParserFlags.ALLOW_TABLES);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBe(1);
			const tableNode = ast[0] as TableNode;

			expect(tableNode.alignments).toEqual([TableAlignment.Left, TableAlignment.Center, TableAlignment.Right]);
		});

		test('table with escaped pipes', () => {
			const input = `| Function | Integral |
|----------|----------|
| 1/x | ln\\|x\\| + C |
| tan x | -ln\\|cos x\\| + C |`;

			const parser = new Parser(input, ParserFlags.ALLOW_TABLES);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBe(1);
			expect(ast[0].type).toBe(NodeType.Table);

			const tableNode = ast[0] as TableNode;
			expect(tableNode.rows.length).toBe(2);

			const cell1 = tableNode.rows[0].cells[1].children[0] as TextNode;
			const cell2 = tableNode.rows[1].cells[1].children[0] as TextNode;

			expect(cell1.content).toBe('ln|x| + C');
			expect(cell2.content).toBe('-ln|cos x| + C');
		});

		test('table with formatted content', () => {
			const input = `| Formatting | Example |
|------------|---------|
| **Bold** | *Italic* |
| ~~Strike~~ | \`Code\` |`;

			const parser = new Parser(input, ParserFlags.ALLOW_TABLES);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBe(1);
			const tableNode = ast[0] as TableNode;

			expect(tableNode.rows[0].cells[0].children[0].type).toBe(NodeType.Strong);
			expect(tableNode.rows[0].cells[1].children[0].type).toBe(NodeType.Emphasis);
			expect(tableNode.rows[1].cells[0].children[0].type).toBe(NodeType.Strikethrough);
			expect(tableNode.rows[1].cells[1].children[0].type).toBe(NodeType.InlineCode);
		});

		test('multi-row integral table with escaped pipes', () => {
			const input = `| Funktion | Integral |
|----------|----------|
| x^n (n ≠ -1) | x^(n+1)/(n+1) + C |
| 1/x | ln\\|x\\| + C |
| e^x | e^x + C |
| a^x | a^x/ln a + C |
| sin x | -cos x + C |
| cos x | sin x + C |
| tan x | -ln\\|cos x\\| + C |
| sec²x | tan x + C |
| 1/√(1-x²) | arcsin x + C |
| 1/(1+x²) | arctan x + C |`;

			const flags = ParserFlags.ALLOW_TABLES;
			const parser = new Parser(input, flags);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBe(1);
			expect(ast[0].type).toBe(NodeType.Table);

			const tableNode = ast[0] as TableNode;
			expect(tableNode.rows.length).toBe(10);

			const row3Cell = tableNode.rows[1].cells[1].children[0] as TextNode;
			const row7Cell = tableNode.rows[6].cells[1].children[0] as TextNode;

			expect(row3Cell.content).toBe('ln|x| + C');
			expect(row7Cell.content).toBe('-ln|cos x| + C');
		});

		test('absolute value notation with escaped pipes', () => {
			const input = `| Expression | Value |
|------------|-------|
| \\|x\\| | Absolute value of x |
| \\|\\|x\\|\\| | Double absolute value |
| \\|x + y\\| | Absolute value of sum |`;

			const parser = new Parser(input, ParserFlags.ALLOW_TABLES);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBe(1);
			const tableNode = ast[0] as TableNode;
			expect(tableNode.rows.length).toBe(3);

			const cell1 = tableNode.rows[0].cells[0].children[0] as TextNode;
			const cell2 = tableNode.rows[1].cells[0].children[0] as TextNode;
			const cell3 = tableNode.rows[2].cells[0].children[0] as TextNode;

			expect(cell1.content).toBe('|x|');
			expect(cell2.content).toBe('||x||');
			expect(cell3.content).toBe('|x + y|');
		});

		test('mixed mathematical notations with escaped pipes', () => {
			const input = `| Function | Example |
|----------|---------|
| log | log\\|x\\| |
| ln | ln\\|x\\| |
| sin | sin(\\|x\\|) |
| abs | \\|f(x)\\| |
| complex | \\|x\\|² + \\|y\\|² |`;

			const parser = new Parser(input, ParserFlags.ALLOW_TABLES);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBe(1);
			const tableNode = ast[0] as TableNode;
			expect(tableNode.rows.length).toBe(5);

			expect((tableNode.rows[0].cells[1].children[0] as TextNode).content).toBe('log|x|');
			expect((tableNode.rows[1].cells[1].children[0] as TextNode).content).toBe('ln|x|');
			expect((tableNode.rows[2].cells[1].children[0] as TextNode).content).toBe('sin(|x|)');
			expect((tableNode.rows[3].cells[1].children[0] as TextNode).content).toBe('|f(x)|');
			expect((tableNode.rows[4].cells[1].children[0] as TextNode).content).toBe('|x|² + |y|²');
		});

		test('set notation with escaped pipes', () => {
			const input = `| Notation | Meaning |
|----------|---------|
| {x \\| x > 0} | Set of positive numbers |
| \\|{x \\| x > 0}\\| | Cardinality of positive numbers |
| A ∩ {x \\| \\|x\\| < 1} | Intersection with unit ball |`;

			const parser = new Parser(input, ParserFlags.ALLOW_TABLES);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBe(1);
			const tableNode = ast[0] as TableNode;
			expect(tableNode.rows.length).toBe(3);

			expect((tableNode.rows[0].cells[0].children[0] as TextNode).content).toBe('{x | x > 0}');
			expect((tableNode.rows[1].cells[0].children[0] as TextNode).content).toBe('|{x | x > 0}|');
			expect((tableNode.rows[2].cells[0].children[0] as TextNode).content).toBe('A ∩ {x | |x| < 1}');
		});

		test('table with links and code', () => {
			const input = `| Description | Example |
|-------------|---------|
| [Link](https://example.com) | \`code\` |
| **[Bold Link](https://example.org)** | *\`inline code\`* |`;

			const parser = new Parser(input, ParserFlags.ALLOW_TABLES | ParserFlags.ALLOW_MASKED_LINKS);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBe(1);
			const tableNode = ast[0] as TableNode;

			expect(tableNode.rows[0].cells[0].children[0].type).toBe(NodeType.Link);
			expect(tableNode.rows[0].cells[1].children[0].type).toBe(NodeType.InlineCode);

			expect(tableNode.rows[1].cells[0].children[0].type).toBe(NodeType.Strong);
		});

		test('invalid table formats', () => {
			const inputs = ['| Header |\n| Cell |', '| H1 |\n|====|\n| C1 |', '| |\n|--|\n| |'];

			for (const input of inputs) {
				const parser = new Parser(input, ParserFlags.ALLOW_TABLES);
				const {nodes: ast} = parser.parse();

				const isTable = ast.some((node) => node.type === NodeType.Table);
				expect(isTable).toBe(false);
			}
		});

		test('table with empty cells', () => {
			const input = `| H1 |    | H3 |
|----|----|----|
| C1 |    | C3 |`;

			const parser = new Parser(input, ParserFlags.ALLOW_TABLES);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBe(1);
			expect(ast[0].type).toBe(NodeType.Table);

			const tableNode = ast[0] as TableNode;
			const emptyCell = tableNode.rows[0].cells[1].children[0] as TextNode;
			expect(emptyCell.type).toBe(NodeType.Text);
			expect(emptyCell.content).toBe('');
		});

		test('table with inconsistent column count', () => {
			const input = `| A | B | C |
|---|---|---|
| 1 | 2 |
| 3 | 4 | 5 | 6 |`;

			const parser = new Parser(input, ParserFlags.ALLOW_TABLES);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBe(1);
			const tableNode = ast[0] as TableNode;

			expect(tableNode.rows[0].cells.length).toBe(3);
			expect(tableNode.rows[1].cells.length).toBe(3);

			const emptyCell = tableNode.rows[0].cells[2].children[0] as TextNode;
			expect(emptyCell.content).toBe('');

			const mergedCell = tableNode.rows[1].cells[2].children[0] as TextNode;
			expect(mergedCell.content.includes('5')).toBe(true);
		});

		test('table with pipes in code examples', () => {
			const input = `| Language | Pipe Example |
|----------|-------------|
| Bash | \`echo "hello" \\| grep "h"\` |
| JavaScript | \`const x = condition \\|\\| defaultValue;\` |
| C++ | \`if (a \\|\\| b && c) {...}\` |`;

			const parser = new Parser(input, ParserFlags.ALLOW_TABLES);
			const {nodes: ast} = parser.parse();

			expect(ast.length).toBe(1);
			const tableNode = ast[0] as TableNode;

			const cell1 = tableNode.rows[0].cells[1].children[0] as InlineCodeNode;
			const cell2 = tableNode.rows[1].cells[1].children[0] as InlineCodeNode;
			const cell3 = tableNode.rows[2].cells[1].children[0] as InlineCodeNode;

			expect(cell1.type).toBe(NodeType.InlineCode);
			expect(cell2.type).toBe(NodeType.InlineCode);
			expect(cell3.type).toBe(NodeType.InlineCode);

			expect(cell1.content).toContain('echo "hello" | grep "h"');
			expect(cell2.content).toContain('condition || defaultValue');
			expect(cell3.content).toContain('if (a || b && c)');
		});

		test('tables disabled', () => {
			const input = `| Header 1 | Header 2 |
|----------|----------|
| Cell 1   | Cell 2   |`;

			const parser = new Parser(input, 0);
			const {nodes: ast} = parser.parse();

			expect(ast.every((node) => node.type === NodeType.Text)).toBe(true);
		});
	});
});
