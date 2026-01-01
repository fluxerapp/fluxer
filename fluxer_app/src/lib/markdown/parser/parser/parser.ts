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

import * as BlockParsers from '../parsers/block-parsers';
import {applyTextPresentation} from '../parsers/emoji-parsers';
import * as InlineParsers from '../parsers/inline-parsers';
import * as ListParsers from '../parsers/list-parsers';
import {MAX_AST_NODES, MAX_LINE_LENGTH, MAX_LINES} from '../types/constants';
import {NodeType, ParserFlags} from '../types/enums';
import type {Node} from '../types/nodes';
import * as ASTUtils from '../utils/ast-utils';

export class Parser {
	private readonly lines: Array<string>;
	private currentLineIndex: number;
	private readonly totalLineCount: number;
	private readonly parserFlags: number;
	private nodeCount: number;

	constructor(input: string, flags: number) {
		if (!input || input === '') {
			this.lines = [];
			this.currentLineIndex = 0;
			this.totalLineCount = 0;
			this.parserFlags = flags;
			this.nodeCount = 0;
			return;
		}

		const lines = input.split('\n');
		if (lines.length > MAX_LINES) {
			lines.length = MAX_LINES;
		}

		if (lines.length === 1 && lines[0] === '') {
			this.lines = [];
		} else {
			this.lines = lines;
		}

		this.currentLineIndex = 0;
		this.totalLineCount = this.lines.length;
		this.parserFlags = flags;
		this.nodeCount = 0;
	}

	parse(): {nodes: Array<Node>} {
		const ast: Array<Node> = [];
		if (this.totalLineCount === 0) {
			return {nodes: ast};
		}

		while (this.currentLineIndex < this.totalLineCount && this.nodeCount <= MAX_AST_NODES) {
			const line = this.lines[this.currentLineIndex];
			let lineLength = line.length;
			if (lineLength > MAX_LINE_LENGTH) {
				this.lines[this.currentLineIndex] = line.slice(0, MAX_LINE_LENGTH);
				lineLength = MAX_LINE_LENGTH;
			}

			const trimmedLine = line.trimStart();
			if (trimmedLine === '') {
				const blankLineCount = this.countBlankLines(this.currentLineIndex);
				if (ast.length > 0 && this.currentLineIndex + blankLineCount < this.totalLineCount) {
					const nextLine = this.lines[this.currentLineIndex + blankLineCount];
					const nextTrimmed = nextLine.trimStart();

					const isNextHeading = nextTrimmed
						? BlockParsers.parseHeading(nextTrimmed, (text) => InlineParsers.parseInline(text, this.parserFlags)) !==
							null
						: false;

					const isPreviousHeading = ast[ast.length - 1]?.type === NodeType.Heading;

					if (!isNextHeading && !isPreviousHeading) {
						const newlines = '\n'.repeat(blankLineCount);
						ast.push({type: NodeType.Text, content: newlines});
						this.nodeCount++;
					}
				}
				this.currentLineIndex += blankLineCount;
				continue;
			}

			const blockResult = BlockParsers.parseBlock(
				this,
				this.lines,
				this.currentLineIndex,
				this.parserFlags,
				this.nodeCount,
			);

			if (blockResult.node) {
				ast.push(blockResult.node);
				if (blockResult.extraNodes) {
					for (const extraNode of blockResult.extraNodes) {
						ast.push(extraNode);
					}
				}
				this.currentLineIndex = blockResult.newLineIndex;
				this.nodeCount = blockResult.newNodeCount;
				continue;
			}

			this.parseInlineLine(ast);
			this.currentLineIndex++;
		}

		ASTUtils.flattenAST(ast);

		for (const node of ast) {
			applyTextPresentation(node);
		}

		return {nodes: ast};
	}

	private countBlankLines(startLine: number): number {
		let count = 0;
		let current = startLine;
		while (current < this.totalLineCount && this.lines[current].trim() === '') {
			count++;
			current++;
		}
		return count;
	}

	private parseInlineLine(ast: Array<Node>): void {
		let text = this.lines[this.currentLineIndex];
		let linesConsumed = 1;

		while (this.currentLineIndex + linesConsumed < this.totalLineCount) {
			const nextLine = this.lines[this.currentLineIndex + linesConsumed];
			const trimmedNext = nextLine.trimStart();

			if (this.isBlockStart(trimmedNext)) {
				break;
			}

			if (trimmedNext === '') {
				break;
			}

			text += `\n${nextLine}`;
			linesConsumed++;
		}

		if (this.currentLineIndex + linesConsumed < this.totalLineCount) {
			const nextLine = this.lines[this.currentLineIndex + linesConsumed];
			const trimmedNext = nextLine.trimStart();
			const isNextLineHeading = trimmedNext.startsWith('#') && !trimmedNext.startsWith('-#');
			const isNextLineBlockquote = trimmedNext.startsWith('>');
			if (trimmedNext === '' || (!isNextLineHeading && !isNextLineBlockquote)) {
				text += '\n';
			}
		}

		const inlineNodes = InlineParsers.parseInline(text, this.parserFlags);

		for (const node of inlineNodes) {
			ast.push(node);
			this.nodeCount++;
			if (this.nodeCount > MAX_AST_NODES) break;
		}

		this.currentLineIndex += linesConsumed - 1;
	}

	private isBlockStart(line: string): boolean {
		return !!(
			line.startsWith('#') ||
			(this.parserFlags & ParserFlags.ALLOW_SUBTEXT && line.startsWith('-#')) ||
			(this.parserFlags & ParserFlags.ALLOW_CODE_BLOCKS && line.startsWith('```')) ||
			(this.parserFlags & ParserFlags.ALLOW_LISTS && ListParsers.matchListItem(line) != null) ||
			(this.parserFlags & (ParserFlags.ALLOW_BLOCKQUOTES | ParserFlags.ALLOW_MULTILINE_BLOCKQUOTES) &&
				(line.startsWith('>') || line.startsWith('>>> ')))
		);
	}
}
