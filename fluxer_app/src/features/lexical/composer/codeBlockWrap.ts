// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type CodeBlockBody,
	FENCE_LANGUAGE_RE,
	hasVisibleContent,
	scanCodeBlocks,
} from '@app/features/lexical/composer/codeBlockIndent';

const MIN_FENCE_LENGTH = 3;

interface CodeBlockWrapEdit {
	start: number;
	end: number;
	text: string;
}

export interface CodeBlockWrapPlan {
	wrapped: boolean;
	opening: CodeBlockWrapEdit;
	closing: CodeBlockWrapEdit;
	selectionStart: number;
	selectionEnd: number;
}

interface FencedBlock {
	start: number;
	contentStart: number;
	contentEnd: number;
	end: number;
}

function isLineStart(text: string, offset: number): boolean {
	return offset === 0 || text[offset - 1] === '\n';
}

function isLineEnd(text: string, offset: number): boolean {
	return offset === text.length || text[offset] === '\n';
}

function openingFenceFor(text: string, offset: number): string {
	let start = offset;
	while (text[start - 1] === '`') {
		start -= 1;
	}
	return text.slice(start, offset);
}

function closingFenceEnd(text: string, offset: number, fence: string): number {
	let end = offset + fence.length;
	if (text.slice(offset, end) !== fence) {
		return -1;
	}
	while (text[end] === ' ' || text[end] === '\t' || text[end] === '\r') {
		end += 1;
	}
	return isLineEnd(text, end) ? end : -1;
}

function isInsideClosedBlock(text: string, bodies: ReadonlyArray<CodeBlockBody>, offset: number): boolean {
	return bodies.some(
		(body) =>
			body.start <= offset && offset <= body.end && text.startsWith(openingFenceFor(text, body.start), body.end),
	);
}

function findFencedBlocks(text: string, bodies: ReadonlyArray<CodeBlockBody>): Array<FencedBlock> {
	return bodies.flatMap((body) => {
		const fence = openingFenceFor(text, body.start);
		const start = body.start - fence.length;
		const openingEnd = text.indexOf('\n', body.start);
		const info = text.slice(body.start, openingEnd);
		const end = closingFenceEnd(text, body.end, fence);
		const fenced =
			body.rendered &&
			end >= 0 &&
			isLineStart(text, start) &&
			text[body.end - 1] === '\n' &&
			(info === '' || FENCE_LANGUAGE_RE.test(info));
		return fenced ? [{start, contentStart: openingEnd + 1, contentEnd: body.end - 1, end}] : [];
	});
}

function fenceFor(content: string): string {
	let length = MIN_FENCE_LENGTH;
	for (const [run] of content.matchAll(/`+/g)) {
		length = Math.max(length, run.length + 1);
	}
	return '`'.repeat(length);
}

export function planCodeBlockWrap(text: string, start: number, end: number): CodeBlockWrapPlan | null {
	let contentStart = start;
	while (contentStart < end && text[contentStart] === '\n') {
		contentStart += 1;
	}
	let contentEnd = end;
	while (contentEnd > contentStart && text[contentEnd - 1] === '\n') {
		contentEnd -= 1;
	}
	if (contentStart === contentEnd) {
		return null;
	}
	const bodies = scanCodeBlocks(text);
	const block = findFencedBlocks(text, bodies).find(
		(candidate) =>
			(candidate.contentStart === contentStart && candidate.contentEnd === contentEnd) ||
			(candidate.start === contentStart && candidate.end === contentEnd),
	);
	if (block != null) {
		return {
			wrapped: true,
			opening: {start: block.start, end: block.contentStart, text: ''},
			closing: {start: block.contentEnd, end: block.end, text: ''},
			selectionStart: block.start,
			selectionEnd: block.start + block.contentEnd - block.contentStart,
		};
	}
	const content = text.slice(contentStart, contentEnd);
	if (
		!text.slice(start, end).includes('\n') ||
		!hasVisibleContent(content) ||
		isInsideClosedBlock(text, bodies, contentStart) ||
		isInsideClosedBlock(text, bodies, contentEnd)
	) {
		return null;
	}
	const fence = fenceFor(content);
	const opening = `${isLineStart(text, contentStart) ? '' : '\n'}${fence}\n`;
	const closing = `\n${fence}${isLineEnd(text, contentEnd) ? '' : '\n'}`;
	return {
		wrapped: false,
		opening: {start: contentStart, end: contentStart, text: opening},
		closing: {start: contentEnd, end: contentEnd, text: closing},
		selectionStart: contentStart + opening.length,
		selectionEnd: contentEnd + opening.length,
	};
}
