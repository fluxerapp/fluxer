// SPDX-License-Identifier: AGPL-3.0-or-later

const INDENT = '\t';
const SPACE_INDENT_WIDTH = 4;
const FENCE_RE = /^ {0,3}`{3,}/;

export interface CodeIndentEdit {
	start: number;
	end: number;
	text: string;
}

export interface CodeIndentPlan {
	edits: Array<CodeIndentEdit>;
	selectionStart: number;
	selectionEnd: number;
}

interface PhysicalLine {
	start: number;
	end: number;
	text: string;
	isCodeContent: boolean;
}

function scanLines(text: string): Array<PhysicalLine> {
	const lines: Array<PhysicalLine> = [];
	let open = false;
	let pos = 0;
	for (const content of text.split('\n')) {
		const fenceMatch = FENCE_RE.exec(content);
		const rest = fenceMatch == null ? '' : content.slice(fenceMatch[0].length);
		const isFence = fenceMatch != null && (!open || /^\s*$/.test(rest) || rest.includes('```'));
		lines.push({start: pos, end: pos + content.length, text: content, isCodeContent: !isFence && open});
		if (isFence) {
			open = !open;
		}
		pos += content.length + 1;
	}
	return lines;
}

function lineAt(lines: Array<PhysicalLine>, offset: number): number {
	for (let i = 0; i < lines.length; i += 1) {
		if (offset >= lines[i]!.start && offset <= lines[i]!.end) {
			return i;
		}
	}
	return lines.length - 1;
}

export function analyzeCodeIndent(
	text: string,
	selectionStart: number,
	selectionEnd: number,
	unindent: boolean,
): CodeIndentPlan | null {
	const lines = scanLines(text);
	const startLine = lineAt(lines, selectionStart);
	let endLine = lineAt(lines, selectionEnd);
	if (selectionEnd > selectionStart && selectionEnd === lines[endLine]!.start && endLine > 0) {
		endLine -= 1;
	}
	for (let i = startLine; i <= endLine; i += 1) {
		if (!lines[i]!.isCodeContent) {
			return null;
		}
	}
	if (!unindent && selectionStart === selectionEnd) {
		return {
			edits: [{start: selectionStart, end: selectionStart, text: INDENT}],
			selectionStart: selectionStart + INDENT.length,
			selectionEnd: selectionStart + INDENT.length,
		};
	}
	const edits: Array<CodeIndentEdit> = [];
	let startDelta = 0;
	let endDelta = 0;
	for (let i = startLine; i <= endLine; i += 1) {
		const line = lines[i]!;
		if (unindent) {
			let removed = 0;
			if (line.text.startsWith(INDENT)) {
				removed = 1;
			} else {
				while (removed < SPACE_INDENT_WIDTH && line.text[removed] === ' ') {
					removed += 1;
				}
			}
			if (removed === 0) {
				continue;
			}
			edits.push({start: line.start, end: line.start + removed, text: ''});
			startDelta -= Math.min(removed, Math.max(0, selectionStart - line.start));
			endDelta -= Math.min(removed, Math.max(0, selectionEnd - line.start));
		} else {
			edits.push({start: line.start, end: line.start, text: INDENT});
			if (line.start <= selectionStart) {
				startDelta += INDENT.length;
			}
			if (line.start <= selectionEnd) {
				endDelta += INDENT.length;
			}
		}
	}
	if (edits.length === 0) {
		return null;
	}
	return {
		edits,
		selectionStart: Math.max(0, selectionStart + startDelta),
		selectionEnd: Math.max(0, selectionEnd + endDelta),
	};
}
