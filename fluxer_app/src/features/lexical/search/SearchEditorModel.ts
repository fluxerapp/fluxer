// SPDX-License-Identifier: AGPL-3.0-or-later

import {$createSearchFilterNode} from '@app/features/lexical/nodes/SearchFilterNode';
import {tokenize} from '@app/features/search/utils/SearchQueryParser';
import {
	$createParagraphNode,
	$createRangeSelection,
	$createTextNode,
	$getRoot,
	$getSelection,
	$isRangeSelection,
	$setSelection,
	COMMAND_PRIORITY_HIGH,
	COMPOSITION_END_COMMAND,
	type ElementNode,
	HISTORY_MERGE_TAG,
	type LexicalEditor,
	type LexicalNode,
	LineBreakNode,
	type PointType,
	TextNode,
} from 'lexical';

const SEARCH_QUERY_QUOTE_CHARS = new Set(['"', '\u201c', '\u201d', '\u201f', '\u2033', '\u00ab', '\u00bb']);

export const SearchSelectionDirection = Object.freeze({
	BACKWARD: 'backward',
	FORWARD: 'forward',
	NONE: 'none',
} as const);

export type SearchSelectionDirection = (typeof SearchSelectionDirection)[keyof typeof SearchSelectionDirection];

function $ensureRootParagraph(): ElementNode {
	const root = $getRoot();
	const first = root.getFirstChild();
	if (first != null && first.getType() === 'paragraph') {
		return first as ElementNode;
	}
	const paragraph = $createParagraphNode();
	root.clear();
	root.append(paragraph);
	return paragraph;
}

export interface SearchSelectionRange {
	start: number;
	end: number;
	direction: SearchSelectionDirection;
}

export interface ResolveSearchFilterPointerBoundaryRequest {
	readonly direction: 'ltr' | 'rtl';
	readonly pointerX: number;
	readonly left: number;
	readonly width: number;
}

interface SetNonTextPointAtOffsetRequest {
	readonly point: PointType;
	readonly paragraph: ElementNode;
	readonly children: ReadonlyArray<LexicalNode>;
	readonly index: number;
	readonly offset: number;
	readonly accumulated: number;
	readonly end: number;
}

interface SetPointAtChildBoundaryRequest {
	readonly point: PointType;
	readonly paragraph: ElementNode;
	readonly children: ReadonlyArray<LexicalNode>;
	readonly index: number;
}

export const SearchFilterPointerBoundary = Object.freeze({
	BEFORE: 'before',
	AFTER: 'after',
} as const);

export type SearchFilterPointerBoundary =
	(typeof SearchFilterPointerBoundary)[keyof typeof SearchFilterPointerBoundary];

function resolveSearchSelectionDirection(anchor: number, focus: number): SearchSelectionDirection {
	if (anchor === focus) {
		return SearchSelectionDirection.NONE;
	}
	if (anchor > focus) {
		return SearchSelectionDirection.BACKWARD;
	}
	return SearchSelectionDirection.FORWARD;
}

function resolveSearchCaretOffset(selection: number | SearchSelectionRange | null): number | null {
	if (typeof selection === 'number') {
		return selection;
	}
	if (selection == null) {
		return null;
	}
	if (selection.direction === SearchSelectionDirection.BACKWARD) {
		return selection.start;
	}
	return selection.end;
}

export function resolveSearchFilterPointerBoundary({
	direction,
	pointerX,
	left,
	width,
}: ResolveSearchFilterPointerBoundaryRequest): SearchFilterPointerBoundary {
	const isVisualLeft = pointerX < left + width / 2;
	let isLogicalBefore: boolean;
	if (direction === 'rtl') {
		isLogicalBefore = !isVisualLeft;
	} else {
		isLogicalBefore = isVisualLeft;
	}
	if (isLogicalBefore) {
		return SearchFilterPointerBoundary.BEFORE;
	}
	return SearchFilterPointerBoundary.AFTER;
}

function normalizeSearchInputText(text: string): string {
	return text.replace(/\r\n?|\n|\u2028|\u2029/g, ' ');
}

export function $insertSearchText(text: string): boolean {
	const selection = $getSelection();
	if (!$isRangeSelection(selection)) {
		return false;
	}
	selection.insertText(normalizeSearchInputText(text));
	return true;
}

function stripQuotes(value: string): string {
	const opening = value[0];
	if (opening == null || !SEARCH_QUERY_QUOTE_CHARS.has(opening)) {
		return value;
	}
	const closing = value[value.length - 1];
	const hasClosing = value.length >= 2 && closing != null && SEARCH_QUERY_QUOTE_CHARS.has(closing);
	if (hasClosing) {
		return value.slice(1, -1);
	}
	return value.slice(1);
}

function $buildParagraphChildren(query: string, selection: number | SearchSelectionRange | null): Array<LexicalNode> {
	const {tokens} = tokenize(query);
	const children: Array<LexicalNode> = [];
	let pos = 0;
	const pushText = (text: string) => {
		if (text.length > 0) {
			children.push($createTextNode(text));
		}
	};
	for (const tok of tokens) {
		if (tok.start > pos) {
			pushText(query.slice(pos, tok.start));
		}
		const raw = query.slice(tok.start, tok.end);
		let selectionCutsToken: boolean;
		if (typeof selection === 'number') {
			selectionCutsToken = selection > tok.start && selection <= tok.end;
		} else {
			selectionCutsToken =
				selection != null &&
				((selection.start > tok.start && selection.start < tok.end) ||
					(selection.end > tok.start && selection.end < tok.end));
		}
		const nextChar = query[tok.end];
		const terminated = nextChar == null || nextChar === ' ';
		if (selectionCutsToken || !terminated) {
			pushText(raw);
		} else {
			children.push(
				$createSearchFilterNode({
					raw,
					filterKey: tok.key,
					value: stripQuotes(tok.value),
					exclude: tok.exclude,
				}),
			);
		}
		pos = tok.end;
	}
	if (pos < query.length) {
		pushText(query.slice(pos));
	}
	return children;
}

export function $getSearchQuery(): string {
	return $getRoot()
		.getTextContent()
		.replace(/[\r\n\u2028\u2029]/g, ' ');
}

function $pillifyTextNode(node: TextNode): void {
	const text = node.getTextContent();
	if (!text.includes(':')) {
		return;
	}
	const {tokens} = tokenize(text);
	if (tokens.length === 0) {
		return;
	}
	const selectionOffsets = readSearchTextNodeSelectionOffsets(node);
	for (const tok of tokens) {
		if (text[tok.end] !== ' ') {
			continue;
		}
		if (selectionOffsets.some((offset) => offset > tok.start && offset <= tok.end)) {
			continue;
		}
		const segments = node.splitText(tok.start, tok.end);
		let tokenIndex = 0;
		if (tok.start > 0) {
			tokenIndex = 1;
		}
		const tokenNode = segments[tokenIndex];
		if (tokenNode == null) {
			return;
		}
		const raw = text.slice(tok.start, tok.end);
		tokenNode.replace(
			$createSearchFilterNode({
				raw,
				filterKey: tok.key,
				value: stripQuotes(tok.value),
				exclude: tok.exclude,
			}),
		);
		return;
	}
}

function readSearchTextNodeSelectionOffsets(node: TextNode): Array<number> {
	const selection = $getSelection();
	const selectionOffsets: Array<number> = [];
	if (!$isRangeSelection(selection)) {
		return selectionOffsets;
	}
	for (const point of [selection.anchor, selection.focus]) {
		if (point.type === 'text' && point.getNode().getKey() === node.getKey()) {
			selectionOffsets.push(point.offset);
		}
	}
	return selectionOffsets;
}

function refreshSearchPillsAfterComposition(editor: LexicalEditor): void {
	queueMicrotask(() => {
		editor.update(
			() => {
				for (const textNode of $getRoot().getAllTextNodes()) {
					textNode.markDirty();
				}
			},
			{discrete: true, tag: HISTORY_MERGE_TAG},
		);
	});
}

function $normalizeSearchTextNode(node: TextNode): boolean {
	const text = node.getTextContent();
	const normalized = normalizeSearchInputText(text);
	if (normalized === text) {
		return false;
	}
	node.setTextContent(normalized);
	return true;
}

export function registerSearchPillTransform(editor: LexicalEditor): () => void {
	const unregisterTransform = editor.registerNodeTransform(TextNode, (node) => {
		if (editor.isComposing()) {
			return;
		}
		if ($normalizeSearchTextNode(node)) {
			return;
		}
		$pillifyTextNode(node);
	});
	const unregisterLineBreak = editor.registerNodeTransform(LineBreakNode, (node) => {
		node.replace($createTextNode(' '));
	});
	const unregisterCompositionEnd = editor.registerCommand(
		COMPOSITION_END_COMMAND,
		() => {
			refreshSearchPillsAfterComposition(editor);
			return false;
		},
		COMMAND_PRIORITY_HIGH,
	);
	return () => {
		unregisterCompositionEnd();
		unregisterLineBreak();
		unregisterTransform();
	};
}

function $getPointOffset(point: PointType, paragraph: ElementNode): number | null {
	const children = paragraph.getChildren();
	if (point.type === 'element') {
		const pointNode = point.getNode();
		if (!pointNode.is(paragraph)) {
			return null;
		}
		let acc = 0;
		for (const [index, child] of children.entries()) {
			if (index >= point.offset) {
				break;
			}
			acc += child.getTextContent().length;
		}
		return acc;
	}
	const pointNode = point.getNode();
	let acc = 0;
	for (const child of children) {
		if (child.getKey() === pointNode.getKey()) {
			return acc + point.offset;
		}
		acc += child.getTextContent().length;
	}
	return null;
}

export function $getSelectionRange(): SearchSelectionRange | null {
	const selection = $getSelection();
	if (!$isRangeSelection(selection)) {
		return null;
	}
	const paragraph = $ensureRootParagraph();
	const anchor = $getPointOffset(selection.anchor, paragraph);
	const focus = $getPointOffset(selection.focus, paragraph);
	if (anchor == null || focus == null) {
		return null;
	}
	return {
		start: Math.min(anchor, focus),
		end: Math.max(anchor, focus),
		direction: resolveSearchSelectionDirection(anchor, focus),
	};
}

function $setElementCaret(paragraph: ElementNode, index: number): void {
	const selection = $createRangeSelection();
	selection.anchor.set(paragraph.getKey(), index, 'element');
	selection.focus.set(paragraph.getKey(), index, 'element');
	$setSelection(selection);
}

function resolveElementPointOffset(index: number, offset: number, accumulated: number): number {
	if (offset - accumulated === 0) {
		return index;
	}
	return index + 1;
}

function $setPointAtChildBoundary({point, paragraph, children, index}: SetPointAtChildBoundaryRequest): void {
	const next = children[index + 1];
	if (next instanceof TextNode) {
		point.set(next.getKey(), 0, 'text');
		return;
	}
	point.set(paragraph.getKey(), index + 1, 'element');
}

function $setPointAtOffset(point: PointType, paragraph: ElementNode, rawOffset: number): void {
	const children = paragraph.getChildren();
	const offset = Math.max(0, rawOffset);
	let acc = 0;
	for (const [index, child] of children.entries()) {
		const end = acc + child.getTextContent().length;
		if (child instanceof TextNode && offset <= end) {
			point.set(child.getKey(), Math.max(0, offset - acc), 'text');
			return;
		}
		if (child instanceof TextNode) {
			acc = end;
			continue;
		}
		if (
			$setNonTextPointAtOffset({
				point,
				paragraph,
				children,
				index,
				offset,
				accumulated: acc,
				end,
			})
		) {
			return;
		}
		acc = end;
	}
	point.set(paragraph.getKey(), children.length, 'element');
}

function $setNonTextPointAtOffset({
	point,
	paragraph,
	children,
	index,
	offset,
	accumulated,
	end,
}: SetNonTextPointAtOffsetRequest): boolean {
	if (offset < end) {
		point.set(paragraph.getKey(), resolveElementPointOffset(index, offset, accumulated), 'element');
		return true;
	}
	if (offset === end) {
		$setPointAtChildBoundary({point, paragraph, children, index});
		return true;
	}
	return false;
}

function $selectRange(
	start: number,
	end: number,
	direction: SearchSelectionDirection = SearchSelectionDirection.NONE,
): void {
	const paragraph = $ensureRootParagraph();
	const queryLength = paragraph.getTextContent().length;
	const clampedEnd = Math.min(Math.max(0, end), queryLength);
	let clampedStart: number;
	if (end < start) {
		clampedStart = clampedEnd;
	} else {
		clampedStart = Math.min(Math.max(0, start), clampedEnd);
	}
	const selection = $createRangeSelection();
	if (direction === SearchSelectionDirection.BACKWARD && clampedStart !== clampedEnd) {
		$setPointAtOffset(selection.anchor, paragraph, clampedEnd);
		$setPointAtOffset(selection.focus, paragraph, clampedStart);
	} else {
		$setPointAtOffset(selection.anchor, paragraph, clampedStart);
		$setPointAtOffset(selection.focus, paragraph, clampedEnd);
	}
	$setSelection(selection);
}

export function $applySearchSelectionRange(range: SearchSelectionRange): void {
	const paragraph = $ensureRootParagraph();
	let endpoints: Array<number>;
	if (range.start === range.end) {
		endpoints = [range.start];
	} else {
		endpoints = [range.start, range.end];
	}
	let offset = 0;
	for (const child of paragraph.getChildren()) {
		const end = offset + child.getTextContent().length;
		if (!(child instanceof TextNode) && endpoints.some((endpoint) => endpoint > offset && endpoint < end)) {
			$replaceSearchDocumentFromQuery($getSearchQuery(), range);
			return;
		}
		offset = end;
	}
	$selectRange(range.start, range.end, range.direction);
}

export function $selectOffset(offset: number): void {
	$selectRange(offset, offset);
}

export function $selectSearchFilterBoundary(node: LexicalNode, boundary: SearchFilterPointerBoundary): void {
	const paragraph = $ensureRootParagraph();
	let offset = 0;
	for (const child of paragraph.getChildren()) {
		if (!child.is(node)) {
			offset += child.getTextContent().length;
			continue;
		}
		if (boundary === SearchFilterPointerBoundary.AFTER) {
			offset += child.getTextContent().length;
		}
		$selectOffset(offset);
		return;
	}
	$selectOffset($getSearchQuery().length);
}

export function $replaceSearchDocumentFromQuery(query: string, selection: number | SearchSelectionRange | null): void {
	const root = $getRoot();
	root.clear();
	const paragraph = $createParagraphNode();
	root.append(paragraph);
	const caretOffset = resolveSearchCaretOffset(selection);
	const children = $buildParagraphChildren(query, selection);
	if (children.length > 0) {
		paragraph.append(...children);
	}
	if (typeof selection === 'object' && selection != null) {
		$selectRange(selection.start, selection.end, selection.direction);
	} else if (caretOffset != null) {
		$selectOffset(Math.min(caretOffset, query.length));
	} else {
		$setElementCaret(paragraph, paragraph.getChildrenSize());
	}
}
