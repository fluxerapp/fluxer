// SPDX-License-Identifier: AGPL-3.0-or-later

import {$createSearchTokenNode, $isSearchTokenNode} from '@app/features/lexical/search/SearchTokenNode';
import {resolveSearchChipDeletion, type SearchChipRole, tokenize} from '@app/features/search/utils/SearchQueryParser';
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
	ParagraphNode,
	type PointType,
	RootNode,
	TextNode,
} from 'lexical';

export const SearchSelectionDirection = Object.freeze({
	BACKWARD: 'backward',
	FORWARD: 'forward',
	NONE: 'none',
} as const);

export type SearchSelectionDirection = (typeof SearchSelectionDirection)[keyof typeof SearchSelectionDirection];

export interface SearchSelectionRange {
	start: number;
	end: number;
	direction: SearchSelectionDirection;
}

interface SearchSegment {
	readonly text: string;
	readonly role: SearchChipRole | null;
	readonly exclude: boolean;
}

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

export const SEARCH_INPUT_MAX_LENGTH = 2048;
const SEARCH_INPUT_COUNTER_HEADROOM = Math.floor(SEARCH_INPUT_MAX_LENGTH / 10);

export function shouldShowSearchInputCounter(currentLength: number): boolean {
	return SEARCH_INPUT_MAX_LENGTH - currentLength <= SEARCH_INPUT_COUNTER_HEADROOM;
}

function clampSearchInputText(text: string): string {
	return text.length > SEARCH_INPUT_MAX_LENGTH ? text.slice(0, SEARCH_INPUT_MAX_LENGTH) : text;
}

function normalizeSearchInputText(text: string): string {
	return text.replace(/[\r\n\u2028\u2029]/g, ' ');
}

export function $insertSearchText(text: string): boolean {
	const selection = $getSelection();
	if (!$isRangeSelection(selection)) {
		return false;
	}
	const documentLength = $getRoot().getTextContent().length;
	const replacedLength = selection.getTextContent().length;
	const available = SEARCH_INPUT_MAX_LENGTH - (documentLength - replacedLength);
	if (available <= 0) {
		return true;
	}
	const normalized = normalizeSearchInputText(text);
	selection.insertText(normalized.length > available ? normalized.slice(0, available) : normalized);
	return true;
}

function buildSearchSegments(query: string): Array<SearchSegment> {
	const {chips} = tokenize(query);
	const segments: Array<SearchSegment> = [];
	let position = 0;
	const pushPlain = (text: string) => {
		if (text.length > 0) {
			segments.push({text, role: null, exclude: false});
		}
	};
	for (const chip of chips) {
		pushPlain(query.slice(position, chip.start));
		segments.push({text: query.slice(chip.start, chip.end), role: chip.role, exclude: chip.exclude});
		position = chip.end;
	}
	pushPlain(query.slice(position));
	return segments;
}

function $createSegmentNode(segment: SearchSegment): TextNode {
	if (segment.role == null) {
		return $createTextNode(segment.text);
	}
	return $createSearchTokenNode(segment.text, segment.role, segment.exclude);
}

function readSegmentRole(node: LexicalNode): SearchChipRole | null {
	if ($isSearchTokenNode(node)) {
		return node.getRole();
	}
	return null;
}

function readSegmentExclude(node: LexicalNode): boolean {
	return $isSearchTokenNode(node) && node.getExclude();
}

function childrenMatchSegments(children: ReadonlyArray<LexicalNode>, segments: ReadonlyArray<SearchSegment>): boolean {
	if (children.length !== segments.length) {
		return false;
	}
	return segments.every((segment, index) => {
		const child = children[index]!;
		return (
			child instanceof TextNode &&
			child.getTextContent() === segment.text &&
			readSegmentRole(child) === segment.role &&
			readSegmentExclude(child) === segment.exclude
		);
	});
}

function $retextChildren(children: ReadonlyArray<LexicalNode>, segments: ReadonlyArray<SearchSegment>): boolean {
	if (children.length !== segments.length) {
		return false;
	}
	const rolesMatch = segments.every((segment, index) => {
		const child = children[index]!;
		return (
			child instanceof TextNode &&
			readSegmentRole(child) === segment.role &&
			readSegmentExclude(child) === segment.exclude
		);
	});
	if (!rolesMatch) {
		return false;
	}
	for (const [index, segment] of segments.entries()) {
		const child = children[index] as TextNode;
		if (child.getTextContent() !== segment.text) {
			child.setTextContent(segment.text);
		}
	}
	return true;
}

function $reconcileSearchChips(paragraph: ElementNode): void {
	const normalized = normalizeSearchInputText(paragraph.getTextContent());
	const query = clampSearchInputText(normalized);
	if (query.length === 0) {
		return;
	}
	const segments = buildSearchSegments(query);
	const children = paragraph.getChildren();
	if (childrenMatchSegments(children, segments)) {
		return;
	}
	const selection = $getSelectionRange();
	if (!$retextChildren(children, segments)) {
		paragraph.clear();
		paragraph.append(...segments.map($createSegmentNode));
	}
	if (selection != null) {
		$selectRange(selection.start, selection.end, selection.direction);
	}
}

export function $getSearchQuery(): string {
	return normalizeSearchInputText($getRoot().getTextContent());
}

export function $deleteSearchChipAtCaret(isForward: boolean): boolean {
	const selection = $getSelection();
	if (!$isRangeSelection(selection) || !selection.isCollapsed()) {
		return false;
	}
	const range = $getSelectionRange();
	if (range == null) {
		return false;
	}
	const chip = resolveSearchChipDeletion($getSearchQuery(), range.start, isForward);
	if (chip == null) {
		return false;
	}
	$selectRange(chip.start, chip.end);
	const chipSelection = $getSelection();
	if (!$isRangeSelection(chipSelection)) {
		return false;
	}
	chipSelection.removeText();
	return true;
}

function refreshSearchChipsAfterComposition(editor: LexicalEditor): void {
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

export function registerSearchChipTransform(editor: LexicalEditor): () => void {
	const unregisterTransform = editor.registerNodeTransform(RootNode, (root) => {
		if (editor.isComposing()) {
			return;
		}
		for (const child of root.getChildren()) {
			if (child instanceof ParagraphNode) {
				$reconcileSearchChips(child);
			}
		}
	});
	const unregisterLineBreak = editor.registerNodeTransform(LineBreakNode, (node) => {
		node.replace($createTextNode(' '));
	});
	const unregisterCompositionEnd = editor.registerCommand(
		COMPOSITION_END_COMMAND,
		() => {
			refreshSearchChipsAfterComposition(editor);
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

function $setPointAtOffset(point: PointType, paragraph: ElementNode, rawOffset: number): void {
	const children = paragraph.getChildren();
	const offset = Math.max(0, rawOffset);
	let acc = 0;
	for (const child of children) {
		const end = acc + child.getTextContent().length;
		if (child instanceof TextNode && offset <= end) {
			point.set(child.getKey(), Math.max(0, offset - acc), 'text');
			return;
		}
		acc = end;
	}
	point.set(paragraph.getKey(), children.length, 'element');
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
	$selectRange(range.start, range.end, range.direction);
}

export function $selectOffset(offset: number): void {
	$selectRange(offset, offset);
}

export function $replaceSearchDocumentFromQuery(query: string, selection: number | SearchSelectionRange | null): void {
	const root = $getRoot();
	root.clear();
	const paragraph = $createParagraphNode();
	root.append(paragraph);
	const normalizedQuery = normalizeSearchInputText(query);
	const children = buildSearchSegments(normalizedQuery).map($createSegmentNode);
	if (children.length > 0) {
		paragraph.append(...children);
	}
	const caretOffset = resolveSearchCaretOffset(selection);
	if (typeof selection === 'object' && selection != null) {
		$selectRange(selection.start, selection.end, selection.direction);
		return;
	}
	if (caretOffset != null) {
		$selectOffset(Math.min(caretOffset, normalizedQuery.length));
		return;
	}
	$setElementCaret(paragraph, paragraph.getChildrenSize());
}
