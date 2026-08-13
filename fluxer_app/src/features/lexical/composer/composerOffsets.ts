// SPDX-License-Identifier: AGPL-3.0-or-later

import {$createComposerCustomEmojiNode} from '@app/features/lexical/composer/nodes/ComposerCustomEmojiNode';
import {
	$createComposerMentionNode,
	type ComposerMentionType,
} from '@app/features/lexical/composer/nodes/ComposerMentionNode';
import {$createComposerPlainSegmentNode} from '@app/features/lexical/composer/nodes/ComposerPlainSegmentNode';
import {$createComposerStandardEmojiNode} from '@app/features/lexical/composer/nodes/ComposerStandardEmojiNode';
import {$isSyntaxMarkerNode} from '@app/features/lexical/composer/nodes/SyntaxMarkerNode';
import {
	$createParagraphNode,
	$createRangeSelection,
	$createTextNode,
	$getRoot,
	$getSelection,
	$isElementNode,
	$isLineBreakNode,
	$isRangeSelection,
	$setSelection,
	type ElementNode,
	type LexicalNode,
	type NodeKey,
	TextNode,
} from 'lexical';
import invariant from 'tiny-invariant';

export type ComposerInsertPayload =
	| {kind: 'text'; text: string}
	| {kind: 'mention'; mentionType: ComposerMentionType; id: string; display: string; wire: string}
	| {kind: 'customEmoji'; emojiId: string; animated: boolean; display: string; wire: string}
	| {kind: 'standardEmoji'; name: string; surrogate: string; url: string | null; display: string};

function $firstParagraph(): ElementNode {
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

interface DisplayPoint {
	key: NodeKey;
	offset: number;
	type: 'text' | 'element';
}

interface DisplayLeaf {
	node: LexicalNode;
	start: number;
	end: number;
	before: DisplayPoint;
	after: DisplayPoint;
}

interface DisplayLayout {
	text: string;
	leaves: Array<DisplayLeaf>;
	elementBoundaries: Map<NodeKey, Array<number>>;
}

export interface ComposerSelectionOffsets {
	anchor: number;
	focus: number;
}

export interface ComposerSelectionWrapperQuery {
	prefix: string;
	suffix: string;
}

export interface ComposerSelectionWrapperResult {
	offsets: ComposerSelectionOffsets | null;
	wrapped: Array<boolean>;
}

function $buildDisplayLayout(): DisplayLayout {
	const root = $getRoot();
	const chunks: Array<string> = [];
	const leaves: Array<DisplayLeaf> = [];
	const elementBoundaries = new Map<NodeKey, Array<number>>();
	let offset = 0;
	const walkElement = (element: ElementNode) => {
		const children = element.getChildren();
		const boundaries: Array<number> = [offset];
		for (let index = 0; index < children.length; index += 1) {
			const child = children[index]!;
			if ($isElementNode(child)) {
				walkElement(child);
			} else {
				const text = $isLineBreakNode(child) ? '\n' : child.getTextContent();
				const start = offset;
				chunks.push(text);
				offset += text.length;
				leaves.push({
					node: child,
					start,
					end: offset,
					before: {key: element.getKey(), offset: index, type: 'element'},
					after: {key: element.getKey(), offset: index + 1, type: 'element'},
				});
			}
			boundaries.push(offset);
		}
		elementBoundaries.set(element.getKey(), boundaries);
	};
	const blocks = root.getChildren();
	const rootBoundaries: Array<number> = [0];
	for (let index = 0; index < blocks.length; index += 1) {
		if (index > 0) {
			chunks.push('\n');
			offset += 1;
			rootBoundaries[index] = offset;
		}
		const block = blocks[index]!;
		if ($isElementNode(block)) {
			walkElement(block);
		} else {
			const start = offset;
			const text = block.getTextContent();
			chunks.push(text);
			offset += text.length;
			leaves.push({
				node: block,
				start,
				end: offset,
				before: {key: root.getKey(), offset: index, type: 'element'},
				after: {key: root.getKey(), offset: index + 1, type: 'element'},
			});
		}
		rootBoundaries[index + 1] = offset;
	}
	elementBoundaries.set(root.getKey(), rootBoundaries);
	return {text: chunks.join(''), leaves, elementBoundaries};
}

function pointToDisplayOffset(
	layout: DisplayLayout,
	pointNode: LexicalNode,
	pointOffset: number,
	pointType: 'text' | 'element',
): number {
	if (pointType === 'text') {
		const leaf = layout.leaves.find((candidate) => candidate.node.getKey() === pointNode.getKey());
		invariant(leaf != null, 'Composer selection points to a text node outside the editor display');
		return leaf.start + Math.min(Math.max(0, pointOffset), leaf.end - leaf.start);
	}
	const boundaries = layout.elementBoundaries.get(pointNode.getKey());
	invariant(boundaries != null, 'Composer selection points to an element outside the editor display');
	return boundaries[Math.min(Math.max(0, pointOffset), boundaries.length - 1)]!;
}

export function $getComposerDisplayOffset(): number | null {
	return $getComposerDisplayOffsetFromLayout($buildDisplayLayout());
}

function $getComposerDisplayOffsetFromLayout(layout: DisplayLayout): number | null {
	const selection = $getSelection();
	if (!$isRangeSelection(selection)) {
		return null;
	}
	const {anchor} = selection;
	return pointToDisplayOffset(layout, anchor.getNode(), anchor.offset, anchor.type);
}

export function $getComposerNodeDisplayStart(node: LexicalNode): number | null {
	const layout = $buildDisplayLayout();
	const leaf = layout.leaves.find((candidate) => candidate.node.getKey() === node.getKey());
	return leaf ? leaf.start : null;
}

export function $captureSelectionOffsets(): ComposerSelectionOffsets | null {
	return $captureSelectionOffsetsFromLayout($buildDisplayLayout());
}

function $captureSelectionOffsetsFromLayout(layout: DisplayLayout): ComposerSelectionOffsets | null {
	const selection = $getSelection();
	if (!$isRangeSelection(selection)) {
		return null;
	}
	return {
		anchor: pointToDisplayOffset(layout, selection.anchor.getNode(), selection.anchor.offset, selection.anchor.type),
		focus: pointToDisplayOffset(layout, selection.focus.getNode(), selection.focus.offset, selection.focus.type),
	};
}

export function $getComposerSelectionRange(): {start: number; end: number} | null {
	const offsets = $captureSelectionOffsets();
	if (offsets == null) {
		return null;
	}
	return {start: Math.min(offsets.anchor, offsets.focus), end: Math.max(offsets.anchor, offsets.focus)};
}

export function $getTextUpToCursor(): string {
	const layout = $buildDisplayLayout();
	const offset = $getComposerDisplayOffsetFromLayout(layout);
	if (offset == null) {
		return '';
	}
	return layout.text.slice(0, offset);
}

export function $getComposerDisplayText(): string {
	return $buildDisplayLayout().text;
}

function $pointAtDisplayOffset(layout: DisplayLayout, offset: number): DisplayPoint {
	const clampedOffset = Math.min(Math.max(0, offset), layout.text.length);
	for (const leaf of layout.leaves) {
		if (leaf.node instanceof TextNode) {
			if (clampedOffset > leaf.start && clampedOffset < leaf.end) {
				return {key: leaf.node.getKey(), offset: clampedOffset - leaf.start, type: 'text'};
			}
		}
	}
	for (const leaf of layout.leaves) {
		if (leaf.node instanceof TextNode && !$isSyntaxMarkerNode(leaf.node) && clampedOffset === leaf.start) {
			return {key: leaf.node.getKey(), offset: 0, type: 'text'};
		}
	}
	for (const leaf of layout.leaves) {
		if (leaf.node instanceof TextNode && !$isSyntaxMarkerNode(leaf.node) && clampedOffset === leaf.end) {
			return {key: leaf.node.getKey(), offset: leaf.end - leaf.start, type: 'text'};
		}
	}
	for (const leaf of layout.leaves) {
		if (clampedOffset <= leaf.start) {
			return leaf.before;
		}
		if (clampedOffset <= leaf.end) {
			return leaf.after;
		}
	}
	const root = $getRoot();
	const first = root.getFirstChild();
	if (first != null && $isElementNode(first) && first.getChildrenSize() === 0) {
		return {key: first.getKey(), offset: 0, type: 'element'};
	}
	return {key: root.getKey(), offset: root.getChildrenSize(), type: 'element'};
}

function $selectComposerRangeWithLayout(layout: DisplayLayout, anchor: number, focus: number): void {
	const anchorPoint = $pointAtDisplayOffset(layout, anchor);
	const focusPoint = $pointAtDisplayOffset(layout, focus);
	const selection = $createRangeSelection();
	selection.anchor.set(anchorPoint.key, anchorPoint.offset, anchorPoint.type);
	selection.focus.set(focusPoint.key, focusPoint.offset, focusPoint.type);
	$setSelection(selection);
}

export function $selectComposerRange(anchor: number, focus: number): void {
	if ($getRoot().getChildrenSize() === 0) {
		$firstParagraph();
	}
	$selectComposerRangeWithLayout($buildDisplayLayout(), anchor, focus);
}

export function $selectComposerOffset(offset: number): void {
	$selectComposerRange(offset, offset);
}

export function $selectComposerNodeBoundary(node: LexicalNode, boundary: 'before' | 'after'): void {
	const layout = $buildDisplayLayout();
	const leaf = layout.leaves.find((candidate) => candidate.node.is(node));
	if (leaf == null) {
		$selectComposerRangeWithLayout(layout, layout.text.length, layout.text.length);
		return;
	}
	const offset = boundary === 'before' ? leaf.start : leaf.end;
	$selectComposerRangeWithLayout(layout, offset, offset);
}

export function $createComposerInsertNode(payload: ComposerInsertPayload, plainText: boolean): LexicalNode {
	switch (payload.kind) {
		case 'mention':
			if (plainText) {
				return $createComposerPlainSegmentNode(payload.mentionType, payload.id, payload.display, payload.wire);
			}
			return $createComposerMentionNode(payload.mentionType, payload.id, payload.display, payload.wire);
		case 'customEmoji':
			if (plainText) {
				return $createComposerPlainSegmentNode('emoji', payload.emojiId, payload.display, payload.wire);
			}
			return $createComposerCustomEmojiNode(payload.emojiId, payload.animated, payload.display, payload.wire);
		case 'standardEmoji':
			if (plainText) {
				return $createTextNode(payload.display);
			}
			return $createComposerStandardEmojiNode(payload.name, payload.surrogate, payload.url, payload.display);
		default:
			return $createTextNode(payload.text);
	}
}

export interface ComposerInsertSpacing {
	leading?: boolean;
	trailing?: boolean;
}

export function $replaceComposerRange(
	start: number,
	end: number,
	payload: ComposerInsertPayload,
	spacing?: ComposerInsertSpacing,
	plainText = false,
): void {
	const leading = spacing == null || spacing.leading == null ? false : spacing.leading;
	const trailing = spacing == null || spacing.trailing == null ? payload.kind !== 'text' : spacing.trailing;
	const orderedStart = Math.min(start, end);
	const orderedEnd = Math.max(start, end);
	$selectComposerRange(orderedStart, orderedEnd);
	const selection = $getSelection();
	if (!$isRangeSelection(selection)) {
		return;
	}
	if (payload.kind === 'text') {
		const text = `${leading ? ' ' : ''}${payload.text}${trailing ? ' ' : ''}`;
		if (text.length === 0) {
			selection.insertText('');
		} else {
			selection.insertRawText(text);
		}
		$selectComposerOffset(orderedStart + (leading ? 1 : 0) + payload.text.length + (trailing ? 1 : 0));
		return;
	}
	const node = $createComposerInsertNode(payload, plainText);
	const nodes: Array<LexicalNode> = [];
	if (leading) nodes.push($createTextNode(' '));
	nodes.push(node);
	if (trailing) nodes.push($createTextNode(' '));
	if (nodes.length > 0) {
		selection.insertNodes(nodes);
	} else {
		selection.insertText('');
	}
	$selectComposerOffset(orderedStart + (leading ? 1 : 0) + node.getTextContent().length + (trailing ? 1 : 0));
}

type WrapClass = 'inside' | 'flank' | 'none';

function countCharacterRunForward(text: string, start: number, character: string): number {
	let end = start;
	while (end < text.length && text[end] === character) {
		end += 1;
	}
	return end - start;
}

function countCharacterRunBackward(text: string, end: number, character: string): number {
	let start = end;
	while (start > 0 && text[start - 1] === character) {
		start -= 1;
	}
	return end - start;
}

function asteriskRunContainsFormat(runLength: number, markerLength: number): boolean {
	return markerLength === 1 ? runLength % 2 === 1 : runLength >= 2;
}

function classifyAsteriskWrap(display: string, start: number, end: number, markerLength: number): WrapClass {
	const selectedLeading = countCharacterRunForward(display, start, '*');
	const selectedTrailing = countCharacterRunBackward(display, end, '*');
	if (
		asteriskRunContainsFormat(selectedLeading, markerLength) &&
		asteriskRunContainsFormat(selectedTrailing, markerLength) &&
		end - start >= selectedLeading + selectedTrailing
	) {
		return 'inside';
	}
	const flankingLeading = countCharacterRunBackward(display, start, '*');
	const flankingTrailing = countCharacterRunForward(display, end, '*');
	if (
		asteriskRunContainsFormat(flankingLeading, markerLength) &&
		asteriskRunContainsFormat(flankingTrailing, markerLength)
	) {
		return 'flank';
	}
	return 'none';
}

function classifyWrap(display: string, start: number, end: number, prefix: string, suffix: string): WrapClass {
	if (prefix === suffix && (prefix === '*' || prefix === '**')) {
		return classifyAsteriskWrap(display, start, end, prefix.length);
	}
	const selected = display.slice(start, end);
	if (selected.length >= prefix.length + suffix.length && selected.startsWith(prefix) && selected.endsWith(suffix)) {
		return 'inside';
	}
	const before = display.slice(Math.max(0, start - prefix.length), start);
	const after = display.slice(end, end + suffix.length);
	if (start >= prefix.length && before === prefix && after === suffix) {
		return 'flank';
	}
	return 'none';
}

export function $isComposerSelectionWrapped(prefix: string, suffix: string): boolean {
	const wrapped = $queryComposerSelectionWrappers([{prefix, suffix}]).wrapped[0];
	return wrapped == null ? false : wrapped;
}

export function $queryComposerSelectionWrappers(
	queries: ReadonlyArray<ComposerSelectionWrapperQuery>,
): ComposerSelectionWrapperResult {
	const layout = $buildDisplayLayout();
	const offsets = $captureSelectionOffsetsFromLayout(layout);
	if (offsets == null) {
		return {offsets: null, wrapped: queries.map(() => false)};
	}
	const start = Math.min(offsets.anchor, offsets.focus);
	const end = Math.max(offsets.anchor, offsets.focus);
	if (start === end) {
		return {offsets, wrapped: queries.map(() => false)};
	}
	return {
		offsets,
		wrapped: queries.map(({prefix, suffix}) => classifyWrap(layout.text, start, end, prefix, suffix) !== 'none'),
	};
}

export function $wrapComposerSelection(prefix: string, suffix: string): void {
	const layout = $buildDisplayLayout();
	const offsets = $captureSelectionOffsetsFromLayout(layout);
	if (offsets == null) {
		return;
	}
	const start = Math.min(offsets.anchor, offsets.focus);
	const end = Math.max(offsets.anchor, offsets.focus);
	const backward = offsets.anchor > offsets.focus;
	const display = layout.text;
	const selected = display.slice(start, end);
	const replaceTextRange = (rangeStart: number, rangeEnd: number, text: string) => {
		$replaceComposerRange(rangeStart, rangeEnd, {kind: 'text', text}, {leading: false, trailing: false});
	};
	const restoreSelection = (nextStart: number, nextEnd: number) => {
		$selectComposerRange(backward ? nextEnd : nextStart, backward ? nextStart : nextEnd);
	};
	switch (classifyWrap(display, start, end, prefix, suffix)) {
		case 'inside': {
			replaceTextRange(end - suffix.length, end, '');
			replaceTextRange(start, start + prefix.length, '');
			restoreSelection(start, end - prefix.length - suffix.length);
			return;
		}
		case 'flank': {
			replaceTextRange(end, end + suffix.length, '');
			replaceTextRange(start - prefix.length, start, '');
			restoreSelection(start - prefix.length, end - prefix.length);
			return;
		}
		default: {
			replaceTextRange(end, end, suffix);
			replaceTextRange(start, start, prefix);
			selected.length === 0
				? $selectComposerOffset(start + prefix.length)
				: restoreSelection(start + prefix.length, end + prefix.length);
		}
	}
}

export function $isComposerEmpty(): boolean {
	return $buildDisplayLayout().text.length === 0;
}
