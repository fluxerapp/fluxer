// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	$captureSelectionOffsets,
	$selectComposerNodeBoundary,
	$selectComposerRange,
} from '@app/features/lexical/composer/composerOffsets';
import {
	$getNearestNodeFromDOMNode,
	$getSelection,
	$isDecoratorNode,
	$isElementNode,
	$isLineBreakNode,
	$isRangeSelection,
	type LexicalEditor,
} from 'lexical';
import type React from 'react';

export function moveSelectionToLineBoundary(event: React.KeyboardEvent<HTMLElement>, editor: LexicalEditor): boolean {
	if (event.key !== 'Home' && event.key !== 'End') {
		return false;
	}
	if (event.ctrlKey || event.metaKey || event.altKey) {
		return false;
	}
	const root = event.currentTarget;
	const selection = root.ownerDocument.defaultView?.getSelection();
	if (selection == null || selection.rangeCount === 0 || selection.focusNode == null) {
		return false;
	}
	event.preventDefault();
	const edge = event.key === 'Home' ? 'start' : 'end';
	const caret = caretRectAt(selection.focusNode, selection.focusOffset);
	const target = caret == null ? null : lineBoundaryPoint(root, caret.top + caret.height / 2, edge);
	if (target == null) {
		selection.modify(event.shiftKey ? 'extend' : 'move', edge === 'start' ? 'backward' : 'forward', 'lineboundary');
		return true;
	}
	const island = islandAtBoundary(root, target.node, target.offset, edge);
	if (island == null) {
		if (event.shiftKey) {
			selection.extend(target.node, target.offset);
		} else {
			selection.collapse(target.node, target.offset);
		}
		return true;
	}
	const extend = event.shiftKey;
	editor.update(() => {
		const node = $getNearestNodeFromDOMNode(island);
		if (node == null) {
			return;
		}
		const anchor = extend ? $captureSelectionOffsets() : null;
		$selectComposerNodeBoundary(node, edge === 'start' ? 'before' : 'after');
		const focus = $captureSelectionOffsets();
		if (anchor != null && focus != null) {
			$selectComposerRange(anchor.anchor, focus.focus);
		}
	});
	return true;
}

function caretRectAt(node: Node, offset: number): DOMRect | null {
	const document = node.ownerDocument;
	if (document == null) {
		return null;
	}
	const range = document.createRange();
	range.setStart(node, offset);
	range.collapse(true);
	const rects = range.getClientRects();
	if (rects.length > 0) {
		return rects[rects.length - 1]!;
	}
	const neighbour = node.childNodes[offset] ?? node.childNodes[offset - 1] ?? node;
	if (neighbour instanceof Element) {
		return neighbour.getBoundingClientRect();
	}
	range.selectNodeContents(neighbour);
	const textRects = range.getClientRects();
	return textRects.length > 0 ? textRects[0]! : null;
}

function lineBoundaryPoint(root: HTMLElement, y: number, edge: 'start' | 'end'): {node: Node; offset: number} | null {
	const bounds = root.getBoundingClientRect();
	const rtl = root.ownerDocument.defaultView?.getComputedStyle(root).direction === 'rtl';
	const x = (edge === 'start') !== rtl ? bounds.left + 1 : bounds.right - 1;
	const point = caretPointFromPoint(root.ownerDocument, x, y);
	return point != null && root.contains(point.node) ? point : null;
}

function caretPointFromPoint(document: Document, x: number, y: number): {node: Node; offset: number} | null {
	if (typeof document.caretPositionFromPoint === 'function') {
		const position = document.caretPositionFromPoint(x, y);
		return position == null ? null : {node: position.offsetNode, offset: position.offset};
	}
	if (typeof document.caretRangeFromPoint === 'function') {
		const range = document.caretRangeFromPoint(x, y);
		return range == null ? null : {node: range.startContainer, offset: range.startOffset};
	}
	return null;
}

function islandAtBoundary(root: HTMLElement, node: Node, offset: number, edge: 'start' | 'end'): HTMLElement | null {
	const element = node instanceof Element ? node : node.parentElement;
	const enclosing = element == null ? null : element.closest<HTMLElement>('[contenteditable="false"]');
	if (enclosing != null && root.contains(enclosing)) {
		return enclosing;
	}
	const neighbour = node.childNodes[edge === 'start' ? offset : offset - 1];
	return neighbour instanceof HTMLElement && neighbour.contentEditable === 'false' ? neighbour : null;
}

export function registerComposerLineStartInsertion(editor: LexicalEditor): () => void {
	const handleBeforeInput = (event: InputEvent) => {
		if (event.inputType !== 'insertText' || event.data == null || editor.isComposing()) {
			return;
		}
		const text = event.data;
		let handled = false;
		editor.update(
			() => {
				const selection = $getSelection();
				if (!$isRangeSelection(selection) || !selection.isCollapsed() || selection.anchor.type !== 'element') {
					return;
				}
				const parent = selection.anchor.getNode();
				if (!$isElementNode(parent)) {
					return;
				}
				const previous = parent.getChildAtIndex(selection.anchor.offset - 1);
				const next = parent.getChildAtIndex(selection.anchor.offset);
				if (
					!($isLineBreakNode(previous) && $isDecoratorNode(next)) &&
					!($isDecoratorNode(previous) && $isLineBreakNode(next))
				) {
					return;
				}
				selection.insertText(text);
				handled = true;
			},
			{discrete: true},
		);
		if (handled) {
			event.preventDefault();
			event.stopImmediatePropagation();
		}
	};
	return editor.registerRootListener((root, previousRoot) => {
		previousRoot?.removeEventListener('beforeinput', handleBeforeInput, true);
		root?.addEventListener('beforeinput', handleBeforeInput, true);
	});
}
