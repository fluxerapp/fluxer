// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	$getNearestNodeFromDOMNode,
	$getSelection,
	$isRangeSelection,
	$isTextNode,
	COMMAND_PRIORITY_HIGH,
	DELETE_CHARACTER_COMMAND,
	getDOMSelection,
	getDOMTextNode,
	isDOMTextNode,
	type LexicalEditor,
	type LexicalNode,
	type TextNode,
} from 'lexical';

interface DOMCaret {
	node: Node;
	offset: number;
}

function readDOMCaret(domSelection: Selection): DOMCaret | null {
	const node = domSelection.anchorNode;
	return node == null ? null : {node, offset: domSelection.anchorOffset};
}

function isSameDOMCaret(left: DOMCaret, right: DOMCaret): boolean {
	return left.node === right.node && left.offset === right.offset;
}

function $isPlainTextNode(node: LexicalNode | null | undefined): node is TextNode {
	return $isTextNode(node) && !node.isSegmented() && !node.isToken();
}

function $textNodeAtDOMCaret(caret: DOMCaret): TextNode | null {
	if (!isDOMTextNode(caret.node)) {
		return null;
	}
	const node = $getNearestNodeFromDOMNode(caret.node);
	return $isPlainTextNode(node) ? node : null;
}

function $deleteCharacterAcrossSoftWrap(editor: LexicalEditor, isBackward: boolean): boolean {
	if (!isBackward || editor.isComposing()) {
		return false;
	}
	const selection = $getSelection();
	if (!$isRangeSelection(selection) || !selection.isCollapsed()) {
		return false;
	}
	const anchor = selection.anchor;
	if (anchor.type !== 'text') {
		return false;
	}
	const anchorNode = anchor.getNode();
	if (!$isPlainTextNode(anchorNode)) {
		return false;
	}
	const rootElement = editor.getRootElement();
	const domSelection = getDOMSelection(rootElement == null ? null : rootElement.ownerDocument.defaultView);
	if (domSelection == null || typeof domSelection.modify !== 'function') {
		return false;
	}
	const anchorElement = editor.getElementByKey(anchor.key);
	const anchorDOM = anchorElement == null ? null : getDOMTextNode(anchorElement);
	if (anchorDOM == null) {
		return false;
	}
	const origin: DOMCaret = {node: anchorDOM, offset: anchor.offset};
	const collapseAtOrigin = () => {
		domSelection.setBaseAndExtent(origin.node, origin.offset, origin.node, origin.offset);
	};
	collapseAtOrigin();
	domSelection.modify('move', 'backward', 'character');
	const afterFirstMove = readDOMCaret(domSelection);
	if (afterFirstMove == null || !isSameDOMCaret(afterFirstMove, origin)) {
		collapseAtOrigin();
		return false;
	}
	domSelection.modify('move', 'backward', 'character');
	const landed = readDOMCaret(domSelection);
	collapseAtOrigin();
	if (landed == null || isSameDOMCaret(landed, origin)) {
		return false;
	}
	const landedNode = $textNodeAtDOMCaret(landed);
	if (landedNode == null || landed.offset > landedNode.getTextContentSize()) {
		return false;
	}
	const anchorParent = anchorNode.getParent();
	const landedParent = landedNode.getParent();
	if (anchorParent == null || landedParent == null || !anchorParent.is(landedParent)) {
		return false;
	}
	if (landedNode.is(anchorNode) && landed.offset >= anchor.offset) {
		return false;
	}
	selection.focus.set(landedNode.getKey(), landed.offset, 'text');
	if (selection.isCollapsed()) {
		return false;
	}
	selection.removeText();
	return true;
}

export function registerComposerSoftWrapDeletion(editor: LexicalEditor): () => void {
	return editor.registerCommand(
		DELETE_CHARACTER_COMMAND,
		(isBackward) => $deleteCharacterAcrossSoftWrap(editor, isBackward),
		COMMAND_PRIORITY_HIGH,
	);
}
