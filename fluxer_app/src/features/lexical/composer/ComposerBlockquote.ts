// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type BlockquoteDeleteUnit,
	type BlockquoteEdit,
	isInsideBlockquoteContent,
	type MultilineBlockquoteMarker,
	planBlockquoteBackspace,
	planBlockquoteContentStartDelete,
	planBlockquoteDropCaret,
	planBlockquoteForwardDelete,
	planBlockquoteLineBreak,
	planBlockquoteMove,
	planBlockquotePaste,
	resolveBlockquoteSelection,
} from '@app/features/lexical/composer/blockquoteLines';
import {
	$insertComposerClipboardSlice,
	COMPOSER_CLIPBOARD_MAX_DISPLAY_LENGTH,
	type ComposerClipboardSlice,
	FLUXER_COMPOSER_CLIPBOARD_MIME,
	getClipboardEvent,
	parseComposerClipboardSlice,
} from '@app/features/lexical/composer/ComposerClipboard';
import {
	$captureSelectionOffsets,
	$getComposerBlockquoteState,
	$getComposerDisplayText,
	$getComposerDropOffset,
	$replaceComposerRange,
	$selectComposerOffset,
	$selectComposerRange,
	type ComposerBlockquoteState,
} from '@app/features/lexical/composer/composerOffsets';
import {
	$createComposerBlockquoteLineNode,
	$getComposerLineNodes,
	$isComposerBlockquoteLineNode,
} from '@app/features/lexical/composer/nodes/ComposerBlockquoteLineNode';
import {$isComposerBlockquoteMarkerNode} from '@app/features/lexical/composer/nodes/ComposerBlockquoteMarkerNode';
import {$isComposerCommandNode} from '@app/features/lexical/composer/nodes/ComposerCommandNode';
import {$isComposerPlainSegmentNode} from '@app/features/lexical/composer/nodes/ComposerPlainSegmentNode';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {mergeRegister} from '@lexical/utils';
import {
	$addUpdateTag,
	$createTextNode,
	$getEditor,
	$getSelection,
	$hasUpdateTag,
	$isLineBreakNode,
	$isRangeSelection,
	COMMAND_PRIORITY_CRITICAL,
	COMMAND_PRIORITY_HIGH,
	COMMAND_PRIORITY_LOW,
	CONTROLLED_TEXT_INSERTION_COMMAND,
	DELETE_CHARACTER_COMMAND,
	DELETE_LINE_COMMAND,
	DELETE_WORD_COMMAND,
	DROP_COMMAND,
	type ElementNode,
	findAllLexicalElementsDeep,
	getEditorPropertyFromDOMNode,
	HISTORIC_TAG,
	INSERT_LINE_BREAK_COMMAND,
	INSERT_PARAGRAPH_COMMAND,
	IS_APPLE,
	isHTMLElement,
	isLexicalEditor,
	KEY_ARROW_LEFT_COMMAND,
	KEY_ARROW_RIGHT_COMMAND,
	KEY_DOWN_COMMAND,
	type LexicalEditor,
	type LexicalNode,
	type LineBreakNode,
	PASTE_COMMAND,
	PASTE_TAG,
	SELECTION_CHANGE_COMMAND,
	TextNode,
} from 'lexical';

export interface ComposerLine {
	nodes: Array<LexicalNode>;
	lineBreak: LineBreakNode | null;
}

type BlockquotePlanner = (state: ComposerBlockquoteState, caret: number) => BlockquoteEdit | null;

const LEXICAL_DRAG_MIME = 'application/x-lexical-drag';

export function $splitComposerLines(paragraph: ElementNode): Array<ComposerLine> {
	const lines: Array<ComposerLine> = [{nodes: [], lineBreak: null}];
	for (const node of $getComposerLineNodes(paragraph)) {
		if ($isLineBreakNode(node)) {
			lines.push({nodes: [], lineBreak: node});
		} else {
			lines[lines.length - 1]!.nodes.push(node);
		}
	}
	return lines;
}

function $applyBlockquotePlan(editor: LexicalEditor, planner: BlockquotePlanner): boolean {
	if (editor.isComposing()) {
		return false;
	}
	const state = $getComposerBlockquoteState();
	if (state.selection == null || state.selection.anchor !== state.selection.focus || state.lines.length === 0) {
		return false;
	}
	const edit = planner(state, state.selection.anchor);
	if (edit == null) {
		return false;
	}
	$replaceComposerRange(edit.start, edit.end, {kind: 'text', text: edit.text}, {leading: false, trailing: false});
	$selectComposerOffset(edit.caret);
	return true;
}

function $insertTextIntoBlockquote(text: string, parserFlags?: number): boolean {
	if ($getEditor().isComposing()) {
		return false;
	}
	const {scanText, selection, lines} = $getComposerBlockquoteState();
	if (selection == null) {
		return false;
	}
	const start = Math.min(selection.anchor, selection.focus);
	const end = Math.max(selection.anchor, selection.focus);
	if (!isInsideBlockquoteContent(lines, start)) {
		return false;
	}
	const slice = {display: text, segments: []};
	if (planBlockquotePaste(scanText, lines, start, end, slice, parserFlags).display === text) {
		return false;
	}
	return $insertComposerClipboardSlice(slice, false, parserFlags);
}

function readDragSourceKey(dataTransfer: DataTransfer): string | null {
	const marker = dataTransfer.getData(LEXICAL_DRAG_MIME);
	if (marker.length === 0) {
		return null;
	}
	let parsed: unknown;
	try {
		parsed = JSON.parse(marker);
	} catch {
		return null;
	}
	if (typeof parsed !== 'object' || parsed == null || !('editorKey' in parsed)) {
		return null;
	}
	return typeof parsed.editorKey === 'string' ? parsed.editorKey : null;
}

function readDraggedSlice(dataTransfer: DataTransfer): ComposerClipboardSlice | null {
	const serialized = dataTransfer.getData(FLUXER_COMPOSER_CLIPBOARD_MIME);
	const slice = serialized.length === 0 ? null : parseComposerClipboardSlice(serialized);
	if (slice != null) {
		return slice;
	}
	const moved = dataTransfer.getData('text/plain');
	if (moved.length === 0 || moved.length > COMPOSER_CLIPBOARD_MAX_DISPLAY_LENGTH) {
		return null;
	}
	return {display: moved, segments: []};
}

function findDragSourceRoot(editor: LexicalEditor, sourceKey: string): HTMLElement | null {
	const root = editor.getRootElement();
	if (root == null) {
		return null;
	}
	for (const element of findAllLexicalElementsDeep(root.ownerDocument)) {
		const source = getEditorPropertyFromDOMNode(element);
		if (isLexicalEditor(source) && source.getKey() === sourceKey && isHTMLElement(element)) {
			return element;
		}
	}
	return null;
}

function $insertDroppedSlice(caret: number, slice: ComposerClipboardSlice, parserFlags?: number): void {
	$selectComposerOffset(caret);
	$snapSelectionOutOfBlockquoteMarker();
	$insertComposerClipboardSlice(slice, false, parserFlags);
	$addUpdateTag(PASTE_TAG);
}

function $moveDroppedTextIntoBlockquote(editor: LexicalEditor, event: DragEvent, parserFlags?: number): boolean {
	if (editor.isComposing() || event.dataTransfer == null) {
		return false;
	}
	const sourceKey = readDragSourceKey(event.dataTransfer);
	const slice = sourceKey == null ? null : readDraggedSlice(event.dataTransfer);
	const dropped = slice == null ? null : $getComposerDropOffset(editor, event.clientX, event.clientY);
	if (sourceKey == null || slice == null || dropped == null) {
		return false;
	}
	if (sourceKey !== editor.getKey()) {
		const sourceRoot = findDragSourceRoot(editor, sourceKey);
		if (sourceRoot == null) {
			return false;
		}
		$insertDroppedSlice(dropped, slice, parserFlags);
		sourceRoot.dispatchEvent(
			new InputEvent('beforeinput', {bubbles: true, cancelable: true, inputType: 'deleteByDrag'}),
		);
		event.preventDefault();
		return true;
	}
	const selection = $captureSelectionOffsets();
	if (selection == null || selection.anchor === selection.focus) {
		return false;
	}
	const start = Math.min(selection.anchor, selection.focus);
	const end = Math.max(selection.anchor, selection.focus);
	const caret = planBlockquoteDropCaret(start, end, dropped);
	if (caret == null) {
		return false;
	}
	$replaceComposerRange(start, end, {kind: 'text', text: ''}, {leading: false, trailing: false});
	$insertDroppedSlice(caret, slice, parserFlags);
	event.preventDefault();
	return true;
}

export function $snapSelectionOutOfBlockquoteMarker(): void {
	const {selection, lines} = $getComposerBlockquoteState();
	if (selection == null || lines.length === 0) {
		return;
	}
	const resolved = resolveBlockquoteSelection(lines, selection.anchor, selection.focus);
	if (resolved.anchor !== selection.anchor || resolved.focus !== selection.focus) {
		$selectComposerRange(resolved.anchor, resolved.focus);
	}
}

function $isFocusRTL(editor: LexicalEditor): boolean {
	const selection = $getSelection();
	const block = $isRangeSelection(selection) ? selection.focus.getNode().getTopLevelElement() : null;
	const dom = block == null ? null : editor.getElementByKey(block.getKey());
	const view = dom == null ? null : dom.ownerDocument.defaultView;
	return dom != null && view != null && view.getComputedStyle(dom).direction === 'rtl';
}

function isWordMove(event: KeyboardEvent): boolean {
	const apple = IS_APPLE;
	return (
		(event.key === 'ArrowLeft' || event.key === 'ArrowRight') &&
		!event.metaKey &&
		event.altKey === apple &&
		event.ctrlKey !== apple
	);
}

function $moveAcrossBlockquoteMarker(
	editor: LexicalEditor,
	event: KeyboardEvent,
	isArrowLeft: boolean,
	byWord: boolean,
): boolean {
	if (isIMEComposing(event) || editor.isComposing()) {
		return false;
	}
	const {selection, lines} = $getComposerBlockquoteState();
	if (selection == null || lines.length === 0 || (!event.shiftKey && selection.anchor !== selection.focus)) {
		return false;
	}
	const isBackward = isArrowLeft !== $isFocusRTL(editor);
	const focus = byWord && !isBackward ? null : planBlockquoteMove(lines, selection.focus, isBackward);
	if (focus == null) {
		return false;
	}
	event.preventDefault();
	$selectComposerRange(event.shiftKey ? selection.anchor : focus, focus);
	return true;
}

function shouldWrapLine(line: ComposerLine, quoted: boolean | undefined): boolean {
	return quoted === true && $isComposerBlockquoteMarkerNode(line.nodes[0]);
}

function $blockquoteStructureMatches(
	paragraph: ElementNode,
	lines: ReadonlyArray<ComposerLine>,
	quoted: ReadonlyArray<boolean>,
): boolean {
	const children = paragraph.getChildren();
	let index = 0;
	for (let lineIndex = 0; lineIndex < lines.length; lineIndex += 1) {
		const line = lines[lineIndex]!;
		if (line.lineBreak != null) {
			if (!line.lineBreak.is(children[index])) {
				return false;
			}
			index += 1;
		}
		if (shouldWrapLine(line, quoted[lineIndex])) {
			const wrapper = children[index];
			index += 1;
			if (!$isComposerBlockquoteLineNode(wrapper)) {
				return false;
			}
			const inner = wrapper.getChildren();
			if (inner.length !== line.nodes.length || inner.some((node, position) => !node.is(line.nodes[position]))) {
				return false;
			}
			continue;
		}
		for (const node of line.nodes) {
			if (!node.is(children[index])) {
				return false;
			}
			index += 1;
		}
	}
	return index === children.length;
}

export function $syncComposerBlockquoteLines(paragraph: ElementNode, quoted: ReadonlyArray<boolean>): void {
	if ($blockquoteStructureMatches(paragraph, $splitComposerLines(paragraph), quoted)) {
		return;
	}
	const selection = $captureSelectionOffsets();
	for (const child of paragraph.getChildren()) {
		if ($isComposerBlockquoteLineNode(child)) {
			for (const node of $getComposerLineNodes(child)) {
				child.insertBefore(node);
			}
			child.remove();
		}
	}
	$splitComposerLines(paragraph).forEach((line, index) => {
		if (shouldWrapLine(line, quoted[index])) {
			const wrapper = $createComposerBlockquoteLineNode();
			line.nodes[0]!.insertBefore(wrapper);
			wrapper.append(...line.nodes);
		}
	});
	if (selection != null) {
		$selectComposerRange(selection.anchor, selection.focus);
	}
}

function isPlainTextLeaf(node: LexicalNode | undefined): node is TextNode {
	return node instanceof TextNode && !$isComposerPlainSegmentNode(node) && !$isComposerCommandNode(node);
}

function $spliceLeadingText(
	nodes: ReadonlyArray<LexicalNode>,
	offset: number,
	deleteCount: number,
	text: string,
): boolean {
	const covered: Array<TextNode> = [];
	let length = 0;
	for (const node of nodes) {
		if (!isPlainTextLeaf(node) || length >= offset + deleteCount) {
			break;
		}
		covered.push(node);
		length += node.getTextContentSize();
	}
	const first = covered[0];
	if (first == null || length < offset + deleteCount) {
		return false;
	}
	const combined = covered.map((node) => node.getTextContent()).join('');
	first.setTextContent(`${combined.slice(0, offset)}${text}${combined.slice(offset + deleteCount)}`);
	for (const node of covered.slice(1)) {
		node.remove();
	}
	return true;
}

function $prefixComposerLine(line: ComposerLine): void {
	const first = line.nodes[0];
	if (isPlainTextLeaf(first)) {
		first.setTextContent(`> ${first.getTextContent()}`);
	} else if (first != null) {
		first.insertBefore($createTextNode('> '));
	} else if (line.lineBreak != null) {
		line.lineBreak.insertAfter($createTextNode('> '));
	}
}

function $insertedBeforeCaret(caret: number): boolean {
	if ($hasUpdateTag(HISTORIC_TAG)) {
		return false;
	}
	const editor = $getEditor();
	const previous = editor.getEditorState().read(() => $getComposerDisplayText(), {editor});
	const current = $getComposerDisplayText();
	return (
		caret > 0 &&
		current.length === previous.length + 1 &&
		`${current.slice(0, caret - 1)}${current.slice(caret)}` === previous
	);
}

export function $rewriteMultilineBlockquoteMarker(
	lines: ReadonlyArray<ComposerLine>,
	marker: MultilineBlockquoteMarker | null,
): boolean {
	if (marker == null) {
		return false;
	}
	const index = marker.line;
	const indent = marker.indent;
	const lineStarts: Array<number> = [];
	let offset = 0;
	for (const line of lines) {
		lineStarts.push(offset);
		offset += line.nodes.reduce((sum, node) => sum + node.getTextContentSize(), 0) + 1;
	}
	const selection = $captureSelectionOffsets();
	const markerStart = lineStarts[index]! + indent;
	const typed =
		selection != null &&
		selection.anchor === selection.focus &&
		selection.anchor > markerStart &&
		selection.anchor <= markerStart + 4 &&
		$insertedBeforeCaret(selection.anchor);
	if (!marker.splits && !(typed && index === lines.length - 1)) {
		return false;
	}
	if (!$spliceLeadingText(lines[index]!.nodes, indent, 4, '> ')) {
		return false;
	}
	const prefixedStarts = lineStarts.slice(index + 1);
	for (const line of lines.slice(index + 1)) {
		$prefixComposerLine(line);
	}
	if (selection != null) {
		const map = (value: number): number => {
			if (value > markerStart && value < markerStart + 4) {
				return markerStart + 2;
			}
			let next = value >= markerStart + 4 ? value - 2 : value;
			for (const start of prefixedStarts) {
				if (value >= start) {
					next += 2;
				}
			}
			return next;
		};
		$selectComposerRange(map(selection.anchor), map(selection.focus));
	}
	return true;
}

export function registerComposerBlockquote(editor: LexicalEditor, parserFlags?: number): () => void {
	const insertLineBreak = () =>
		$applyBlockquotePlan(editor, ({scanText, lines}, caret) =>
			planBlockquoteLineBreak(scanText, lines, caret, parserFlags),
		);
	const deleteAcrossMarker = (unit: BlockquoteDeleteUnit) => (isBackward: boolean) =>
		$applyBlockquotePlan(editor, ({scanText, lines, atoms}, caret) =>
			isBackward
				? planBlockquoteBackspace(scanText, lines, caret, parserFlags)
				: (planBlockquoteForwardDelete(scanText, lines, caret, parserFlags) ??
					planBlockquoteContentStartDelete(scanText, lines, atoms, caret, unit)),
		);
	return mergeRegister(
		editor.registerCommand(
			INSERT_LINE_BREAK_COMMAND,
			(selectStart) => !selectStart && insertLineBreak(),
			COMMAND_PRIORITY_HIGH,
		),
		editor.registerCommand(INSERT_PARAGRAPH_COMMAND, insertLineBreak, COMMAND_PRIORITY_HIGH),
		editor.registerCommand(DELETE_CHARACTER_COMMAND, deleteAcrossMarker('character'), COMMAND_PRIORITY_CRITICAL),
		editor.registerCommand(DELETE_WORD_COMMAND, deleteAcrossMarker('word'), COMMAND_PRIORITY_CRITICAL),
		editor.registerCommand(DELETE_LINE_COMMAND, deleteAcrossMarker('line'), COMMAND_PRIORITY_CRITICAL),
		editor.registerCommand(
			KEY_ARROW_LEFT_COMMAND,
			(event) => $moveAcrossBlockquoteMarker(editor, event, true, false),
			COMMAND_PRIORITY_HIGH,
		),
		editor.registerCommand(
			KEY_ARROW_RIGHT_COMMAND,
			(event) => $moveAcrossBlockquoteMarker(editor, event, false, false),
			COMMAND_PRIORITY_HIGH,
		),
		editor.registerCommand(
			KEY_DOWN_COMMAND,
			(event) => isWordMove(event) && $moveAcrossBlockquoteMarker(editor, event, event.key === 'ArrowLeft', true),
			COMMAND_PRIORITY_HIGH,
		),
		editor.registerCommand(
			PASTE_COMMAND,
			(event) => {
				const clipboardEvent = getClipboardEvent(event);
				if (
					clipboardEvent == null ||
					clipboardEvent.clipboardData == null ||
					!$insertTextIntoBlockquote(clipboardEvent.clipboardData.getData('text/plain'), parserFlags)
				) {
					return false;
				}
				$addUpdateTag(PASTE_TAG);
				clipboardEvent.preventDefault();
				return true;
			},
			COMMAND_PRIORITY_LOW,
		),
		editor.registerCommand(
			DROP_COMMAND,
			(event) => $moveDroppedTextIntoBlockquote(editor, event, parserFlags),
			COMMAND_PRIORITY_LOW,
		),
		editor.registerCommand(
			CONTROLLED_TEXT_INSERTION_COMMAND,
			(payload) =>
				typeof payload !== 'string' &&
				payload.dataTransfer != null &&
				$insertTextIntoBlockquote(payload.dataTransfer.getData('text/plain'), parserFlags),
			COMMAND_PRIORITY_LOW,
		),
		editor.registerCommand(
			SELECTION_CHANGE_COMMAND,
			() => {
				if (!editor.isComposing()) {
					$snapSelectionOutOfBlockquoteMarker();
				}
				return false;
			},
			COMMAND_PRIORITY_HIGH,
		),
	);
}
