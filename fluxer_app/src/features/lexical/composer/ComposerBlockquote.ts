// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type BlockquoteEdit,
	type BlockquoteLine,
	planBlockquoteArrowLeft,
	planBlockquoteBackspace,
	planBlockquoteLineBreak,
	resolveBlockquoteCaret,
} from '@app/features/lexical/composer/blockquoteLines';
import {
	$captureSelectionOffsets,
	$getComposerBlockquoteState,
	$replaceComposerRange,
	$selectComposerOffset,
	$selectComposerRange,
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
	$createTextNode,
	$isLineBreakNode,
	COMMAND_PRIORITY_CRITICAL,
	COMMAND_PRIORITY_HIGH,
	DELETE_CHARACTER_COMMAND,
	type ElementNode,
	INSERT_LINE_BREAK_COMMAND,
	INSERT_PARAGRAPH_COMMAND,
	KEY_ARROW_LEFT_COMMAND,
	type LexicalEditor,
	type LexicalNode,
	type LineBreakNode,
	SELECTION_CHANGE_COMMAND,
	TextNode,
} from 'lexical';

export interface ComposerLine {
	nodes: Array<LexicalNode>;
	lineBreak: LineBreakNode | null;
}

type BlockquotePlanner = (text: string, lines: ReadonlyArray<BlockquoteLine>, caret: number) => BlockquoteEdit | null;

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
	const {scanText, selection, lines} = $getComposerBlockquoteState();
	if (selection == null || selection.anchor !== selection.focus || lines.length === 0) {
		return false;
	}
	const edit = planner(scanText, lines, selection.anchor);
	if (edit == null) {
		return false;
	}
	$replaceComposerRange(edit.start, edit.end, {kind: 'text', text: edit.text}, {leading: false, trailing: false});
	$selectComposerOffset(edit.caret);
	return true;
}

export function $snapCaretOutOfBlockquoteMarker(): void {
	const {selection, lines} = $getComposerBlockquoteState();
	if (selection == null || selection.anchor !== selection.focus || lines.length === 0) {
		return;
	}
	const caret = resolveBlockquoteCaret(lines, selection.anchor);
	if (caret !== selection.anchor) {
		$selectComposerOffset(caret);
	}
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

export function $rewriteMultilineBlockquoteMarker(
	lines: ReadonlyArray<ComposerLine>,
	lineSources: ReadonlyArray<string>,
	markerEnds: ReadonlyArray<number>,
): boolean {
	const index = markerEnds.findIndex(
		(end, lineIndex) => end > 0 && lineSources[lineIndex]!.slice(0, end).trimStart().startsWith('>>> '),
	);
	if (index < 0) {
		return false;
	}
	const indent = markerEnds[index]! - 4;
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
		selection.anchor <= markerStart + 4;
	if (!$spliceLeadingText(lines[index]!.nodes, indent, 4, '> ')) {
		return false;
	}
	const prefixedStarts = typed ? [] : lineStarts.slice(index + 1);
	if (!typed) {
		for (const line of lines.slice(index + 1)) {
			$prefixComposerLine(line);
		}
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

export function registerComposerBlockquote(editor: LexicalEditor): () => void {
	return mergeRegister(
		editor.registerCommand(
			INSERT_LINE_BREAK_COMMAND,
			(selectStart) => !selectStart && $applyBlockquotePlan(editor, planBlockquoteLineBreak),
			COMMAND_PRIORITY_HIGH,
		),
		editor.registerCommand(
			INSERT_PARAGRAPH_COMMAND,
			() => $applyBlockquotePlan(editor, planBlockquoteLineBreak),
			COMMAND_PRIORITY_HIGH,
		),
		editor.registerCommand(
			DELETE_CHARACTER_COMMAND,
			(isBackward) => isBackward && $applyBlockquotePlan(editor, planBlockquoteBackspace),
			COMMAND_PRIORITY_CRITICAL,
		),
		editor.registerCommand(
			KEY_ARROW_LEFT_COMMAND,
			(event) => {
				if (event.shiftKey || event.metaKey || isIMEComposing(event) || editor.isComposing()) {
					return false;
				}
				const {selection, lines} = $getComposerBlockquoteState();
				if (selection == null || selection.anchor !== selection.focus) {
					return false;
				}
				const caret = planBlockquoteArrowLeft(lines, selection.anchor);
				if (caret == null) {
					return false;
				}
				event.preventDefault();
				$selectComposerOffset(caret);
				return true;
			},
			COMMAND_PRIORITY_HIGH,
		),
		editor.registerCommand(
			SELECTION_CHANGE_COMMAND,
			() => {
				if (!editor.isComposing()) {
					$snapCaretOutOfBlockquoteMarker();
				}
				return false;
			},
			COMMAND_PRIORITY_HIGH,
		),
	);
}
