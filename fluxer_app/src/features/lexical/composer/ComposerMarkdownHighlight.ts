// SPDX-License-Identifier: AGPL-3.0-or-later

import {$captureSelectionOffsets, $selectComposerRange} from '@app/features/lexical/composer/composerOffsets';
import {
	computeMarkdownHighlightSpans,
	MarkdownHl,
	type MarkdownHlFormat,
	type MarkdownSpan,
} from '@app/features/lexical/composer/markdownSpans';
import {$isComposerCommandNode} from '@app/features/lexical/composer/nodes/ComposerCommandNode';
import {$isComposerCustomEmojiNode} from '@app/features/lexical/composer/nodes/ComposerCustomEmojiNode';
import {
	$isComposerMentionNode,
	ComposerMentionPresentation,
	type ComposerMentionPresentationFormat,
} from '@app/features/lexical/composer/nodes/ComposerMentionNode';
import {$isComposerPlainSegmentNode} from '@app/features/lexical/composer/nodes/ComposerPlainSegmentNode';
import {$isComposerStandardEmojiNode} from '@app/features/lexical/composer/nodes/ComposerStandardEmojiNode';
import {$isSlashSlotNode} from '@app/features/lexical/composer/nodes/SlashSlotNode';
import {$createSyntaxMarkerNode, $isSyntaxMarkerNode} from '@app/features/lexical/composer/nodes/SyntaxMarkerNode';
import {
	$createNodeSelection,
	$createTextNode,
	$getNodeByKey,
	$getSelection,
	$isLineBreakNode,
	$isNodeSelection,
	$isRangeSelection,
	$setSelection,
	type LexicalEditor,
	type LexicalNode,
	ParagraphNode,
	RootNode,
	type TextFormatType,
	TextNode,
} from 'lexical';

const STYLE_BY_BIT: ReadonlyArray<{bit: number; style: string}> = [
	{
		bit: MarkdownHl.spoiler,
		style: 'background-color:var(--background-modifier-active,rgba(0,0,0,0.15));border-radius:0.1875rem',
	},
	{bit: MarkdownHl.heading, style: 'font-weight:700'},
	{bit: MarkdownHl.blockquote, style: 'color:var(--text-secondary)'},
	{bit: MarkdownHl.subtext, style: 'font-size:0.85em;color:var(--text-muted,var(--text-secondary))'},
	{bit: MarkdownHl.link, style: 'color:var(--text-link)'},
	{bit: MarkdownHl.codeBlock, style: 'font-size:0.75em'},
];

const LEXICAL_TEXT_FORMATS: ReadonlyArray<{bit: number; type: TextFormatType}> = [
	{bit: MarkdownHl.bold, type: 'bold'},
	{bit: MarkdownHl.italic, type: 'italic'},
	{bit: MarkdownHl.underline, type: 'underline'},
	{bit: MarkdownHl.strike, type: 'strikethrough'},
	{bit: MarkdownHl.code, type: 'code'},
];

function expectedStyle(format: MarkdownHlFormat): string {
	const styles: Array<string> = [];
	for (const {bit, style} of STYLE_BY_BIT) {
		if (format & bit) {
			styles.push(style);
		}
	}
	return styles.join(';');
}

function applyMarkdownFormat(node: TextNode, format: MarkdownHlFormat): void {
	for (const {bit, type} of LEXICAL_TEXT_FORMATS) {
		if (format & bit) {
			node.toggleFormat(type);
		}
	}
	const style = expectedStyle(format);
	if (style.length > 0) {
		node.setStyle(style);
	}
}

export function registerComposerMarkdownHighlight(editor: LexicalEditor, parserFlags?: number): () => void {
	return editor.registerNodeTransform(RootNode, (root) => {
		if (editor.isComposing()) {
			return;
		}
		for (const child of root.getChildren()) {
			if (child instanceof ParagraphNode) {
				$reconcileParagraph(child, parserFlags);
			}
		}
	});
}

type Desired =
	| {role: 'marker'; text: string}
	| {role: 'content'; text: string; format: MarkdownHlFormat}
	| {role: 'keep'; node: LexicalNode};

type BuildableDesired = Exclude<Desired, {role: 'keep'}>;

export function $reconcileLineOf(node: TextNode, parserFlags?: number): void {
	const parent = node.getParent();
	if (parent == null || parent.getType() !== 'paragraph') {
		return;
	}
	$reconcileParagraph(parent as ParagraphNode, parserFlags);
}

function $reconcileParagraph(paragraph: ParagraphNode, parserFlags?: number): void {
	const lines: Array<Array<LexicalNode>> = [];
	let line: Array<LexicalNode> = [];
	for (const child of paragraph.getChildren()) {
		if ($isLineBreakNode(child)) {
			lines.push(line);
			line = [];
		} else {
			line.push(child);
		}
	}
	lines.push(line);
	const lineSources = lines.map((nodes) => nodes.map($nodeWireText).join(''));
	const source = lineSources.join('\n');
	const spans = computeMarkdownHighlightSpans(source, parserFlags);
	let lineStart = 0;
	for (let index = 0; index < lines.length; index += 1) {
		const nodes = lines[index]!;
		const lineSource = lineSources[index]!;
		const lineEnd = lineStart + lineSource.length;
		if (nodes.length > 0) {
			const lineSpans = spans
				.map((span) => ({
					...span,
					start: Math.max(span.start, lineStart) - lineStart,
					end: Math.min(span.end, lineEnd) - lineStart,
				}))
				.filter((span) => span.end > span.start);
			$reconcileLine(nodes, parserFlags, lineSource, lineSpans);
		}
		lineStart = lineEnd + 1;
	}
}

function $reconcileLine(
	line: Array<LexicalNode>,
	parserFlags?: number,
	precomputedSource?: string,
	precomputedSpans?: Array<MarkdownSpan>,
): void {
	const desired: Array<Desired> = [];
	const source = precomputedSource == null ? line.map($nodeWireText).join('') : precomputedSource;
	const spans = precomputedSpans == null ? computeMarkdownHighlightSpans(source, parserFlags) : precomputedSpans;
	let sourceOffset = 0;
	for (const node of line) {
		const nodeText = $nodeWireText(node);
		const nodeEnd = sourceOffset + nodeText.length;
		if ($isComposerMentionNode(node) || $isComposerCustomEmojiNode(node) || $isComposerStandardEmojiNode(node)) {
			const literal = spans.some(
				(span) =>
					span.role === 'content' &&
					(span.format & MarkdownHl.code) !== 0 &&
					span.start <= sourceOffset &&
					span.end >= nodeEnd,
			);
			if (node.isLiteral() !== literal) {
				node.setLiteral(literal);
			}
			const spoiler = spans.some(
				(span) =>
					span.role === 'content' &&
					(span.format & MarkdownHl.spoiler) !== 0 &&
					span.start <= sourceOffset &&
					span.end >= nodeEnd,
			);
			if (node.isSpoiler() !== spoiler) {
				node.setSpoiler(spoiler);
			}
		}
		if ($isComposerMentionNode(node)) {
			const coveringSpan = spans.find(
				(span) => span.role === 'content' && span.start <= sourceOffset && span.end >= nodeEnd,
			);
			const presentation = mentionPresentation(coveringSpan == null ? MarkdownHl.none : coveringSpan.format);
			if (node.getPresentation() !== presentation) {
				node.setPresentation(presentation);
			}
		}
		if (node instanceof TextNode && !$isComposerPlainSegmentNode(node) && !$isComposerCommandNode(node)) {
			for (const span of spans) {
				const start = Math.max(sourceOffset, span.start);
				const end = Math.min(nodeEnd, span.end);
				if (end <= start) {
					continue;
				}
				const text = source.slice(start, end);
				desired.push(span.role === 'marker' ? {role: 'marker', text} : {role: 'content', text, format: span.format});
			}
		} else {
			desired.push({role: 'keep', node});
		}
		sourceOffset += nodeText.length;
	}
	if (desired.length === 0) {
		return;
	}
	if ($descriptorsMatch(line, desired)) {
		return;
	}
	const currentSelection = $getSelection();
	const selectionKeys = $isRangeSelection(currentSelection)
		? {anchor: currentSelection.anchor.key, focus: currentSelection.focus.key}
		: null;
	const nodeSelectionKeys = $isNodeSelection(currentSelection)
		? currentSelection.getNodes().map((node) => node.getKey())
		: null;
	const selection = $captureSelectionOffsets();
	$applyDescriptors(line, desired);
	if (nodeSelectionKeys != null) {
		if (nodeSelectionKeys.every((key) => $getNodeByKey(key)?.isAttached() === true)) {
			const restored = $createNodeSelection();
			for (const key of nodeSelectionKeys) {
				restored.add(key);
			}
			$setSelection(restored);
		}
		return;
	}
	const anchorNode = selectionKeys == null ? null : $getNodeByKey(selectionKeys.anchor);
	const focusNode = selectionKeys == null ? null : $getNodeByKey(selectionKeys.focus);
	const selectionNodesSurvived =
		selectionKeys != null &&
		anchorNode != null &&
		anchorNode.isAttached() === true &&
		focusNode != null &&
		focusNode.isAttached() === true;
	if (selection != null && !selectionNodesSurvived) {
		$selectComposerRange(selection.anchor, selection.focus);
	}
}

function mentionPresentation(format: MarkdownHlFormat): ComposerMentionPresentationFormat {
	let presentation = ComposerMentionPresentation.none;
	if ((format & MarkdownHl.italic) !== 0) {
		presentation |= ComposerMentionPresentation.italic;
	}
	if ((format & MarkdownHl.underline) !== 0) {
		presentation |= ComposerMentionPresentation.underline;
	}
	if ((format & MarkdownHl.strike) !== 0) {
		presentation |= ComposerMentionPresentation.strike;
	}
	if ((format & MarkdownHl.subtext) !== 0) {
		presentation |= ComposerMentionPresentation.subtext;
	}
	return presentation;
}

function $nodeWireText(node: LexicalNode): string {
	if (
		$isComposerMentionNode(node) ||
		$isComposerCustomEmojiNode(node) ||
		$isComposerPlainSegmentNode(node) ||
		$isSlashSlotNode(node)
	) {
		return node.getWireText();
	}
	return node.getTextContent();
}

function $contentNodeMatchesFormat(node: TextNode, format: MarkdownHlFormat): boolean {
	for (const {bit, type} of LEXICAL_TEXT_FORMATS) {
		if (node.hasFormat(type) !== ((format & bit) !== 0)) {
			return false;
		}
	}
	return node.getStyle() === expectedStyle(format);
}

function $descriptorsMatch(line: Array<LexicalNode>, desired: Array<Desired>): boolean {
	if (line.length !== desired.length) {
		return false;
	}
	for (let i = 0; i < desired.length; i += 1) {
		const want = desired[i]!;
		const node = line[i]!;
		if (want.role === 'keep') {
			if (node.getKey() !== want.node.getKey()) {
				return false;
			}
			continue;
		}
		if (!(node instanceof TextNode) || node.getTextContent() !== want.text) {
			return false;
		}
		if (want.role === 'marker') {
			if (!$isSyntaxMarkerNode(node)) {
				return false;
			}
		} else if ($isSyntaxMarkerNode(node) || !$contentNodeMatchesFormat(node, want.format)) {
			return false;
		}
	}
	return true;
}

function $buildDescriptorNode(desired: BuildableDesired): TextNode {
	if (desired.role === 'marker') {
		return $createSyntaxMarkerNode(desired.text);
	}
	const node = $createTextNode(desired.text);
	applyMarkdownFormat(node, desired.format);
	return node;
}

function $applyDescriptors(line: Array<LexicalNode>, desired: Array<Desired>): void {
	const lineSegments: Array<Array<LexicalNode>> = [];
	const desiredSegments: Array<Array<BuildableDesired>> = [];
	let lineSegment: Array<LexicalNode> = [];
	let desiredSegment: Array<BuildableDesired> = [];
	for (const node of line) {
		if (node instanceof TextNode && !$isComposerPlainSegmentNode(node) && !$isComposerCommandNode(node)) {
			lineSegment.push(node);
		} else {
			lineSegments.push(lineSegment);
			lineSegment = [];
		}
	}
	lineSegments.push(lineSegment);
	for (const want of desired) {
		if (want.role === 'keep') {
			desiredSegments.push(desiredSegment);
			desiredSegment = [];
		} else {
			desiredSegment.push(want);
		}
	}
	desiredSegments.push(desiredSegment);
	for (let i = 0; i < lineSegments.length; i += 1) {
		$replaceSegment(lineSegments[i]!, desiredSegments[i] == null ? [] : desiredSegments[i]!);
	}
}

function $replaceSegment(oldNodes: Array<LexicalNode>, descriptors: Array<BuildableDesired>): void {
	if (oldNodes.length === 0) {
		return;
	}
	const anchor = oldNodes[0]!;
	let previous: LexicalNode | null = null;
	for (const descriptor of descriptors) {
		const built = $buildDescriptorNode(descriptor);
		if (previous == null) {
			anchor.insertBefore(built);
		} else {
			previous.insertAfter(built);
		}
		previous = built;
	}
	for (const old of oldNodes) {
		old.remove();
	}
}
