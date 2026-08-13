// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	$captureSelectionOffsets,
	$getComposerNodeDisplayStart,
	$selectComposerNodeBoundary,
	$selectComposerRange,
} from '@app/features/lexical/composer/composerOffsets';
import {$createComposerCustomEmojiNode} from '@app/features/lexical/composer/nodes/ComposerCustomEmojiNode';
import {
	$createComposerStandardEmojiNode,
	$isComposerStandardEmojiNode,
} from '@app/features/lexical/composer/nodes/ComposerStandardEmojiNode';
import {$isSyntaxMarkerNode} from '@app/features/lexical/composer/nodes/SyntaxMarkerNode';
import {findTypedEmojiShortcode} from '@app/features/messaging/utils/markdown/TypedEmojiMatch';
import type {ResolvedTypedEmoji} from '@app/features/messaging/utils/TypedEmojiShortcodeUtils';
import {type LexicalEditor, TextNode} from 'lexical';

export type ComposerEmojiResolver = (shortcodeName: string) => ResolvedTypedEmoji | null;

export function registerComposerEmojiShortcode(editor: LexicalEditor, resolve: ComposerEmojiResolver): () => void {
	return editor.registerNodeTransform(TextNode, (node) => {
		if (!editor.isComposing()) {
			$convertEmojiShortcode(node, resolve);
		}
	});
}

export function $convertEmojiShortcode(node: TextNode, resolve: ComposerEmojiResolver): void {
	if ($isSyntaxMarkerNode(node) || node.hasFormat('code')) {
		return;
	}
	const parent = node.getParent();
	if (parent == null || parent.getType() !== 'paragraph') {
		return;
	}
	const text = node.getTextContent();
	let searchFrom = 0;
	while (true) {
		const match = findTypedEmojiShortcode(text, searchFrom);
		if (match == null) {
			return;
		}
		const codeMarkers = text.slice(0, match.start).match(/`/g);
		if ((codeMarkers == null ? 0 : codeMarkers.length) % 2 === 1) {
			return;
		}
		const previous = match.start === 0 ? node.getPreviousSibling() : null;
		if (/^skin-tone-[1-5]$/.test(match.name) && $isComposerStandardEmojiNode(previous)) {
			const baseName = previous.getEmojiName().split('::')[0]!;
			const combined = resolve(`${baseName}::${match.name}`);
			if (combined != null && combined.kind === 'standard') {
				const selection = $captureSelectionOffsets();
				const previousDisplayStart = $getComposerNodeDisplayStart(previous);
				const previousOldLen = previous.getTextContentSize();
				const segments = node.splitText(match.start, match.end);
				const target = segments[0];
				if (target == null) {
					return;
				}
				const replacement = $createComposerStandardEmojiNode(
					combined.name,
					combined.surrogate,
					combined.url,
					combined.display,
				);
				previous.replace(replacement);
				target.remove();
				if (selection != null && previousDisplayStart != null) {
					const adjusted = $adjustEmbedCaret(
						selection,
						previousDisplayStart,
						0,
						previousOldLen + (match.end - match.start),
						combined.display.length,
					);
					$selectComposerRange(adjusted.anchor, adjusted.focus);
				} else {
					$selectComposerNodeBoundary(replacement, 'after');
				}
				return;
			}
		}
		const resolved = resolve(match.name);
		if (resolved == null) {
			searchFrom = match.end;
			continue;
		}
		const selection = $captureSelectionOffsets();
		const nodeDisplayStart = $getComposerNodeDisplayStart(node);
		const emojiNode =
			resolved.kind === 'standard'
				? $createComposerStandardEmojiNode(resolved.name, resolved.surrogate, resolved.url, resolved.display)
				: $createComposerCustomEmojiNode(resolved.emojiId, resolved.animated, resolved.display, resolved.wire);
		const segments = node.splitText(match.start, match.end);
		const target = segments[match.start > 0 ? 1 : 0];
		if (target == null) {
			return;
		}
		target.replace(emojiNode);
		if (selection != null) {
			const adjusted = $adjustEmbedCaret(selection, nodeDisplayStart, match.start, match.end, resolved.display.length);
			$selectComposerRange(adjusted.anchor, adjusted.focus);
		}
		return;
	}
}

function $adjustEmbedCaret(
	selection: {anchor: number; focus: number},
	nodeDisplayStart: number | null,
	tokenStart: number,
	tokenEnd: number,
	newLen: number,
): {anchor: number; focus: number} {
	if (nodeDisplayStart == null) {
		return selection;
	}
	const start = nodeDisplayStart + tokenStart;
	const end = nodeDisplayStart + tokenEnd;
	const delta = newLen - (tokenEnd - tokenStart);
	const adjust = (offset: number): number => {
		if (offset <= start) {
			return offset;
		}
		if (offset >= end) {
			return offset + delta;
		}
		return start + newLen;
	};
	return {anchor: adjust(selection.anchor), focus: adjust(selection.focus)};
}
