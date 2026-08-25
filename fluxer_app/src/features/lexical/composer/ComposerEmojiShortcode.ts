// SPDX-License-Identifier: AGPL-3.0-or-later

import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
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
import {findTypedEmojiShortcode, type TypedEmojiMatch} from '@app/features/messaging/utils/markdown/TypedEmojiMatch';
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

function findUnicodeEmoji(text: string, startIndex: number): TypedEmojiMatch | null {
	const pattern = new RegExp(UnicodeEmojis.EMOJI_SURROGATE_RE.source, 'g');
	pattern.lastIndex = startIndex;
	const match = pattern.exec(text);
	if (match == null) {
		return null;
	}
	const name = UnicodeEmojis.nameForSurrogate(match[0], false);
	if (!name) {
		return null;
	}
	return {start: match.index, end: match.index + match[0].length, name};
}

interface EmojiToken extends TypedEmojiMatch {
	surrogate: boolean;
}

function findNextEmojiToken(text: string, startIndex: number): EmojiToken | null {
	const shortcode = findTypedEmojiShortcode(text, startIndex);
	const surrogate = findUnicodeEmoji(text, startIndex);
	if (shortcode == null) {
		return surrogate == null ? null : {...surrogate, surrogate: true};
	}
	if (surrogate == null) {
		return {...shortcode, surrogate: false};
	}
	return surrogate.start < shortcode.start ? {...surrogate, surrogate: true} : {...shortcode, surrogate: false};
}

function isEscapedAt(text: string, index: number): boolean {
	let backslashes = 0;
	let cursor = index - 1;
	while (cursor >= 0 && text[cursor] === '\\') {
		backslashes += 1;
		cursor -= 1;
	}
	return backslashes % 2 === 1;
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
		const match = findNextEmojiToken(text, searchFrom);
		if (match == null) {
			return;
		}
		const codeMarkers = text.slice(0, match.start).match(/`/g);
		if ((codeMarkers == null ? 0 : codeMarkers.length) % 2 === 1) {
			return;
		}
		if (isEscapedAt(text, match.start)) {
			if (match.surrogate) {
				const shortcodeText = `:${match.name}:`;
				const selection = $captureSelectionOffsets();
				const nodeDisplayStart = $getComposerNodeDisplayStart(node);
				const replaced = `${text.slice(0, match.start)}${shortcodeText}${text.slice(match.end)}`;
				node.setTextContent(replaced);
				if (selection != null) {
					const adjusted = $adjustEmbedCaret(selection, nodeDisplayStart, match.start, match.end, shortcodeText.length);
					$selectComposerRange(adjusted.anchor, adjusted.focus);
				}
				return;
			}
			searchFrom = match.end;
			continue;
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
