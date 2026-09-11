// SPDX-License-Identifier: AGPL-3.0-or-later

import {$nodeWireText} from '@app/features/lexical/composer/ComposerMarkdownHighlight';
import {
	$captureSelectionOffsets,
	$createComposerInsertNode,
	$selectComposerRange,
} from '@app/features/lexical/composer/composerOffsets';
import {$getComposerLineNodes} from '@app/features/lexical/composer/nodes/ComposerBlockquoteLineNode';
import {ComposerMentionNode} from '@app/features/lexical/composer/nodes/ComposerMentionNode';
import {ComposerPlainSegmentNode} from '@app/features/lexical/composer/nodes/ComposerPlainSegmentNode';
import {$isSyntaxMarkerNode} from '@app/features/lexical/composer/nodes/SyntaxMarkerNode';
import {
	createSpecialMentionPayload,
	findTypedSpecialMention,
	isSpecialMentionKind,
	SPECIAL_MENTION_PATTERN,
} from '@app/features/lexical/composer/specialMentions';
import {mergeRegister} from '@lexical/utils';
import {
	$addUpdateTag,
	$createTextNode,
	$getRoot,
	$hasUpdateTag,
	$isElementNode,
	$isLineBreakNode,
	$isTextNode,
	HISTORY_MERGE_TAG,
	HISTORY_PUSH_TAG,
	type LexicalEditor,
	type LexicalNode,
	PASTE_TAG,
	RootNode,
	type TextNode,
} from 'lexical';

export const COMPOSER_RESCAN_TAG = 'composer-rescan';

interface SpecialMentionCandidate {
	node: TextNode;
	sourceStart: number;
	displayStart: number;
}

export function registerComposerSpecialMention(
	editor: LexicalEditor,
	allowed: boolean,
	plainText: boolean,
): () => void {
	if (!allowed) {
		return mergeRegister(
			editor.registerNodeTransform(ComposerMentionNode, (node) => {
				if (node.getSegmentType() === 'special') {
					node.replace($createTextNode(node.getTextContent()));
				}
			}),
			editor.registerNodeTransform(ComposerPlainSegmentNode, (node) => {
				if (node.getSegmentType() === 'special' && isSpecialMentionKind(node.getSegmentId())) {
					node.replace($createTextNode(node.getTextContent()));
				}
			}),
		);
	}
	const unregister = editor.registerNodeTransform(RootNode, (root) => {
		if (!editor.isComposing()) {
			$convertSpecialMention(root, plainText);
		}
	});
	editor.update(
		() => {
			$getRoot().markDirty();
		},
		{discrete: true, tag: [COMPOSER_RESCAN_TAG, HISTORY_MERGE_TAG]},
	);
	return unregister;
}

function $sourceText(node: LexicalNode): string {
	const wire = $nodeWireText(node);
	if ($isLineBreakNode(node) || $isSyntaxMarkerNode(node) || ($isTextNode(node) && node.isSimpleText())) {
		return wire;
	}
	return ' '.repeat(wire.length);
}

function $convertSpecialMention(root: RootNode, plainText: boolean): void {
	let source = '';
	let displayLength = 0;
	const candidates: Array<SpecialMentionCandidate> = [];
	const blocks = root.getChildren();
	for (let index = 0; index < blocks.length; index += 1) {
		const block = blocks[index]!;
		if (index > 0) {
			source += '\n';
			displayLength += 1;
		}
		for (const child of $isElementNode(block) ? $getComposerLineNodes(block) : [block]) {
			if (
				$isTextNode(child) &&
				child.isSimpleText() &&
				!child.hasFormat('code') &&
				SPECIAL_MENTION_PATTERN.test(child.getTextContent())
			) {
				candidates.push({node: child, sourceStart: source.length, displayStart: displayLength});
			}
			source += $sourceText(child);
			displayLength += child.getTextContentSize();
		}
	}
	if (candidates.length === 0) {
		return;
	}
	const selection = $captureSelectionOffsets();
	const caret =
		selection == null ||
		selection.anchor !== selection.focus ||
		$hasUpdateTag(PASTE_TAG) ||
		$hasUpdateTag(COMPOSER_RESCAN_TAG)
			? null
			: selection.anchor;
	for (const {node, sourceStart, displayStart} of candidates) {
		const match = findTypedSpecialMention(
			source,
			sourceStart,
			sourceStart + node.getTextContentSize(),
			caret == null ? null : caret - displayStart + sourceStart,
		);
		if (match != null) {
			const parts = node.splitText(match.start - sourceStart, match.end - sourceStart);
			parts[match.start > sourceStart ? 1 : 0]!.replace(
				$createComposerInsertNode(createSpecialMentionPayload(match.kind), plainText),
			);
			if (selection != null) {
				$selectComposerRange(selection.anchor, selection.focus);
			}
			if (!$hasUpdateTag(HISTORY_MERGE_TAG)) {
				$addUpdateTag(HISTORY_PUSH_TAG);
			}
			return;
		}
	}
}
