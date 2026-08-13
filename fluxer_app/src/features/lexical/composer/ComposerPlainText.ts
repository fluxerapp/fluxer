// SPDX-License-Identifier: AGPL-3.0-or-later

import {ComposerPlainSegmentNode} from '@app/features/lexical/composer/nodes/ComposerPlainSegmentNode';
import {$isSyntaxMarkerNode, SyntaxMarkerNode} from '@app/features/lexical/composer/nodes/SyntaxMarkerNode';
import {isSlashSlotStateSegmentId} from '@app/features/lexical/composer/SlashSlotPersistence';
import {mergeRegister} from '@lexical/utils';
import {$createTextNode, type LexicalEditor, TextNode} from 'lexical';

export function registerComposerPlainText(editor: LexicalEditor): () => void {
	return mergeRegister(
		editor.registerNodeTransform(SyntaxMarkerNode, (node) => {
			if (!editor.isComposing()) {
				node.replace($createTextNode(node.getTextContent()));
			}
		}),
		editor.registerNodeTransform(ComposerPlainSegmentNode, (node) => {
			if (editor.isComposing()) {
				return;
			}
			if (!node.isSegmentValid() && !isSlashSlotStateSegmentId(node.getSegmentId())) {
				node.replace($createTextNode(node.getTextContent()));
				return;
			}
			if (node.getFormat() !== 0) {
				node.setFormat(0);
			}
			if (node.getStyle().length > 0) {
				node.setStyle('');
			}
		}),
		editor.registerNodeTransform(TextNode, (node) => {
			if (editor.isComposing() || $isSyntaxMarkerNode(node)) {
				return;
			}
			if (node.getFormat() !== 0) {
				node.setFormat(0);
			}
			if (node.getStyle().length > 0) {
				node.setStyle('');
			}
		}),
	);
}
