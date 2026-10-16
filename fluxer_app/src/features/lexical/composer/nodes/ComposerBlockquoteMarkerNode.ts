// SPDX-License-Identifier: AGPL-3.0-or-later

import {SyntaxMarkerNode} from '@app/features/lexical/composer/nodes/SyntaxMarkerNode';
import type {EditorConfig, LexicalNode, SerializedTextNode} from 'lexical';

export class ComposerBlockquoteMarkerNode extends SyntaxMarkerNode {
	static override getType(): string {
		return 'composer-blockquote-marker';
	}

	static override clone(node: ComposerBlockquoteMarkerNode): ComposerBlockquoteMarkerNode {
		return new ComposerBlockquoteMarkerNode(node.__text, node.__key);
	}

	static override importJSON(serializedNode: SerializedTextNode): ComposerBlockquoteMarkerNode {
		return $createComposerBlockquoteMarkerNode(serializedNode.text).updateFromJSON(serializedNode);
	}

	override createDOM(config: EditorConfig): HTMLElement {
		const dom = super.createDOM(config);
		const className = config.theme.composerBlockquoteMarker;
		if (typeof className === 'string') {
			dom.classList.add(className);
		}
		return dom;
	}

	override canInsertTextBefore(): false {
		return false;
	}

	override canInsertTextAfter(): false {
		return false;
	}
}

export function $createComposerBlockquoteMarkerNode(text: string): ComposerBlockquoteMarkerNode {
	return new ComposerBlockquoteMarkerNode(text);
}

export function $isComposerBlockquoteMarkerNode(
	node: LexicalNode | null | undefined,
): node is ComposerBlockquoteMarkerNode {
	return node instanceof ComposerBlockquoteMarkerNode;
}
