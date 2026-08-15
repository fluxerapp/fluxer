// SPDX-License-Identifier: AGPL-3.0-or-later

import {type EditorConfig, type LexicalNode, type SerializedTextNode, TextNode} from 'lexical';

export class SyntaxMarkerNode extends TextNode {
	static override getType(): string {
		return 'syntax-marker';
	}

	static override clone(node: SyntaxMarkerNode): SyntaxMarkerNode {
		return new SyntaxMarkerNode(node.__text, node.__key);
	}

	static override importJSON(serializedNode: SerializedTextNode): SyntaxMarkerNode {
		return $createSyntaxMarkerNode(serializedNode.text).updateFromJSON(serializedNode);
	}

	override createDOM(config: EditorConfig): HTMLElement {
		const dom = super.createDOM(config);
		const className = config.theme.syntaxMarker;
		if (typeof className === 'string') {
			dom.classList.add(className);
		}
		return dom;
	}
}

export function $createSyntaxMarkerNode(text: string): SyntaxMarkerNode {
	return new SyntaxMarkerNode(text);
}

export function $isSyntaxMarkerNode(node: LexicalNode | null | undefined): node is SyntaxMarkerNode {
	return node instanceof SyntaxMarkerNode;
}
