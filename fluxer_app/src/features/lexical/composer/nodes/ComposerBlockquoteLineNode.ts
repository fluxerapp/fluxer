// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	$applyNodeReplacement,
	type EditorConfig,
	ElementNode,
	type LexicalNode,
	type RangeSelection,
	type SerializedElementNode,
} from 'lexical';

export class ComposerBlockquoteLineNode extends ElementNode {
	static override getType(): string {
		return 'composer-blockquote-line';
	}

	static override clone(node: ComposerBlockquoteLineNode): ComposerBlockquoteLineNode {
		return new ComposerBlockquoteLineNode(node.__key);
	}

	static override importJSON(serializedNode: SerializedElementNode): ComposerBlockquoteLineNode {
		return $createComposerBlockquoteLineNode().updateFromJSON(serializedNode);
	}

	override createDOM(config: EditorConfig): HTMLElement {
		const span = document.createElement('span');
		const className = config.theme.composerBlockquoteLine;
		if (typeof className === 'string') {
			span.className = className;
		}
		return span;
	}

	override updateDOM(): boolean {
		return false;
	}

	override isInline(): true {
		return true;
	}

	override canBeEmpty(): false {
		return false;
	}

	override insertNewAfter(_selection: RangeSelection, restoreSelection = true): ComposerBlockquoteLineNode {
		const next = $createComposerBlockquoteLineNode();
		this.insertAfter(next, restoreSelection);
		return next;
	}
}

export function $createComposerBlockquoteLineNode(): ComposerBlockquoteLineNode {
	return $applyNodeReplacement(new ComposerBlockquoteLineNode());
}

export function $isComposerBlockquoteLineNode(
	node: LexicalNode | null | undefined,
): node is ComposerBlockquoteLineNode {
	return node instanceof ComposerBlockquoteLineNode;
}

export function $getComposerLineNodes(element: ElementNode): Array<LexicalNode> {
	const nodes: Array<LexicalNode> = [];
	for (const child of element.getChildren()) {
		if ($isComposerBlockquoteLineNode(child)) {
			nodes.push(...$getComposerLineNodes(child));
		} else {
			nodes.push(child);
		}
	}
	return nodes;
}
