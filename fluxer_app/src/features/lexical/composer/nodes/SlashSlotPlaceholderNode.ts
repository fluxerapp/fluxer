// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	$applyNodeReplacement,
	type EditorConfig,
	type LexicalNode,
	type NodeKey,
	type SerializedTextNode,
	TextNode,
} from 'lexical';

const PLACEHOLDER_CHAR = '\uFEFF';

export class SlashSlotPlaceholderNode extends TextNode {
	static override getType(): string {
		return 'slash-slot-placeholder';
	}

	static override clone(node: SlashSlotPlaceholderNode): SlashSlotPlaceholderNode {
		const cloned = new SlashSlotPlaceholderNode(node.__key);
		cloned.__mode = node.__mode;
		return cloned;
	}

	static override importJSON(): SlashSlotPlaceholderNode {
		return $createSlashSlotPlaceholderNode();
	}

	constructor(key?: NodeKey) {
		super(PLACEHOLDER_CHAR, key);
	}

	override exportJSON(): SerializedTextNode {
		return {
			...super.exportJSON(),
			type: SlashSlotPlaceholderNode.getType(),
			text: PLACEHOLDER_CHAR,
		};
	}

	override createDOM(config: EditorConfig): HTMLElement {
		const dom = super.createDOM(config);
		const className = config.theme.slashSlotPlaceholder;
		if (typeof className === 'string' && className.length > 0) {
			dom.classList.add(className);
		}
		return dom;
	}

	override getTextContent(): string {
		return '';
	}

	override getTextContentSize(): number {
		return 0;
	}

	override canInsertTextBefore(): false {
		return false;
	}

	override canInsertTextAfter(): false {
		return false;
	}
}

export function $createSlashSlotPlaceholderNode(): SlashSlotPlaceholderNode {
	return $applyNodeReplacement(new SlashSlotPlaceholderNode()).setMode('token');
}

export function $isSlashSlotPlaceholderNode(node: LexicalNode | null | undefined): node is SlashSlotPlaceholderNode {
	return node instanceof SlashSlotPlaceholderNode;
}
