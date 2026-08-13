// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/lexical/composer/LexicalMessageComposer.module.css';
import {DecoratorNode, type LexicalNode, type SerializedLexicalNode} from 'lexical';
import type {JSX} from 'react';

export class SlashSeparatorNode extends DecoratorNode<JSX.Element> {
	static override getType(): string {
		return 'slash-separator';
	}

	static override clone(node: SlashSeparatorNode): SlashSeparatorNode {
		return new SlashSeparatorNode(node.__key);
	}

	static override importJSON(): SlashSeparatorNode {
		return $createSlashSeparatorNode();
	}

	override exportJSON(): SerializedLexicalNode {
		return {
			...super.exportJSON(),
			type: SlashSeparatorNode.getType(),
		};
	}

	override createDOM(): HTMLElement {
		const span = document.createElement('span');
		span.setAttribute('data-lexical-composer-slot-separator', 'true');
		span.spellcheck = false;
		return span;
	}

	override updateDOM(): boolean {
		return false;
	}

	override getTextContent(): string {
		return ' ';
	}

	override isInline(): true {
		return true;
	}

	override isKeyboardSelectable(): boolean {
		return false;
	}

	override decorate(): JSX.Element {
		return (
			<span className={styles.slashSlotSeparator} aria-hidden="true">
				{' '}
			</span>
		);
	}
}

export function $createSlashSeparatorNode(): SlashSeparatorNode {
	return new SlashSeparatorNode();
}

export function $isSlashSeparatorNode(node: LexicalNode | null | undefined): node is SlashSeparatorNode {
	return node instanceof SlashSeparatorNode;
}
