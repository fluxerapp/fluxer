// SPDX-License-Identifier: AGPL-3.0-or-later

import {SlashOptionalHintPill} from '@app/features/lexical/composer/nodes/SlashOptionalHintPill';
import {DecoratorNode, type LexicalNode, type NodeKey, type SerializedLexicalNode, type Spread} from 'lexical';
import type {JSX} from 'react';

export type SerializedSlashOptionalHintNode = Spread<
	{
		remaining: number;
	},
	SerializedLexicalNode
>;

export class SlashOptionalHintNode extends DecoratorNode<JSX.Element> {
	__remaining: number;

	static override getType(): string {
		return 'slash-optional-hint';
	}

	static override clone(node: SlashOptionalHintNode): SlashOptionalHintNode {
		return new SlashOptionalHintNode(node.__remaining, node.__key);
	}

	static override importJSON(serializedNode: SerializedSlashOptionalHintNode): SlashOptionalHintNode {
		return $createSlashOptionalHintNode(serializedNode.remaining == null ? 0 : serializedNode.remaining);
	}

	constructor(remaining: number, key?: NodeKey) {
		super(key);
		this.__remaining = remaining;
	}

	override exportJSON(): SerializedSlashOptionalHintNode {
		return {
			...super.exportJSON(),
			remaining: this.__remaining,
		};
	}

	override createDOM(): HTMLElement {
		const span = document.createElement('span');
		span.setAttribute('data-lexical-composer-optional-hint', 'true');
		span.spellcheck = false;
		return span;
	}

	override updateDOM(): boolean {
		return false;
	}

	override getTextContent(): string {
		return '';
	}

	getRemaining(): number {
		return this.getLatest().__remaining;
	}

	setRemaining(remaining: number): this {
		const writable = this.getWritable();
		writable.__remaining = remaining;
		return writable;
	}

	override isInline(): true {
		return true;
	}

	override isKeyboardSelectable(): boolean {
		return false;
	}

	override decorate(): JSX.Element {
		return <SlashOptionalHintPill remaining={this.getLatest().__remaining} />;
	}
}

export function $createSlashOptionalHintNode(remaining: number): SlashOptionalHintNode {
	return new SlashOptionalHintNode(remaining);
}

export function $isSlashOptionalHintNode(node: LexicalNode | null | undefined): node is SlashOptionalHintNode {
	return node instanceof SlashOptionalHintNode;
}
