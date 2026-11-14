// SPDX-License-Identifier: AGPL-3.0-or-later

import {ComposerAtomicPresentation} from '@app/features/lexical/composer/nodes/ComposerAtomicPresentation';
import {ComposerAtomicTokenNode} from '@app/features/lexical/composer/nodes/ComposerAtomicTokenNode';
import {ComposerCustomEmoji} from '@app/features/lexical/composer/nodes/ComposerCustomEmoji';
import styles from '@app/features/lexical/composer/nodes/ComposerInline.module.css';
import type {DOMExportOutput, EditorConfig, LexicalNode, NodeKey, SerializedLexicalNode, Spread} from 'lexical';
import type {JSX} from 'react';

export type SerializedComposerCustomEmojiNode = Spread<
	{
		emojiId: string;
		animated: boolean;
		display: string;
		wire: string;
		literal: boolean;
		spoiler: boolean;
	},
	SerializedLexicalNode
>;

export class ComposerCustomEmojiNode extends ComposerAtomicTokenNode {
	__emojiId: string;
	__animated: boolean;
	__wire: string;

	static override getType(): string {
		return 'composer-custom-emoji';
	}

	static override clone(node: ComposerCustomEmojiNode): ComposerCustomEmojiNode {
		return new ComposerCustomEmojiNode(
			node.__emojiId,
			node.__animated,
			node.__display,
			node.__wire,
			node.__literal,
			node.__spoiler,
			node.__key,
		);
	}

	static override importJSON(serializedNode: SerializedComposerCustomEmojiNode): ComposerCustomEmojiNode {
		return $createComposerCustomEmojiNode(
			serializedNode.emojiId,
			serializedNode.animated,
			serializedNode.display,
			serializedNode.wire,
			serializedNode.literal == null ? false : serializedNode.literal,
			serializedNode.spoiler == null ? false : serializedNode.spoiler,
		);
	}

	constructor(
		emojiId: string,
		animated: boolean,
		display: string,
		wire: string,
		literal = false,
		spoiler = false,
		key?: NodeKey,
	) {
		super(display, literal, spoiler, key);
		this.__emojiId = emojiId;
		this.__animated = animated;
		this.__wire = wire;
	}

	override exportJSON(): SerializedComposerCustomEmojiNode {
		return {
			...super.exportJSON(),
			emojiId: this.__emojiId,
			animated: this.__animated,
			display: this.__display,
			wire: this.__wire,
			literal: this.__literal,
			spoiler: this.__spoiler,
		};
	}

	override createDOM(config: EditorConfig): HTMLElement {
		const span = document.createElement('span');
		const className = config.theme.composerCustomEmoji;
		if (typeof className === 'string') {
			span.className = className;
		}
		span.setAttribute('data-lexical-composer-emoji', this.__emojiId);
		span.spellcheck = false;
		return span;
	}

	override exportDOM(): DOMExportOutput {
		const element = document.createElement('span');
		element.textContent = this.__wire;
		return {element};
	}

	getWireText(): string {
		return this.getLatest().__wire;
	}

	getEmojiId(): string {
		return this.getLatest().__emojiId;
	}

	override decorate(): JSX.Element {
		return (
			<ComposerAtomicPresentation
				spoiler={this.__spoiler}
				data-flx="lexical.composer.nodes.composer-custom-emoji-node.composer-atomic-presentation"
			>
				{this.__literal ? (
					<span className={styles.literal} data-flx="lexical.composer.nodes.composer-custom-emoji-node.literal">
						{this.__wire}
					</span>
				) : (
					<ComposerCustomEmoji
						emojiId={this.__emojiId}
						animated={this.__animated}
						display={this.__display}
						data-flx="lexical.composer.nodes.composer-custom-emoji-node.composer-custom-emoji"
					/>
				)}
			</ComposerAtomicPresentation>
		);
	}
}

export function $createComposerCustomEmojiNode(
	emojiId: string,
	animated: boolean,
	display: string,
	wire: string,
	literal = false,
	spoiler = false,
): ComposerCustomEmojiNode {
	return new ComposerCustomEmojiNode(emojiId, animated, display, wire, literal, spoiler);
}

export function $isComposerCustomEmojiNode(node: LexicalNode | null | undefined): node is ComposerCustomEmojiNode {
	return node instanceof ComposerCustomEmojiNode;
}
