// SPDX-License-Identifier: AGPL-3.0-or-later

import {ComposerAtomicPresentation} from '@app/features/lexical/composer/nodes/ComposerAtomicPresentation';
import {ComposerCustomEmoji} from '@app/features/lexical/composer/nodes/ComposerCustomEmoji';
import styles from '@app/features/lexical/composer/nodes/ComposerInline.module.css';
import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {
	DecoratorNode,
	type DOMExportOutput,
	type EditorConfig,
	type LexicalNode,
	type NodeKey,
	type SerializedLexicalNode,
	type Spread,
} from 'lexical';
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

export class ComposerCustomEmojiNode extends DecoratorNode<JSX.Element> {
	__emojiId: string;
	__animated: boolean;
	__display: string;
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

	__literal: boolean;
	__spoiler: boolean;

	constructor(
		emojiId: string,
		animated: boolean,
		display: string,
		wire: string,
		literal = false,
		spoiler = false,
		key?: NodeKey,
	) {
		super(key);
		this.__emojiId = emojiId;
		this.__animated = animated;
		this.__display = display;
		this.__wire = wire;
		this.__literal = literal;
		this.__spoiler = spoiler;
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

	override updateDOM(): boolean {
		return false;
	}

	override exportDOM(): DOMExportOutput {
		const element = document.createElement('span');
		element.textContent = this.__wire;
		return {element};
	}

	override getTextContent(): string {
		return this.getLatest().__display;
	}

	getWireText(): string {
		return this.getLatest().__wire;
	}

	getSegmentType(): MentionSegment['type'] {
		return 'emoji';
	}

	getEmojiId(): string {
		return this.getLatest().__emojiId;
	}

	isLiteral(): boolean {
		return this.getLatest().__literal;
	}

	setLiteral(literal: boolean): this {
		this.getWritable().__literal = literal;
		return this;
	}

	isSpoiler(): boolean {
		return this.getLatest().__spoiler;
	}

	setSpoiler(spoiler: boolean): this {
		this.getWritable().__spoiler = spoiler;
		return this;
	}

	override isInline(): true {
		return true;
	}

	override isKeyboardSelectable(): boolean {
		return true;
	}

	override decorate(): JSX.Element {
		return (
			<ComposerAtomicPresentation spoiler={this.__spoiler}>
				{this.__literal ? (
					<span className={styles.literal}>{this.__wire}</span>
				) : (
					<ComposerCustomEmoji emojiId={this.__emojiId} animated={this.__animated} display={this.__display} />
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
