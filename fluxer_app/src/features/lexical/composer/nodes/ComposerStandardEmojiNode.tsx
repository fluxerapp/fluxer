// SPDX-License-Identifier: AGPL-3.0-or-later

import {ComposerAtomicPresentation} from '@app/features/lexical/composer/nodes/ComposerAtomicPresentation';
import styles from '@app/features/lexical/composer/nodes/ComposerInline.module.css';
import {ComposerStandardEmoji} from '@app/features/lexical/composer/nodes/ComposerStandardEmoji';
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

export type SerializedComposerStandardEmojiNode = Spread<
	{
		name: string;
		surrogate: string;
		url: string | null;
		display: string;
		literal: boolean;
		spoiler: boolean;
	},
	SerializedLexicalNode
>;

export class ComposerStandardEmojiNode extends DecoratorNode<JSX.Element> {
	__name: string;
	__surrogate: string;
	__url: string | null;
	__display: string;

	static override getType(): string {
		return 'composer-standard-emoji';
	}

	static override clone(node: ComposerStandardEmojiNode): ComposerStandardEmojiNode {
		return new ComposerStandardEmojiNode(
			node.__name,
			node.__surrogate,
			node.__url,
			node.__display,
			node.__literal,
			node.__spoiler,
			node.__key,
		);
	}

	static override importJSON(serializedNode: SerializedComposerStandardEmojiNode): ComposerStandardEmojiNode {
		return $createComposerStandardEmojiNode(
			serializedNode.name,
			serializedNode.surrogate,
			serializedNode.url,
			serializedNode.display,
			serializedNode.literal == null ? false : serializedNode.literal,
			serializedNode.spoiler == null ? false : serializedNode.spoiler,
		);
	}

	__literal: boolean;
	__spoiler: boolean;

	constructor(
		name: string,
		surrogate: string,
		url: string | null,
		display: string,
		literal = false,
		spoiler = false,
		key?: NodeKey,
	) {
		super(key);
		this.__name = name;
		this.__surrogate = surrogate;
		this.__url = url;
		this.__display = display;
		this.__literal = literal;
		this.__spoiler = spoiler;
	}

	override exportJSON(): SerializedComposerStandardEmojiNode {
		return {
			...super.exportJSON(),
			name: this.__name,
			surrogate: this.__surrogate,
			url: this.__url,
			display: this.__display,
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
		span.setAttribute('data-lexical-composer-standard-emoji', this.__name);
		span.spellcheck = false;
		return span;
	}

	override updateDOM(): boolean {
		return false;
	}

	override exportDOM(): DOMExportOutput {
		const element = document.createElement('span');
		element.textContent = this.__display;
		return {element};
	}

	override getTextContent(): string {
		return this.getLatest().__display;
	}

	getEmojiName(): string {
		return this.getLatest().__name;
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
					<span className={styles.literal}>{this.__display}</span>
				) : (
					<ComposerStandardEmoji
						name={this.__name}
						surrogate={this.__surrogate}
						url={this.__url}
						display={this.__display}
					/>
				)}
			</ComposerAtomicPresentation>
		);
	}
}

export function $createComposerStandardEmojiNode(
	name: string,
	surrogate: string,
	url: string | null,
	display: string,
	literal = false,
	spoiler = false,
): ComposerStandardEmojiNode {
	return new ComposerStandardEmojiNode(name, surrogate, url, display, literal, spoiler);
}

export function $isComposerStandardEmojiNode(node: LexicalNode | null | undefined): node is ComposerStandardEmojiNode {
	return node instanceof ComposerStandardEmojiNode;
}
