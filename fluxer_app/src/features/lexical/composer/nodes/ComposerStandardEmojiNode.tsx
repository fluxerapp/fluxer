// SPDX-License-Identifier: AGPL-3.0-or-later

import {ComposerAtomicPresentation} from '@app/features/lexical/composer/nodes/ComposerAtomicPresentation';
import {ComposerAtomicTokenNode} from '@app/features/lexical/composer/nodes/ComposerAtomicTokenNode';
import styles from '@app/features/lexical/composer/nodes/ComposerInline.module.css';
import {ComposerStandardEmoji} from '@app/features/lexical/composer/nodes/ComposerStandardEmoji';
import type {DOMExportOutput, EditorConfig, LexicalNode, NodeKey, SerializedLexicalNode, Spread} from 'lexical';
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

export class ComposerStandardEmojiNode extends ComposerAtomicTokenNode {
	__name: string;
	__surrogate: string;
	__url: string | null;

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

	constructor(
		name: string,
		surrogate: string,
		url: string | null,
		display: string,
		literal = false,
		spoiler = false,
		key?: NodeKey,
	) {
		super(display, literal, spoiler, key);
		this.__name = name;
		this.__surrogate = surrogate;
		this.__url = url;
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

	override exportDOM(): DOMExportOutput {
		const element = document.createElement('span');
		element.textContent = this.__display;
		return {element};
	}

	getWireText(): string {
		const self = this.getLatest();
		return self.__literal ? self.__display : self.__surrogate;
	}

	getEmojiName(): string {
		return this.getLatest().__name;
	}

	override decorate(): JSX.Element {
		return (
			<ComposerAtomicPresentation
				spoiler={this.__spoiler}
				data-flx="lexical.composer.nodes.composer-standard-emoji-node.composer-atomic-presentation"
			>
				{this.__literal ? (
					<span className={styles.literal} data-flx="lexical.composer.nodes.composer-standard-emoji-node.literal">
						{this.__display}
					</span>
				) : (
					<ComposerStandardEmoji
						surrogate={this.__surrogate}
						url={this.__url}
						display={this.__display}
						data-flx="lexical.composer.nodes.composer-standard-emoji-node.composer-standard-emoji"
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
