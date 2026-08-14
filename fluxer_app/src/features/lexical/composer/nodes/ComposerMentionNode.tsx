// SPDX-License-Identifier: AGPL-3.0-or-later

import {ComposerAtomicPresentation} from '@app/features/lexical/composer/nodes/ComposerAtomicPresentation';
import styles from '@app/features/lexical/composer/nodes/ComposerInline.module.css';
import {ComposerMentionPill} from '@app/features/lexical/composer/nodes/ComposerMentionPill';
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
import type {CSSProperties, JSX} from 'react';

export type ComposerMentionType = 'user' | 'role' | 'channel' | 'special';

export const ComposerMentionPresentation = {
	none: 0,
	italic: 1 << 0,
	underline: 1 << 1,
	strike: 1 << 2,
	subtext: 1 << 3,
} as const;

export type ComposerMentionPresentationFormat = number;

export type SerializedComposerMentionNode = Spread<
	{
		mentionType: ComposerMentionType;
		mentionId: string;
		display: string;
		wire: string;
		literal: boolean;
		spoiler: boolean;
		presentation?: ComposerMentionPresentationFormat;
	},
	SerializedLexicalNode
>;

export class ComposerMentionNode extends DecoratorNode<JSX.Element> {
	__mentionType: ComposerMentionType;
	__mentionId: string;
	__display: string;
	__wire: string;

	static override getType(): string {
		return 'composer-mention';
	}

	static override clone(node: ComposerMentionNode): ComposerMentionNode {
		return new ComposerMentionNode(
			node.__mentionType,
			node.__mentionId,
			node.__display,
			node.__wire,
			node.__literal,
			node.__spoiler,
			node.__presentation,
			node.__key,
		);
	}

	static override importJSON(serializedNode: SerializedComposerMentionNode): ComposerMentionNode {
		return $createComposerMentionNode(
			serializedNode.mentionType,
			serializedNode.mentionId,
			serializedNode.display,
			serializedNode.wire,
			serializedNode.literal == null ? false : serializedNode.literal,
			serializedNode.spoiler == null ? false : serializedNode.spoiler,
			serializedNode.presentation == null ? ComposerMentionPresentation.none : serializedNode.presentation,
		);
	}

	__literal: boolean;
	__spoiler: boolean;
	__presentation: ComposerMentionPresentationFormat;

	constructor(
		mentionType: ComposerMentionType,
		mentionId: string,
		display: string,
		wire: string,
		literal = false,
		spoiler = false,
		presentation: ComposerMentionPresentationFormat = ComposerMentionPresentation.none,
		key?: NodeKey,
	) {
		super(key);
		this.__mentionType = mentionType;
		this.__mentionId = mentionId;
		this.__display = display;
		this.__wire = wire;
		this.__literal = literal;
		this.__spoiler = spoiler;
		this.__presentation = presentation;
	}

	override exportJSON(): SerializedComposerMentionNode {
		const serialized: SerializedComposerMentionNode = {
			...super.exportJSON(),
			mentionType: this.__mentionType,
			mentionId: this.__mentionId,
			display: this.__display,
			wire: this.__wire,
			literal: this.__literal,
			spoiler: this.__spoiler,
		};
		if (this.__presentation !== ComposerMentionPresentation.none) {
			serialized.presentation = this.__presentation;
		}
		return serialized;
	}

	override createDOM(config: EditorConfig): HTMLElement {
		const span = document.createElement('span');
		const className = config.theme.composerMention;
		if (typeof className === 'string') {
			span.className = className;
		}
		span.setAttribute('data-lexical-composer-mention', this.__mentionType);
		span.spellcheck = false;
		return span;
	}

	override updateDOM(): boolean {
		return false;
	}

	override exportDOM(): DOMExportOutput {
		const element = document.createElement('span');
		element.setAttribute('data-lexical-composer-mention', this.__mentionType);
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
		return this.getLatest().__mentionType;
	}

	getMentionId(): string {
		return this.getLatest().__mentionId;
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

	getPresentation(): ComposerMentionPresentationFormat {
		return this.getLatest().__presentation;
	}

	setPresentation(presentation: ComposerMentionPresentationFormat): this {
		this.getWritable().__presentation = presentation;
		return this;
	}

	override isInline(): true {
		return true;
	}

	override isKeyboardSelectable(): boolean {
		return true;
	}

	override decorate(): JSX.Element {
		const presentationStyle = mentionPresentationStyle(this.__presentation);
		return (
			<ComposerAtomicPresentation spoiler={this.__spoiler}>
				<span style={presentationStyle}>
					{this.__literal ? (
						<span className={styles.literal}>{this.__wire}</span>
					) : (
						<ComposerMentionPill
							mentionType={this.__mentionType}
							mentionId={this.__mentionId}
							display={this.__display}
						/>
					)}
				</span>
			</ComposerAtomicPresentation>
		);
	}
}

function mentionPresentationStyle(presentation: ComposerMentionPresentationFormat): CSSProperties | undefined {
	if (presentation === ComposerMentionPresentation.none) {
		return undefined;
	}
	const underline = (presentation & ComposerMentionPresentation.underline) !== 0;
	const strike = (presentation & ComposerMentionPresentation.strike) !== 0;
	return {
		fontStyle: (presentation & ComposerMentionPresentation.italic) !== 0 ? 'italic' : undefined,
		textDecorationLine:
			underline && strike ? 'underline line-through' : underline ? 'underline' : strike ? 'line-through' : undefined,
		fontSize: (presentation & ComposerMentionPresentation.subtext) !== 0 ? '0.85em' : undefined,
		color:
			(presentation & ComposerMentionPresentation.subtext) !== 0
				? 'var(--text-muted,var(--text-secondary))'
				: undefined,
	};
}

export function $createComposerMentionNode(
	mentionType: ComposerMentionType,
	mentionId: string,
	display: string,
	wire: string,
	literal = false,
	spoiler = false,
	presentation: ComposerMentionPresentationFormat = ComposerMentionPresentation.none,
): ComposerMentionNode {
	return new ComposerMentionNode(mentionType, mentionId, display, wire, literal, spoiler, presentation);
}

export function $isComposerMentionNode(node: LexicalNode | null | undefined): node is ComposerMentionNode {
	return node instanceof ComposerMentionNode;
}
