// SPDX-License-Identifier: AGPL-3.0-or-later

import {DecoratorNode, type NodeKey} from 'lexical';
import type {JSX} from 'react';

export abstract class ComposerAtomicTokenNode extends DecoratorNode<JSX.Element> {
	__display: string;
	__literal: boolean;
	__spoiler: boolean;

	constructor(display: string, literal: boolean, spoiler: boolean, key?: NodeKey) {
		super(key);
		this.__display = display;
		this.__literal = literal;
		this.__spoiler = spoiler;
	}

	override updateDOM(): boolean {
		return false;
	}

	override getTextContent(): string {
		return this.getLatest().__display;
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

	override isKeyboardSelectable(): false {
		return false;
	}
}
