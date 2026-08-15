// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MentionSegment} from '@app/features/messaging/utils/TextareaSegmentManager';
import {type LexicalNode, type NodeKey, type SerializedTextNode, type Spread, TextNode} from 'lexical';

export type SerializedComposerPlainSegmentNode = Spread<
	{
		segmentType: MentionSegment['type'];
		segmentId: string;
		display: string;
		wire: string;
	},
	SerializedTextNode
>;

export class ComposerPlainSegmentNode extends TextNode {
	__segmentType: MentionSegment['type'];
	__segmentId: string;
	__display: string;
	__wire: string;

	static override getType(): string {
		return 'composer-plain-segment';
	}

	static override clone(node: ComposerPlainSegmentNode): ComposerPlainSegmentNode {
		return new ComposerPlainSegmentNode(
			node.__segmentType,
			node.__segmentId,
			node.__display,
			node.__wire,
			node.__text,
			node.__key,
		);
	}

	static override importJSON(serializedNode: SerializedComposerPlainSegmentNode): ComposerPlainSegmentNode {
		return $createComposerPlainSegmentNode(
			serializedNode.segmentType,
			serializedNode.segmentId,
			serializedNode.display,
			serializedNode.wire,
			serializedNode.text,
		).updateFromJSON(serializedNode);
	}

	constructor(
		segmentType: MentionSegment['type'],
		segmentId: string,
		display: string,
		wire: string,
		text = display,
		key?: NodeKey,
	) {
		super(text, key);
		this.__segmentType = segmentType;
		this.__segmentId = segmentId;
		this.__display = display;
		this.__wire = wire;
	}

	override exportJSON(): SerializedComposerPlainSegmentNode {
		return {
			...super.exportJSON(),
			segmentType: this.__segmentType,
			segmentId: this.__segmentId,
			display: this.__display,
			wire: this.__wire,
		};
	}

	getSegmentType(): MentionSegment['type'] {
		return this.getLatest().__segmentType;
	}

	getSegmentId(): string {
		return this.getLatest().__segmentId;
	}

	getDisplayText(): string {
		return this.getLatest().__display;
	}

	getWireText(): string {
		return this.getLatest().__wire;
	}

	isSegmentValid(): boolean {
		return this.getTextContent() === this.getDisplayText();
	}

	override canInsertTextBefore(): false {
		return false;
	}

	override canInsertTextAfter(): false {
		return false;
	}
}

export function $createComposerPlainSegmentNode(
	segmentType: MentionSegment['type'],
	segmentId: string,
	display: string,
	wire: string,
	text = display,
): ComposerPlainSegmentNode {
	return new ComposerPlainSegmentNode(segmentType, segmentId, display, wire, text);
}

export function $isComposerPlainSegmentNode(node: LexicalNode | null | undefined): node is ComposerPlainSegmentNode {
	return node instanceof ComposerPlainSegmentNode;
}
