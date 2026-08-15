// SPDX-License-Identifier: AGPL-3.0-or-later

import {SearchFilterPill} from '@app/features/lexical/search/SearchFilterPill';
import {
	DecoratorNode,
	type DOMExportOutput,
	type EditorConfig,
	type NodeKey,
	type SerializedLexicalNode,
	type Spread,
} from 'lexical';
import type {JSX} from 'react';

export type SerializedSearchFilterNode = Spread<
	{
		raw: string;
		filterKey: string;
		value: string;
		exclude: boolean;
	},
	SerializedLexicalNode
>;

export interface SearchFilterNodeOptions {
	readonly raw: string;
	readonly filterKey: string;
	readonly value: string;
	readonly exclude: boolean;
	readonly key?: NodeKey;
}

export class SearchFilterNode extends DecoratorNode<JSX.Element> {
	__raw: string;
	__filterKey: string;
	__value: string;
	__exclude: boolean;

	static override getType(): string {
		return 'search-filter';
	}

	static override clone(node: SearchFilterNode): SearchFilterNode {
		return new SearchFilterNode({
			raw: node.__raw,
			filterKey: node.__filterKey,
			value: node.__value,
			exclude: node.__exclude,
			key: node.__key,
		});
	}

	static override importJSON(serializedNode: SerializedSearchFilterNode): SearchFilterNode {
		return $createSearchFilterNode({
			raw: serializedNode.raw,
			filterKey: serializedNode.filterKey,
			value: serializedNode.value,
			exclude: serializedNode.exclude,
		});
	}

	constructor({raw, filterKey, value, exclude, key}: SearchFilterNodeOptions) {
		super(key);
		this.__raw = raw;
		this.__filterKey = filterKey;
		this.__value = value;
		this.__exclude = exclude;
	}

	override exportJSON(): SerializedSearchFilterNode {
		return {
			...super.exportJSON(),
			raw: this.__raw,
			filterKey: this.__filterKey,
			value: this.__value,
			exclude: this.__exclude,
		};
	}

	override createDOM(config: EditorConfig): HTMLElement {
		const span = document.createElement('span');
		const className = config.theme.searchFilter;
		if (typeof className === 'string') {
			span.className = className;
		}
		span.setAttribute('data-lexical-search-filter', 'true');
		span.spellcheck = false;
		return span;
	}

	override updateDOM(): boolean {
		return false;
	}

	override exportDOM(): DOMExportOutput {
		const element = document.createElement('span');
		element.setAttribute('data-lexical-search-filter', 'true');
		element.textContent = this.__raw;
		return {element};
	}

	override getTextContent(): string {
		return this.getLatest().__raw;
	}

	override isInline(): true {
		return true;
	}

	override isKeyboardSelectable(): false {
		return false;
	}

	override decorate(): JSX.Element {
		return (
			<SearchFilterPill
				nodeKey={this.getKey()}
				filterKey={this.__filterKey}
				value={this.__value}
				exclude={this.__exclude}
			/>
		);
	}
}

export function $createSearchFilterNode(options: SearchFilterNodeOptions): SearchFilterNode {
	return new SearchFilterNode(options);
}
