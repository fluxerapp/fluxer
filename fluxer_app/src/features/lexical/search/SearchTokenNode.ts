// SPDX-License-Identifier: AGPL-3.0-or-later

import {SearchChipRole} from '@app/features/search/utils/SearchQueryParser';
import {
	type EditorConfig,
	type LexicalNode,
	type NodeKey,
	type SerializedTextNode,
	type Spread,
	TextNode,
} from 'lexical';

export type SerializedSearchTokenNode = Spread<{role: SearchChipRole; exclude: boolean}, SerializedTextNode>;

function resolveRoleThemeClass(role: SearchChipRole, config: EditorConfig): unknown {
	switch (role) {
		case SearchChipRole.KEY:
			return config.theme.searchTokenKey;
		case SearchChipRole.UNAPPLIED_VALUE:
			return config.theme.searchTokenUnapplied;
		default:
			return config.theme.searchTokenValue;
	}
}

export class SearchTokenNode extends TextNode {
	__role: SearchChipRole;
	__exclude: boolean;

	static override getType(): string {
		return 'search-token';
	}

	static override clone(node: SearchTokenNode): SearchTokenNode {
		return new SearchTokenNode(node.__text, node.__role, node.__exclude, node.__key);
	}

	static override importJSON(serializedNode: SerializedSearchTokenNode): SearchTokenNode {
		return $createSearchTokenNode(serializedNode.text, serializedNode.role, serializedNode.exclude).updateFromJSON(
			serializedNode,
		);
	}

	constructor(text: string, role: SearchChipRole, exclude: boolean, key?: NodeKey) {
		super(text, key);
		this.__role = role;
		this.__exclude = exclude;
	}

	override exportJSON(): SerializedSearchTokenNode {
		return {...super.exportJSON(), role: this.getRole(), exclude: this.getExclude()};
	}

	getRole(): SearchChipRole {
		return this.getLatest().__role;
	}

	getExclude(): boolean {
		return this.getLatest().__exclude;
	}

	override createDOM(config: EditorConfig): HTMLElement {
		const dom = super.createDOM(config);
		const baseClassName = config.theme.searchTokenBase;
		if (typeof baseClassName === 'string') {
			dom.classList.add(baseClassName);
		}
		const roleClassName = resolveRoleThemeClass(this.__role, config);
		if (typeof roleClassName === 'string') {
			dom.classList.add(roleClassName);
		}
		const excludeClassName = config.theme.searchTokenExclude;
		if (this.__exclude && typeof excludeClassName === 'string') {
			dom.classList.add(excludeClassName);
		}
		dom.setAttribute('data-search-token', this.__role);
		if (this.__exclude) {
			dom.setAttribute('data-search-token-exclude', 'true');
		}
		return dom;
	}

	override updateDOM(prevNode: this, dom: HTMLElement, config: EditorConfig): boolean {
		if (prevNode.__role !== this.__role || prevNode.__exclude !== this.__exclude) {
			return true;
		}
		return super.updateDOM(prevNode, dom, config);
	}
}

export function $createSearchTokenNode(text: string, role: SearchChipRole, exclude: boolean): SearchTokenNode {
	return new SearchTokenNode(text, role, exclude);
}

export function $isSearchTokenNode(node: LexicalNode | null | undefined): node is SearchTokenNode {
	return node instanceof SearchTokenNode;
}
