// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Element, ElementContent} from 'hast';

const WORD_BREAK =
	/(?<=[^/]\/)(?=[^/])|(?<=\w)(?=[?&])|(?<=\w=)(?=[^\s=])|(?<=\w_)(?=[a-zA-Z]\w{3})|(?<!=\S*)(?<=\w\.)(?=[a-zA-Z]\w{3})/uy;
const HYPHEN_WORD = /\w-\w/u;
const HYPHEN_GLUE_LIMIT = 24;

function splitWord(before: string, piece: string): Array<string> {
	const text = before + piece;
	const parts: Array<string> = [];
	let start = 0;
	for (let offset = before.length === 0 ? 1 : 0; offset < piece.length; offset += 1) {
		WORD_BREAK.lastIndex = before.length + offset;
		if (WORD_BREAK.test(text)) {
			parts.push(piece.slice(start, offset));
			start = offset;
		}
	}
	parts.push(piece.slice(start));
	return parts;
}

interface Fragment {
	readonly text: string;
	readonly chain: ReadonlyArray<Element>;
}

function flatten(nodes: ReadonlyArray<ElementContent>, chain: ReadonlyArray<Element>, out: Array<Fragment>): void {
	for (const node of nodes) {
		if (node.type === 'text') {
			out.push({text: node.value, chain});
		} else if (node.type === 'element') {
			flatten(node.children, [...chain, node], out);
		}
	}
}

function rebuild(chain: ReadonlyArray<Element>, text: string): ElementContent {
	let node: ElementContent = {type: 'text', value: text};
	for (let index = chain.length - 1; index >= 0; index -= 1) {
		const source = chain[index]!;
		node = {type: 'element', tagName: source.tagName, properties: {...source.properties}, children: [node]};
	}
	return node;
}

export function groupWords(nodes: ReadonlyArray<ElementContent>): Array<ElementContent> {
	const fragments: Array<Fragment> = [];
	flatten(nodes, [], fragments);
	const out: Array<ElementContent> = [];
	let word: Element | null = null;
	let wordText = '';
	for (const fragment of fragments) {
		for (const piece of fragment.text.split(/(\s+)/u)) {
			if (piece.length === 0) {
				continue;
			}
			if (/^\s/u.test(piece)) {
				word = null;
				wordText = '';
				out.push(rebuild(fragment.chain, piece));
				continue;
			}
			if (word === null) {
				word = {type: 'element', tagName: 'span', properties: {className: ['ec-tok']}, children: []};
				out.push(word);
			}
			const parts = splitWord(wordText, piece);
			wordText += piece;
			for (const [index, part] of parts.entries()) {
				if (index > 0) {
					word.children.push({type: 'element', tagName: 'wbr', properties: {}, children: []});
				}
				if (part.length === 0) {
					continue;
				}
				const node = rebuild(fragment.chain, part);
				word.children.push(HYPHEN_WORD.test(part) && part.length <= HYPHEN_GLUE_LIMIT ? glue([node]) : node);
			}
		}
	}
	return keepContinuation(keepFlagValues(out));
}

const FLAG = /^-{1,2}[A-Za-z][\w-]*$/u;

function keepFlagValues(nodes: Array<ElementContent>): Array<ElementContent> {
	const out: Array<ElementContent> = [];
	for (let index = 0; index < nodes.length; index += 1) {
		const flag = nodes[index];
		const space = nodes[index + 1];
		const value = nodes[index + 2];
		out.push(flag!);
		if (!isWord(flag) || !isWord(value) || !FLAG.test(textOf(flag)) || !/^[ \t]+$/u.test(textOf(space))) {
			continue;
		}
		const valueText = textOf(value);
		if (valueText.startsWith('-') || valueText === '\\') {
			continue;
		}
		const cut = value.children.findIndex((child) => child.type === 'element' && child.tagName === 'wbr');
		const head = cut === -1 ? value.children : value.children.slice(0, cut);
		const rest = cut === -1 ? [] : value.children.slice(cut + 1);
		flag.children.push(space!, ...head);
		index += 2;
		if (rest.length > 0) {
			value.children = rest;
			out.push(value);
		}
	}
	return out;
}

function glue(children: Array<ElementContent>): Element {
	return {type: 'element', tagName: 'span', properties: {className: ['ec-glue']}, children};
}

function textOf(node: ElementContent | undefined): string {
	if (node === undefined) {
		return '';
	}
	if (node.type === 'text') {
		return node.value;
	}
	return node.type === 'element' ? node.children.map(textOf).join('') : '';
}

function isWord(node: ElementContent | undefined): node is Element {
	return (
		node?.type === 'element' && Array.isArray(node.properties.className) && node.properties.className.includes('ec-tok')
	);
}

function keepContinuation(nodes: Array<ElementContent>): Array<ElementContent> {
	const [word, space, slash] = nodes.slice(-3);
	if (!isWord(word) || !isWord(slash) || textOf(slash) !== '\\' || !/^[ \t]+$/u.test(textOf(space))) {
		return nodes;
	}
	word.children.push(glue([space!]), ...slash.children);
	return nodes.slice(0, -2);
}

function isCode(node: ElementContent): node is Element {
	if (node.type !== 'element') {
		return false;
	}
	const className = node.properties.className;
	return Array.isArray(className) && className.includes('code');
}

export const ecWordTokens = {
	name: 'flx-word-tokens',
	hooks: {
		postprocessRenderedLine: ({renderData}: {renderData: {lineAst: Element}}): void => {
			const code = renderData.lineAst.children.find(isCode);
			if (code !== undefined) {
				code.children = groupWords(code.children);
			}
		},
	},
};
