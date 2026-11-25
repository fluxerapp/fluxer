// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RehypePlugin} from '@astrojs/markdown-remark';
import type {Element, ElementContent, Root, RootContent} from 'hast';

function textOf(node: Root | RootContent | ElementContent): string {
	if (node.type === 'text') {
		return node.value;
	}
	if ('children' in node) {
		return (node.children as Array<RootContent>).map(textOf).join('');
	}
	return '';
}

function slugify(text: string): string {
	return text
		.toLowerCase()
		.trim()
		.replace(/[^\p{L}\p{N}\s_-]/gu, '')
		.replace(/\s+/gu, '-');
}

function headings(node: Root | RootContent, out: Array<Element>): void {
	if (node.type === 'element' && (node.tagName === 'h2' || node.tagName === 'h3')) {
		out.push(node);
		return;
	}
	if ('children' in node) {
		for (const child of node.children as Array<RootContent>) {
			headings(child, out);
		}
	}
}

export const rehypeHeadingContext = (): RehypePlugin => () => (tree) => {
	const list: Array<Element> = [];
	headings(tree, list);
	const counts = new Map<string, number>();
	for (const heading of list) {
		if (heading.tagName === 'h3') {
			const text = textOf(heading).trim();
			counts.set(text, (counts.get(text) ?? 0) + 1);
		}
	}
	const ids = new Set<string>();
	let section = '';
	for (const heading of list) {
		const text = textOf(heading).trim();
		if (heading.tagName === 'h2') {
			section = text;
			continue;
		}
		if ((counts.get(text) ?? 0) < 2 || section.length === 0 || typeof heading.properties.id === 'string') {
			continue;
		}
		const base = slugify(`${section} ${text}`);
		let id = base;
		for (let n = 2; ids.has(id); n += 1) {
			id = `${base}-${n.toString()}`;
		}
		ids.add(id);
		heading.properties.id = id;
		heading.children.unshift({
			type: 'element',
			tagName: 'span',
			properties: {className: ['sr-only']},
			children: [{type: 'text', value: `${section} `}],
		});
	}
};
