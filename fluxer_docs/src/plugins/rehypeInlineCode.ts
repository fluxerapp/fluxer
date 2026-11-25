// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RehypePlugin} from '@astrojs/markdown-remark';
import type {Element, ElementContent, Root, RootContent} from 'hast';
import {CODE_NOWRAP_LIMIT} from '../table/DocsTableMetrics.ts';

const BREAK_LIMIT = 34;

function textOf(node: ElementContent): string {
	if (node.type === 'text') {
		return node.value;
	}
	if (node.type === 'element') {
		return node.children.map(textOf).join('');
	}
	return '';
}

const LONG_BREAKS = /(?<=_)(?=[^_])(?![^{}]*\})|(?<=[\w}][/:])(?=[\w{])|(?<=\w\.)(?=\w{4})/u;
export const CELL_BREAKS = /(?<=[\w}][_./])(?=[\w{])/u;
const FINGERPRINT = /^[0-9A-F]{32,}$/u;
const FLAG = /^(--?\w[\w-]*)(\s.+)$/u;
const FINGERPRINT_BREAKS = /(?<=^(?:[0-9A-F]{4})+)(?=[0-9A-F])/u;

export const CELL_BREAK_MIN = 17;

const HYPHENATED = /(\S*\S-\S+|-\S+)/u;
const HYPHEN_RUN_LIMIT = 30;

function keepHyphens(children: Array<ElementContent>, maxRun: number): Array<ElementContent> {
	return children.flatMap((child): Array<ElementContent> => {
		if (child.type !== 'text') {
			return [child];
		}
		return child.value
			.split(HYPHENATED)
			.filter((part) => part.length > 0)
			.map((part): ElementContent => {
				if (part.length > maxRun || !HYPHENATED.test(part)) {
					return {type: 'text', value: part};
				}
				return {
					type: 'element',
					tagName: 'span',
					properties: {className: ['flx-nowrap']},
					children: [{type: 'text', value: part}],
				};
			});
	});
}

export function insertBreaks(children: Array<ElementContent>, pattern: RegExp, minWord = 0): Array<ElementContent> {
	return children.flatMap((child): Array<ElementContent> => {
		if (child.type !== 'text') {
			return [child];
		}
		const out: Array<ElementContent> = [{type: 'text', value: ''}];
		for (const word of child.value.split(/(?=\s)|(?<=\s)/u)) {
			const parts = word.length < minWord ? [word] : word.split(pattern);
			for (const [index, part] of parts.entries()) {
				if (index > 0) {
					out.push({type: 'element', tagName: 'wbr', properties: {}, children: []}, {type: 'text', value: ''});
				}
				const last = out[out.length - 1] as {value: string};
				last.value += part;
			}
		}
		return out;
	});
}

function mark(code: Element, inCell: boolean): void {
	const text = textOf(code);
	if (FINGERPRINT.test(text)) {
		code.children = insertBreaks(code.children, FINGERPRINT_BREAKS);
		return;
	}
	if (text.length >= BREAK_LIMIT) {
		code.children = keepHyphens(insertBreaks(code.children, LONG_BREAKS), inCell ? CELL_BREAK_MIN : HYPHEN_RUN_LIMIT);
		return;
	}
	const flag = FLAG.exec(text);
	if (inCell && flag !== null && text.length > CODE_NOWRAP_LIMIT && code.children.length === 1) {
		code.children = [
			{
				type: 'element',
				tagName: 'span',
				properties: {className: ['flx-nowrap']},
				children: [{type: 'text', value: flag[1]!}],
			},
			{type: 'text', value: flag[2]!},
		];
		return;
	}
	if (inCell) {
		code.children = keepHyphens(insertBreaks(code.children, CELL_BREAKS, CELL_BREAK_MIN), CELL_BREAK_MIN);
	}
	if (text.length > CODE_NOWRAP_LIMIT) {
		if (!inCell) {
			code.children = keepHyphens(code.children, HYPHEN_RUN_LIMIT);
		}
		return;
	}
	const current = code.properties.className;
	const list = Array.isArray(current) ? current.map(String) : [];
	code.properties.className = [...list, 'flx-nowrap'];
}

function walk(node: Root | RootContent, inCell: boolean): void {
	if (!('children' in node)) {
		return;
	}
	if (node.type === 'element') {
		if (node.tagName === 'pre') {
			return;
		}
		if (node.tagName === 'code') {
			mark(node, inCell);
			return;
		}
	}
	const name = node.type === 'element' ? node.tagName : (node as {name?: string | null}).name;
	const cell = inCell || name === 'td' || name === 'th';
	for (const child of node.children as Array<RootContent>) {
		walk(child, cell);
	}
	if (typeof name !== 'string') {
		return;
	}
	const parent = node as {children: Array<ElementContent>};
	if (cell) {
		parent.children = glueCode(parent.children, breaksWhole);
	} else if (name !== 'a') {
		parent.children = glueCode(parent.children, isToken);
	}
	if (name === 'a' && isCodeOnly(node as Element)) {
		addClass(node as Element, 'flx-code-link');
	}
}

function isCodeOnly(link: Element): boolean {
	const kept = link.children.filter((child) => child.type !== 'text' || child.value.trim().length > 0);
	const only = kept[0];
	if (kept.length !== 1 || only?.type !== 'element') {
		return false;
	}
	if (only.tagName === 'code') {
		return true;
	}
	return only.tagName === 'span' && hasClass(only, 'flx-glue');
}

function hasClass(node: Element, name: string): boolean {
	const className = node.properties.className;
	return Array.isArray(className) && className.includes(name);
}

function addClass(node: Element, name: string): void {
	const current = node.properties.className;
	const list = Array.isArray(current) ? current.map(String) : [];
	node.properties.className = [...list, name];
}

function isLongToken(node: ElementContent | undefined): node is Element {
	if (node?.type !== 'element' || node.tagName !== 'code') {
		return false;
	}
	const text = textOf(node);
	return text.length > CODE_NOWRAP_LIMIT && !/\s/u.test(text);
}

function isToken(node: ElementContent | undefined): node is Element {
	if (node?.type !== 'element' || node.tagName !== 'a') {
		return isLongToken(node);
	}
	const kept = node.children.filter((child) => child.type !== 'text' || child.value.trim().length > 0);
	return kept.length === 1 && isLongToken(kept[0]);
}

function breaksWhole(node: ElementContent | undefined): node is Element {
	if (node?.type !== 'element' || node.tagName !== 'code') {
		return false;
	}
	const className = node.properties.className;
	return (
		Array.isArray(className) &&
		className.includes('flx-nowrap') &&
		node.children.some((child) => child.type === 'element' && child.tagName === 'wbr')
	);
}

function glueCode(
	children: Array<ElementContent>,
	keepWhole: (node: ElementContent | undefined) => node is Element,
): Array<ElementContent> {
	const out: Array<ElementContent> = [];
	for (let index = 0; index < children.length; index += 1) {
		const child = children[index]!;
		if (!keepWhole(child)) {
			out.push(child);
			continue;
		}
		const previous = out.at(-1);
		const next = children[index + 1];
		const lead = previous?.type === 'text' ? (/[^\s\w]+$/u.exec(previous.value)?.[0] ?? '') : '';
		const trail = next?.type === 'text' ? (/^[^\s\w]+/u.exec(next.value)?.[0] ?? '') : '';
		const kept: Array<ElementContent> = [];
		if (previous?.type === 'text' && lead.length > 0) {
			previous.value = previous.value.slice(0, -lead.length);
			kept.push({type: 'text', value: lead});
		}
		kept.push(child);
		if (next?.type === 'text' && trail.length > 0) {
			kept.push({type: 'text', value: trail});
			next.value = next.value.slice(trail.length);
		}
		out.push({type: 'element', tagName: 'span', properties: {className: ['flx-glue']}, children: kept});
	}
	return out.filter((child) => child.type !== 'text' || child.value.length > 0);
}

export const rehypeInlineCode = (): RehypePlugin => () => (tree) => {
	walk(tree, false);
};
