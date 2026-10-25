// SPDX-License-Identifier: AGPL-3.0-or-later

import {ensureSyntaxTree, foldService, syntaxTree} from '@codemirror/language';
import type {EditorState, Extension} from '@codemirror/state';

export type OutlineEntryKind = 'rule' | 'at-rule' | 'region';

export interface OutlineEntry {
	kind: OutlineEntryKind;
	label: string;
	from: number;
	depth: number;
}

const AT_RULE_NODES = new Set([
	'ImportStatement',
	'MediaStatement',
	'CharsetStatement',
	'NamespaceStatement',
	'KeyframesStatement',
	'SupportsStatement',
	'ScopeStatement',
	'FontFeatureStatement',
	'AtRule',
]);
const REGION_START = /^\/\*\s*#region\b\s*(.*?)\s*\*\/$/s;
const REGION_END = /^\/\*\s*#endregion\b.*\*\/$/s;
const REGION_START_LINE = /^\s*\/\*\s*#region\b/;
const REGION_END_LINE = /^\s*\/\*\s*#endregion\b/;
const LABEL_MAX_LENGTH = 120;
const PARSE_TIMEOUT_MS = 200;

function compact(text: string): string {
	const collapsed = text.replace(/\s+/g, ' ').trim();
	return collapsed.length > LABEL_MAX_LENGTH ? `${collapsed.slice(0, LABEL_MAX_LENGTH - 1)}…` : collapsed;
}

export function buildCssOutline(state: EditorState): Array<OutlineEntry> {
	const tree = ensureSyntaxTree(state, state.doc.length, PARSE_TIMEOUT_MS) ?? syntaxTree(state);
	const entries: Array<OutlineEntry> = [];
	let depth = 0;
	let regionDepth = 0;
	tree.iterate({
		enter: (node) => {
			if (node.name === 'Comment') {
				const text = state.sliceDoc(node.from, node.to);
				const start = REGION_START.exec(text);
				if (start) {
					entries.push({kind: 'region', label: compact(start[1]), from: node.from, depth: depth + regionDepth});
					regionDepth += 1;
				} else if (REGION_END.test(text) && regionDepth > 0) {
					regionDepth -= 1;
				}
				return false;
			}
			if (node.name === 'RuleSet' || AT_RULE_NODES.has(node.name)) {
				const block = node.node.getChild('Block') ?? node.node.getChild('KeyframeList');
				const label = compact(state.sliceDoc(node.from, block?.from ?? node.to).replace(/;$/, ''));
				entries.push({
					kind: node.name === 'RuleSet' ? 'rule' : 'at-rule',
					label,
					from: node.from,
					depth: depth + regionDepth,
				});
				depth += 1;
			}
			return undefined;
		},
		leave: (node) => {
			if (node.name === 'RuleSet' || AT_RULE_NODES.has(node.name)) depth -= 1;
		},
	});
	return entries;
}

export function cssRegionFolding(): Extension {
	return foldService.of((state, lineStart, lineEnd) => {
		const startLine = state.doc.lineAt(lineStart);
		if (!REGION_START_LINE.test(startLine.text)) return null;
		let nesting = 0;
		for (let number = startLine.number + 1; number <= state.doc.lines; number += 1) {
			const line = state.doc.line(number);
			if (REGION_START_LINE.test(line.text)) nesting += 1;
			else if (REGION_END_LINE.test(line.text)) {
				if (nesting === 0) return line.from > lineEnd ? {from: lineEnd, to: line.to} : null;
				nesting -= 1;
			}
		}
		return null;
	});
}
