// SPDX-License-Identifier: AGPL-3.0-or-later

import {ensureSyntaxTree, syntaxTree} from '@codemirror/language';
import type {EditorState} from '@codemirror/state';
import type {EditorView} from '@codemirror/view';

const PARSE_TIMEOUT_MS = 500;

export function hasCssParseErrors(state: EditorState): boolean {
	const tree = ensureSyntaxTree(state, state.doc.length, PARSE_TIMEOUT_MS) ?? syntaxTree(state);
	let found = false;
	tree.iterate({
		enter: (node) => {
			if (found) return false;
			if (node.type.isError) found = true;
			return undefined;
		},
	});
	return found;
}

export type FormatResult = 'formatted' | 'unchanged' | 'has-errors' | 'stale';

export async function formatCssDocument(
	view: EditorView,
	options: {useTabs: boolean; tabWidth: number},
): Promise<FormatResult> {
	const startDoc = view.state.doc;
	if (hasCssParseErrors(view.state)) return 'has-errors';
	const [prettier, postcss] = await Promise.all([import('prettier/standalone'), import('prettier/plugins/postcss')]);
	const source = startDoc.toString();
	let result: {formatted: string; cursorOffset: number};
	try {
		result = await prettier.formatWithCursor(source, {
			parser: 'css',
			plugins: [postcss],
			useTabs: options.useTabs,
			tabWidth: options.tabWidth,
			cursorOffset: view.state.selection.main.head,
		});
	} catch {
		return 'has-errors';
	}
	if (view.state.doc !== startDoc) return 'stale';
	if (result.formatted === source) return 'unchanged';
	view.dispatch({
		changes: {from: 0, to: source.length, insert: result.formatted},
		selection: {anchor: Math.max(0, Math.min(result.cursorOffset, result.formatted.length))},
		scrollIntoView: true,
		userEvent: 'input.format',
	});
	return 'formatted';
}
