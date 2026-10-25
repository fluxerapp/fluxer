// SPDX-License-Identifier: AGPL-3.0-or-later

import {normalizeCustomThemeCss} from '@app/features/accessibility/state/Accessibility';
import {historyField} from '@codemirror/commands';
import type {EditorState} from '@codemirror/state';

export interface CachedEditorState {
	doc: string;
	json: unknown;
	scrollTop: number;
}

interface CacheEntry extends CachedEditorState {
	source: string;
}

const entries = new Map<string, CacheEntry>();

export function readCachedEditorState(key: string, storedCss: string): CachedEditorState | null {
	const entry = entries.get(key);
	if (!entry) return null;
	if (entry.source !== storedCss && normalizeCustomThemeCss(entry.doc) !== normalizeCustomThemeCss(storedCss)) {
		entries.delete(key);
		return null;
	}
	return entry;
}

export function writeCachedEditorState(key: string, storedCss: string, state: EditorState, scrollTop: number): void {
	entries.set(key, {
		source: storedCss,
		doc: state.doc.toString(),
		json: state.toJSON({history: historyField}),
		scrollTop,
	});
}
