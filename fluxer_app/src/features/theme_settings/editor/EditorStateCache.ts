// SPDX-License-Identifier: AGPL-3.0-or-later

import {normalizeCustomThemeCss} from '@app/features/accessibility/state/Accessibility';
import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {historyField} from '@codemirror/commands';
import type {EditorState} from '@codemirror/state';
import {isObjectLike as isRecord} from '@fluxer/record_utils/src/RecordGuards';

export interface CachedEditorState {
	doc: string;
	json: unknown;
	scrollTop: number;
}

interface CacheEntry extends CachedEditorState {
	source: string;
}

interface StoredHistory {
	key: string;
	doc: string;
	json: unknown;
	savedAt: number;
}

const STORED_HISTORY_LIMIT = 8;
const STORED_HISTORY_MAX_CHARS = 512 * 1024;

const entries = new Map<string, CacheEntry>();
const openedCss = new Map<string, string>();
let liveEditorCapture: (() => void) | null = null;

function sameCss(left: string, right: string): boolean {
	return normalizeCustomThemeCss(left) === normalizeCustomThemeCss(right);
}

function readStoredHistory(): Array<StoredHistory> {
	try {
		const raw = AppStorage.getItem(AppStorageKey.QUICK_CSS_EDITOR_HISTORY);
		if (raw === null) return [];
		const parsed: unknown = JSON.parse(raw);
		if (!Array.isArray(parsed)) return [];
		return parsed.filter(
			(item): item is StoredHistory =>
				isRecord(item) &&
				typeof item.key === 'string' &&
				typeof item.doc === 'string' &&
				typeof item.savedAt === 'number' &&
				isRecord(item.json),
		);
	} catch {
		return [];
	}
}

function storeHistory(key: string, entry: CacheEntry): void {
	const item: StoredHistory = {key, doc: entry.doc, json: entry.json, savedAt: Date.now()};
	let serialized: string;
	try {
		serialized = JSON.stringify(item);
	} catch {
		return;
	}
	const others = readStoredHistory().filter((stored) => stored.key !== key);
	const next = serialized.length > STORED_HISTORY_MAX_CHARS ? others : [item, ...others];
	next.sort((left, right) => right.savedAt - left.savedAt);
	try {
		AppStorage.setItem(AppStorageKey.QUICK_CSS_EDITOR_HISTORY, JSON.stringify(next.slice(0, STORED_HISTORY_LIMIT)));
	} catch {}
}

export function readCachedEditorState(key: string, storedCss: string): CachedEditorState | null {
	const entry = entries.get(key);
	if (entry) {
		if (entry.source === storedCss || sameCss(entry.doc, storedCss)) return entry;
		entries.delete(key);
	}
	const stored = readStoredHistory().find((item) => item.key === key);
	if (!stored || !sameCss(stored.doc, storedCss)) return null;
	return {doc: stored.doc, json: stored.json, scrollTop: 0};
}

export function writeCachedEditorState(key: string, storedCss: string, state: EditorState, scrollTop: number): void {
	const entry: CacheEntry = {
		source: storedCss,
		doc: state.doc.toString(),
		json: state.toJSON({history: historyField}),
		scrollTop,
	};
	entries.set(key, entry);
	storeHistory(key, entry);
}

export function readOpenedCss(key: string, current: string): string {
	const opened = openedCss.get(key);
	if (opened !== undefined) return opened;
	openedCss.set(key, current);
	return current;
}

export function forgetOpenedCss(): void {
	openedCss.clear();
}

export function registerLiveEditorCapture(capture: () => void): () => void {
	liveEditorCapture = capture;
	return () => {
		if (liveEditorCapture === capture) liveEditorCapture = null;
	};
}

export function flushEditorHistory(): void {
	liveEditorCapture?.();
}
