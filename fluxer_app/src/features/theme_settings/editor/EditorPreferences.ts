// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {isObjectLike as isRecord} from '@fluxer/record_utils/src/RecordGuards';
import {makeAutoObservable} from 'mobx';

export const EDITOR_FONT_SIZES: ReadonlyArray<number> = [11, 12, 13, 14, 15, 16, 18, 20];
export const EDITOR_TAB_SIZES: ReadonlyArray<number> = [2, 4, 8];

interface EditorPreferenceValues {
	fontSize: number;
	tabSize: number;
	indentWithTabs: boolean;
	wrapLines: boolean;
	lineNumbers: boolean;
	completeWhileTyping: boolean;
	livePreview: boolean;
}

const DEFAULTS: EditorPreferenceValues = {
	fontSize: 13,
	tabSize: 4,
	indentWithTabs: true,
	wrapLines: true,
	lineNumbers: true,
	completeWhileTyping: true,
	livePreview: true,
};

function readStoredPreferences(): EditorPreferenceValues {
	try {
		const raw = AppStorage.getItem(AppStorageKey.QUICK_CSS_EDITOR_PREFERENCES);
		if (raw === null) return DEFAULTS;
		const parsed: unknown = JSON.parse(raw);
		if (!isRecord(parsed)) return DEFAULTS;
		const pick = <K extends keyof EditorPreferenceValues>(
			key: K,
			valid: (value: unknown) => boolean,
		): EditorPreferenceValues[K] => (valid(parsed[key]) ? (parsed[key] as EditorPreferenceValues[K]) : DEFAULTS[key]);
		const isBoolean = (value: unknown) => typeof value === 'boolean';
		return {
			fontSize: pick('fontSize', (value) => EDITOR_FONT_SIZES.includes(value as number)),
			tabSize: pick('tabSize', (value) => EDITOR_TAB_SIZES.includes(value as number)),
			indentWithTabs: pick('indentWithTabs', isBoolean),
			wrapLines: pick('wrapLines', isBoolean),
			lineNumbers: pick('lineNumbers', isBoolean),
			completeWhileTyping: pick('completeWhileTyping', isBoolean),
			livePreview: pick('livePreview', isBoolean),
		};
	} catch {
		return DEFAULTS;
	}
}

class EditorPreferences implements EditorPreferenceValues {
	fontSize = DEFAULTS.fontSize;
	tabSize = DEFAULTS.tabSize;
	indentWithTabs = DEFAULTS.indentWithTabs;
	wrapLines = DEFAULTS.wrapLines;
	lineNumbers = DEFAULTS.lineNumbers;
	completeWhileTyping = DEFAULTS.completeWhileTyping;
	livePreview = DEFAULTS.livePreview;

	constructor() {
		Object.assign(this, readStoredPreferences());
		makeAutoObservable(this, {}, {autoBind: true});
	}

	set<K extends keyof EditorPreferenceValues>(key: K, value: EditorPreferenceValues[K]): void {
		this[key] = value as this[K];
		const values: EditorPreferenceValues = {
			fontSize: this.fontSize,
			tabSize: this.tabSize,
			indentWithTabs: this.indentWithTabs,
			wrapLines: this.wrapLines,
			lineNumbers: this.lineNumbers,
			completeWhileTyping: this.completeWhileTyping,
			livePreview: this.livePreview,
		};
		try {
			AppStorage.setItem(AppStorageKey.QUICK_CSS_EDITOR_PREFERENCES, JSON.stringify(values));
		} catch {}
	}
}

export default new EditorPreferences();
