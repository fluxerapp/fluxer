// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {readCustomCssOff, writeCustomCssOff} from '@app/features/theme_settings/state/CustomCssOff';
import {makeAutoObservable} from 'mobx';

const EXPANDED_GROUPS_STORAGE_KEY = 'ThemeStudio:expandedGroups';

export const QUICK_CSS_EDITOR_FILE = 'quick-css';

export type QuickCssEditorPlacement = 'panel' | 'window';
export type QuickCssEditorTab = 'css' | 'colors' | 'themes' | 'assets' | 'reference';
export type QuickCssEditorFocusTarget = 'editor' | 'tab' | 'float' | 'dock-left' | 'dock-right';

export interface ThemePreview {
	id: string;
	css: string;
}

const BLOCKED_URL_LIMIT = 50;

function readStoredPlacement(): QuickCssEditorPlacement {
	try {
		return AppStorage.getItem(AppStorageKey.QUICK_CSS_EDITOR_PLACEMENT) === 'window' ? 'window' : 'panel';
	} catch {
		return 'panel';
	}
}

function readStoredExpandedGroups(): Array<string> {
	try {
		const raw = AppStorage.getItem(EXPANDED_GROUPS_STORAGE_KEY);
		if (raw === null) return [];
		const parsed: unknown = JSON.parse(raw);
		if (Array.isArray(parsed) && parsed.every((value) => typeof value === 'string')) {
			return parsed;
		}
	} catch {}
	return [];
}

class ThemeSettingsState {
	librarySearch = '';
	tokenSearch = '';
	expandedGroups: ReadonlySet<string> = new Set(readStoredExpandedGroups());
	editorPlacement: QuickCssEditorPlacement | null = null;
	preferredPlacement: QuickCssEditorPlacement = readStoredPlacement();
	editorTab: QuickCssEditorTab = 'css';
	pendingFocus: QuickCssEditorFocusTarget | null = null;
	editorGeneration = 0;
	editorFile: string = QUICK_CSS_EDITOR_FILE;
	editorDirty = false;
	missingReferences: ReadonlyArray<string> = [];
	blockedUrls: ReadonlyArray<string> = [];
	themePreview: ThemePreview | null = null;
	customCssOff = readCustomCssOff();
	editorWindow: Window | null = null;
	editorFileRequestHandler: ((file: string) => void) | null = null;
	quickCssWriteFlusher: (() => void) | null = null;

	constructor() {
		makeAutoObservable(
			this,
			{editorWindow: false, editorFileRequestHandler: false, quickCssWriteFlusher: false},
			{autoBind: true},
		);
	}

	get editorOpen(): boolean {
		return this.editorPlacement !== null;
	}

	setLibrarySearch(value: string): void {
		this.librarySearch = value;
	}

	setTokenSearch(value: string): void {
		this.tokenSearch = value;
	}

	isGroupExpanded(groupId: string): boolean {
		return this.expandedGroups.has(groupId);
	}

	setGroupExpanded(groupId: string, expanded: boolean): void {
		const next = new Set(this.expandedGroups);
		if (expanded) {
			next.add(groupId);
		} else {
			next.delete(groupId);
		}
		this.expandedGroups = next;
		try {
			AppStorage.setItem(EXPANDED_GROUPS_STORAGE_KEY, JSON.stringify([...next]));
		} catch {}
	}

	openEditor(file: string, windowAllowed: boolean): void {
		if (this.editorOpen) {
			this.requestEditorFile(file);
			return;
		}
		this.editorFile = file;
		this.editorDirty = false;
		this.editorTab = 'css';
		this.editorGeneration += 1;
		this.editorPlacement = windowAllowed ? this.preferredPlacement : 'panel';
		this.pendingFocus = 'editor';
	}

	moveEditor(placement: QuickCssEditorPlacement, generation: number): void {
		if (generation !== this.editorGeneration || this.editorPlacement === null) return;
		if (this.editorPlacement === placement) return;
		this.editorGeneration += 1;
		this.editorPlacement = placement;
		this.editorWindow = null;
		this.preferredPlacement = placement;
		this.pendingFocus = 'editor';
		try {
			AppStorage.setItem(AppStorageKey.QUICK_CSS_EDITOR_PLACEMENT, placement);
		} catch {}
	}

	setEditorTab(tab: QuickCssEditorTab): void {
		this.editorTab = tab;
	}

	requestFocus(target: QuickCssEditorFocusTarget | null): void {
		this.pendingFocus = target;
	}

	requestEditorFile(file: string): void {
		if (file === this.editorFile) return;
		if (this.editorFileRequestHandler) {
			this.editorFileRequestHandler(file);
			return;
		}
		this.setEditorFile(file);
	}

	setEditorFile(file: string): void {
		this.editorFile = file;
		this.editorDirty = false;
	}

	setEditorFileRequestHandler(handler: ((file: string) => void) | null): void {
		this.editorFileRequestHandler = handler;
	}

	setEditorDirty(dirty: boolean): void {
		this.editorDirty = dirty;
	}

	setEditorWindow(editorWindow: Window | null): void {
		this.editorWindow = editorWindow;
	}

	setQuickCssWriteFlusher(flusher: (() => void) | null): void {
		this.quickCssWriteFlusher = flusher;
	}

	closeEditor(generation: number): void {
		if (generation !== this.editorGeneration || this.editorPlacement === null) return;
		this.editorGeneration += 1;
		this.editorPlacement = null;
		this.editorWindow = null;
		this.editorFile = QUICK_CSS_EDITOR_FILE;
		this.editorDirty = false;
		this.pendingFocus = null;
	}

	setMissingReferences(references: ReadonlyArray<string>): void {
		this.missingReferences = references;
	}

	recordBlockedUrl(url: string): void {
		if (this.blockedUrls.includes(url)) return;
		this.blockedUrls = [...this.blockedUrls, url].slice(-BLOCKED_URL_LIMIT);
	}

	setCustomCssOff(off: boolean): void {
		this.customCssOff = off;
		writeCustomCssOff(off);
	}

	setThemePreview(preview: ThemePreview | null): void {
		if (preview === null && this.themePreview === null) return;
		this.themePreview = preview;
	}
}

export default new ThemeSettingsState();
