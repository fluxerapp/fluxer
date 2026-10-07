// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {makeAutoObservable} from 'mobx';

type ThemeStudioSection = 'library' | 'tokens' | 'quickCss' | 'assets' | 'settings';

const SECTION_STORAGE_KEY = 'ThemeStudio:section';
const EXPANDED_GROUPS_STORAGE_KEY = 'ThemeStudio:expandedGroups';
const readStoredSection = (): ThemeStudioSection => {
	if (typeof window === 'undefined') return 'tokens';
	try {
		const raw = AppStorage.getItem(SECTION_STORAGE_KEY);
		if (raw === 'library' || raw === 'tokens' || raw === 'quickCss' || raw === 'assets' || raw === 'settings') {
			return raw;
		}
	} catch {}
	return 'tokens';
};
const readStoredExpandedGroups = (): Array<string> | null => {
	if (typeof window === 'undefined') return null;
	try {
		const raw = AppStorage.getItem(EXPANDED_GROUPS_STORAGE_KEY);
		if (raw === null) return null;
		const parsed: unknown = JSON.parse(raw);
		if (Array.isArray(parsed) && parsed.every((value) => typeof value === 'string')) {
			return parsed;
		}
	} catch {}
	return null;
};

class ThemeStudioState {
	section: ThemeStudioSection = readStoredSection();
	tokenSearch = '';
	librarySearch = '';
	expandedGroups: Set<string> = new Set(readStoredExpandedGroups() ?? []);
	hasInitializedExpansion: boolean = readStoredExpandedGroups() !== null;
	isPoppedOut = false;
	popupRef: Window | null = null;

	constructor() {
		makeAutoObservable(this, {popupRef: false}, {autoBind: true});
	}

	setSection(next: ThemeStudioSection): void {
		this.section = next;
		if (typeof window === 'undefined') return;
		try {
			AppStorage.setItem(SECTION_STORAGE_KEY, next);
		} catch {}
	}

	setTokenSearch(value: string): void {
		this.tokenSearch = value;
	}

	setLibrarySearch(value: string): void {
		this.librarySearch = value;
	}

	isGroupExpanded(groupId: string): boolean {
		return this.expandedGroups.has(groupId);
	}

	toggleGroup(groupId: string, expanded?: boolean): void {
		const next = new Set(this.expandedGroups);
		const shouldExpand = expanded ?? !next.has(groupId);
		if (shouldExpand) {
			next.add(groupId);
		} else {
			next.delete(groupId);
		}
		this.expandedGroups = next;
		this.hasInitializedExpansion = true;
		this.persistExpansion();
	}

	expandAllGroups(groupIds: ReadonlyArray<string>): void {
		this.expandedGroups = new Set(groupIds);
		this.hasInitializedExpansion = true;
		this.persistExpansion();
	}

	private persistExpansion(): void {
		if (typeof window === 'undefined') return;
		try {
			AppStorage.setItem(EXPANDED_GROUPS_STORAGE_KEY, JSON.stringify([...this.expandedGroups]));
		} catch {}
	}

	markPoppedOut(popupRef: Window | null): void {
		this.isPoppedOut = true;
		this.popupRef = popupRef;
	}

	clearPoppedOut(): void {
		this.isPoppedOut = false;
		this.popupRef = null;
	}
}

export default new ThemeStudioState();
