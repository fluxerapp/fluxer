// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage from '@app/features/platform/state/PersistentStorage';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {makeAutoObservable} from 'mobx';

const SIDEBAR_WIDTH_STORAGE_KEY = 'fluxer:ui:sidebar-width';
const SIDEBAR_WIDTH_MIN = 200;
const SIDEBAR_WIDTH_MAX = 480;
const SIDEBAR_WIDTH_DEFAULT = 320;

function clampSidebarWidth(value: number): number {
	return Math.max(SIDEBAR_WIDTH_MIN, Math.min(Math.round(value), SIDEBAR_WIDTH_MAX));
}

function parseSidebarWidth(value: string | null): number | null {
	if (value == null || value === '') return null;
	const parsed = Number(value);
	if (!Number.isFinite(parsed)) return null;
	return clampSidebarWidth(parsed);
}

class SidebarWidth {
	width: number | null = parseSidebarWidth(AppStorage.getItem(SIDEBAR_WIDTH_STORAGE_KEY));

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		AppStorage.subscribe(this.loadStoredWidth, {key: SIDEBAR_WIDTH_STORAGE_KEY, source: 'external'});
	}

	private loadStoredWidth(): void {
		this.width = parseSidebarWidth(AppStorage.getItem(SIDEBAR_WIDTH_STORAGE_KEY));
	}

	get cssValue(): string | null {
		if (this.width == null) return null;
		return remFromPx(this.width);
	}

	setWidth(width: number, persist: boolean): number {
		const clamped = clampSidebarWidth(width);
		this.width = clamped;
		if (persist) AppStorage.setItem(SIDEBAR_WIDTH_STORAGE_KEY, `${clamped}`);
		return clamped;
	}

	reset(): void {
		this.width = null;
		AppStorage.removeItem(SIDEBAR_WIDTH_STORAGE_KEY);
	}
}

export {SIDEBAR_WIDTH_DEFAULT, SIDEBAR_WIDTH_MAX, SIDEBAR_WIDTH_MIN};
export default new SidebarWidth();
