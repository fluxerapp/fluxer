/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable} from 'mobx';

interface TabData {
	onReset?: () => void;
	onSave?: () => void;
	isSubmitting?: boolean;
}

class UnsavedChangesStore {
	unsavedChanges: Record<string, boolean> = {};
	flashTriggers: Record<string, number> = {};
	tabData: Record<string, TabData> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	setUnsavedChanges(tabId: string, hasChanges: boolean): void {
		this.unsavedChanges = {
			...this.unsavedChanges,
			[tabId]: hasChanges,
		};
	}

	triggerFlash(tabId: string): void {
		this.flashTriggers = {
			...this.flashTriggers,
			[tabId]: (this.flashTriggers[tabId] || 0) + 1,
		};
	}

	clearUnsavedChanges(tabId: string): void {
		const {[tabId]: _unsaved, ...remainingUnsaved} = this.unsavedChanges;
		const {[tabId]: _tabData, ...remainingTabData} = this.tabData;
		this.unsavedChanges = remainingUnsaved;
		this.tabData = remainingTabData;
	}

	setTabData(tabId: string, data: TabData): void {
		this.tabData = {
			...this.tabData,
			[tabId]: data,
		};
	}

	hasUnsavedChanges(tabId: string): boolean {
		return this.unsavedChanges[tabId] || false;
	}

	getFlashTrigger(tabId: string): number {
		return this.flashTriggers[tabId] || 0;
	}

	getTabData(tabId: string): TabData {
		return this.tabData[tabId] || {};
	}
}

export default new UnsavedChangesStore();
