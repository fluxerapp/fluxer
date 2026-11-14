// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ButtonVariant} from '@app/features/ui/button/Button';
import {makeAutoObservable} from 'mobx';

export interface TabData {
	onReset?: () => void;
	onSave?: () => void;
	isSubmitting?: boolean;
	bannerText?: string;
	resetLabel?: string;
	saveLabel?: string;
	saveVariant?: ButtonVariant;
}

class UnsavedChanges {
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
}

export default new UnsavedChanges();
