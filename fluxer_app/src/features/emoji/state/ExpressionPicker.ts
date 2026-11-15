// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ExpressionPickerTabType} from '@app/features/expressions/components/popouts/ExpressionPickerPopout';
import {AccountScopedWork} from '@app/features/platform/state/AccountScopedWork';
import {makeAutoObservable, runInAction} from 'mobx';

class ExpressionPicker {
	isOpen = false;
	selectedTab: ExpressionPickerTabType = 'emojis';
	channelId: string | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	open(channelId: string, tab?: ExpressionPickerTabType): void {
		runInAction(() => {
			this.isOpen = true;
			if (tab !== undefined) {
				this.selectedTab = tab;
			}
			this.channelId = channelId;
		});
	}

	close(): void {
		runInAction(() => {
			if (this.isOpen) {
				this.isOpen = false;
			}
		});
	}

	toggle(channelId: string, tab: ExpressionPickerTabType): void {
		runInAction(() => {
			if (this.isOpen && this.selectedTab === tab && this.channelId === channelId) {
				this.isOpen = false;
			} else {
				this.isOpen = true;
				this.selectedTab = tab;
				this.channelId = channelId;
			}
		});
	}

	setTab(tab: ExpressionPickerTabType): void {
		runInAction(() => {
			if (this.selectedTab !== tab) {
				this.selectedTab = tab;
			}
		});
	}
}

const expressionPicker = new ExpressionPicker();

AccountScopedWork.registerCancellation(() => expressionPicker.close());

export default expressionPicker;
