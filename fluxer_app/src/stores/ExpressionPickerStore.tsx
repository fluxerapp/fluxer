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

import {makeAutoObservable, runInAction} from 'mobx';
import type {ExpressionPickerTabType} from '~/components/popouts/ExpressionPickerPopout';

class ExpressionPickerStore {
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

export default new ExpressionPickerStore();
