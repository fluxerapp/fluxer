// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('Inbox');

export type InboxTab = 'bookmarks' | 'mentions' | 'unreadChannels';

class Inbox {
	selectedTab: InboxTab = 'bookmarks';
	hasAutoOpenedBookmarksPopoutForFirstSave = false;
	skipMarkAllAsReadConfirmation = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'Inbox', [
			'selectedTab',
			'hasAutoOpenedBookmarksPopoutForFirstSave',
			'skipMarkAllAsReadConfirmation',
		]);
	}

	setSkipMarkAllAsReadConfirmation(value: boolean): void {
		this.skipMarkAllAsReadConfirmation = value;
	}

	setTab(tab: InboxTab): void {
		if (this.selectedTab !== tab) {
			this.selectedTab = tab;
			logger.debug(`Set inbox tab to: ${tab}`);
		}
	}

	getSelectedTab(): InboxTab {
		return this.selectedTab;
	}

	shouldAutoOpenBookmarksPopoutForFirstSave(): boolean {
		return !this.hasAutoOpenedBookmarksPopoutForFirstSave;
	}

	markBookmarksPopoutAutoOpenedForFirstSave(): void {
		this.hasAutoOpenedBookmarksPopoutForFirstSave = true;
	}
}

export default new Inbox();
