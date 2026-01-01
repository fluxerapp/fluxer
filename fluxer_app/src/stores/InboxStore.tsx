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
import {Logger} from '~/lib/Logger';
import {makePersistent} from '~/lib/MobXPersistence';

const logger = new Logger('InboxStore');

export type InboxTab = 'bookmarks' | 'mentions' | 'scheduled';

class InboxStore {
	selectedTab: InboxTab = 'bookmarks';

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'InboxStore', ['selectedTab']);
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
}

export default new InboxStore();
