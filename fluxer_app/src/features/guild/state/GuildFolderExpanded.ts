// SPDX-License-Identifier: AGPL-3.0-or-later

import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {makeAutoObservable} from 'mobx';

class GuildFolderExpanded {
	expandedFolderIds: Array<number> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'GuildFolderExpanded', ['expandedFolderIds']);
	}

	isExpanded(folderId: number): boolean {
		return this.expandedFolderIds.includes(folderId);
	}

	toggleExpanded(folderId: number): void {
		if (this.expandedFolderIds.includes(folderId)) {
			const index = this.expandedFolderIds.indexOf(folderId);
			if (index > -1) {
				this.expandedFolderIds.splice(index, 1);
			}
		} else {
			this.expandedFolderIds.push(folderId);
		}
	}
}

export default new GuildFolderExpanded();
