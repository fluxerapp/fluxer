// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('UserPinnedDM');

class UserPinnedDM {
	pinnedDMsArray: Array<string> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	setPinnedDMs(pinnedDMs: Array<string>): void {
		this.pinnedDMsArray = pinnedDMs;
		logger.debug(`Set pinned DMs: ${pinnedDMs.length} channels`);
	}

	pinDM(channelId: string): void {
		if (this.pinnedDMsArray.includes(channelId)) {
			return;
		}
		this.pinnedDMsArray = [...this.pinnedDMsArray, channelId];
		logger.debug(`Pinned DM channel ${channelId}`);
	}

	unpinDM(channelId: string): void {
		if (!this.pinnedDMsArray.includes(channelId)) {
			return;
		}
		this.pinnedDMsArray = this.pinnedDMsArray.filter((pinnedChannelId) => pinnedChannelId !== channelId);
		logger.debug(`Unpinned DM channel ${channelId}`);
	}

	get pinnedDMs() {
		return this.pinnedDMsArray;
	}
}

export default new UserPinnedDM();
