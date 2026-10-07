// SPDX-License-Identifier: AGPL-3.0-or-later

import {isSyncExcludedChannelId} from '@app/features/threads/utils/SyncedPreferenceGuard';
import {makeSyncedField} from '@app/features/user/state/SyncedField';
import {UnreadChannelsStateSchema} from '@fluxer/schema/src/gen/fluxer/user/preferences/v1/preferences_pb';
import {makeAutoObservable} from 'mobx';

class UnreadChannels {
	collapsedChannelIds: Array<string> = [];

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makeSyncedField(this, {
			field: 'unreadChannels',
			schema: UnreadChannelsStateSchema,
			persist: ['collapsedChannelIds'],
			toMessage: (s) => ({collapsedChannelIds: [...s.collapsedChannelIds]}),
			applyMessage: (s, m) => {
				s.collapsedChannelIds = [...m.collapsedChannelIds];
			},
		});
	}

	isCollapsed(channelId: string): boolean {
		return this.collapsedChannelIds.includes(channelId);
	}

	setCollapsed(channelId: string, collapsed: boolean): void {
		if (isSyncExcludedChannelId(channelId)) return;
		const next = new Set(this.collapsedChannelIds);
		if (collapsed) {
			next.add(channelId);
		} else {
			next.delete(channelId);
		}
		this.collapsedChannelIds = Array.from(next);
	}

	toggleCollapsed(channelId: string): void {
		this.setCollapsed(channelId, !this.isCollapsed(channelId));
	}
}

export default new UnreadChannels();
