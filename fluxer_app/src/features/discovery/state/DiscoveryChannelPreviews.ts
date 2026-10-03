// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DiscoveryChannelPreview} from '@app/features/discovery/commands/DiscoveryCommands';
import * as DiscoveryCommands from '@app/features/discovery/commands/DiscoveryCommands';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {makeAutoObservable, observable, runInAction} from 'mobx';

type DiscoveryChannelPreviewEntry =
	| {status: 'loading'}
	| {status: 'ready'; preview: DiscoveryChannelPreview}
	| {status: 'unavailable'};

function entryKey(guildId: string, channelId: string): string {
	return `${guildId}:${channelId}`;
}

class DiscoveryChannelPreviews {
	private entries = observable.map<string, DiscoveryChannelPreviewEntry>();
	private discoveryDisabled = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	get(guildId: string, channelId: string): DiscoveryChannelPreviewEntry | undefined {
		return this.entries.get(entryKey(guildId, channelId));
	}

	request(guildId: string, channelId: string): void {
		const key = entryKey(guildId, channelId);
		if (this.discoveryDisabled || this.entries.has(key)) return;
		this.entries.set(key, {status: 'loading'});
		DiscoveryCommands.getChannelPreview(guildId, channelId).then(
			(preview) => {
				runInAction(() => this.entries.set(key, {status: 'ready', preview}));
			},
			(error: unknown) => {
				const code = failureCode(error);
				runInAction(() => {
					if (code === APIErrorCodes.DISCOVERY_DISABLED) {
						this.discoveryDisabled = true;
						this.entries.set(key, {status: 'unavailable'});
					} else if (code === APIErrorCodes.DISCOVERY_NOT_DISCOVERABLE) {
						this.entries.set(key, {status: 'unavailable'});
					} else {
						this.entries.delete(key);
					}
				});
			},
		);
	}
}

export default new DiscoveryChannelPreviews();
