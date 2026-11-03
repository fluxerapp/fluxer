// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResettableStates} from '@app/features/app/state/ResettableStates';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
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
	private discoveryDisabledByApi = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	private get discoveryDisabled(): boolean {
		return !RuntimeConfig.discoveryEnabled || this.discoveryDisabledByApi;
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
						this.discoveryDisabledByApi = true;
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

	reset(): void {
		this.entries.clear();
		this.discoveryDisabledByApi = false;
	}
}

const discoveryChannelPreviews = new DiscoveryChannelPreviews();

ResettableStates.register(discoveryChannelPreviews, () => discoveryChannelPreviews.reset());

export default discoveryChannelPreviews;
