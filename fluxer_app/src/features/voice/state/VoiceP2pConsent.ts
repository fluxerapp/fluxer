// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeAutoObservable} from 'mobx';

class VoiceP2pConsent {
	agreedChannelId: string | null = null;
	pendingChannelId: string | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	agree(channelId: string): void {
		this.pendingChannelId = channelId;
	}

	commit(channelId: string): void {
		this.agreedChannelId = this.isAgreed(channelId) ? channelId : null;
		this.pendingChannelId = null;
	}

	isAgreed(channelId: string): boolean {
		return this.agreedChannelId === channelId || this.pendingChannelId === channelId;
	}

	revoke(channelId: string): void {
		if (this.agreedChannelId === channelId) this.agreedChannelId = null;
		if (this.pendingChannelId === channelId) this.pendingChannelId = null;
	}

	clear(): void {
		this.agreedChannelId = null;
		this.pendingChannelId = null;
	}
}

export default new VoiceP2pConsent();
