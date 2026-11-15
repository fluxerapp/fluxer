// SPDX-License-Identifier: AGPL-3.0-or-later

import {ResettableStates} from '@app/features/app/state/ResettableStates';
import {makeAutoObservable} from 'mobx';

class CallInitiator {
	private initiatedRecipients = new Map<string, Set<string>>();

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	markInitiated(channelId: string, recipients: ReadonlyArray<string>): void {
		const filtered = recipients.filter(Boolean);
		if (filtered.length === 0) {
			this.initiatedRecipients.delete(channelId);
			return;
		}
		this.initiatedRecipients.set(channelId, new Set(filtered));
	}

	getInitiatedRecipients(channelId: string): Array<string> {
		const recipients = this.initiatedRecipients.get(channelId);
		return recipients ? Array.from(recipients) : [];
	}

	hasInitiated(channelId: string): boolean {
		return this.initiatedRecipients.has(channelId);
	}

	clearChannel(channelId: string): void {
		this.initiatedRecipients.delete(channelId);
	}

	reset(): void {
		this.initiatedRecipients.clear();
	}
}

const callInitiator = new CallInitiator();

ResettableStates.register(callInitiator, () => callInitiator.reset());

export default callInitiator;
