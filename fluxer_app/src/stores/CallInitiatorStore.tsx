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

class CallInitiatorStore {
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

	clearChannel(channelId: string): void {
		this.initiatedRecipients.delete(channelId);
	}
}

export default new CallInitiatorStore();
