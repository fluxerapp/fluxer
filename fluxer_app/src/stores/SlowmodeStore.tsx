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
import {makePersistent} from '~/lib/MobXPersistence';

class SlowmodeStore {
	lastSendTimestamps: Record<string, number> = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'SlowmodeStore', ['lastSendTimestamps']);
	}

	recordMessageSend(channelId: string): void {
		this.lastSendTimestamps = {
			...this.lastSendTimestamps,
			[channelId]: Date.now(),
		};
	}

	updateSlowmodeTimestamp(channelId: string, timestamp: number): void {
		if (this.lastSendTimestamps[channelId] === timestamp) {
			return;
		}

		this.lastSendTimestamps = {
			...this.lastSendTimestamps,
			[channelId]: timestamp,
		};
	}

	deleteChannel(channelId: string): void {
		if (!this.lastSendTimestamps[channelId]) {
			return;
		}

		const {[channelId]: _, ...remainingTimestamps} = this.lastSendTimestamps;
		this.lastSendTimestamps = remainingTimestamps;
	}

	getLastSendTimestamp(channelId: string): number | null {
		return this.lastSendTimestamps[channelId] ?? null;
	}

	getSlowmodeRemaining(channelId: string, rateLimitPerUser: number): number {
		const lastSentTime = this.lastSendTimestamps[channelId];
		if (!lastSentTime) return 0;

		const timeSinceLastMessage = Date.now() - lastSentTime;
		return Math.max(0, rateLimitPerUser * 1000 - timeSinceLastMessage);
	}
}

export default new SlowmodeStore();
