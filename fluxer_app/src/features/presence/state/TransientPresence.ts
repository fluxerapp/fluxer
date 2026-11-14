// SPDX-License-Identifier: AGPL-3.0-or-later

import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {makeAutoObservable, observable, runInAction} from 'mobx';

const PRESENCE_TTL_MS = 5 * 60 * 1000;
const CLEANUP_INTERVAL_MS = 60 * 1000;

interface TransientPresence {
	status: StatusType;
	timestamp: number;
}

class TransientPresenceRegistryClass {
	presences = observable.map<string, TransientPresence>();
	private cleanupInterval: NodeJS.Timeout | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	private startCleanup(): void {
		if (this.cleanupInterval) return;
		this.cleanupInterval = setInterval(() => {
			this.pruneStale();
		}, CLEANUP_INTERVAL_MS);
	}

	private pruneStale(): void {
		const now = Date.now();
		runInAction(() => {
			for (const [userId, presence] of this.presences) {
				if (now - presence.timestamp > PRESENCE_TTL_MS) {
					this.presences.delete(userId);
				}
			}
		});
		this.stopCleanupIfIdle();
	}

	updatePresences(
		presences: Array<{
			userId: string;
			status: StatusType;
		}>,
	): void {
		runInAction(() => {
			const now = Date.now();
			for (const {userId, status} of presences) {
				this.presences.set(userId, {status, timestamp: now});
			}
		});
		if (presences.length > 0) {
			this.startCleanup();
		}
	}

	getStatus(userId: string): StatusType {
		const transient = this.presences.get(userId);
		if (transient && Date.now() - transient.timestamp <= PRESENCE_TTL_MS) {
			return transient.status;
		}
		return StatusTypes.OFFLINE;
	}

	getTransientStatus(userId: string): StatusType | null {
		const transient = this.presences.get(userId);
		if (transient && Date.now() - transient.timestamp <= PRESENCE_TTL_MS) {
			return transient.status;
		}
		return null;
	}

	clearPresence(userId: string): void {
		this.presences.delete(userId);
		this.stopCleanupIfIdle();
	}

	clear(): void {
		this.presences.clear();
		this.stopCleanupIfIdle();
	}

	private stopCleanupIfIdle(): void {
		if (this.presences.size > 0) return;
		if (!this.cleanupInterval) return;
		clearInterval(this.cleanupInterval);
		this.cleanupInterval = null;
	}
}

export default new TransientPresenceRegistryClass();
