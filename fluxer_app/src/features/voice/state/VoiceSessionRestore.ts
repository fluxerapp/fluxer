// SPDX-License-Identifier: AGPL-3.0-or-later

import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {MS_PER_MINUTE} from '@fluxer/date_utils/src/DateConstants';
import {makeAutoObservable} from 'mobx';

const VOICE_SESSION_RESTORE_DECAY_MS = 5 * MS_PER_MINUTE;
export const VOICE_SESSION_RESTORE_HEARTBEAT_MS = MS_PER_MINUTE;

export interface VoiceSessionRestoreSnapshot {
	userId: string;
	guildId: string | null;
	channelId: string;
	selfVideo: boolean;
	selfStream: boolean;
	updatedAt: number;
}

class VoiceSessionRestore {
	snapshot: VoiceSessionRestoreSnapshot | null = null;
	isHydrated = false;
	private snapshotExpirationTimerId: NodeJS.Timeout | null = null;

	constructor() {
		makeAutoObservable<this, 'snapshotExpirationTimerId'>(this, {snapshotExpirationTimerId: false}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'VoiceSessionRestore', ['snapshot']);
		this.clearExpiredSnapshot();
		this.scheduleSnapshotExpiration();
		this.isHydrated = true;
	}

	saveSnapshot(snapshot: Omit<VoiceSessionRestoreSnapshot, 'updatedAt'>): void {
		this.snapshot = {
			...snapshot,
			updatedAt: Date.now(),
		};
		this.scheduleSnapshotExpiration();
	}

	getSnapshotForUser(userId: string | null | undefined): VoiceSessionRestoreSnapshot | null {
		if (!userId) {
			return null;
		}
		if (!this.snapshot || this.snapshot.userId !== userId) {
			return null;
		}
		if (this.isSnapshotExpired(this.snapshot)) {
			this.clearSnapshot();
			return null;
		}
		return this.snapshot;
	}

	clearSnapshot(): void {
		this.snapshot = null;
		this.clearSnapshotExpiration();
	}

	clearSnapshotForChannel(channelId: string): void {
		if (this.snapshot?.channelId === channelId) {
			this.clearSnapshot();
		}
	}

	private clearExpiredSnapshot(): void {
		if (this.snapshot && this.isSnapshotExpired(this.snapshot)) {
			this.snapshot = null;
		}
	}

	private isSnapshotExpired(snapshot: VoiceSessionRestoreSnapshot): boolean {
		if (!Number.isFinite(snapshot.updatedAt)) {
			return true;
		}
		return Date.now() - snapshot.updatedAt > VOICE_SESSION_RESTORE_DECAY_MS;
	}

	private scheduleSnapshotExpiration(): void {
		this.clearSnapshotExpiration();
		if (!this.snapshot) return;
		const expiresInMs = Math.max(0, VOICE_SESSION_RESTORE_DECAY_MS - (Date.now() - this.snapshot.updatedAt));
		this.snapshotExpirationTimerId = setTimeout(() => {
			this.snapshotExpirationTimerId = null;
			this.clearExpiredSnapshot();
			this.scheduleSnapshotExpiration();
		}, expiresInMs + 1);
	}

	private clearSnapshotExpiration(): void {
		if (this.snapshotExpirationTimerId !== null) {
			clearTimeout(this.snapshotExpirationTimerId);
			this.snapshotExpirationTimerId = null;
		}
	}
}

export default new VoiceSessionRestore();
