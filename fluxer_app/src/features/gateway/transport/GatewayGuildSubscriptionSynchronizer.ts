// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GatewaySocket} from '@app/features/gateway/transport/GatewaySocket';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {FAVORITES_GUILD_ID} from '@fluxer/constants/src/AppConstants';

const logger = new Logger('GatewayGuildSubscriptionSynchronizer');

export interface GatewayGuildSubscriptionHost {
	getSocket(): GatewaySocket | null;
	isReady(): boolean;
	getSessionId(): string | null;
	getSelectedGuildId(): string | null;
}

export interface GuildSubscriptionSyncRequest {
	readonly force: boolean;
	readonly reason: string | null;
}

export class GatewayGuildSubscriptionSynchronizer {
	private pendingGuildId: string | null = null;
	private readonly syncedSessionByGuild = new Map<string, string | null>();
	private readonly completedSessionByGuild = new Map<string, string | null>();
	private initialGuildIdAtIdentify: string | null = null;

	constructor(private readonly host: GatewayGuildSubscriptionHost) {}

	sync(guildId: string, request: GuildSubscriptionSyncRequest): void {
		if (!guildId || guildId === FAVORITES_GUILD_ID) {
			return;
		}
		const socket = this.host.getSocket();
		if (socket === null || !this.host.isReady()) {
			this.pendingGuildId = guildId;
			return;
		}
		const sessionId = this.host.getSessionId();
		if (!request.force && (this.syncedSessionByGuild.get(guildId) ?? null) === sessionId) {
			return;
		}
		try {
			socket.updateGuildSubscriptions({
				subscriptions: {
					[guildId]: {
						active: true,
						sync: true,
					},
				},
			});
			this.syncedSessionByGuild.set(guildId, sessionId);
			this.pendingGuildId = null;
		} catch (error) {
			logger.warn('Failed to update guild subscriptions, will retry when possible', {
				guildId,
				reason: request.reason,
				error,
			});
			this.pendingGuildId = guildId;
		}
	}

	hasCompleted(guildId: string): boolean {
		const sessionId = this.host.getSessionId();
		if (!sessionId) {
			return false;
		}
		return this.completedSessionByGuild.get(guildId) === sessionId;
	}

	markCompleted(guildId: string): void {
		if (!guildId) {
			return;
		}
		const sessionId = this.host.getSessionId();
		if (!sessionId) {
			return;
		}
		this.syncedSessionByGuild.set(guildId, sessionId);
		this.completedSessionByGuild.set(guildId, sessionId);
		if (this.pendingGuildId === guildId) {
			this.pendingGuildId = null;
		}
	}

	clearPending(): void {
		this.pendingGuildId = null;
	}

	flush(): void {
		const guildId = this.pendingGuildId ?? this.host.getSelectedGuildId();
		if (!guildId || guildId === FAVORITES_GUILD_ID) {
			return;
		}
		this.sync(guildId, {force: false, reason: 'flush'});
	}

	recordInitialGuild(guildId: string | null): void {
		this.initialGuildIdAtIdentify = guildId;
	}

	markInitialGuildCompleted(sessionId: string | null): void {
		const guildId = this.initialGuildIdAtIdentify;
		if (!guildId || !sessionId) {
			return;
		}
		this.syncedSessionByGuild.set(guildId, sessionId);
		this.completedSessionByGuild.set(guildId, sessionId);
	}

	resetSessionTracking(): void {
		this.syncedSessionByGuild.clear();
		this.completedSessionByGuild.clear();
	}

	resetConnectionTracking(): void {
		this.pendingGuildId = null;
		this.resetSessionTracking();
	}

	prepareAdoptedSession(): void {
		this.resetConnectionTracking();
		this.initialGuildIdAtIdentify = null;
	}

	clearInitialGuild(): void {
		this.initialGuildIdAtIdentify = null;
	}
}
