// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createHandlerRegistry,
	type GatewayGeoipPayload,
	type GatewayHandlerContext,
	type GatewayHandlerRegistry,
} from '@app/features/gateway/events/EventRouter';
import {
	parseSnapshotGatewayEvent,
	type SnapshotGatewayEvent,
} from '@app/features/gateway/snapshot/SnapshotDispatchEvent';
import SnapshotSync from '@app/features/gateway/snapshot/SnapshotSync';
import type {GatewayDispatchDelivery, GatewaySocket} from '@app/features/gateway/transport/GatewaySocket';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import {runInAction} from 'mobx';

export interface ForegroundGatewayDispatchHost {
	getSocket(): GatewaySocket | null;
	getForegroundSocket(accountKey: string | null): GatewaySocket | null;
	getAccountUserId(accountKey: string): string | null;
	getPreviousSessionId(): string | null;
	setPreviousSessionId(sessionId: string): void;
	handleReady(accountKey: string | null, isCurrent: () => boolean): boolean;
	handleGeoip(data: GatewayGeoipPayload): void;
	markGuildSynced(guildId: string): void;
}

export class ForegroundGatewayDispatchAccountUserUnknownError extends Error {
	constructor(accountKey: string) {
		super(`Gateway dispatch account ${accountKey} has no stored user`);
		this.name = 'ForegroundGatewayDispatchAccountUserUnknownError';
	}
}

export class ForegroundGatewayDispatchProcessor {
	private readonly handlerRegistry: GatewayHandlerRegistry;

	constructor(private readonly host: ForegroundGatewayDispatchHost) {
		this.handlerRegistry = createHandlerRegistry();
	}

	dispatch(
		socket: GatewaySocket,
		delivery: GatewayDispatchDelivery,
		accountKey: string | null,
		isCurrent: () => boolean,
	): void {
		if (this.host.getForegroundSocket(accountKey) !== socket || !socket.isDispatchActive(delivery.receipt)) {
			return;
		}
		try {
			const account = accountKey === null ? null : {key: accountKey, userId: this.requireAccountUserId(accountKey)};
			const snapshotEvent = parseSnapshotGatewayEvent(delivery.type, delivery.data);
			const handler = this.handlerRegistry.get(delivery.type);
			if (handler !== undefined && !ThreadGuilds.isPurgedEvent(delivery.data)) {
				const context = this.createHandlerContext(accountKey, account?.userId ?? null, isCurrent);
				runInAction(() => {
					if (snapshotEvent !== null) {
						handler(this.foregroundSnapshotEvent(delivery, accountKey, snapshotEvent).data, context);
					} else {
						handler(delivery.data, context);
					}
				});
			}
			if (account !== null) {
				this.applyForegroundSnapshotEvent(account.key, account.userId, snapshotEvent);
			}
		} catch (error) {
			socket.failDispatchProcessing(delivery.receipt, error);
			return;
		}
		socket.completeDispatchProcessing(delivery.receipt);
	}

	applyForegroundSnapshot(accountKey: string, expectedUserId: string, delivery: GatewayDispatchDelivery): void {
		const event = parseSnapshotGatewayEvent(delivery.type, delivery.data);
		this.applyForegroundSnapshotEvent(accountKey, expectedUserId, event);
	}

	private applyForegroundSnapshotEvent(
		accountKey: string,
		expectedUserId: string,
		event: SnapshotGatewayEvent | null,
	): void {
		if (event === null) {
			return;
		}
		if (event.type === 'READY') {
			const receivedUserId = event.data.user.id;
			if (receivedUserId !== expectedUserId) {
				throw new Error(`Gateway READY user mismatch: expected ${expectedUserId}, received ${String(receivedUserId)}`);
			}
			SnapshotSync.applyReady(accountKey, event.data, 'foreground');
			return;
		}
		SnapshotSync.applyDispatch(accountKey, event);
	}

	private requireAccountUserId(accountKey: string): string {
		const userId = this.host.getAccountUserId(accountKey);
		if (userId === null) {
			throw new ForegroundGatewayDispatchAccountUserUnknownError(accountKey);
		}
		return userId;
	}

	private createHandlerContext(
		accountKey: string | null,
		expectedUserId: string | null,
		isCurrent: () => boolean,
	): GatewayHandlerContext {
		return {
			socket: this.host.getSocket(),
			accountKey,
			expectedUserId,
			previousSessionId: this.host.getPreviousSessionId(),
			setPreviousSessionId: (sessionId: string) => {
				this.host.setPreviousSessionId(sessionId);
			},
			setReady: () => this.host.handleReady(accountKey, isCurrent),
			setConnectionGeoip: (data: GatewayGeoipPayload) => {
				this.host.handleGeoip(data);
			},
			markGuildSynced: (guildId: string) => {
				this.host.markGuildSynced(guildId);
			},
		};
	}

	private foregroundSnapshotEvent(
		delivery: GatewayDispatchDelivery,
		accountKey: string | null,
		snapshotEvent: SnapshotGatewayEvent,
	): SnapshotGatewayEvent {
		if (
			snapshotEvent.type !== 'AUTH_SESSION_CHANGE' ||
			accountKey === null ||
			delivery.persistedAuthTokenAccountKey !== accountKey
		) {
			return snapshotEvent;
		}
		const {new_token: _persistedToken, ...remaining} = snapshotEvent.data;
		return {...snapshotEvent, data: remaining};
	}
}
