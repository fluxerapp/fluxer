// SPDX-License-Identifier: AGPL-3.0-or-later

import {exceedsDispatchBufferCapacity} from '@app/features/gateway/transport/GatewayDispatchBufferBounds';
import {
	type GatewayDispatchDelivery,
	type GatewaySocket,
	MAX_GATEWAY_AUTH_TOKEN_LENGTH,
} from '@app/features/gateway/transport/GatewaySocket';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('ForegroundDemotionHandover');

interface ActiveForegroundDemotion {
	readonly accountKey: string;
	readonly expectedUserId: string;
	readonly socket: GatewaySocket;
	readonly deliveries: Array<GatewayDispatchDelivery>;
	retainedByteSize: number;
	overflowed: boolean;
	disconnected: boolean;
	latestAuthToken: string | null;
	latestAuthTokenRetainedByteSize: number;
}

export interface ForegroundDemotionStart {
	readonly accountKey: string;
	readonly expectedUserId: string;
	readonly socket: GatewaySocket;
}

export interface ForegroundDemotionRestore {
	readonly outcome: 'restore';
	readonly socket: GatewaySocket;
	readonly deliveries: Array<GatewayDispatchDelivery>;
	readonly persistedAuthToken: string | null;
	readonly canRestore: boolean;
}

export interface ForegroundDemotionRetire {
	readonly outcome: 'retire';
}

export type ForegroundDemotionRollback = ForegroundDemotionRestore | ForegroundDemotionRetire;

export interface ForegroundDemotionHandoverHost {
	applySnapshotDispatch(accountKey: string, expectedUserId: string, delivery: GatewayDispatchDelivery): void;
	persistRetiringToken(accountKey: string, token: string): Promise<boolean>;
	persistAccountToken(accountKey: string, token: string): Promise<boolean>;
	getAccountToken(accountKey: string): string | null;
}

class ForegroundDemotionBufferOverflowError extends Error {
	constructor(entryCount: number, retainedByteSize: number) {
		super(
			`Foreground gateway demotion buffer exceeded its bound at ${entryCount} entries and ${retainedByteSize} bytes`,
		);
		this.name = 'ForegroundDemotionBufferOverflowError';
	}
}

class ForegroundDemotionAuthTokenError extends Error {
	constructor(accountKey: string) {
		super(`Foreground gateway received an invalid authentication token rotation for ${accountKey}`);
		this.name = 'ForegroundDemotionAuthTokenError';
	}
}

export class ForegroundDemotionHandover {
	private active: ActiveForegroundDemotion | null = null;

	constructor(private readonly host: ForegroundDemotionHandoverHost) {}

	get isActive(): boolean {
		return this.active !== null;
	}

	assertAvailable(): void {
		if (this.active !== null) {
			throw new Error(`Foreground gateway demotion is already active for ${this.active.accountKey}`);
		}
	}

	begin(start: ForegroundDemotionStart): void {
		this.assertAvailable();
		const active: ActiveForegroundDemotion = {
			accountKey: start.accountKey,
			expectedUserId: start.expectedUserId,
			socket: start.socket,
			deliveries: [],
			retainedByteSize: 0,
			overflowed: false,
			disconnected: false,
			latestAuthToken: null,
			latestAuthTokenRetainedByteSize: 0,
		};
		this.active = active;
		active.socket.on('dispatch', (delivery: GatewayDispatchDelivery) => {
			if (this.active !== active || !active.socket.isDispatchActive(delivery.receipt)) {
				return;
			}
			this.bufferDispatch(active, delivery);
		});
		active.socket.on('disconnect', () => {
			if (this.active === active) {
				active.disconnected = true;
			}
		});
	}

	async consume(accountKey: string): Promise<boolean> {
		const active = this.activeForAccount(accountKey);
		if (active === null) {
			return false;
		}
		try {
			await this.persistLatestRetiringToken(active);
		} catch (error) {
			active.socket.reset(true);
			this.release(active);
			logger.error('Failed to persist a foreground gateway token rotation during demotion', error);
			return false;
		}
		if (!this.canContinue(active)) {
			this.release(active);
			return false;
		}
		for (const delivery of active.deliveries) {
			if (!active.socket.isDispatchActive(delivery.receipt)) {
				continue;
			}
			try {
				this.host.applySnapshotDispatch(active.accountKey, active.expectedUserId, delivery);
			} catch (error) {
				active.socket.failDispatchProcessing(delivery.receipt, error);
				this.release(active);
				return false;
			}
			active.socket.completeDispatchProcessing(delivery.receipt);
		}
		this.release(active);
		return this.canContinue(active);
	}

	async rollback(accountKey: string): Promise<ForegroundDemotionRollback | null> {
		const active = this.activeForAccount(accountKey);
		if (active === null) {
			return null;
		}
		let persistedAuthToken: string | null = null;
		try {
			while (active.latestAuthToken !== null && active.latestAuthToken !== persistedAuthToken) {
				persistedAuthToken = active.latestAuthToken;
				if (!(await this.host.persistAccountToken(accountKey, persistedAuthToken))) {
					throw new Error(`Foreground gateway token rotation was rejected during rollback for ${accountKey}`);
				}
				if (this.host.getAccountToken(accountKey) !== persistedAuthToken) {
					throw new Error(`Foreground gateway token rotation did not update ${accountKey} during rollback`);
				}
				active.socket.setToken(persistedAuthToken);
			}
		} catch (error) {
			this.release(active);
			logger.error('Failed to persist a foreground gateway token rotation during rollback', error);
			return {outcome: 'retire'};
		}
		this.release(active);
		return {
			outcome: 'restore',
			socket: active.socket,
			deliveries: active.deliveries,
			persistedAuthToken,
			canRestore: this.canContinue(active),
		};
	}

	clear(): void {
		const active = this.active;
		if (active !== null) {
			this.release(active);
		}
	}

	private activeForAccount(accountKey: string): ActiveForegroundDemotion | null {
		const active = this.active;
		return active !== null && active.accountKey === accountKey ? active : null;
	}

	private async persistLatestRetiringToken(active: ActiveForegroundDemotion): Promise<void> {
		let persistedAuthToken: string | null = null;
		while (active.latestAuthToken !== null && active.latestAuthToken !== persistedAuthToken) {
			persistedAuthToken = active.latestAuthToken;
			if (!(await this.host.persistRetiringToken(active.accountKey, persistedAuthToken))) {
				throw new Error(`Foreground gateway token rotation was rejected during demotion for ${active.accountKey}`);
			}
			active.socket.setToken(persistedAuthToken);
		}
	}

	private bufferDispatch(active: ActiveForegroundDemotion, delivery: GatewayDispatchDelivery): void {
		let retainedAuthTokenByteSize = active.latestAuthTokenRetainedByteSize;
		if (delivery.type === 'AUTH_SESSION_CHANGE') {
			const newToken = (delivery.data as {new_token?: unknown} | null)?.new_token;
			if (
				newToken !== undefined &&
				(typeof newToken !== 'string' || newToken.length === 0 || newToken.length > MAX_GATEWAY_AUTH_TOKEN_LENGTH)
			) {
				active.overflowed = true;
				active.deliveries.length = 0;
				active.retainedByteSize = active.latestAuthTokenRetainedByteSize;
				this.removeListeners(active);
				active.socket.failDispatchProcessing(delivery.receipt, new ForegroundDemotionAuthTokenError(active.accountKey));
				return;
			}
			if (typeof newToken === 'string') {
				active.latestAuthToken = newToken;
				retainedAuthTokenByteSize = newToken.length * 2;
			}
		}
		const nextEntryCount = active.deliveries.length + 1;
		const retainedByteSize =
			active.retainedByteSize - active.latestAuthTokenRetainedByteSize + retainedAuthTokenByteSize;
		const nextRetainedByteSize = retainedByteSize + delivery.retainedByteSize;
		active.latestAuthTokenRetainedByteSize = retainedAuthTokenByteSize;
		if (exceedsDispatchBufferCapacity(nextEntryCount, nextRetainedByteSize, delivery.retainedByteSize)) {
			active.overflowed = true;
			active.deliveries.length = 0;
			active.retainedByteSize = active.latestAuthTokenRetainedByteSize;
			this.removeListeners(active);
			active.socket.failDispatchProcessing(
				delivery.receipt,
				new ForegroundDemotionBufferOverflowError(nextEntryCount, nextRetainedByteSize),
			);
			active.socket.reset(false);
			return;
		}
		active.deliveries.push(delivery);
		active.retainedByteSize = nextRetainedByteSize;
	}

	private canContinue(active: ActiveForegroundDemotion): boolean {
		return !active.overflowed && !active.disconnected && active.socket.isConnected();
	}

	private release(active: ActiveForegroundDemotion): void {
		this.removeListeners(active);
		if (this.active === active) {
			this.active = null;
		}
	}

	private removeListeners(active: ActiveForegroundDemotion): void {
		active.socket.removeAllListeners('dispatch');
		active.socket.removeAllListeners('disconnect');
	}
}
