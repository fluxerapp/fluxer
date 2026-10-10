// SPDX-License-Identifier: AGPL-3.0-or-later

import Initialization from '@app/features/app/state/Initialization';
import {
	BackgroundGatewaySessionManager,
	MAX_BACKGROUND_GATEWAY_CONNECTIONS,
	readMaxBackgroundGatewayConnections,
} from '@app/features/gateway/transport/BackgroundGatewaySessionManager';
import {installDeferredReaction} from '@app/features/gateway/transport/DeferredReaction';
import {
	ForegroundGatewayConnectionRecoverableError,
	ForegroundGatewayRecoveryExhaustedError,
} from '@app/features/gateway/transport/ForegroundGatewayConnectionFailure';
import {
	type ForegroundDemotionOutcome,
	ForegroundGatewayHandoff,
} from '@app/features/gateway/transport/ForegroundGatewayHandoff';
import {
	PooledGatewayConnection,
	selectForegroundActivationGuildId,
} from '@app/features/gateway/transport/PooledGatewayConnection';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import SessionManager from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import LocalPresence from '@app/features/presence/state/LocalPresence';
import {reaction} from 'mobx';

export {MAX_BACKGROUND_GATEWAY_CONNECTIONS, readMaxBackgroundGatewayConnections};

const logger = new Logger('GatewaySessionPool');
const FOREGROUND_RECOVERY_RETRY_DELAYS_MS = [1_000, 3_000, 10_000] as const;
const FAILED_FOREGROUND_RECOVERY_REARM_INTERVAL_MS = 60_000;

type ForegroundRecoveryState =
	| {readonly kind: 'idle'}
	| {readonly kind: 'running'; readonly accountKey: string}
	| {readonly kind: 'failed'; readonly accountKey: string; readonly error: Error};

class GatewaySessionPool {
	readonly foregroundConnection = new PooledGatewayConnection();
	private readonly backgroundSessions = new BackgroundGatewaySessionManager();
	private readonly handoff = new ForegroundGatewayHandoff(this.foregroundConnection, this.backgroundSessions);
	private started = false;
	private disposers: Array<() => void> = [];
	private foregroundRecoveryRevision = 0;
	private foregroundRecoveryTimer: number | null = null;
	private foregroundRecovery: ForegroundRecoveryState = {kind: 'idle'};
	private failedRecoveryDisposer: (() => void) | null = null;

	start(): void {
		if (this.started) {
			return;
		}
		this.started = true;
		this.disposers.push(
			installDeferredReaction(() =>
				reaction(
					() => LocalPresence.presenceKey,
					() => this.syncForegroundPresence(),
				),
			),
		);
		this.disposers.push(
			installDeferredReaction(() =>
				reaction(
					() => ({
						guildId: selectForegroundActivationGuildId(),
						nonce: SelectedGuild.selectionNonce,
					}),
					({guildId}) => this.syncSelectedGuild(guildId),
				),
			),
		);
		this.disposers.push(
			installDeferredReaction(() =>
				reaction(
					() => this.foregroundConnection.isReady,
					(ready) => this.syncForegroundReadyState(ready),
				),
			),
		);
		this.backgroundSessions.start();
	}

	startRestoredSession(): void {
		const accountKey = SessionManager.currentAccountKey;
		if (accountKey === null) {
			return;
		}
		this.startForegroundRecovery(accountKey);
	}

	recoverForegroundSession(accountKey: string): void {
		if (SessionManager.currentAccountKey !== accountKey) {
			throw new Error(
				`Cannot recover foreground gateway for ${accountKey} while ${SessionManager.currentAccountKey ?? 'no account'} owns the session`,
			);
		}
		this.startForegroundRecovery(accountKey);
	}

	dispose(): void {
		this.stopForegroundRecovery();
		this.started = false;
		const disposers = this.disposers;
		this.disposers = [];
		for (const dispose of disposers) {
			dispose();
		}
		this.backgroundSessions.shutdown();
	}

	private startForegroundRecovery(accountKey: string): void {
		this.start();
		if (this.foregroundConnection.foregroundAccountKey === accountKey && this.foregroundConnection.isReady) {
			return;
		}
		if (this.foregroundRecovery.kind === 'running' && this.foregroundRecovery.accountKey === accountKey) {
			return;
		}
		this.stopForegroundRecovery();
		this.foregroundRecovery = {kind: 'running', accountKey};
		const revision = this.foregroundRecoveryRevision;
		void this.runForegroundRecoveryAttempt(accountKey, revision, 0);
	}

	private stopForegroundRecovery(): void {
		const failedRecoveryDisposer = this.failedRecoveryDisposer;
		this.failedRecoveryDisposer = null;
		failedRecoveryDisposer?.();
		this.foregroundRecoveryRevision += 1;
		if (this.foregroundRecoveryTimer !== null) {
			window.clearTimeout(this.foregroundRecoveryTimer);
			this.foregroundRecoveryTimer = null;
		}
		if (this.foregroundRecovery.kind === 'running') {
			this.foregroundConnection.cancelPendingSessionStart(this.foregroundRecovery.accountKey);
		}
		this.foregroundRecovery = {kind: 'idle'};
	}

	private async runForegroundRecoveryAttempt(accountKey: string, revision: number, retryIndex: number): Promise<void> {
		if (
			!this.started ||
			revision !== this.foregroundRecoveryRevision ||
			SessionManager.currentAccountKey !== accountKey
		) {
			return;
		}
		try {
			await this.waitForForegroundReady(accountKey);
			if (
				this.started &&
				revision === this.foregroundRecoveryRevision &&
				SessionManager.currentAccountKey === accountKey
			) {
				this.stopForegroundRecovery();
			}
		} catch (error) {
			if (
				!this.started ||
				revision !== this.foregroundRecoveryRevision ||
				SessionManager.currentAccountKey !== accountKey
			) {
				return;
			}
			if (!(error instanceof ForegroundGatewayConnectionRecoverableError)) {
				logger.error('Foreground gateway recovery stopped after a terminal failure', {accountKey, error});
				this.failForegroundRecovery(accountKey, error instanceof Error ? error : new Error(String(error)));
				return;
			}
			if (this.foregroundConnection.isSocketReconnecting(accountKey)) {
				logger.debug('Foreground gateway is still reconnecting with backoff, waiting for it', {accountKey});
				this.foregroundRecoveryTimer = window.setTimeout(() => {
					this.foregroundRecoveryTimer = null;
					void this.runForegroundRecoveryAttempt(accountKey, revision, retryIndex);
				}, 0);
				return;
			}
			const delayMs = FOREGROUND_RECOVERY_RETRY_DELAYS_MS[retryIndex];
			if (delayMs === undefined) {
				const exhausted = new ForegroundGatewayRecoveryExhaustedError(accountKey, retryIndex + 1, error);
				logger.error('Foreground gateway recovery exhausted its startup attempts', exhausted);
				this.failForegroundRecovery(accountKey, exhausted);
				return;
			}
			logger.warn('Foreground gateway recovery attempt failed, retrying', {
				accountKey,
				delayMs,
				error,
			});
			this.foregroundRecoveryTimer = window.setTimeout(() => {
				this.foregroundRecoveryTimer = null;
				void this.runForegroundRecoveryAttempt(accountKey, revision, retryIndex + 1);
			}, delayMs);
		}
	}

	private failForegroundRecovery(accountKey: string, error: Error): void {
		this.stopForegroundRecovery();
		if (SessionManager.currentAccountKey !== accountKey) {
			return;
		}
		this.foregroundRecovery = {kind: 'failed', accountKey, error};
		Initialization.setError(error.message);
		if (error instanceof ForegroundGatewayRecoveryExhaustedError) {
			this.failedRecoveryDisposer = this.armFailedForegroundRecovery(accountKey);
		}
	}

	private armFailedForegroundRecovery(accountKey: string): () => void {
		const rearm = (): void => {
			if (
				!this.started ||
				this.foregroundRecovery.kind !== 'failed' ||
				this.foregroundRecovery.accountKey !== accountKey ||
				SessionManager.currentAccountKey !== accountKey
			) {
				return;
			}
			if (this.foregroundConnection.foregroundAccountKey === accountKey && this.foregroundConnection.isReady) {
				this.stopForegroundRecovery();
				return;
			}
			logger.info('Retrying foreground gateway recovery after it was exhausted', {accountKey});
			this.startForegroundRecovery(accountKey);
		};
		const handleVisibilityChange = (): void => {
			if (document.visibilityState === 'visible') {
				rearm();
			}
		};
		window.addEventListener('online', rearm);
		document.addEventListener('visibilitychange', handleVisibilityChange);
		const interval = window.setInterval(rearm, FAILED_FOREGROUND_RECOVERY_REARM_INTERVAL_MS);
		return () => {
			window.removeEventListener('online', rearm);
			document.removeEventListener('visibilitychange', handleVisibilityChange);
			window.clearInterval(interval);
		};
	}

	async waitForForegroundReady(accountKey: string | null): Promise<void> {
		this.start();
		const targetAccountKey = accountKey ?? SessionManager.currentAccountKey;
		if (targetAccountKey !== SessionManager.currentAccountKey) {
			throw new Error(
				`Cannot start foreground gateway for ${targetAccountKey ?? 'no account'} while ${SessionManager.currentAccountKey ?? 'no account'} owns the session`,
			);
		}
		if (this.foregroundConnection.foregroundAccountKey === targetAccountKey && this.foregroundConnection.isReady) {
			return;
		}
		await this.foregroundConnection.startSessionAndWaitForReady(targetAccountKey);
	}

	beginPromote(accountKey: string): Promise<void> {
		this.stopForegroundRecovery();
		return this.handoff.beginPromotion(accountKey);
	}

	completePromote(accountKey: string): Promise<void> {
		return this.handoff.completePromotion(accountKey);
	}

	rollbackPromote(accountKey: string): Promise<void> {
		return this.handoff.rollbackPromotion(accountKey);
	}

	demoteForegroundForAccountSwitch(): Promise<ForegroundDemotionOutcome> {
		return this.handoff.demoteForegroundForAccountSwitch();
	}

	isBackgroundTransportAvailable(): boolean {
		return this.backgroundSessions.isTransportAvailable();
	}

	bootstrapBackground(): void {
		this.backgroundSessions.start();
	}

	get isBackgroundBootstrapped(): boolean {
		return this.backgroundSessions.isBootstrapped;
	}

	get backgroundConnectionCount(): number {
		return this.backgroundSessions.connectionCount;
	}

	private syncForegroundPresence(): void {
		const presence = LocalPresence.getGatewayPresence();
		if (presence === null) {
			return;
		}
		this.foregroundConnection.socket?.updatePresence(
			presence.status,
			presence.afk,
			presence.mobile,
			presence.custom_status,
		);
	}

	private syncSelectedGuild(guildId: string | null): void {
		if (!guildId) {
			this.foregroundConnection.clearPendingGuildSync();
			return;
		}
		this.foregroundConnection.syncGuildIfNeeded(guildId, 'select');
	}

	private syncForegroundReadyState(ready: boolean): void {
		if (ready) {
			this.foregroundConnection.flushPendingGuildSync();
			this.syncForegroundPresence();
		}
	}
}

export default new GatewaySessionPool();
