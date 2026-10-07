// SPDX-License-Identifier: AGPL-3.0-or-later

import {SNAPSHOT_SCHEMA_EPOCH} from '@app/features/gateway/snapshot/SnapshotEntities';
import {
	captureForegroundMentionCounts,
	hydrateStoresFromSnapshotScope,
} from '@app/features/gateway/snapshot/SnapshotHydration';
import SnapshotSync from '@app/features/gateway/snapshot/SnapshotSync';
import type {StateSnapshotCapture} from '@app/features/gateway/snapshot/SnapshotTypes';
import {gatewayEndpointForAccount} from '@app/features/gateway/transport/BackgroundGatewayAccounts';
import {BackgroundGatewaySession} from '@app/features/gateway/transport/BackgroundGatewaySession';
import {
	type BackgroundGatewaySessionManager,
	readMaxBackgroundGatewayConnections,
} from '@app/features/gateway/transport/BackgroundGatewaySessionManager';
import {
	ForegroundPromoteCoordinator,
	type ForegroundPromoteHost,
	type ForegroundPromotionOutcome,
} from '@app/features/gateway/transport/ForegroundPromoteCoordinator';
import type {GatewaySocket, GatewaySocketProperties} from '@app/features/gateway/transport/GatewaySocket';
import type {PooledGatewayConnection} from '@app/features/gateway/transport/PooledGatewayConnection';
import SessionManager, {type Account} from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('ForegroundGatewayHandoff');

export type ForegroundDemotionRetirementReason =
	| 'no_foreground'
	| 'handover_unavailable'
	| 'socket_lost'
	| 'account_unavailable'
	| 'transport_disabled'
	| 'capacity_disabled'
	| 'endpoint_unavailable'
	| 'metadata_unavailable'
	| 'snapshot_unavailable';

export type ForegroundDemotionOutcome =
	| {readonly mode: 'retained'; readonly accountKey: string; readonly sessionId: string}
	| {
			readonly mode: 'retired';
			readonly accountKey: string | null;
			readonly reason: ForegroundDemotionRetirementReason;
	  };

export class ForegroundGatewayHandoff {
	private readonly promotion: ForegroundPromoteCoordinator;
	private promotionTargetAccountKey: string | null = null;
	private promotionStartedAtMs: number | null = null;
	private demotionAccountKey: string | null = null;

	constructor(
		private readonly foreground: PooledGatewayConnection,
		private readonly background: BackgroundGatewaySessionManager,
	) {
		this.promotion = new ForegroundPromoteCoordinator(this.createPromotionHost());
	}

	async beginPromotion(accountKey: string): Promise<void> {
		if (this.promotionTargetAccountKey !== null) {
			throw new Error(`Foreground gateway promotion is already active for ${this.promotionTargetAccountKey}`);
		}
		this.promotionTargetAccountKey = accountKey;
		this.promotionStartedAtMs = Date.now();
		this.background.protect(accountKey);
		const previousAccountKey = this.foreground.foregroundAccountKey;
		try {
			if (
				previousAccountKey !== null &&
				previousAccountKey !== accountKey &&
				this.foreground.beginForegroundDemotionHandover(previousAccountKey)
			) {
				this.demotionAccountKey = previousAccountKey;
				this.background.protect(previousAccountKey);
			}
			if (this.background.isTransportAvailable()) {
				try {
					await this.background.preloadProperties();
				} catch (error) {
					logger.warn('Failed to pre-resolve client metadata for gateway handover', error);
				}
			}
			await this.promotion.begin(accountKey);
		} catch (error) {
			const demotionAccountKey = this.demotionAccountKey ?? previousAccountKey;
			try {
				if (demotionAccountKey !== null) {
					await this.foreground.rollbackForegroundDemotionHandover(demotionAccountKey);
				}
			} finally {
				this.releaseHandoffAccounts();
			}
			throw error;
		}
	}

	async completePromotion(accountKey: string): Promise<void> {
		if (this.promotionTargetAccountKey !== accountKey) {
			throw new Error(`Foreground gateway promotion target mismatch for ${accountKey}`);
		}
		try {
			const outcome = await this.promotion.complete(accountKey);
			this.logPromotionOutcome(accountKey, outcome);
		} finally {
			await this.unwindHandoff(accountKey);
		}
	}

	private logPromotionOutcome(accountKey: string, outcome: ForegroundPromotionOutcome): void {
		const startedAtMs = this.promotionStartedAtMs;
		if (startedAtMs === null) {
			throw new Error(`Foreground gateway promotion for ${accountKey} has no start time`);
		}
		logger.info('Foreground gateway promotion completed', {
			accountKey,
			mode: outcome.mode,
			reason: outcome.mode === 'cold' ? outcome.reason : null,
			elapsedMs: Date.now() - startedAtMs,
		});
	}

	async rollbackPromotion(accountKey: string): Promise<void> {
		if (this.promotionTargetAccountKey !== accountKey) {
			return;
		}
		await this.unwindHandoff(accountKey);
	}

	private async unwindHandoff(accountKey: string): Promise<void> {
		try {
			await this.promotion.rollback(accountKey);
		} finally {
			try {
				if (this.demotionAccountKey !== null) {
					await this.foreground.rollbackForegroundDemotionHandover(this.demotionAccountKey);
				}
			} finally {
				this.releaseHandoffAccounts();
			}
		}
	}

	async demoteForegroundForAccountSwitch(): Promise<ForegroundDemotionOutcome> {
		const startedAtMs = Date.now();
		const previousAccountKey = this.foreground.foregroundAccountKey;
		try {
			const outcome = await this.executeForegroundDemotion(previousAccountKey);
			logger.info('Foreground gateway demotion completed', {
				accountKey: outcome.accountKey,
				mode: outcome.mode,
				reason: outcome.mode === 'retired' ? outcome.reason : null,
				sessionId: outcome.mode === 'retained' ? outcome.sessionId : null,
				elapsedMs: Date.now() - startedAtMs,
			});
			return outcome;
		} catch (error) {
			this.foreground.retireForAccountSwitch();
			logger.error('Foreground gateway demotion failed', {
				accountKey: previousAccountKey,
				elapsedMs: Date.now() - startedAtMs,
				error,
			});
			throw error;
		} finally {
			if (previousAccountKey !== null && this.demotionAccountKey === previousAccountKey) {
				this.demotionAccountKey = null;
				this.background.unprotect(previousAccountKey);
			}
			this.background.sync();
		}
	}

	private async executeForegroundDemotion(previousAccountKey: string | null): Promise<ForegroundDemotionOutcome> {
		if (previousAccountKey === null) {
			return this.retireForeground(null, 'no_foreground');
		}
		if (this.demotionAccountKey === null) {
			if (!this.foreground.beginForegroundDemotionHandover(previousAccountKey)) {
				return this.retireForeground(previousAccountKey, 'handover_unavailable');
			}
			this.demotionAccountKey = previousAccountKey;
			this.background.protect(previousAccountKey);
		} else if (this.demotionAccountKey !== previousAccountKey) {
			throw new Error(`Foreground gateway demotion belongs to ${this.demotionAccountKey}, not ${previousAccountKey}`);
		}
		this.background.invalidateSync();
		if (!(await this.foreground.consumeForegroundDemotionHandover(previousAccountKey))) {
			return this.retireForeground(previousAccountKey, 'socket_lost');
		}
		const retainedAccount = SessionManager.getAccount(previousAccountKey);
		if (retainedAccount === null || !retainedAccount.isValid || retainedAccount.instance === undefined) {
			return this.retireForeground(previousAccountKey, 'account_unavailable');
		}
		if (!this.background.isTransportAvailable()) {
			return this.retireForeground(previousAccountKey, 'transport_disabled');
		}
		const maxBackgroundConnections = readMaxBackgroundGatewayConnections(
			retainedAccount.instance.features.max_background_gateway_connections,
		);
		if (maxBackgroundConnections <= 0) {
			return this.retireForeground(previousAccountKey, 'capacity_disabled');
		}
		const gatewayEndpoint = gatewayEndpointForAccount(retainedAccount);
		if (gatewayEndpoint === null) {
			return this.retireForeground(previousAccountKey, 'endpoint_unavailable');
		}
		const properties = this.background.cachedProperties;
		if (properties === null) {
			return this.retireForeground(previousAccountKey, 'metadata_unavailable');
		}
		await SnapshotSync.flush(previousAccountKey);
		const foregroundSocket = this.foreground.foregroundSocketForAccount(previousAccountKey);
		const sessionId = foregroundSocket?.getSessionId() ?? null;
		if (foregroundSocket === null || sessionId === null || !foregroundSocket.isConnected()) {
			return this.retireForeground(previousAccountKey, 'socket_lost');
		}
		const snapshot = SnapshotSync.demoteForeground(previousAccountKey);
		if (snapshot === null) {
			return this.retireForeground(previousAccountKey, 'snapshot_unavailable');
		}
		if (snapshot.cursor.sessionId !== sessionId || snapshot.cursor.schemaEpoch !== SNAPSHOT_SCHEMA_EPOCH) {
			throw new Error(
				`Foreground gateway snapshot invariant failed for ${previousAccountKey}: live=${sessionId}, snapshot=${snapshot.cursor.sessionId}, schema=${snapshot.cursor.schemaEpoch}`,
			);
		}
		const socket = this.foreground.detachForegroundSocket();
		if (socket === null || socket !== foregroundSocket) {
			throw new Error(`Foreground gateway socket detach invariant failed for ${previousAccountKey}`);
		}
		this.adoptDemotedSocket(previousAccountKey, retainedAccount, gatewayEndpoint, properties, socket, snapshot);
		this.foreground.retireForAccountSwitch();
		return {mode: 'retained', accountKey: previousAccountKey, sessionId};
	}

	private retireForeground(
		accountKey: string | null,
		reason: ForegroundDemotionRetirementReason,
	): ForegroundDemotionOutcome {
		this.foreground.retireForAccountSwitch();
		return {mode: 'retired', accountKey, reason};
	}

	private adoptDemotedSocket(
		accountKey: string,
		account: Account,
		gatewayEndpoint: string,
		properties: GatewaySocketProperties,
		socket: GatewaySocket,
		snapshot: StateSnapshotCapture,
	): void {
		const session = new BackgroundGatewaySession({
			account,
			gatewayEndpoint,
			properties,
			onStopRequested: () => this.background.stop(accountKey),
		});
		try {
			session.adoptSocket(socket, snapshot, captureForegroundMentionCounts());
			this.background.adopt(accountKey, session);
		} catch (error) {
			session.stop();
			if (socket.isConnected()) {
				this.discardDetachedSocket(socket);
			}
			throw new Error(`Failed to retain foreground gateway ${accountKey} as a background session`, {cause: error});
		}
	}

	private createPromotionHost(): ForegroundPromoteHost {
		return {
			findWarmCandidate: (accountKey) => this.background.find(accountKey),
			prepareForegroundSnapshot: async (accountKey) => {
				await SnapshotSync.flush(accountKey);
				const candidate = this.background.find(accountKey);
				const capture = SnapshotSync.captureDemotionSnapshot(accountKey);
				if (
					candidate === null ||
					capture === null ||
					capture.cursor.sessionId !== candidate.backgroundSessionId ||
					capture.cursor.schemaEpoch !== SNAPSHOT_SCHEMA_EPOCH
				) {
					return false;
				}
				return SnapshotSync.prepareForegroundPromotion(accountKey, capture.entries);
			},
			hydrateForegroundSnapshot: (accountKey) =>
				hydrateStoresFromSnapshotScope(accountKey, this.background.find(accountKey)?.captureMentionCounts() ?? null),
			commitForegroundSnapshot: (accountKey) => SnapshotSync.commitForegroundPromotion(accountKey),
			abortForegroundSnapshot: (accountKey) => SnapshotSync.abortForegroundPromotion(accountKey),
			finalizeForegroundSnapshot: (accountKey) => SnapshotSync.finalizeForegroundPromotion(accountKey),
			stopBackgroundConnection: (accountKey) => this.background.stop(accountKey),
			prepareForegroundReplay: () => this.foreground.preparePromotedForegroundReplay(),
			adoptForegroundSocket: (accountKey, socket) => {
				const sessionId = socket.getSessionId();
				if (sessionId === null) {
					throw new Error('Cannot promote a background gateway without its live session id');
				}
				this.foreground.adoptForegroundSocket(socket, {accountKey, sessionId});
			},
			replayForegroundDispatch: (accountKey, delivery) =>
				this.foreground.replayForegroundDispatch(accountKey, delivery),
			finalizeForeground: (accountKey) => this.foreground.finalizePromotedForegroundSession(accountKey),
			removeBackgroundConnection: (accountKey) => {
				if (!this.background.detachRegistration(accountKey)) {
					throw new Error(`Promoted background gateway ${accountKey} was not registered`);
				}
			},
			discardDetachedSocket: (socket) => this.discardDetachedSocket(socket),
			discardForegroundConnection: (accountKey) => {
				if (this.foreground.foregroundAccountKey !== accountKey) {
					throw new Error(`Cannot discard a foreground gateway not owned by ${accountKey}`);
				}
				this.foreground.retireForAccountSwitch();
			},
		};
	}

	private discardDetachedSocket(socket: GatewaySocket): void {
		try {
			socket.reset(false);
		} catch (error) {
			logger.warn('Failed to reset a detached gateway promotion socket', error);
		}
	}

	private releaseHandoffAccounts(): void {
		if (this.promotionTargetAccountKey !== null) {
			this.background.unprotect(this.promotionTargetAccountKey);
		}
		if (this.demotionAccountKey !== null) {
			this.background.unprotect(this.demotionAccountKey);
		}
		this.promotionTargetAccountKey = null;
		this.promotionStartedAtMs = null;
		this.demotionAccountKey = null;
		this.background.sync();
	}
}
