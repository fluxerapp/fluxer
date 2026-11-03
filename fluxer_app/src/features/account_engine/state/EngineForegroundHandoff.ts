// SPDX-License-Identifier: AGPL-3.0-or-later

import {SNAPSHOT_SCHEMA_EPOCH} from '@app/features/gateway/snapshot/SnapshotEntities';
import type {StateSnapshotCapture} from '@app/features/gateway/snapshot/SnapshotTypes';
import type {GatewayHandoffSeed, GatewayResumeSeed} from '@app/features/gateway/transport/GatewaySocket';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {
	AccountEngineDemotion,
	AccountEnginePromotion,
	AccountEngineRequestMethod,
	AccountEngineRequests,
	AccountEngineResumeSession,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';

const logger = new Logger('EngineForegroundHandoff');

const BOOT_ENGINE_WAIT_MS = 5_000;
const TIMED_OUT: unique symbol = Symbol('timed-out');

function within<T>(promise: Promise<T>, timeoutMs: number): Promise<T | typeof TIMED_OUT> {
	let timer: ReturnType<typeof setTimeout> | undefined;
	const timeout = new Promise<typeof TIMED_OUT>((resolve) => {
		timer = setTimeout(() => resolve(TIMED_OUT), timeoutMs);
	});
	return Promise.race([promise, timeout]).finally(() => clearTimeout(timer));
}

interface EngineHandoffEngine {
	readonly available: boolean;
	request<M extends AccountEngineRequestMethod>(
		method: M,
		params: AccountEngineRequests[M]['params'],
	): Promise<AccountEngineRequests[M]['result']>;
}

interface EngineHandoffForeground {
	readonly foregroundAccountKey: string | null;
	beginForegroundDemotionHandover(accountKey: string): boolean;
	finishForegroundDemotionHandover(accountKey: string): Promise<GatewayHandoffSeed | null>;
	rollbackForegroundDemotionHandover(accountKey: string): Promise<void>;
	retireForAccountSwitch(): void;
	preparePromotedForegroundReplay(): void;
	startResumedSession(accountKey: string, seed: GatewayResumeSeed): Promise<boolean>;
}

interface EngineHandoffSnapshots {
	flush(accountKey: string): Promise<void>;
	captureDemotionSnapshot(accountKey: string): StateSnapshotCapture | null;
	adoptPromotedSnapshot(accountKey: string, selfUserId: string, capture: StateSnapshotCapture): boolean;
	evict(accountKey: string): void;
}

interface EngineHandoffAccount {
	readonly userId: string;
	readonly gatewayEndpoint: string | null;
}

export interface EngineHandoffHost {
	readonly engine: EngineHandoffEngine;
	readonly foreground: EngineHandoffForeground;
	readonly snapshots: EngineHandoffSnapshots;
	getAccount(accountKey: string): EngineHandoffAccount | null;
	isCurrentAccount(accountKey: string): boolean;
	captureMentionCounts(): ReadonlyMap<string, number>;
	hydrate(accountKey: string, mentionCounts: ReadonlyMap<string, number>): Promise<void>;
	whenForegroundLive(accountKey: string): Promise<boolean | null>;
	whenEngineConfigured(): Promise<void>;
}

type ForegroundDemotionRetirementReason = 'no_foreground' | 'handover_unavailable' | 'engine_unavailable';

export type ForegroundDemotionOutcome =
	| {
			readonly mode: 'handed_off';
			readonly accountKey: string;
			readonly session: AccountEngineResumeSession | null;
			readonly captured: boolean;
	  }
	| {
			readonly mode: 'retired';
			readonly accountKey: string | null;
			readonly reason: ForegroundDemotionRetirementReason;
	  };

export type ForegroundPromotionMode = 'resume' | 'hydrated' | 'cold';

interface ActivePromotion {
	readonly accountKey: string;
	readonly requested: boolean;
	readonly request: Promise<AccountEnginePromotion | null>;
	readonly demotionAccountKey: string | null;
}

export class EngineForegroundHandoff {
	private active: ActivePromotion | null = null;
	private readonly awaitingLive = new Set<string>();

	constructor(private readonly host: EngineHandoffHost) {}

	beginPromotion(accountKey: string): void {
		if (this.active !== null) {
			throw new Error(`Foreground promotion is already active for ${this.active.accountKey}`);
		}
		const previousAccountKey = this.host.foreground.foregroundAccountKey;
		const demotionAccountKey =
			previousAccountKey !== null &&
			previousAccountKey !== accountKey &&
			this.host.foreground.beginForegroundDemotionHandover(previousAccountKey)
				? previousAccountKey
				: null;
		const requested = this.host.engine.available;
		this.active = {
			accountKey,
			requested,
			request: requested ? this.requestPromotion(accountKey) : Promise.resolve(null),
			demotionAccountKey,
		};
	}

	async completePromotion(accountKey: string): Promise<ForegroundPromotionMode> {
		const active = this.requireActive(accountKey);
		try {
			const promotion = await active.request;
			return await this.adoptAndSettle(accountKey, promotion, active.requested);
		} finally {
			if (this.active === active) {
				this.active = null;
			}
			if (active.demotionAccountKey !== null) {
				await this.host.foreground.rollbackForegroundDemotionHandover(active.demotionAccountKey);
			}
		}
	}

	async rollbackPromotion(accountKey: string): Promise<void> {
		const active = this.active;
		if (active === null || active.accountKey !== accountKey) {
			return;
		}
		this.active = null;
		try {
			await active.request;
			if (active.requested) {
				this.send('abortPromotion', {accountKey});
			}
		} finally {
			if (active.demotionAccountKey !== null) {
				await this.host.foreground.rollbackForegroundDemotionHandover(active.demotionAccountKey);
			}
		}
	}

	async demoteForeground(): Promise<ForegroundDemotionOutcome> {
		const foreground = this.host.foreground;
		const accountKey = foreground.foregroundAccountKey;
		if (accountKey === null) {
			foreground.retireForAccountSwitch();
			return {mode: 'retired', accountKey: null, reason: 'no_foreground'};
		}
		if (!this.host.engine.available) {
			this.host.snapshots.evict(accountKey);
			foreground.retireForAccountSwitch();
			return {mode: 'retired', accountKey, reason: 'engine_unavailable'};
		}
		const handoverActive =
			this.active?.demotionAccountKey === accountKey || foreground.beginForegroundDemotionHandover(accountKey);
		try {
			const seed = handoverActive ? await foreground.finishForegroundDemotionHandover(accountKey) : null;
			const capture = await this.captureDemotion(accountKey);
			const account = this.host.getAccount(accountKey);
			const session =
				seed !== null &&
				capture !== null &&
				capture.cursor.sessionId === seed.sessionId &&
				account?.gatewayEndpoint != null
					? {sessionId: seed.sessionId, sequence: seed.sequence, gatewayUrl: account.gatewayEndpoint}
					: null;
			const demotion: AccountEngineDemotion = {
				accountKey,
				session,
				release: session !== null && seed !== null ? seed.release : null,
				capture,
				mentionCounts: capture === null ? null : Object.fromEntries(this.host.captureMentionCounts()),
			};
			this.host.snapshots.evict(accountKey);
			foreground.retireForAccountSwitch();
			if (account !== null) {
				this.settleLive(accountKey);
				this.send('demote', demotion);
			}
			if (!handoverActive) {
				return {mode: 'retired', accountKey, reason: 'handover_unavailable'};
			}
			return {mode: 'handed_off', accountKey, session, captured: capture !== null};
		} catch (error) {
			this.host.snapshots.evict(accountKey);
			foreground.retireForAccountSwitch();
			throw error;
		}
	}

	async bootForeground(accountKey: string): Promise<ForegroundPromotionMode> {
		if (!this.host.engine.available) {
			return 'cold';
		}
		const configured = await within(this.host.whenEngineConfigured(), BOOT_ENGINE_WAIT_MS);
		if (configured === TIMED_OUT) {
			logger.warn('The account engine did not answer in time, starting the foreground session cold', {accountKey});
			if (this.host.isCurrentAccount(accountKey)) {
				this.send('claimForeground', {accountKey});
			}
			return 'cold';
		}
		if (!this.canBoot(accountKey)) {
			return 'cold';
		}
		const request = this.requestPromotion(accountKey);
		const promotion = await within(request, BOOT_ENGINE_WAIT_MS);
		if (promotion === TIMED_OUT) {
			logger.warn('The account engine did not hand over in time, starting the foreground session cold', {accountKey});
			void request.then((late) => this.settleAbandonedBoot(accountKey, late));
			return 'cold';
		}
		if (!this.canBoot(accountKey)) {
			this.settleAbandonedBoot(accountKey, promotion);
			return 'cold';
		}
		return await this.adoptAndSettle(accountKey, promotion, true);
	}

	private settleAbandonedBoot(accountKey: string, promotion: AccountEnginePromotion | null): void {
		if (this.host.isCurrentAccount(accountKey)) {
			this.settleCold(accountKey, promotion);
			return;
		}
		if (promotion !== null) {
			this.send('abortPromotion', {accountKey});
		}
	}

	private canBoot(accountKey: string): boolean {
		return (
			this.active === null &&
			this.host.isCurrentAccount(accountKey) &&
			this.host.foreground.foregroundAccountKey === null
		);
	}

	private async adoptAndSettle(
		accountKey: string,
		promotion: AccountEnginePromotion | null,
		requested: boolean,
	): Promise<ForegroundPromotionMode> {
		let mode: ForegroundPromotionMode = 'cold';
		try {
			mode = await this.adoptPromotion(accountKey, promotion);
			return mode;
		} finally {
			if (mode !== 'resume' && requested) {
				this.settleCold(accountKey, promotion);
			}
		}
	}

	private async adoptPromotion(
		accountKey: string,
		promotion: AccountEnginePromotion | null,
	): Promise<ForegroundPromotionMode> {
		const capture = promotion?.capture ?? null;
		const account = this.host.getAccount(accountKey);
		if (
			promotion === null ||
			capture === null ||
			account === null ||
			!this.host.engine.available ||
			capture.cursor.schemaEpoch !== SNAPSHOT_SCHEMA_EPOCH
		) {
			return 'cold';
		}
		try {
			this.host.foreground.preparePromotedForegroundReplay();
			if (!this.host.snapshots.adoptPromotedSnapshot(accountKey, account.userId, capture)) {
				return 'cold';
			}
			await this.host.hydrate(accountKey, new Map(Object.entries(promotion.mentionCounts)));
		} catch (error) {
			logger.warn('Falling back to a cold foreground session after promotion hydration failed', {
				accountKey,
				error,
			});
			this.host.snapshots.evict(accountKey);
			return 'cold';
		}
		const session = promotion.session;
		if (session === null || session.sessionId !== capture.cursor.sessionId) {
			return 'hydrated';
		}
		const started = await this.host.foreground.startResumedSession(accountKey, {
			sessionId: session.sessionId,
			sequence: session.sequence,
		});
		if (!started) {
			return 'hydrated';
		}
		this.awaitingLive.add(accountKey);
		const settle = () => this.settleLive(accountKey);
		void this.host.whenForegroundLive(accountKey).then(settle, settle);
		return 'resume';
	}

	private async captureDemotion(accountKey: string): Promise<StateSnapshotCapture | null> {
		try {
			await this.host.snapshots.flush(accountKey);
		} catch (error) {
			logger.warn('Dropping an invalid snapshot on demotion', {accountKey, error});
			return null;
		}
		const capture = this.host.snapshots.captureDemotionSnapshot(accountKey);
		return capture !== null && capture.cursor.schemaEpoch === SNAPSHOT_SCHEMA_EPOCH ? capture : null;
	}

	private settleLive(accountKey: string): void {
		if (!this.awaitingLive.delete(accountKey)) {
			return;
		}
		this.send('completePromotion', {accountKey});
	}

	private settleCold(accountKey: string, promotion: AccountEnginePromotion | null): void {
		if (promotion === null) {
			this.send('claimForeground', {accountKey});
			return;
		}
		this.send('completePromotion', {accountKey});
	}

	private requestPromotion(accountKey: string): Promise<AccountEnginePromotion | null> {
		return this.host.engine.request('beginPromotion', {accountKey}).catch((error: unknown) => {
			logger.warn('The account engine could not hand over the account, switching cold', {accountKey, error});
			return null;
		});
	}

	private send<M extends AccountEngineRequestMethod>(method: M, params: AccountEngineRequests[M]['params']): void {
		if (!this.host.engine.available) {
			return;
		}
		void this.host.engine.request(method, params).catch((error: unknown) => {
			logger.warn(`Account engine ${method} failed`, {error});
		});
	}

	private requireActive(accountKey: string): ActivePromotion {
		const active = this.active;
		if (active === null || active.accountKey !== accountKey) {
			throw new Error(
				`Foreground promotion for ${accountKey} is not active (${active?.accountKey ?? 'none'} is active)`,
			);
		}
		return active;
	}
}
