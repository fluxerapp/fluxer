// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type BackgroundDispatchHandoverEnd,
	BackgroundDispatchHandoverOutcome,
} from '@app/features/gateway/transport/BackgroundGatewayDispatchQueue';
import {exceedsDispatchBufferCapacity} from '@app/features/gateway/transport/GatewayDispatchBufferBounds';
import type {GatewayDispatchDelivery, GatewaySocket} from '@app/features/gateway/transport/GatewaySocket';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = new Logger('ForegroundPromoteCoordinator');

const WARM_PROMOTE_READY_TIMEOUT_MS = 10_000;

export type ForegroundPromotionColdReason =
	| 'no_candidate'
	| 'candidate_not_ready_timeout'
	| 'candidate_lost'
	| 'snapshot_unavailable'
	| 'buffer_capacity';

export type ForegroundPromotionOutcome =
	| {readonly mode: 'warm'}
	| {readonly mode: 'cold'; readonly reason: ForegroundPromotionColdReason};

export interface WarmForegroundCandidate {
	readonly isBackgroundConnectionReady: boolean;
	beginBackgroundDispatchHandover(listener: (delivery: GatewayDispatchDelivery) => void): void;
	endBackgroundDispatchHandover(request: BackgroundDispatchHandoverEnd): void;
	detachBackgroundSocket(): GatewaySocket | null;
	waitForBackgroundConnectionReady(timeoutMs: number): Promise<boolean>;
	waitForBackgroundDispatchIdle(): Promise<void>;
}

export interface ForegroundPromoteHost {
	findWarmCandidate(accountKey: string): WarmForegroundCandidate | null;
	prepareForegroundSnapshot(accountKey: string): Promise<boolean>;
	hydrateForegroundSnapshot(accountKey: string): Promise<void>;
	commitForegroundSnapshot(accountKey: string): boolean;
	abortForegroundSnapshot(accountKey: string): void;
	finalizeForegroundSnapshot(accountKey: string): void;
	stopBackgroundConnection(accountKey: string): void | Promise<void>;
	prepareForegroundReplay(): void;
	adoptForegroundSocket(accountKey: string, socket: GatewaySocket): void;
	replayForegroundDispatch(accountKey: string, delivery: GatewayDispatchDelivery): void | Promise<void>;
	finalizeForeground(accountKey: string): void;
	removeBackgroundConnection(accountKey: string): void;
	discardDetachedSocket(socket: GatewaySocket): void;
	discardForegroundConnection(accountKey: string): void;
}

interface PromoteDispatchBuffer {
	readonly deliveries: Array<GatewayDispatchDelivery>;
	retainedByteSize: number;
	overflowed: boolean;
}

interface ColdPromotion {
	readonly mode: 'cold';
	readonly targetAccountKey: string;
	readonly reason: ForegroundPromotionColdReason;
}

interface WarmingPromotion {
	readonly mode: 'warming';
	readonly targetAccountKey: string;
	readonly candidate: WarmForegroundCandidate;
}

interface WarmPromotion {
	readonly mode: 'warm';
	readonly targetAccountKey: string;
	readonly candidate: WarmForegroundCandidate;
	readonly buffer: PromoteDispatchBuffer;
	handoverActive: boolean;
}

type ActivePromotion = ColdPromotion | WarmingPromotion | WarmPromotion;

export class ForegroundPromoteCoordinator {
	private active: ActivePromotion | null = null;

	constructor(private readonly host: ForegroundPromoteHost) {}

	async begin(targetAccountKey: string): Promise<void> {
		if (this.active !== null) {
			throw new Error(`Foreground gateway promotion is already active for ${this.active.targetAccountKey}`);
		}
		const candidate = this.host.findWarmCandidate(targetAccountKey);
		if (candidate === null) {
			this.active = {mode: 'cold', targetAccountKey, reason: 'no_candidate'};
			return;
		}
		if (!candidate.isBackgroundConnectionReady) {
			const coldReason = await this.waitForWarmCandidate(targetAccountKey, candidate);
			if (coldReason !== null) {
				this.active = {mode: 'cold', targetAccountKey, reason: coldReason};
				return;
			}
		}
		const active: WarmPromotion = {
			mode: 'warm',
			targetAccountKey,
			candidate,
			buffer: {deliveries: [], retainedByteSize: 0, overflowed: false},
			handoverActive: false,
		};
		this.active = active;
		try {
			candidate.beginBackgroundDispatchHandover((delivery) => this.captureDispatch(active.buffer, delivery));
			active.handoverActive = true;
			const handoverReason = await this.settleWarmCheckpoint(active);
			if (handoverReason !== null) {
				this.fallbackToCold(active, handoverReason);
				return;
			}
			const snapshotPrepared = await this.host.prepareForegroundSnapshot(targetAccountKey);
			const snapshotReason = await this.settleWarmCheckpoint(active);
			if (snapshotReason !== null) {
				this.fallbackToCold(active, snapshotReason);
				return;
			}
			if (!snapshotPrepared) {
				this.fallbackToCold(active, 'snapshot_unavailable');
			}
		} catch (error) {
			await this.abortFailedWarmPromotion(active);
			throw error;
		}
	}

	async complete(targetAccountKey: string): Promise<ForegroundPromotionOutcome> {
		const active = this.requireActivePromotion(targetAccountKey);
		if (active.mode === 'cold') {
			this.active = null;
			await this.host.stopBackgroundConnection(targetAccountKey);
			return {mode: 'cold', reason: active.reason};
		}
		if (active.mode === 'warming') {
			throw new Error(`Foreground gateway promotion for ${targetAccountKey} is still warming`);
		}
		let detachedSocket: GatewaySocket | null = null;
		let foregroundAdopted = false;
		try {
			const handoverReason = await this.settleWarmCheckpoint(active);
			if (handoverReason !== null) {
				return await this.finishCold(active, handoverReason);
			}
			this.host.prepareForegroundReplay();
			try {
				await this.host.hydrateForegroundSnapshot(targetAccountKey);
			} catch (error) {
				logger.warn('Falling back to a cold foreground promotion after snapshot hydration failed', {
					targetAccountKey,
					error,
				});
				return await this.finishCold(active, 'snapshot_unavailable');
			}
			const hydrationReason = await this.settleWarmCheckpoint(active);
			if (hydrationReason !== null) {
				return await this.finishCold(active, hydrationReason);
			}
			if (!this.host.commitForegroundSnapshot(targetAccountKey)) {
				throw new Error(`Foreground snapshot commit rejected ${targetAccountKey}`);
			}
			this.endHandover(active, {outcome: BackgroundDispatchHandoverOutcome.CONSUMED});
			detachedSocket = active.candidate.detachBackgroundSocket();
			if (detachedSocket === null) {
				throw new Error(`Ready background gateway ${targetAccountKey} had no socket to promote`);
			}
			this.host.removeBackgroundConnection(targetAccountKey);
			this.host.adoptForegroundSocket(targetAccountKey, detachedSocket);
			foregroundAdopted = true;
			for (const delivery of active.buffer.deliveries) {
				await this.host.replayForegroundDispatch(targetAccountKey, delivery);
			}
			this.clearBuffer(active.buffer);
			this.host.finalizeForeground(targetAccountKey);
			this.host.finalizeForegroundSnapshot(targetAccountKey);
			this.active = null;
			return {mode: 'warm'};
		} catch (error) {
			if (this.active === active) {
				await this.cleanFailedCompletion(active, detachedSocket, foregroundAdopted);
			}
			throw error;
		}
	}

	async rollback(targetAccountKey: string): Promise<void> {
		const active = this.active;
		if (active === null) {
			return;
		}
		if (active.targetAccountKey !== targetAccountKey) {
			throw new Error(
				`Cannot roll back foreground gateway promotion for ${targetAccountKey} because ${active.targetAccountKey} is active`,
			);
		}
		if (active.mode === 'cold' || active.mode === 'warming') {
			this.active = null;
			return;
		}
		try {
			await active.candidate.waitForBackgroundDispatchIdle();
			this.restoreOrDiscardHandover(active);
			this.host.abortForegroundSnapshot(targetAccountKey);
			this.active = null;
		} catch (error) {
			await this.abortFailedWarmPromotion(active);
			throw error;
		}
	}

	private captureDispatch(buffer: PromoteDispatchBuffer, delivery: GatewayDispatchDelivery): void {
		if (buffer.overflowed) {
			return;
		}
		if (
			exceedsDispatchBufferCapacity(
				buffer.deliveries.length + 1,
				buffer.retainedByteSize + delivery.retainedByteSize,
				delivery.retainedByteSize,
			)
		) {
			buffer.overflowed = true;
			return;
		}
		buffer.deliveries.push(delivery);
		buffer.retainedByteSize += delivery.retainedByteSize;
	}

	private requireActivePromotion(targetAccountKey: string): ActivePromotion {
		const active = this.active;
		if (active === null) {
			throw new Error(`No foreground gateway promotion is active for ${targetAccountKey}`);
		}
		if (active.targetAccountKey !== targetAccountKey) {
			throw new Error(`Foreground gateway promotion belongs to ${active.targetAccountKey}, not ${targetAccountKey}`);
		}
		return active;
	}

	private async settleWarmCheckpoint(active: WarmPromotion): Promise<ForegroundPromotionColdReason | null> {
		await active.candidate.waitForBackgroundDispatchIdle();
		if (active.buffer.overflowed) {
			return 'buffer_capacity';
		}
		if (!this.isWarmPromotionAvailable(active)) {
			return 'candidate_lost';
		}
		return null;
	}

	private isWarmPromotionAvailable(active: WarmPromotion): boolean {
		return (
			active.candidate.isBackgroundConnectionReady &&
			this.host.findWarmCandidate(active.targetAccountKey) === active.candidate
		);
	}

	private async waitForWarmCandidate(
		targetAccountKey: string,
		candidate: WarmForegroundCandidate,
	): Promise<ForegroundPromotionColdReason | null> {
		const active: WarmingPromotion = {mode: 'warming', targetAccountKey, candidate};
		this.active = active;
		try {
			const ready = await candidate.waitForBackgroundConnectionReady(WARM_PROMOTE_READY_TIMEOUT_MS);
			if (this.active !== active || this.host.findWarmCandidate(targetAccountKey) !== candidate) {
				return 'candidate_lost';
			}
			if (!ready) {
				return 'candidate_not_ready_timeout';
			}
			if (!candidate.isBackgroundConnectionReady) {
				return 'candidate_lost';
			}
			return null;
		} catch (error) {
			if (this.active === active) {
				this.active = null;
			}
			throw error;
		}
	}

	private fallbackToCold(active: WarmPromotion, reason: ForegroundPromotionColdReason): void {
		this.restoreOrDiscardHandover(active);
		this.host.abortForegroundSnapshot(active.targetAccountKey);
		this.active = {mode: 'cold', targetAccountKey: active.targetAccountKey, reason};
	}

	private async finishCold(
		active: WarmPromotion,
		reason: ForegroundPromotionColdReason,
	): Promise<ForegroundPromotionOutcome> {
		this.restoreOrDiscardHandover(active);
		this.host.abortForegroundSnapshot(active.targetAccountKey);
		this.active = null;
		await this.host.stopBackgroundConnection(active.targetAccountKey);
		return {mode: 'cold', reason};
	}

	private restoreOrDiscardHandover(active: WarmPromotion): void {
		this.endHandover(
			active,
			active.buffer.overflowed
				? {outcome: BackgroundDispatchHandoverOutcome.LOST}
				: {outcome: BackgroundDispatchHandoverOutcome.RESTORED, deliveries: active.buffer.deliveries},
		);
		this.clearBuffer(active.buffer);
	}

	private endHandover(active: WarmPromotion, request: BackgroundDispatchHandoverEnd): void {
		if (!active.handoverActive) {
			return;
		}
		active.handoverActive = false;
		active.candidate.endBackgroundDispatchHandover(request);
	}

	private async abortFailedWarmPromotion(active: WarmPromotion): Promise<void> {
		try {
			if (active.handoverActive) {
				this.endHandover(active, {outcome: BackgroundDispatchHandoverOutcome.LOST});
			}
		} finally {
			this.clearBuffer(active.buffer);
			this.host.abortForegroundSnapshot(active.targetAccountKey);
			this.active = null;
			await this.host.stopBackgroundConnection(active.targetAccountKey);
		}
	}

	private async cleanFailedCompletion(
		active: WarmPromotion,
		detachedSocket: GatewaySocket | null,
		foregroundAdopted: boolean,
	): Promise<void> {
		try {
			if (active.handoverActive) {
				this.endHandover(active, {outcome: BackgroundDispatchHandoverOutcome.LOST});
			}
			if (foregroundAdopted) {
				this.host.discardForegroundConnection(active.targetAccountKey);
			} else if (detachedSocket !== null) {
				this.host.discardDetachedSocket(detachedSocket);
			}
		} finally {
			this.clearBuffer(active.buffer);
			this.host.abortForegroundSnapshot(active.targetAccountKey);
			this.active = null;
			await this.host.stopBackgroundConnection(active.targetAccountKey);
		}
	}

	private clearBuffer(buffer: PromoteDispatchBuffer): void {
		buffer.deliveries.length = 0;
		buffer.retainedByteSize = 0;
	}
}
