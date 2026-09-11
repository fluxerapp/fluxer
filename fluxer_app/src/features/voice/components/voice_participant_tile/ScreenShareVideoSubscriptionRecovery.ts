// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	selectVoiceMediaGraphFailure,
	selectVoiceMediaGraphViewerStreamKeys,
	selectVoiceMediaGraphWatchGeneration,
	type VoiceMediaGraphEvent,
	type VoiceMediaGraphSnapshot,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '@app/features/voice/engine/VoiceMediaGraphStore';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {
	getScreenShareWatchFailureForPublicationOperation,
	ScreenShareWatchErrorCode,
} from '@app/features/voice/state/ScreenShareWatchFailures';
import {
	refreshScreenSharePublicationSubscription,
	resubscribeScreenSharePublication,
	type ScreenSharePublicationErrorHandler,
	type ScreenSharePublicationOperation,
	type ScreenSharePublicationTarget,
} from '@app/features/voice/utils/ScreenShareSubscriptionPolicy';

export type ScreenShareVideoSubscriptionRecoveryMode = 'refresh' | 'resubscribe';

export interface ScreenShareVideoSubscriptionRecoveryPublication extends ScreenSharePublicationTarget {
	track?: unknown;
	trackSid?: string | null;
}

export interface ScreenShareVideoSubscriptionRecoveryRetryInfo {
	participantIdentity?: string | null;
	trackSid: string | null;
	attempt: number;
	delayMs: number;
	mode: ScreenShareVideoSubscriptionRecoveryMode;
}

export interface ScreenShareVideoSubscriptionRecoveryLeaseOptions {
	key: string;
	publication: ScreenShareVideoSubscriptionRecoveryPublication;
	streamKey?: string | null;
	participantIdentity?: string | null;
	isStillWanted: () => boolean;
	onError?: ScreenSharePublicationErrorHandler;
	onRetry?: (info: ScreenShareVideoSubscriptionRecoveryRetryInfo) => void;
	recover?: (mode: ScreenShareVideoSubscriptionRecoveryMode) => void;
}

export interface ScreenShareVideoSubscriptionRecoveryScheduler {
	setTimeout(callback: () => void, delayMs: number): number;
	clearTimeout(timeoutId: number): void;
}

export interface ScreenShareVideoSubscriptionRecoveryGraph {
	getGraphSnapshot(): VoiceMediaGraphSnapshot;
	nowMs(): number;
	transition(event: VoiceMediaGraphEvent): unknown;
}

interface ScreenShareVideoSubscriptionRecoverySession {
	key: string;
	leaseCount: number;
	attempt: number;
	timeoutId: number | null;
	graphGeneration: number | null;
	options: ScreenShareVideoSubscriptionRecoveryLeaseOptions;
}

const SCREEN_SHARE_VIDEO_SUBSCRIPTION_RETRY_INITIAL_DELAY_MS = 2500;
const SCREEN_SHARE_VIDEO_SUBSCRIPTION_RETRY_MAX_DELAY_MS = 30_000;
const SCREEN_SHARE_VIDEO_SUBSCRIPTION_HEALTH_CHECK_DELAY_MS = 2500;
const SCREEN_SHARE_VIDEO_SUBSCRIPTION_REFRESH_ATTEMPTS = 0;
const SCREEN_SHARE_VIDEO_SUBSCRIPTION_FIRST_FRAME_RECOVERY_ATTEMPTS = 3;
const SCREEN_SHARE_VIDEO_SUBSCRIPTION_FIRST_FRAME_RECOVERY_DELAY_MS = 20_000;
const SCREEN_SHARE_VIDEO_SUBSCRIPTION_FIRST_FRAME_RECOVERY_RECORDS = 32;

const defaultScheduler: ScreenShareVideoSubscriptionRecoveryScheduler = {
	setTimeout: (callback, delayMs) => window.setTimeout(callback, delayMs),
	clearTimeout: (timeoutId) => window.clearTimeout(timeoutId),
};

interface ScreenShareRecoveryMediaStreamTrackLike {
	readyState?: string;
	muted?: boolean;
}

interface ScreenShareRecoveryTrackLike {
	mediaStreamTrack?: ScreenShareRecoveryMediaStreamTrackLike;
	isMuted?: boolean;
	muted?: boolean;
}

function asScreenShareRecoveryTrackLike(track: unknown): ScreenShareRecoveryTrackLike | null {
	if (track == null) return null;
	if (typeof track !== 'object') return null;
	return track as ScreenShareRecoveryTrackLike;
}

function isTrackReceivable(track: ScreenShareRecoveryTrackLike, {allowMuted}: {allowMuted: boolean}): boolean {
	const mediaStreamTrack = track.mediaStreamTrack;
	if (mediaStreamTrack?.readyState === 'ended') return false;
	if (allowMuted) return true;
	if (mediaStreamTrack?.muted === true) return false;
	if (track.isMuted === true) return false;
	if (track.muted === true) return false;
	return true;
}

function hasReceivableTrack(
	publication: ScreenShareVideoSubscriptionRecoveryPublication,
	{allowMuted}: {allowMuted: boolean},
): boolean {
	if (publication.isSubscribed === false) return false;
	const track = asScreenShareRecoveryTrackLike(publication.track);
	if (track === null) return false;
	return isTrackReceivable(track, {allowMuted});
}

function firstFrameRecoveryKey(streamKey: string, generation: number): string {
	return `${streamKey}:${generation}`;
}

export function isScreenShareVideoSubscriptionRecoveryWanted(
	snapshot: VoiceMediaGraphSnapshot,
	streamKey: string | null | undefined,
): boolean {
	if (!streamKey) return false;
	if (!selectVoiceMediaGraphViewerStreamKeys(snapshot).includes(streamKey)) return false;
	const failure = selectVoiceMediaGraphFailure(snapshot, {streamKey});
	return failure == null || failure.code === ScreenShareWatchErrorCode.FirstFrameTimeout;
}

export function getScreenShareVideoSubscriptionRecoveryKey({
	trackSid,
	streamKey,
	participantIdentity,
}: {
	trackSid?: string | null;
	streamKey?: string | null;
	participantIdentity?: string | null;
}): string | null {
	return trackSid ?? streamKey ?? participantIdentity ?? null;
}

export function getScreenShareVideoSubscriptionRetryDelayMs(attempt: number): number {
	const retryIndex = Math.max(0, attempt - 1);
	return Math.min(
		SCREEN_SHARE_VIDEO_SUBSCRIPTION_RETRY_INITIAL_DELAY_MS * 2 ** retryIndex,
		SCREEN_SHARE_VIDEO_SUBSCRIPTION_RETRY_MAX_DELAY_MS,
	);
}

export function selectScreenShareVideoSubscriptionRecoveryMode(
	attempt: number,
): ScreenShareVideoSubscriptionRecoveryMode {
	return attempt <= SCREEN_SHARE_VIDEO_SUBSCRIPTION_REFRESH_ATTEMPTS ? 'refresh' : 'resubscribe';
}

export class ScreenShareVideoSubscriptionRecoveryCoordinator {
	private readonly sessions = new Map<string, ScreenShareVideoSubscriptionRecoverySession>();
	private readonly firstFrameRecoveriesByWatch = new Map<string, number>();
	private readonly scheduler: ScreenShareVideoSubscriptionRecoveryScheduler;
	private readonly graph: ScreenShareVideoSubscriptionRecoveryGraph;

	constructor(
		scheduler: ScreenShareVideoSubscriptionRecoveryScheduler = defaultScheduler,
		graph: ScreenShareVideoSubscriptionRecoveryGraph = voiceMediaGraphStore,
	) {
		this.scheduler = scheduler;
		this.graph = graph;
	}

	acquire(options: ScreenShareVideoSubscriptionRecoveryLeaseOptions): () => void {
		this.resetFirstFrameRecoveriesUnlessStalled(options.streamKey ?? null);
		let session = this.sessions.get(options.key);
		if (!session) {
			session = {
				key: options.key,
				leaseCount: 0,
				attempt: 0,
				timeoutId: null,
				graphGeneration: null,
				options,
			};
			this.sessions.set(options.key, session);
		}
		session.leaseCount += 1;
		session.options = options;
		this.ensureScheduled(session);
		let released = false;
		return () => {
			if (released) return;
			released = true;
			this.release(options.key);
		};
	}

	dispose(): void {
		for (const session of this.sessions.values()) {
			this.clearTimer(session);
		}
		this.sessions.clear();
	}

	getActiveSessionCount(): number {
		return this.sessions.size;
	}

	getFirstFrameRecoveryCount(streamKey: string | null | undefined, generation: number): number {
		if (!streamKey) return 0;
		return this.firstFrameRecoveriesByWatch.get(firstFrameRecoveryKey(streamKey, generation)) ?? 0;
	}

	hasFirstFrameRecoveryBudget(streamKey: string | null | undefined, generation: number): boolean {
		if (!streamKey) return false;
		return (
			this.getFirstFrameRecoveryCount(streamKey, generation) <
			SCREEN_SHARE_VIDEO_SUBSCRIPTION_FIRST_FRAME_RECOVERY_ATTEMPTS
		);
	}

	private release(key: string): void {
		const session = this.sessions.get(key);
		if (!session) return;
		session.leaseCount -= 1;
		if (session.leaseCount > 0) return;
		this.closeSession(session);
	}

	private closeSession(session: ScreenShareVideoSubscriptionRecoverySession): void {
		this.clearTimer(session);
		this.sessions.delete(session.key);
	}

	private clearTimer(session: ScreenShareVideoSubscriptionRecoverySession): void {
		if (session.timeoutId === null) return;
		this.scheduler.clearTimeout(session.timeoutId);
		session.timeoutId = null;
	}

	private ensureScheduled(session: ScreenShareVideoSubscriptionRecoverySession): void {
		if (session.timeoutId !== null) return;
		const allowMuted = this.firstFrameStalledStreamKey(session) === null;
		const delayMs = hasReceivableTrack(session.options.publication, {allowMuted})
			? SCREEN_SHARE_VIDEO_SUBSCRIPTION_HEALTH_CHECK_DELAY_MS
			: getScreenShareVideoSubscriptionRetryDelayMs(session.attempt + 1);
		this.schedule(session, delayMs);
	}

	private schedule(session: ScreenShareVideoSubscriptionRecoverySession, delayMs: number): void {
		session.graphGeneration = this.captureGraphGeneration(session.options.streamKey ?? null);
		session.timeoutId = this.scheduler.setTimeout(() => this.run(session.key), delayMs);
	}

	private watchGeneration(streamKey: string): number {
		return selectVoiceMediaGraphWatchGeneration(this.graph.getGraphSnapshot(), streamKey);
	}

	private captureGraphGeneration(streamKey: string | null): number | null {
		if (!streamKey) return null;
		return this.watchGeneration(streamKey);
	}

	private isGraphStateCurrent(session: ScreenShareVideoSubscriptionRecoverySession): boolean {
		const streamKey = session.options.streamKey ?? null;
		if (!streamKey) return true;
		const snapshot = this.graph.getGraphSnapshot();
		if (!selectVoiceMediaGraphViewerStreamKeys(snapshot).includes(streamKey)) return false;
		return selectVoiceMediaGraphWatchGeneration(snapshot, streamKey) === session.graphGeneration;
	}

	private hasFirstFrameTimeout(streamKey: string): boolean {
		const failure = selectVoiceMediaGraphFailure(this.graph.getGraphSnapshot(), {streamKey});
		return failure?.code === ScreenShareWatchErrorCode.FirstFrameTimeout;
	}

	private firstFrameStalledStreamKey(session: ScreenShareVideoSubscriptionRecoverySession): string | null {
		const streamKey = session.options.streamKey ?? null;
		if (!streamKey) return null;
		return this.hasFirstFrameTimeout(streamKey) ? streamKey : null;
	}

	private resetFirstFrameRecoveriesUnlessStalled(streamKey: string | null): void {
		if (!streamKey) return;
		if (this.hasFirstFrameTimeout(streamKey)) return;
		this.firstFrameRecoveriesByWatch.delete(firstFrameRecoveryKey(streamKey, this.watchGeneration(streamKey)));
	}

	private noteFirstFrameRecovery(streamKey: string, generation: number): void {
		const key = firstFrameRecoveryKey(streamKey, generation);
		const next = this.getFirstFrameRecoveryCount(streamKey, generation) + 1;
		this.firstFrameRecoveriesByWatch.delete(key);
		this.firstFrameRecoveriesByWatch.set(key, next);
		while (this.firstFrameRecoveriesByWatch.size > SCREEN_SHARE_VIDEO_SUBSCRIPTION_FIRST_FRAME_RECOVERY_RECORDS) {
			const oldest = this.firstFrameRecoveriesByWatch.keys().next().value;
			if (oldest === undefined) break;
			this.firstFrameRecoveriesByWatch.delete(oldest);
		}
	}

	private run(key: string): void {
		const session = this.sessions.get(key);
		if (!session) return;
		session.timeoutId = null;
		const {options} = session;
		if (session.leaseCount <= 0 || !options.isStillWanted() || !this.isGraphStateCurrent(session)) {
			this.closeSession(session);
			return;
		}
		const stalledStreamKey = this.firstFrameStalledStreamKey(session);
		if (stalledStreamKey === null) {
			if (hasReceivableTrack(options.publication, {allowMuted: true})) {
				session.attempt = 0;
				this.schedule(session, SCREEN_SHARE_VIDEO_SUBSCRIPTION_HEALTH_CHECK_DELAY_MS);
				return;
			}
		} else {
			const generation = this.watchGeneration(stalledStreamKey);
			if (!this.hasFirstFrameRecoveryBudget(stalledStreamKey, generation)) {
				this.closeSession(session);
				return;
			}
			this.noteFirstFrameRecovery(stalledStreamKey, generation);
		}
		session.attempt += 1;
		const mode = selectScreenShareVideoSubscriptionRecoveryMode(session.attempt);
		const delayMs = getScreenShareVideoSubscriptionRetryDelayMs(session.attempt);
		options.onRetry?.({
			participantIdentity: options.participantIdentity,
			trackSid: options.publication.trackSid ?? null,
			attempt: session.attempt,
			delayMs,
			mode,
		});
		this.recoverPublication(session, mode);
		this.schedule(
			session,
			stalledStreamKey !== null
				? SCREEN_SHARE_VIDEO_SUBSCRIPTION_FIRST_FRAME_RECOVERY_DELAY_MS
				: getScreenShareVideoSubscriptionRetryDelayMs(session.attempt + 1),
		);
	}

	private reportCommandFailed(
		options: ScreenShareVideoSubscriptionRecoveryLeaseOptions,
		operation: ScreenSharePublicationOperation,
	): void {
		const participantIdentity = options.participantIdentity;
		if (!participantIdentity) return;
		const failure = getScreenShareWatchFailureForPublicationOperation(operation);
		this.graph.transition({
			type: 'subscription.commandFailed',
			participantIdentity,
			source: VoiceTrackSource.ScreenShare,
			at: this.graph.nowMs(),
			code: failure.code,
			reason: failure.reason,
		});
	}

	private recoverPublication(
		session: ScreenShareVideoSubscriptionRecoverySession,
		mode: ScreenShareVideoSubscriptionRecoveryMode,
	): void {
		const {options} = session;
		if (options.recover) {
			options.recover(mode);
			return;
		}
		const label =
			mode === 'refresh' ? 'screen share video publication refresh' : 'screen share video publication retry';
		const onError: ScreenSharePublicationErrorHandler = (operation, errorLabel, error) => {
			this.reportCommandFailed(options, operation);
			options.onError?.(operation, errorLabel, error);
		};
		if (mode === 'refresh') {
			refreshScreenSharePublicationSubscription({
				publication: options.publication,
				label,
				shouldEnable: true,
				onError,
			});
			return;
		}
		resubscribeScreenSharePublication({
			publication: options.publication,
			label,
			shouldEnable: true,
			onError,
		});
	}
}

export const screenShareVideoSubscriptionRecoveryCoordinator = new ScreenShareVideoSubscriptionRecoveryCoordinator();
