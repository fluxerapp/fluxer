// SPDX-License-Identifier: AGPL-3.0-or-later

import {runInAction} from 'mobx';
import {
	selectVoiceMediaGraphAttempt,
	selectVoiceMediaGraphFailure,
	selectVoiceMediaGraphHasFailureForStreamKey,
	selectVoiceMediaGraphWatchGeneration,
	VOICE_MEDIA_GRAPH_SCREEN_SHARE_SOURCE,
	type VoiceMediaGraphEvent,
	type VoiceMediaGraphSnapshot,
} from '../engine/VoiceMediaGraph';
import {voiceMediaGraphStore} from '../engine/VoiceMediaGraphStore';

export const ScreenShareWatchErrorCode = {
	SubscriptionSetSubscribedFailed: -2101,
	SubscriptionSetEnabledFailed: -2102,
	SubscriptionSetVideoQualityFailed: -2103,
	SubscriptionEmitTrackUpdateFailed: -2104,
	ObserverAttachFailed: -2105,
	ObserverDetachFailed: -2106,
	RemoteTrackSubscriptionFailed: -2202,
	PublicationMissingTimeout: -2301,
	SubscriptionAttachTimeout: -2302,
	FirstFrameTimeout: -2303,
	RepublishTimeout: -2304,
	NativeInboundBridgeUnavailable: -2401,
} as const;

export type ScreenShareWatchErrorCode = (typeof ScreenShareWatchErrorCode)[keyof typeof ScreenShareWatchErrorCode];

export type ScreenShareWatchFailureReason =
	| 'subscription-set-subscribed-failed'
	| 'subscription-set-enabled-failed'
	| 'subscription-set-video-quality-failed'
	| 'subscription-emit-track-update-failed'
	| 'observer-attach-failed'
	| 'observer-detach-failed'
	| 'remote-track-subscription-failed'
	| 'publication-missing-timeout'
	| 'subscription-attach-timeout'
	| 'first-frame-timeout'
	| 'republish-timeout'
	| 'native-inbound-bridge-unavailable';

export interface ScreenShareWatchFailure {
	code: ScreenShareWatchErrorCode;
	reason: ScreenShareWatchFailureReason;
	reportedAt: number;
	streamKey?: string;
	participantIdentity?: string;
	participantSid?: string;
	trackSid?: string;
	source?: string;
	error?: unknown;
	generation?: number;
}

export interface ScreenShareWatchFailureTarget {
	streamKey?: string | null;
	participantIdentity?: string | null;
	participantSid?: string | null;
	trackSid?: string | null;
	source?: string | null;
}

export type ScreenSharePublicationOperation = 'setEnabled' | 'setSubscribed' | 'setVideoQuality' | 'emitTrackUpdate';

export interface ScreenShareWatchAttempt {
	attemptKey: string;
	startedAt: number;
	hasRenderedVideoFrame: boolean;
	generation: number;
}

export function getScreenShareWatchFailureForPublicationOperation(
	operation: ScreenSharePublicationOperation,
): Pick<ScreenShareWatchFailure, 'code' | 'reason'> {
	switch (operation) {
		case 'setSubscribed':
			return {
				code: ScreenShareWatchErrorCode.SubscriptionSetSubscribedFailed,
				reason: 'subscription-set-subscribed-failed',
			};
		case 'setEnabled':
			return {
				code: ScreenShareWatchErrorCode.SubscriptionSetEnabledFailed,
				reason: 'subscription-set-enabled-failed',
			};
		case 'setVideoQuality':
			return {
				code: ScreenShareWatchErrorCode.SubscriptionSetVideoQualityFailed,
				reason: 'subscription-set-video-quality-failed',
			};
		case 'emitTrackUpdate':
			return {
				code: ScreenShareWatchErrorCode.SubscriptionEmitTrackUpdateFailed,
				reason: 'subscription-emit-track-update-failed',
			};
	}
}

export function selectScreenShareWatchTimeoutFailureCode({
	hasPublication,
	isPublicationDesired,
	hasSubscribedVideo,
	isRepublishBuffering = false,
}: {
	hasPublication: boolean;
	isPublicationDesired: boolean;
	hasSubscribedVideo: boolean;
	isRepublishBuffering?: boolean;
}): Pick<ScreenShareWatchFailure, 'code' | 'reason'> {
	if (isRepublishBuffering) {
		return {
			code: ScreenShareWatchErrorCode.RepublishTimeout,
			reason: 'republish-timeout',
		};
	}
	if (!hasPublication) {
		return {
			code: ScreenShareWatchErrorCode.PublicationMissingTimeout,
			reason: 'publication-missing-timeout',
		};
	}
	if (!isPublicationDesired || !hasSubscribedVideo) {
		return {
			code: ScreenShareWatchErrorCode.SubscriptionAttachTimeout,
			reason: 'subscription-attach-timeout',
		};
	}
	return {
		code: ScreenShareWatchErrorCode.FirstFrameTimeout,
		reason: 'first-frame-timeout',
	};
}

class ScreenShareWatchFailuresStore {
	private get graph(): VoiceMediaGraphSnapshot<ScreenShareWatchFailure> {
		return voiceMediaGraphStore.graph as VoiceMediaGraphSnapshot<ScreenShareWatchFailure>;
	}

	private transition(event: VoiceMediaGraphEvent<ScreenShareWatchFailure>): void {
		voiceMediaGraphStore.transitionTypedFailure(event);
	}

	markWatchStarted(streamKey: string): number {
		if (!streamKey) return 0;
		let nextGeneration = 0;
		runInAction(() => {
			this.transition({type: 'watch.started', streamKey, at: voiceMediaGraphStore.nowMs()});
			nextGeneration = selectVoiceMediaGraphWatchGeneration(this.graph, streamKey);
		});
		return nextGeneration;
	}

	markWatchStopped(streamKey: string): void {
		if (!streamKey) return;
		runInAction(() => {
			this.transition({type: 'watch.stopped', streamKey});
		});
	}

	getWatchGeneration(streamKey: string): number {
		return selectVoiceMediaGraphWatchGeneration(this.graph, streamKey);
	}

	getAttempt(streamKey: string): ScreenShareWatchAttempt | null {
		return selectVoiceMediaGraphAttempt(this.graph, streamKey);
	}

	ensureAttempt(
		target: ScreenShareWatchFailureTarget & {streamKey: string},
		attemptKey: string,
	): ScreenShareWatchAttempt {
		const generation = selectVoiceMediaGraphWatchGeneration(this.graph, target.streamKey);
		runInAction(() => {
			this.transition({
				type: 'watch.attemptEnsured',
				streamKey: target.streamKey,
				attemptKey,
				startedAt: voiceMediaGraphStore.nowMs(),
				generation,
			});
		});
		return selectVoiceMediaGraphAttempt(this.graph, target.streamKey)!;
	}

	releaseAttempt(target: ScreenShareWatchFailureTarget & {streamKey: string}, attemptKey: string): void {
		if (!target.streamKey || !attemptKey) return;
		runInAction(() => {
			this.transition({type: 'watch.attemptReleased', streamKey: target.streamKey, attemptKey});
		});
	}

	markRenderedVideoFrame(target: ScreenShareWatchFailureTarget & {streamKey: string}, attemptKey: string): void {
		const existingAttempt = selectVoiceMediaGraphAttempt(this.graph, target.streamKey);
		if (existingAttempt && existingAttempt.attemptKey !== attemptKey) return;
		const generation =
			existingAttempt?.generation ?? selectVoiceMediaGraphWatchGeneration(this.graph, target.streamKey);
		runInAction(() => {
			this.transition({
				type: 'watch.renderedFrame',
				streamKey: target.streamKey,
				attemptKey,
				renderedAt: voiceMediaGraphStore.nowMs(),
				generation,
			});
		});
	}

	reportFailure(failure: Omit<ScreenShareWatchFailure, 'reportedAt'>): ScreenShareWatchFailure {
		const normalizedFailure: ScreenShareWatchFailure = {
			...failure,
			source: failure.source ?? VOICE_MEDIA_GRAPH_SCREEN_SHARE_SOURCE,
			reportedAt: voiceMediaGraphStore.nowMs(),
		};
		runInAction(() => {
			this.transition({type: 'failure.reported', failure: normalizedFailure, generation: normalizedFailure.generation});
		});
		return normalizedFailure;
	}

	hasFailureForStreamKey(streamKey: string | null | undefined): boolean {
		return selectVoiceMediaGraphHasFailureForStreamKey(this.graph, streamKey);
	}

	getFailure(target: ScreenShareWatchFailureTarget): ScreenShareWatchFailure | null {
		return selectVoiceMediaGraphFailure(this.graph, target);
	}

	clearFailure(target: ScreenShareWatchFailureTarget): void {
		runInAction(() => {
			this.transition({type: 'failure.cleared', target});
		});
	}

	clearAll(): void {
		runInAction(() => {
			this.transition({type: 'failureWatch.clearAll'});
		});
	}
}

export const ScreenShareWatchFailures = new ScreenShareWatchFailuresStore();

export default ScreenShareWatchFailures;
