// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RemoteTrackPublication} from 'livekit-client';
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
import type {ScreenSharePublicationOperation} from '../utils/ScreenShareSubscriptionPolicy';

export type {ScreenSharePublicationOperation};

const FAILURE_HISTORY_LIMIT = 16;
const HARVESTED_FAILURE_ID_LIMIT = 64;

export const ScreenShareWatchErrorCode = {
	SubscriptionSetSubscribedFailed: -2101,
	SubscriptionSetEnabledFailed: -2102,
	SubscriptionSetVideoQualityFailed: -2103,
	SubscriptionEmitTrackUpdateFailed: -2104,
	ObserverAttachFailed: -2105,
	ObserverDetachFailed: -2106,
	SubscriptionSetVideoDimensionsFailed: -2107,
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
	| 'subscription-set-video-dimensions-failed'
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

export interface ScreenShareWatchAttempt {
	attemptKey: string;
	startedAt: number;
	hasRenderedVideoFrame: boolean;
	generation: number;
}

export interface ScreenShareWatchFailureInboundSnapshot {
	packetsReceived: number | null;
	bytesReceived: number | null;
	framesReceived: number | null;
	framesDecoded: number | null;
	keyFramesDecoded: number | null;
	framesDropped: number | null;
	pliCount: number | null;
	firCount: number | null;
	nackCount: number | null;
	freezeCount: number | null;
	decoderImplementation: string | null;
	powerEfficientDecoder: boolean | null;
	mimeType: string | null;
	sdpFmtpLine: string | null;
	frameWidth: number | null;
	frameHeight: number | null;
}

export interface ScreenShareWatchFailureTileSnapshot {
	hasVideoElement: boolean;
	readyState: number | null;
	videoWidth: number | null;
	videoHeight: number | null;
	clientWidth: number | null;
	clientHeight: number | null;
	visibilityState: string | null;
}

export interface ScreenShareWatchFailureRecord {
	code: ScreenShareWatchErrorCode;
	reason: ScreenShareWatchFailureReason;
	reportedAt: number;
	streamKey: string | null;
	trackSid: string | null;
	participantIdentity: string | null;
	inbound: ScreenShareWatchFailureInboundSnapshot | null;
	tile: ScreenShareWatchFailureTileSnapshot | null;
}

export interface ScreenShareWatchVideoElementRef {
	current: HTMLVideoElement | null;
}

export interface ScreenShareWatchTarget {
	videoRef?: ScreenShareWatchVideoElementRef | null;
	publication?: RemoteTrackPublication | null;
}

interface InboundStatsEntry {
	type?: string;
	kind?: string;
	mediaType?: string;
	codecId?: string;
	packetsReceived?: number;
	bytesReceived?: number;
	framesReceived?: number;
	framesDecoded?: number;
	keyFramesDecoded?: number;
	framesDropped?: number;
	pliCount?: number;
	firCount?: number;
	nackCount?: number;
	freezeCount?: number;
	decoderImplementation?: string;
	powerEfficientDecoder?: boolean;
	frameWidth?: number;
	frameHeight?: number;
	mimeType?: string;
	sdpFmtpLine?: string;
}

function numberOrNull(value: number | undefined): number | null {
	return typeof value === 'number' && Number.isFinite(value) ? value : null;
}

function stringOrNull(value: string | undefined): string | null {
	return typeof value === 'string' && value !== '' ? value : null;
}

function findInboundVideoEntry(report: RTCStatsReport): InboundStatsEntry | null {
	for (const raw of report.values()) {
		const entry = raw as InboundStatsEntry;
		if (entry.type !== 'inbound-rtp') continue;
		const kind = entry.kind ?? entry.mediaType;
		if (kind !== 'video') continue;
		return entry;
	}
	return null;
}

function buildInboundSnapshot(report: RTCStatsReport): ScreenShareWatchFailureInboundSnapshot | null {
	const inbound = findInboundVideoEntry(report);
	if (!inbound) return null;
	const codec = inbound.codecId ? (report.get(inbound.codecId) as InboundStatsEntry | undefined) : undefined;
	return {
		packetsReceived: numberOrNull(inbound.packetsReceived),
		bytesReceived: numberOrNull(inbound.bytesReceived),
		framesReceived: numberOrNull(inbound.framesReceived),
		framesDecoded: numberOrNull(inbound.framesDecoded),
		keyFramesDecoded: numberOrNull(inbound.keyFramesDecoded),
		framesDropped: numberOrNull(inbound.framesDropped),
		pliCount: numberOrNull(inbound.pliCount),
		firCount: numberOrNull(inbound.firCount),
		nackCount: numberOrNull(inbound.nackCount),
		freezeCount: numberOrNull(inbound.freezeCount),
		decoderImplementation: stringOrNull(inbound.decoderImplementation),
		powerEfficientDecoder: typeof inbound.powerEfficientDecoder === 'boolean' ? inbound.powerEfficientDecoder : null,
		mimeType: stringOrNull(codec?.mimeType),
		sdpFmtpLine: stringOrNull(codec?.sdpFmtpLine),
		frameWidth: numberOrNull(inbound.frameWidth),
		frameHeight: numberOrNull(inbound.frameHeight),
	};
}

function buildTileSnapshot(videoRef: ScreenShareWatchVideoElementRef | null | undefined) {
	const video = videoRef?.current ?? null;
	const visibilityState = typeof document === 'undefined' ? null : document.visibilityState;
	if (!video) {
		return {
			hasVideoElement: false,
			readyState: null,
			videoWidth: null,
			videoHeight: null,
			clientWidth: null,
			clientHeight: null,
			visibilityState,
		};
	}
	return {
		hasVideoElement: true,
		readyState: numberOrNull(video.readyState),
		videoWidth: numberOrNull(video.videoWidth),
		videoHeight: numberOrNull(video.videoHeight),
		clientWidth: numberOrNull(video.clientWidth),
		clientHeight: numberOrNull(video.clientHeight),
		visibilityState,
	};
}

function readInboundStatsReport(publication: RemoteTrackPublication | null | undefined) {
	try {
		return publication?.videoTrack?.getRTCStatsReport?.();
	} catch {
		return undefined;
	}
}

function failureHistoryId(failure: ScreenShareWatchFailure): string {
	return `${failure.code}:${failure.streamKey ?? ''}:${failure.trackSid ?? ''}:${failure.reportedAt}`;
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
		case 'setVideoDimensions':
			return {
				code: ScreenShareWatchErrorCode.SubscriptionSetVideoDimensionsFailed,
				reason: 'subscription-set-video-dimensions-failed',
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
	private readonly failureHistory: Array<ScreenShareWatchFailureRecord> = [];
	private readonly harvestedFailureIds = new Set<string>();
	private readonly watchTargetsByStreamKey = new Map<string, ScreenShareWatchTarget>();
	private graphUnsubscribe: (() => void) | null = null;

	private get graph(): VoiceMediaGraphSnapshot<ScreenShareWatchFailure> {
		return voiceMediaGraphStore.graph as VoiceMediaGraphSnapshot<ScreenShareWatchFailure>;
	}

	private transition(event: VoiceMediaGraphEvent<ScreenShareWatchFailure>): void {
		voiceMediaGraphStore.transitionTypedFailure(event);
	}

	private rememberFailure(failure: ScreenShareWatchFailure): void {
		const id = failureHistoryId(failure);
		if (this.harvestedFailureIds.has(id)) return;
		if (this.harvestedFailureIds.size >= HARVESTED_FAILURE_ID_LIMIT) {
			const oldest = this.harvestedFailureIds.values().next().value;
			if (oldest !== undefined) this.harvestedFailureIds.delete(oldest);
		}
		this.harvestedFailureIds.add(id);
		const target = failure.streamKey ? this.watchTargetsByStreamKey.get(failure.streamKey) : undefined;
		const record: ScreenShareWatchFailureRecord = {
			code: failure.code,
			reason: failure.reason,
			reportedAt: failure.reportedAt,
			streamKey: failure.streamKey ?? null,
			trackSid: failure.trackSid ?? null,
			participantIdentity: failure.participantIdentity ?? null,
			inbound: null,
			tile: buildTileSnapshot(target?.videoRef),
		};
		if (this.failureHistory.length >= FAILURE_HISTORY_LIMIT) this.failureHistory.shift();
		this.failureHistory.push(record);
		void this.attachInboundSnapshot(record, target?.publication);
	}

	private async attachInboundSnapshot(
		record: ScreenShareWatchFailureRecord,
		publication: RemoteTrackPublication | null | undefined,
	): Promise<void> {
		try {
			const report = await readInboundStatsReport(publication);
			if (!report) return;
			record.inbound = buildInboundSnapshot(report);
		} catch {
			record.inbound = null;
		}
	}

	private harvestGraphFailures(): void {
		for (const failure of this.graph.failuresByKey.values()) {
			this.rememberFailure(failure);
		}
	}

	private ensureGraphSubscription(): void {
		if (this.graphUnsubscribe !== null) return;
		this.graphUnsubscribe = voiceMediaGraphStore.subscribe(() => {
			this.harvestGraphFailures();
		});
	}

	private releaseGraphSubscriptionIfIdle(): void {
		if (this.watchTargetsByStreamKey.size > 0) return;
		if (this.graphUnsubscribe === null) return;
		this.graphUnsubscribe();
		this.graphUnsubscribe = null;
	}

	getFailureHistory(): Array<ScreenShareWatchFailureRecord> {
		this.harvestGraphFailures();
		return this.failureHistory.map((record) => ({...record}));
	}

	markWatchStarted(streamKey: string): number {
		if (!streamKey) return 0;
		this.harvestGraphFailures();
		let nextGeneration = 0;
		runInAction(() => {
			this.transition({type: 'watch.started', streamKey, at: voiceMediaGraphStore.nowMs()});
			nextGeneration = selectVoiceMediaGraphWatchGeneration(this.graph, streamKey);
		});
		return nextGeneration;
	}

	markWatchStopped(streamKey: string): void {
		if (!streamKey) return;
		this.harvestGraphFailures();
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

	setWatchTarget(streamKey: string, watchTarget: ScreenShareWatchTarget): void {
		if (!streamKey) return;
		this.watchTargetsByStreamKey.set(streamKey, watchTarget);
		this.ensureGraphSubscription();
	}

	clearWatchTarget(streamKey: string): void {
		if (!streamKey) return;
		this.harvestGraphFailures();
		this.watchTargetsByStreamKey.delete(streamKey);
		this.releaseGraphSubscriptionIfIdle();
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
		this.harvestGraphFailures();
		runInAction(() => {
			this.transition({type: 'watch.attemptReleased', streamKey: target.streamKey, attemptKey});
		});
	}

	markRenderedVideoFrame(target: ScreenShareWatchFailureTarget & {streamKey: string}, attemptKey: string): void {
		this.harvestGraphFailures();
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
		this.rememberFailure(normalizedFailure);
		return normalizedFailure;
	}

	hasFailureForStreamKey(streamKey: string | null | undefined): boolean {
		return selectVoiceMediaGraphHasFailureForStreamKey(this.graph, streamKey);
	}

	getFailure(target: ScreenShareWatchFailureTarget): ScreenShareWatchFailure | null {
		return selectVoiceMediaGraphFailure(this.graph, target);
	}

	clearFailure(target: ScreenShareWatchFailureTarget): void {
		this.harvestGraphFailures();
		runInAction(() => {
			this.transition({type: 'failure.cleared', target});
		});
	}

	clearAll(): void {
		this.harvestGraphFailures();
		runInAction(() => {
			this.transition({type: 'failureWatch.clearAll'});
		});
	}
}

export const ScreenShareWatchFailures = new ScreenShareWatchFailuresStore();

export default ScreenShareWatchFailures;
