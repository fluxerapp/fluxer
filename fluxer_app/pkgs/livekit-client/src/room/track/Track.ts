// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {type AudioTrackFeature, StreamState as ProtoStreamState, TrackSource, TrackType} from '@livekit/protocol';
import {EventEmitter} from 'events';
import type TypedEventEmitter from 'typed-emitter';
import type {SignalClient} from '../../api/SignalClient.ts';
import log, {getLogger, LoggerNames, type StructuredLogger} from '../../logger.ts';
import type {NonSharedUint8Array} from '../../type-polyfills/non-shared-typed-arrays.ts';
import {TrackEvent} from '../events.ts';
import {summarizeStatsReport} from '../statsSummary.ts';
import CriticalTimers, {type TimerHandle} from '../timers.ts';
import type {LoggerOptions} from '../types.ts';
import {isFireFox, isSafari, isWeb} from '../utils.ts';
import type {TrackProcessor} from './processor/types.ts';
import {getLogContextFromTrack} from './utils.ts';

const BACKGROUND_REACTION_DELAY = 5000;

const recycledElements: Array<HTMLAudioElement> = [];

type TrackEventHandler<TArgs extends Array<unknown> = []> = {
	bivarianceHack(...args: TArgs): void;
}['bivarianceHack'];

export type TrackProcessorEventValue = TrackProcessor<Track.Kind, never>;

export enum VideoQuality {
	LOW = 0,
	MEDIUM = 1,
	HIGH = 2,
}
export abstract class Track<
	TrackKind extends Track.Kind = Track.Kind,
> extends (EventEmitter as new () => TypedEventEmitter<TrackEventCallbacks>) {
	readonly kind: TrackKind;

	attachedElements: Array<HTMLMediaElement> = [];

	isMuted: boolean = false;

	source: Track.Source;

	private _streamState: Track.StreamState = Track.StreamState.Active;

	sid?: Track.SID;

	mediaStream?: MediaStream;

	get streamState(): Track.StreamState {
		return this._streamState;
	}

	setStreamState(value: Track.StreamState) {
		if (this._streamState !== value) {
			this.log.debug(`stream state changed: ${this._streamState} -> ${value}`);
		}
		this._streamState = value;
	}

	rtpTimestamp: number | undefined;

	protected _mediaStreamTrack: MediaStreamTrack;

	protected _mediaStreamID: string;

	protected isInBackground: boolean = false;

	private backgroundTimeout: TimerHandle | undefined;

	private loggerContextCb: LoggerOptions['loggerContextCb'];

	protected timeSyncHandle: number | undefined;

	protected _currentBitrate: number = 0;
	protected monitorInterval?: TimerHandle;
	protected monitorInFlight: boolean = false;
	private finalStatsLogged = false;

	protected log: StructuredLogger = log;

	protected constructor(mediaTrack: MediaStreamTrack, kind: TrackKind, loggerOptions: LoggerOptions = {}) {
		super();
		this.loggerContextCb = loggerOptions.loggerContextCb;
		this.log = getLogger(loggerOptions.loggerName ?? LoggerNames.Track, () => this.logContext);

		this.setMaxListeners(100);
		this.kind = kind;
		this._mediaStreamTrack = mediaTrack;
		this._mediaStreamID = mediaTrack.id;
		this.source = Track.Source.Unknown;
	}

	protected get logContext() {
		return {
			...this.loggerContextCb?.(),
			...getLogContextFromTrack(this),
		};
	}

	get currentBitrate(): number {
		return this._currentBitrate;
	}

	get mediaStreamTrack() {
		return this._mediaStreamTrack;
	}

	abstract get isLocal(): boolean;

	get mediaStreamID(): string {
		return this._mediaStreamID;
	}

	attach(): HTMLMediaElement;

	attach(element: HTMLMediaElement): HTMLMediaElement;
	attach(element?: HTMLMediaElement): HTMLMediaElement {
		let elementType = 'audio';
		if (this.kind === Track.Kind.Video) {
			elementType = 'video';
		}
		if (this.attachedElements.length === 0 && this.kind === Track.Kind.Video) {
			this.addAppVisibilityListener();
		}
		if (!element) {
			if (elementType === 'audio') {
				recycledElements.forEach((e) => {
					if (e.parentElement === null && !element) {
						element = e;
					}
				});
				if (element) {
					recycledElements.splice(recycledElements.indexOf(element), 1);
				}
			}
			if (!element) {
				element = <HTMLMediaElement>document.createElement(elementType);
			}
		}

		if (!this.attachedElements.includes(element)) {
			this.attachedElements.push(element);
		}

		attachToElement(this.mediaStreamTrack, element);

		const allMediaStreamTracks = (element.srcObject as MediaStream).getTracks();
		const hasAudio = allMediaStreamTracks.some((tr) => tr.kind === 'audio');

		element
			.play()
			.then(() => {
				this.emit(hasAudio ? TrackEvent.AudioPlaybackStarted : TrackEvent.VideoPlaybackStarted);
			})
			.catch((e) => {
				if (e.name === 'NotAllowedError') {
					this.emit(hasAudio ? TrackEvent.AudioPlaybackFailed : TrackEvent.VideoPlaybackFailed, e);
				} else if (e.name === 'AbortError') {
					this.log.debug(`${hasAudio ? 'audio' : 'video'} playback aborted, likely due to new play request`);
				} else {
					this.log.warn(`could not playback ${hasAudio ? 'audio' : 'video'}`, {error: e});
				}
				if (
					hasAudio &&
					element &&
					allMediaStreamTracks.some((tr) => tr.kind === 'video') &&
					e.name === 'NotAllowedError'
				) {
					element.muted = true;
					element.play().catch(() => {});
				}
			});

		this.emit(TrackEvent.ElementAttached, element);
		return element;
	}

	detach(): Array<HTMLMediaElement>;

	detach(element: HTMLMediaElement): HTMLMediaElement;
	detach(element?: HTMLMediaElement): HTMLMediaElement | Array<HTMLMediaElement> {
		try {
			if (element) {
				detachTrack(this.mediaStreamTrack, element);
				const idx = this.attachedElements.indexOf(element);
				if (idx >= 0) {
					this.attachedElements.splice(idx, 1);
					this.recycleElement(element);
					this.emit(TrackEvent.ElementDetached, element);
				}
				return element;
			}

			const detached: Array<HTMLMediaElement> = [];
			this.attachedElements.forEach((elm) => {
				detachTrack(this.mediaStreamTrack, elm);
				detached.push(elm);
				this.recycleElement(elm);
				this.emit(TrackEvent.ElementDetached, elm);
			});

			this.attachedElements = [];
			return detached;
		} finally {
			if (this.attachedElements.length === 0) {
				this.removeAppVisibilityListener();
			}
		}
	}

	stop() {
		this.log.debug('stopping track');
		this.stopMonitor();
		this._mediaStreamTrack.stop();
	}

	protected enable() {
		this._mediaStreamTrack.enabled = true;
	}

	protected disable() {
		this._mediaStreamTrack.enabled = false;
	}

	abstract startMonitor(signalClient?: SignalClient): void;
	protected runMonitor(monitor: () => void | Promise<void>): void {
		if (this.monitorInFlight) return;
		this.monitorInFlight = true;
		void Promise.resolve()
			.then(monitor)
			.finally(() => {
				this.monitorInFlight = false;
			});
	}
	abstract getRTCStatsReport(): Promise<RTCStatsReport | undefined>;

	stopMonitor() {
		if (this.monitorInterval) {
			CriticalTimers.clearInterval(this.monitorInterval);
			this.monitorInterval = undefined;
		}
		this.monitorInFlight = false;
		if (this.timeSyncHandle !== undefined) {
			cancelAnimationFrame(this.timeSyncHandle);
			this.timeSyncHandle = undefined;
		}
		this.logFinalStats();
	}

	private logFinalStats() {
		if (this.finalStatsLogged) {
			return;
		}
		this.finalStatsLogged = true;
		this.getRTCStatsReport()
			.then((report) => {
				if (report) {
					this.log.info('final track stats', summarizeStatsReport(report));
				}
			})
			.catch((error) => this.log.debug('could not collect final track stats', {error}));
	}

	updateLoggerOptions(loggerOptions: LoggerOptions) {
		if (loggerOptions.loggerContextCb) {
			this.loggerContextCb = loggerOptions.loggerContextCb;
		}
		if (loggerOptions.loggerName) {
			this.log = getLogger(loggerOptions.loggerName, () => this.logContext);
		}
	}

	private recycleElement(element: HTMLMediaElement) {
		if (element instanceof HTMLAudioElement) {
			let shouldCache = true;
			element.pause();
			recycledElements.forEach((e) => {
				if (!e.parentElement) {
					shouldCache = false;
				}
			});
			if (shouldCache) {
				recycledElements.push(element);
			}
		}
	}

	protected appVisibilityChangedListener = () => {
		if (this.backgroundTimeout) {
			CriticalTimers.clearTimeout(this.backgroundTimeout);
		}
		if (document.visibilityState === 'hidden') {
			this.backgroundTimeout = CriticalTimers.setTimeout(
				() => this.handleAppVisibilityChanged(),
				BACKGROUND_REACTION_DELAY,
			);
		} else {
			this.handleAppVisibilityChanged();
		}
	};

	protected async handleAppVisibilityChanged() {
		this.isInBackground = document.visibilityState === 'hidden';
		if (!this.isInBackground && this.kind === Track.Kind.Video) {
			setTimeout(() => this.attachedElements.forEach((el) => el.play().catch(() => {})), 0);
		}
	}

	protected addAppVisibilityListener() {
		if (isWeb()) {
			this.isInBackground = document.visibilityState === 'hidden';
			document.addEventListener('visibilitychange', this.appVisibilityChangedListener);
		} else {
			this.isInBackground = false;
		}
	}

	protected removeAppVisibilityListener() {
		if (isWeb()) {
			document.removeEventListener('visibilitychange', this.appVisibilityChangedListener);
		}
	}
}

export function attachToElement(track: MediaStreamTrack, element: HTMLMediaElement) {
	let mediaStream: MediaStream;
	if (element.srcObject instanceof MediaStream) {
		mediaStream = element.srcObject;
	} else {
		mediaStream = new MediaStream();
	}

	let existingTracks: Array<MediaStreamTrack>;
	if (track.kind === 'audio') {
		existingTracks = mediaStream.getAudioTracks();
	} else {
		existingTracks = mediaStream.getVideoTracks();
	}
	if (!existingTracks.includes(track)) {
		existingTracks.forEach((et) => {
			mediaStream.removeTrack(et);
		});
		mediaStream.addTrack(track);
	}

	if (!isSafari() || !(element instanceof HTMLVideoElement)) {
		element.autoplay = true;
	}
	element.muted = mediaStream.getAudioTracks().length === 0;
	if (element instanceof HTMLVideoElement) {
		element.playsInline = true;
	}

	if (element.srcObject !== mediaStream) {
		element.srcObject = mediaStream;
		if ((isSafari() || isFireFox()) && element instanceof HTMLVideoElement) {
			setTimeout(() => {
				element.srcObject = mediaStream;
				element.play().catch(() => {});
			}, 0);
		}
	}
}

export function detachTrack(track: MediaStreamTrack, element: HTMLMediaElement) {
	if (element.srcObject instanceof MediaStream) {
		const mediaStream = element.srcObject;
		mediaStream.removeTrack(track);
		if (mediaStream.getTracks().length > 0) {
			element.srcObject = mediaStream;
		} else {
			element.srcObject = null;
		}
	}
}

export namespace Track {
	export enum Kind {
		Audio = 'audio',
		Video = 'video',
		Unknown = 'unknown',
	}
	export type SID = string;
	export enum Source {
		Camera = 'camera',
		Microphone = 'microphone',
		ScreenShare = 'screen_share',
		ScreenShareAudio = 'screen_share_audio',
		Unknown = 'unknown',
	}

	export enum StreamState {
		Active = 'active',
		Paused = 'paused',
		Unknown = 'unknown',
	}

	export interface Dimensions {
		width: number;
		height: number;
	}

	export function kindToProto(k: Kind): TrackType {
		switch (k) {
			case Kind.Audio:
				return TrackType.AUDIO;
			case Kind.Video:
				return TrackType.VIDEO;
			default:
				return TrackType.DATA;
		}
	}

	export function kindFromProto(t: TrackType): Kind | undefined {
		switch (t) {
			case TrackType.AUDIO:
				return Kind.Audio;
			case TrackType.VIDEO:
				return Kind.Video;
			default:
				return Kind.Unknown;
		}
	}

	export function sourceToProto(s: Source): TrackSource {
		switch (s) {
			case Source.Camera:
				return TrackSource.CAMERA;
			case Source.Microphone:
				return TrackSource.MICROPHONE;
			case Source.ScreenShare:
				return TrackSource.SCREEN_SHARE;
			case Source.ScreenShareAudio:
				return TrackSource.SCREEN_SHARE_AUDIO;
			default:
				return TrackSource.UNKNOWN;
		}
	}

	export function sourceFromProto(s: TrackSource): Source {
		switch (s) {
			case TrackSource.CAMERA:
				return Source.Camera;
			case TrackSource.MICROPHONE:
				return Source.Microphone;
			case TrackSource.SCREEN_SHARE:
				return Source.ScreenShare;
			case TrackSource.SCREEN_SHARE_AUDIO:
				return Source.ScreenShareAudio;
			default:
				return Source.Unknown;
		}
	}

	export function streamStateFromProto(s: ProtoStreamState): StreamState {
		switch (s) {
			case ProtoStreamState.ACTIVE:
				return StreamState.Active;
			case ProtoStreamState.PAUSED:
				return StreamState.Paused;
			default:
				return StreamState.Unknown;
		}
	}
}

export type TrackEventCallbacks = {
	message: TrackEventHandler;
	muted: TrackEventHandler<[track?: unknown]>;
	unmuted: TrackEventHandler<[track?: unknown]>;
	restarted: TrackEventHandler<[track?: unknown]>;
	ended: TrackEventHandler<[track?: unknown]>;
	updateSettings: TrackEventHandler;
	updateSubscription: TrackEventHandler;
	audioPlaybackStarted: TrackEventHandler;
	audioPlaybackFailed: TrackEventHandler<[error?: Error]>;
	audioSilenceDetected: TrackEventHandler;
	visibilityChanged: TrackEventHandler<[visible: boolean, track?: unknown]>;
	videoDimensionsChanged: TrackEventHandler<[dimensions: Track.Dimensions, track?: unknown]>;
	videoPlaybackStarted: TrackEventHandler;
	videoPlaybackFailed: TrackEventHandler<[error?: Error]>;
	elementAttached: TrackEventHandler<[element: HTMLMediaElement]>;
	elementDetached: TrackEventHandler<[element: HTMLMediaElement]>;
	upstreamPaused: TrackEventHandler<[track: unknown]>;
	upstreamResumed: TrackEventHandler<[track: unknown]>;
	trackProcessorUpdate: TrackEventHandler<[processor?: TrackProcessorEventValue]>;
	audioTrackFeatureUpdate: TrackEventHandler<[track: unknown, feature: AudioTrackFeature, enabled: boolean]>;
	timeSyncUpdate: TrackEventHandler<[update: {timestamp: number; rtpTimestamp: number}]>;
	preConnectBufferFlushed: TrackEventHandler<[buffer: Array<NonSharedUint8Array>]>;
	cpuConstrained: TrackEventHandler;
};
