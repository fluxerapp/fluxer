// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import {
	VideoQuality as ProtoVideoQuality,
	type SubscribedCodec,
	SubscribedQuality,
	VideoLayer,
} from '@livekit/protocol';
import type {SignalClient} from '../../api/SignalClient.ts';
import type {StructuredLogger} from '../../logger.ts';
import {TrackEvent} from '../events.ts';
import {ScalabilityMode} from '../participant/publishUtils.ts';
import type {VideoSenderStats} from '../stats.ts';
import {computeBitrate, monitorFrequency} from '../stats.ts';
import type {LoggerOptions} from '../types.ts';
import {isFireFox, isMobile, isSVCCodec, isWeb} from '../utils.ts';
import LocalTrack from './LocalTrack.ts';
import type {VideoCaptureOptions, VideoCodec} from './options.ts';
import type {TrackProcessor} from './processor/types.ts';
import {Track, VideoQuality} from './Track.ts';
import {constraintsForOptions} from './utils.ts';

export class SimulcastTrackInfo {
	codec: VideoCodec;

	mediaStreamTrack: MediaStreamTrack;

	sender?: RTCRtpSender;

	encodings?: Array<RTCRtpEncodingParameters>;

	constructor(codec: VideoCodec, mediaStreamTrack: MediaStreamTrack) {
		this.codec = codec;
		this.mediaStreamTrack = mediaStreamTrack;
	}
}

const refreshSubscribedCodecAfterNewCodec = 5000;

function restoreSecondarySenderTrack(
	sender: RTCRtpSender | undefined,
	track: MediaStreamTrack | null,
): Promise<void> | undefined {
	if (!sender) return undefined;
	if (sender.track === track) return undefined;
	if (track != null && track.readyState !== 'live') return undefined;
	return sender.replaceTrack(track);
}

function createProcessorRecoveryError(
	primaryError: unknown,
	cleanupErrors: ReadonlyArray<unknown>,
	rollbackErrors: ReadonlyArray<unknown>,
): AggregateError {
	const recoveryErrors: Array<AggregateError> = [];
	if (cleanupErrors.length > 0) {
		recoveryErrors.push(new AggregateError(cleanupErrors, 'Video processor candidate cleanup failed'));
	}
	if (rollbackErrors.length > 0) {
		recoveryErrors.push(new AggregateError(rollbackErrors, 'Video processor secondary sender rollback failed'));
	}
	return new AggregateError(recoveryErrors, 'Video processor apply failed and recovery was incomplete', {
		cause: primaryError,
	});
}

export default class LocalVideoTrack extends LocalTrack<Track.Kind.Video> {
	signalClient?: SignalClient;

	private prevStats?: Map<string, VideoSenderStats>;

	private encodings?: Array<RTCRtpEncodingParameters>;

	simulcastCodecs: Map<VideoCodec, SimulcastTrackInfo> = new Map<VideoCodec, SimulcastTrackInfo>();

	private subscribedCodecs?: Array<SubscribedCodec>;

	private senderLock: Mutex;

	private degradationPreference: RTCDegradationPreference = 'balanced';

	private isCpuConstrained: boolean = false;

	private optimizeForPerformance: boolean = false;

	override get sender(): RTCRtpSender | undefined {
		return this._sender;
	}

	override set sender(sender: RTCRtpSender | undefined) {
		this._sender = sender;
		if (this.degradationPreference) {
			this.setDegradationPreference(this.degradationPreference);
		}
	}

	constructor(
		mediaTrack: MediaStreamTrack,
		constraints?: MediaTrackConstraints,
		userProvidedTrack = true,
		loggerOptions?: LoggerOptions,
	) {
		super(mediaTrack, Track.Kind.Video, constraints, userProvidedTrack, loggerOptions);
		this.senderLock = new Mutex();
	}

	get isSimulcast(): boolean {
		if (this.sender && this.sender.getParameters().encodings.length > 1) {
			return true;
		}
		return false;
	}

	startMonitor(signalClient: SignalClient) {
		this.signalClient = signalClient;
		if (!isWeb()) {
			return;
		}
		const params = this.sender?.getParameters();
		if (params) {
			this.encodings = params.encodings;
		}

		if (this.monitorInterval) {
			return;
		}
		this.monitorInterval = setInterval(() => {
			this.runMonitor(this.monitorSender);
		}, monitorFrequency);
	}

	override stop() {
		this._mediaStreamTrack.getConstraints();
		this.simulcastCodecs.forEach((trackInfo) => {
			trackInfo.mediaStreamTrack.stop();
		});
		super.stop();
	}

	override async pauseUpstream() {
		await super.pauseUpstream();
		for await (const sc of this.simulcastCodecs.values()) {
			await sc.sender?.replaceTrack(null);
		}
	}

	override async resumeUpstream() {
		await super.resumeUpstream();
		for await (const sc of this.simulcastCodecs.values()) {
			await sc.sender?.replaceTrack(sc.mediaStreamTrack);
		}
	}

	override async mute(): Promise<typeof this> {
		const unlock = await this.muteLock.lock();
		try {
			if (this.isMuted) {
				this.log.debug('Track already muted', this.logContext);
				return this;
			}

			if (this.source === Track.Source.Camera && !this.isUserProvided) {
				this.log.debug('stopping camera track', this.logContext);
				this._mediaStreamTrack.stop();
			}
			await super.mute();
			return this;
		} finally {
			unlock();
		}
	}

	override async unmute(): Promise<typeof this> {
		const unlock = await this.muteLock.lock();
		try {
			if (!this.isMuted) {
				this.log.debug('Track already unmuted', this.logContext);
				return this;
			}

			if (this.source === Track.Source.Camera && !this.isUserProvided) {
				this.log.debug('reacquiring camera track', this.logContext);
				await this.restartTrack();
			}
			await super.unmute();
			return this;
		} finally {
			unlock();
		}
	}

	protected override setTrackMuted(muted: boolean) {
		super.setTrackMuted(muted);
		for (const sc of this.simulcastCodecs.values()) {
			sc.mediaStreamTrack.enabled = !muted;
		}
	}

	async getSenderStats(): Promise<Array<VideoSenderStats>> {
		if (!this.sender?.getStats) {
			return [];
		}

		const items: Array<VideoSenderStats> = [];

		const stats = await this.sender.getStats();
		stats.forEach((v) => {
			if (v.type === 'outbound-rtp') {
				const vs: VideoSenderStats = {
					type: 'video',
					streamId: v.id,
					frameHeight: v.frameHeight,
					frameWidth: v.frameWidth,
					framesPerSecond: v.framesPerSecond,
					framesSent: v.framesSent,
					firCount: v.firCount,
					pliCount: v.pliCount,
					nackCount: v.nackCount,
					packetsSent: v.packetsSent,
					bytesSent: v.bytesSent,
					qualityLimitationReason: v.qualityLimitationReason,
					qualityLimitationDurations: v.qualityLimitationDurations,
					qualityLimitationResolutionChanges: v.qualityLimitationResolutionChanges,
					encoderImplementation: v.encoderImplementation,
					powerEfficientEncoder: v.powerEfficientEncoder,
					rid: v.rid ?? v.id,
					retransmittedPacketsSent: v.retransmittedPacketsSent,
					targetBitrate: v.targetBitrate,
					timestamp: v.timestamp,
				};

				const r = stats.get(v.remoteId) as
					| (RTCStats & {
							jitter?: number;
							packetsLost?: number;
							roundTripTime?: number;
					  })
					| undefined;
				if (r) {
					vs.jitter = r.jitter;
					vs.packetsLost = r.packetsLost;
					vs.roundTripTime = r.roundTripTime;
				}

				items.push(vs);
			}
		});

		items.sort((a, b) => (b.frameWidth ?? 0) - (a.frameWidth ?? 0));
		return items;
	}

	setPublishingQuality(maxQuality: VideoQuality) {
		const qualities: Array<SubscribedQuality> = [];
		for (let q = VideoQuality.LOW; q <= VideoQuality.HIGH; q += 1) {
			qualities.push(
				new SubscribedQuality({
					quality: q,
					enabled: q <= maxQuality,
				}),
			);
		}
		this.log.debug(`setting publishing quality. max quality ${maxQuality}`, this.logContext);
		this.setPublishingLayers(isSVCCodec(this.codec), qualities);
	}

	async restartTrack(options?: VideoCaptureOptions) {
		let constraints: MediaTrackConstraints | undefined;
		if (options) {
			const streamConstraints = constraintsForOptions({video: options});
			if (typeof streamConstraints.video !== 'boolean') {
				constraints = streamConstraints.video;
			}
		}
		await this.restart(constraints);

		this.isCpuConstrained = false;

		const processedTrack = this.processor?.processedTrack;
		for await (const sc of this.simulcastCodecs.values()) {
			if (sc.sender && sc.sender.transport?.state !== 'closed') {
				const previousTrack = sc.mediaStreamTrack;
				const nextTrack = this._mediaStreamTrack.clone();
				try {
					await sc.sender.replaceTrack(processedTrack ?? nextTrack);
					sc.mediaStreamTrack = nextTrack;
					previousTrack.stop();
				} catch (error) {
					nextTrack.stop();
					throw error;
				}
			}
		}
	}

	override async setProcessor(processor: TrackProcessor<Track.Kind.Video>, showProcessedStreamLocally = true) {
		const secondarySenderSnapshots = Array.from(this.simulcastCodecs.values(), (trackInfo) => ({
			sender: trackInfo.sender,
			track: trackInfo.sender?.track ?? null,
		}));
		try {
			await super.setProcessor(processor, showProcessedStreamLocally);
			if (this.processor?.processedTrack) {
				for await (const sc of this.simulcastCodecs.values()) {
					await sc.sender?.replaceTrack(this.processor.processedTrack);
				}
			}
		} catch (error) {
			const cleanupErrors: Array<unknown> = [];
			if (this.processor === processor) {
				try {
					await this.stopProcessor(false);
				} catch (cleanupError) {
					cleanupErrors.push(cleanupError);
				}
			}
			const rollbackResults = await Promise.allSettled(
				secondarySenderSnapshots.map(({sender, track}) => restoreSecondarySenderTrack(sender, track)),
			);
			const rollbackErrors = rollbackResults.flatMap((result) => (result.status === 'rejected' ? [result.reason] : []));
			if (cleanupErrors.length === 0 && rollbackErrors.length === 0) {
				throw error;
			}
			throw createProcessorRecoveryError(error, cleanupErrors, rollbackErrors);
		}
	}

	protected override async internalStopProcessor(keepElement = true) {
		const processor = this.processor;
		if (!processor) {
			await super.internalStopProcessor(keepElement);
			return;
		}
		const secondarySenderSnapshots = Array.from(this.simulcastCodecs.values(), (trackInfo) => ({
			sender: trackInfo.sender,
			track: trackInfo.sender?.track ?? null,
			replacement: trackInfo.mediaStreamTrack,
		}));
		try {
			for (const {sender, replacement} of secondarySenderSnapshots) {
				await sender?.replaceTrack(replacement);
			}
			await super.internalStopProcessor(keepElement);
		} catch (error) {
			const rollbackResults = await Promise.allSettled(
				secondarySenderSnapshots.map(({sender, track}) => restoreSecondarySenderTrack(sender, track)),
			);
			const rollbackErrors = rollbackResults.flatMap((result) => (result.status === 'rejected' ? [result.reason] : []));
			if (rollbackErrors.length === 0) {
				throw error;
			}
			throw new AggregateError(
				rollbackErrors,
				'Video processor stop failed and secondary sender recovery was incomplete',
				{
					cause: error,
				},
			);
		}
	}

	async setDegradationPreference(preference: RTCDegradationPreference) {
		this.degradationPreference = preference;
		if (this.sender) {
			try {
				this.log.debug(`setting degradationPreference to ${preference}`, this.logContext);
				const params = this.sender.getParameters();
				params.degradationPreference = preference;
				await this.sender.setParameters(params);
			} catch (e: unknown) {
				this.log.warn(`failed to set degradationPreference`, {error: e, ...this.logContext});
			}
		}
	}

	addSimulcastTrack(codec: VideoCodec, encodings?: Array<RTCRtpEncodingParameters>): SimulcastTrackInfo | undefined {
		if (this.simulcastCodecs.has(codec)) {
			this.log.error(`${codec} already added, skipping adding simulcast codec`, this.logContext);
			return;
		}
		const simulcastCodecInfo: SimulcastTrackInfo = {
			codec,
			mediaStreamTrack: this._mediaStreamTrack.clone(),
			sender: undefined,
			encodings,
		};
		this.simulcastCodecs.set(codec, simulcastCodecInfo);
		return simulcastCodecInfo;
	}

	setSimulcastTrackSender(codec: VideoCodec, sender: RTCRtpSender) {
		const simulcastCodecInfo = this.simulcastCodecs.get(codec);
		if (!simulcastCodecInfo) {
			return;
		}
		simulcastCodecInfo.sender = sender;
		const processedTrack = this.processor?.processedTrack;
		if (processedTrack) {
			void sender.replaceTrack(processedTrack).catch((error: unknown) => {
				this.log.warn('failed to route processed track to secondary sender', {...this.logContext, error});
			});
		}

		setTimeout(() => {
			if (this.subscribedCodecs) {
				this.setPublishingCodecs(this.subscribedCodecs);
			}
		}, refreshSubscribedCodecAfterNewCodec);
	}

	async setPublishingCodecs(codecs: Array<SubscribedCodec>): Promise<Array<VideoCodec>> {
		this.log.debug('setting publishing codecs', {
			...this.logContext,
			codecs,
			currentCodec: this.codec,
		});
		if (!this.codec && codecs.length > 0) {
			await this.setPublishingLayers(isSVCCodec(codecs[0].codec), codecs[0].qualities);

			return [];
		}

		this.subscribedCodecs = codecs;

		const newCodecs: Array<VideoCodec> = [];
		for await (const codec of codecs) {
			if (!this.codec || this.codec === codec.codec) {
				await this.setPublishingLayers(isSVCCodec(codec.codec), codec.qualities);
			} else {
				const simulcastCodecInfo = this.simulcastCodecs.get(codec.codec as VideoCodec);
				this.log.debug(`try setPublishingCodec for ${codec.codec}`, {
					...this.logContext,
					simulcastCodecInfo,
				});
				if (!simulcastCodecInfo || !simulcastCodecInfo.sender) {
					for (const q of codec.qualities) {
						if (q.enabled) {
							newCodecs.push(codec.codec as VideoCodec);
							break;
						}
					}
				} else if (simulcastCodecInfo.encodings) {
					this.log.debug(`try setPublishingLayersForSender ${codec.codec}`, this.logContext);
					await setPublishingLayersForSender(
						simulcastCodecInfo.sender,
						simulcastCodecInfo.encodings!,
						codec.qualities,
						this.senderLock,
						isSVCCodec(codec.codec),
						this.log,
						this.logContext,
					);
				}
			}
		}
		return newCodecs;
	}

	async setPublishingLayers(isSvc: boolean, qualities: Array<SubscribedQuality>) {
		if (this.optimizeForPerformance) {
			this.log.info('skipping setPublishingLayers due to optimized publishing performance', {
				...this.logContext,
				qualities,
			});
			return;
		}
		this.log.debug('setting publishing layers', {...this.logContext, qualities});
		if (!this.sender || !this.encodings) {
			return;
		}

		await setPublishingLayersForSender(
			this.sender,
			this.encodings,
			qualities,
			this.senderLock,
			isSvc,
			this.log,
			this.logContext,
		);
	}

	async prioritizePerformance() {
		if (!this.sender) {
			throw new Error('sender not found');
		}

		const unlock = await this.senderLock.lock();

		try {
			this.optimizeForPerformance = true;
			const params = this.sender.getParameters();

			params.encodings = params.encodings.map((e, idx) => ({
				...e,
				active: idx === 0,
				scaleResolutionDownBy: Math.max(1, Math.ceil((this.mediaStreamTrack.getSettings().height ?? 360) / 360)),
				scalabilityMode: idx === 0 && isSVCCodec(this.codec) ? 'L1T3' : undefined,
				maxFramerate: idx === 0 ? 15 : 0,
				maxBitrate: idx === 0 ? e.maxBitrate : 0,
			}));
			this.log.debug('setting performance optimised encodings', {
				...this.logContext,
				encodings: params.encodings,
			});
			this.encodings = params.encodings;
			await this.sender.setParameters(params);
		} catch (e) {
			this.log.error('failed to set performance optimised encodings', {
				...this.logContext,
				error: e,
			});
			this.optimizeForPerformance = false;
		} finally {
			unlock();
		}
	}

	protected monitorSender = async () => {
		if (!this.sender) {
			this._currentBitrate = 0;
			return;
		}

		let stats: Array<VideoSenderStats> | undefined;
		try {
			stats = await this.getSenderStats();
		} catch (e) {
			this.log.error('could not get video sender stats', {...this.logContext, error: e});
			return;
		}
		const statsMap = new Map<string, VideoSenderStats>(stats.map((s) => [s.rid, s]));

		const isCpuConstrained = stats.some((s) => s.qualityLimitationReason === 'cpu');
		if (isCpuConstrained !== this.isCpuConstrained) {
			this.isCpuConstrained = isCpuConstrained;
			if (this.isCpuConstrained) {
				this.emit(TrackEvent.CpuConstrained);
			}
		}

		if (this.prevStats) {
			let totalBitrate = 0;
			statsMap.forEach((s, key) => {
				const prev = this.prevStats?.get(key);
				totalBitrate += computeBitrate(s, prev);
			});
			this._currentBitrate = totalBitrate;
		}

		this.prevStats = statsMap;
	};

	protected override async handleAppVisibilityChanged() {
		await super.handleAppVisibilityChanged();
		if (!isMobile()) return;
		if (this.isInBackground && this.source === Track.Source.Camera) {
			this._mediaStreamTrack.enabled = false;
		}
	}
}

async function setPublishingLayersForSender(
	sender: RTCRtpSender,
	senderEncodings: Array<RTCRtpEncodingParameters>,
	qualities: Array<SubscribedQuality>,
	senderLock: Mutex,
	isSVC: boolean,
	log: StructuredLogger,
	logContext: Record<string, unknown>,
) {
	const unlock = await senderLock.lock();
	log.debug('setPublishingLayersForSender', {...logContext, sender, qualities, senderEncodings});
	try {
		const params = sender.getParameters();
		const {encodings} = params;
		if (!encodings) {
			return;
		}

		if (encodings.length !== senderEncodings.length) {
			log.warn('cannot set publishing layers, encodings mismatch', {
				...logContext,
				encodings,
				senderEncodings,
			});
			return;
		}

		let hasChanged = false;

		const closableSpatial = false;
		if (closableSpatial && (encodings[0] as Record<string, unknown>).scalabilityMode) {
			const encoding = encodings[0];
			let maxQuality = ProtoVideoQuality.OFF;
			qualities.forEach((q) => {
				if (q.enabled && (maxQuality === ProtoVideoQuality.OFF || q.quality > maxQuality)) {
					maxQuality = q.quality;
				}
			});

			if (maxQuality === ProtoVideoQuality.OFF) {
				if (encoding.active) {
					encoding.active = false;
					hasChanged = true;
				}
			} else if (!encoding.active) {
				hasChanged = true;
				encoding.active = true;
			}
		} else {
			if (isSVC) {
				const hasEnabledEncoding = qualities.some((q) => q.enabled);
				if (hasEnabledEncoding) {
					qualities.forEach((q) => (q.enabled = true));
				}
			}
			encodings.forEach((encoding, idx) => {
				let rid = encoding.rid ?? '';
				if (rid === '') {
					rid = 'q';
				}
				const quality = videoQualityForRid(rid);
				const subscribedQuality = qualities.find((q) => q.quality === quality);
				if (!subscribedQuality) {
					return;
				}
				if (encoding.active !== subscribedQuality.enabled) {
					hasChanged = true;
					encoding.active = subscribedQuality.enabled;
					log.debug(
						`setting layer ${subscribedQuality.quality} to ${encoding.active ? 'enabled' : 'disabled'}`,
						logContext,
					);

					if (isFireFox()) {
						if (subscribedQuality.enabled) {
							encoding.scaleResolutionDownBy = senderEncodings[idx].scaleResolutionDownBy;
							encoding.maxBitrate = senderEncodings[idx].maxBitrate;
							encoding.maxFramerate = senderEncodings[idx].maxFramerate;
						} else {
							encoding.scaleResolutionDownBy = 4;
							encoding.maxBitrate = 10;
							encoding.maxFramerate = 2;
						}
					}
				}
			});
		}

		if (hasChanged) {
			params.encodings = encodings;
			log.debug(`setting encodings`, {...logContext, encodings: params.encodings});
			await sender.setParameters(params);
		}
	} finally {
		unlock();
	}
}

function videoQualityForRid(rid: string): VideoQuality {
	switch (rid) {
		case 'f':
			return VideoQuality.HIGH;
		case 'h':
			return VideoQuality.MEDIUM;
		case 'q':
			return VideoQuality.LOW;
		default:
			return VideoQuality.HIGH;
	}
}

export function videoLayersFromEncodings(
	width: number,
	height: number,
	encodings?: Array<RTCRtpEncodingParameters>,
	svc?: boolean,
): Array<VideoLayer> {
	if (!encodings) {
		return [
			new VideoLayer({
				quality: VideoQuality.HIGH,
				width,
				height,
				bitrate: 0,
				ssrc: 0,
			}),
		];
	}

	if (svc) {
		const encodingSM = (encodings[0] as Record<string, unknown>).scalabilityMode as string;
		if (!encodingSM) {
			return [
				new VideoLayer({
					quality: VideoQuality.HIGH,
					width,
					height,
					bitrate: encodings[0].maxBitrate ?? 0,
					ssrc: 0,
				}),
			];
		}
		const sm = new ScalabilityMode(encodingSM);
		const layers = [];
		const resRatio = sm.suffix === 'h' ? 1.5 : 2;
		const bitratesRatio = sm.suffix === 'h' ? 2 : 3;
		for (let i = 0; i < sm.spatial; i += 1) {
			layers.push(
				new VideoLayer({
					quality: Math.min(VideoQuality.HIGH, sm.spatial - 1) - i,
					width: Math.ceil(width / resRatio ** i),
					height: Math.ceil(height / resRatio ** i),
					bitrate: encodings[0].maxBitrate ? Math.ceil(encodings[0].maxBitrate / bitratesRatio ** i) : 0,
					ssrc: 0,
				}),
			);
		}
		return layers;
	}

	return encodings.map((encoding) => {
		const scale = encoding.scaleResolutionDownBy ?? 1;
		const quality = videoQualityForRid(encoding.rid ?? '');
		return new VideoLayer({
			quality,
			width: Math.ceil(width / scale),
			height: Math.ceil(height / scale),
			bitrate: encoding.maxBitrate ?? 0,
			ssrc: 0,
		});
	});
}
