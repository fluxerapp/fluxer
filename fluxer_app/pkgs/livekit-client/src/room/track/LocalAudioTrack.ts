// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {AudioTrackFeature} from '@livekit/protocol';
import {TrackEvent} from '../events.ts';
import type {AudioSenderStats} from '../stats.ts';
import {computeBitrate, monitorFrequency} from '../stats.ts';
import type {LoggerOptions} from '../types.ts';
import {isReactNative, isWeb} from '../utils.ts';
import LocalTrack from './LocalTrack.ts';
import type {AudioCaptureOptions} from './options.ts';
import type {AudioProcessorOptions, TrackProcessor} from './processor/types.ts';
import {Track} from './Track.ts';
import {constraintsForOptions, detectSilence} from './utils.ts';

export default class LocalAudioTrack extends LocalTrack<Track.Kind.Audio> {
	stopOnMute: boolean = false;

	private prevStats?: AudioSenderStats;

	private isKrispNoiseFilterEnabled = false;

	protected override processor?: TrackProcessor<Track.Kind.Audio, AudioProcessorOptions> | undefined = undefined;

	get enhancedNoiseCancellation() {
		return this.isKrispNoiseFilterEnabled;
	}

	constructor(
		mediaTrack: MediaStreamTrack,
		constraints?: MediaTrackConstraints,
		userProvidedTrack = true,
		audioContext?: AudioContext,
		loggerOptions?: LoggerOptions,
	) {
		super(mediaTrack, Track.Kind.Audio, constraints, userProvidedTrack, loggerOptions);
		this.audioContext = audioContext;
		this.checkForSilence();
	}

	override async mute(): Promise<typeof this> {
		const unlock = await this.muteLock.lock();
		try {
			if (this.isMuted) {
				this.log.debug('Track already muted', this.logContext);
				return this;
			}

			if (this.source === Track.Source.Microphone && this.stopOnMute && !this.isUserProvided) {
				this.log.debug('stopping mic track', this.logContext);
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

			if (
				this.source === Track.Source.Microphone &&
				(this.stopOnMute || this._mediaStreamTrack.readyState === 'ended' || this.pendingDeviceChange) &&
				!this.isUserProvided
			) {
				this.log.debug('reacquiring mic track', this.logContext);
				await this.restartTrack();
			}
			await super.unmute();

			return this;
		} finally {
			unlock();
		}
	}

	async restartTrack(options?: AudioCaptureOptions) {
		let constraints: MediaTrackConstraints | undefined;
		if (options) {
			const streamConstraints = constraintsForOptions({audio: options});
			if (typeof streamConstraints.audio !== 'boolean') {
				constraints = streamConstraints.audio;
			}
		}
		await this.restart(constraints);
	}

	protected override async restart(constraints?: MediaTrackConstraints): Promise<typeof this> {
		const track = await super.restart(constraints);
		this.checkForSilence();
		return track;
	}

	startMonitor() {
		if (!isWeb()) {
			return;
		}
		if (this.monitorInterval) {
			return;
		}
		this.monitorInterval = setInterval(() => {
			this.runMonitor(this.monitorSender);
		}, monitorFrequency);
	}

	protected monitorSender = async () => {
		if (!this.sender) {
			this._currentBitrate = 0;
			return;
		}

		let stats: AudioSenderStats | undefined;
		try {
			stats = await this.getSenderStats();
		} catch (e) {
			this.log.error('could not get audio sender stats', {...this.logContext, error: e});
			return;
		}

		if (stats && this.prevStats) {
			this._currentBitrate = computeBitrate(stats, this.prevStats);
		}

		this.prevStats = stats;
	};

	private handleKrispNoiseFilterEnable = () => {
		this.isKrispNoiseFilterEnabled = true;
		this.log.debug(`Krisp noise filter enabled`, this.logContext);
		this.emit(TrackEvent.AudioTrackFeatureUpdate, this, AudioTrackFeature.TF_ENHANCED_NOISE_CANCELLATION, true);
	};

	private handleKrispNoiseFilterDisable = () => {
		this.isKrispNoiseFilterEnabled = false;
		this.log.debug(`Krisp noise filter disabled`, this.logContext);
		this.emit(TrackEvent.AudioTrackFeatureUpdate, this, AudioTrackFeature.TF_ENHANCED_NOISE_CANCELLATION, false);
	};

	override async setProcessor(processor: TrackProcessor<Track.Kind.Audio, AudioProcessorOptions>) {
		const unlock = await this.trackChangeLock.lock();
		try {
			if (!isReactNative() && !this.audioContext) {
				throw Error('Audio context needs to be set on LocalAudioTrack in order to enable processors');
			}
			if (this.processor) {
				await this.internalStopProcessor();
			}

			const processorOptions = {
				kind: this.kind,
				track: this._mediaStreamTrack,
				audioContext: this.audioContext as AudioContext,
			};
			this.log.debug(`setting up audio processor ${processor.name}`, this.logContext);
			try {
				await processor.init(processorOptions);
			} catch (error) {
				try {
					await processor.destroy();
				} catch (cleanupError) {
					throw new AggregateError(
						[error, cleanupError],
						'Audio track processor setup and candidate cleanup both failed',
					);
				}
				throw error;
			}
			const processedTrack = processor.processedTrack;
			try {
				if (processedTrack) await this.sender?.replaceTrack(processedTrack);
				this.processor = processor;
				if (processedTrack) {
					processedTrack.addEventListener('enable-lk-krisp-noise-filter', this.handleKrispNoiseFilterEnable);
					processedTrack.addEventListener('disable-lk-krisp-noise-filter', this.handleKrispNoiseFilterDisable);
				}
				this.emit(TrackEvent.TrackProcessorUpdate, processor);
			} catch (error) {
				const cleanupErrors: Array<unknown> = [];
				if (this.processor === processor) this.processor = undefined;
				processedTrack?.removeEventListener('enable-lk-krisp-noise-filter', this.handleKrispNoiseFilterEnable);
				processedTrack?.removeEventListener('disable-lk-krisp-noise-filter', this.handleKrispNoiseFilterDisable);
				try {
					await processor.destroy();
				} catch (cleanupError) {
					cleanupErrors.push(cleanupError);
				}
				if (processedTrack && processedTrack.readyState !== 'ended') {
					processedTrack.enabled = false;
					try {
						processedTrack.stop();
					} catch (cleanupError) {
						cleanupErrors.push(cleanupError);
					}
				}
				const sender = this.sender;
				if (sender && sender.transport?.state !== 'closed') {
					const rawSenderTrack = this._mediaStreamTrack.readyState === 'live' ? this._mediaStreamTrack : null;
					if (sender.track !== rawSenderTrack) {
						try {
							await sender.replaceTrack(rawSenderTrack);
						} catch (cleanupError) {
							cleanupErrors.push(cleanupError);
							if (sender.track?.readyState === 'ended') {
								try {
									await sender.replaceTrack(null);
								} catch (failCloseError) {
									cleanupErrors.push(failCloseError);
								}
							}
						}
					}
				}
				if (cleanupErrors.length > 0) {
					throw new AggregateError([error, ...cleanupErrors], 'Audio track processor install rollback was incomplete');
				}
				throw error;
			}
		} finally {
			unlock();
		}
	}

	setAudioContext(audioContext: AudioContext | undefined) {
		this.audioContext = audioContext;
	}

	async getSenderStats(): Promise<AudioSenderStats | undefined> {
		if (!this.sender?.getStats) {
			return undefined;
		}

		const stats = await this.sender.getStats();
		let audioStats: AudioSenderStats | undefined;
		stats.forEach((v) => {
			if (v.type === 'outbound-rtp') {
				audioStats = {
					type: 'audio',
					streamId: v.id,
					packetsSent: v.packetsSent,
					packetsLost: v.packetsLost,
					bytesSent: v.bytesSent,
					timestamp: v.timestamp,
					roundTripTime: v.roundTripTime,
					jitter: v.jitter,
				};
			}
		});

		return audioStats;
	}

	async checkForSilence() {
		const trackIsSilent = await detectSilence(this);
		if (trackIsSilent) {
			if (!this.isMuted) {
				this.log.debug('silence detected on local audio track', this.logContext);
			}
			this.emit(TrackEvent.AudioSilenceDetected);
		}
		return trackIsSilent;
	}
}
