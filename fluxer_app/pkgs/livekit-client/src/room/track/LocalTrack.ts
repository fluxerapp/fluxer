// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import {debounce} from 'ts-debounce';
import {getBrowser} from '../../utils/browserParser.ts';
import DeviceManager from '../DeviceManager.ts';
import {DeviceUnsupportedError, TrackInvalidError} from '../errors.ts';
import {TrackEvent} from '../events.ts';
import CriticalTimers, {type TimerHandle} from '../timers.ts';
import type {LoggerOptions} from '../types.ts';
import {compareVersions, isMobile, sleep, unwrapConstraint} from '../utils.ts';
import type {VideoCodec} from './options.ts';
import type {TrackProcessor} from './processor/types.ts';
import {isRecordingSupported, LocalTrackRecorder} from './record.ts';
import {attachToElement, detachTrack, Track} from './Track.ts';
import type {ReplaceTrackOptions} from './types.ts';

const DEFAULT_DIMENSIONS_TIMEOUT = 1000;
const PRE_CONNECT_BUFFER_TIMEOUT = 10_000;

interface SetMediaStreamTrackOptions {
	force?: boolean;
	deferEndedListener?: boolean;
	preservePreviousTrack: boolean;
}

export default abstract class LocalTrack<TrackKind extends Track.Kind = Track.Kind> extends Track<TrackKind> {
	protected _sender?: RTCRtpSender;

	private autoStopPreConnectBuffer: TimerHandle | undefined;

	get sender(): RTCRtpSender | undefined {
		return this._sender;
	}

	set sender(sender: RTCRtpSender | undefined) {
		this._sender = sender;
	}

	codec?: VideoCodec;

	get constraints() {
		return this._constraints;
	}

	get hasPreConnectBuffer() {
		return !!this.localTrackRecorder;
	}

	protected _constraints: MediaTrackConstraints;

	protected reacquireTrack: boolean;

	protected providedByUser: boolean;

	protected muteLock: Mutex;

	protected pauseUpstreamLock: Mutex;

	protected processorElement?: HTMLMediaElement;

	protected processor?: TrackProcessor<TrackKind, any>;

	protected audioContext?: AudioContext;

	protected manuallyStopped: boolean = false;

	protected localTrackRecorder: LocalTrackRecorder<typeof this> | undefined;

	protected trackChangeLock: Mutex;

	protected pendingDeviceChange: boolean = false;

	private stagedReplacementTrack: MediaStreamTrack | undefined;

	protected constructor(
		mediaTrack: MediaStreamTrack,
		kind: TrackKind,
		constraints?: MediaTrackConstraints,
		userProvidedTrack = false,
		loggerOptions?: LoggerOptions,
	) {
		super(mediaTrack, kind, loggerOptions);
		this.reacquireTrack = false;
		this.providedByUser = userProvidedTrack;
		this.muteLock = new Mutex();
		this.pauseUpstreamLock = new Mutex();
		this.trackChangeLock = new Mutex();
		this.trackChangeLock.lock().then(async (unlock) => {
			try {
				await this.setMediaStreamTrack(mediaTrack, {
					force: true,
					preservePreviousTrack: userProvidedTrack,
				});
			} finally {
				unlock();
			}
		});

		this._constraints = mediaTrack.getConstraints();
		if (constraints) {
			this._constraints = constraints;
		}
	}

	get id(): string {
		return this._mediaStreamTrack.id;
	}

	get dimensions(): Track.Dimensions | undefined {
		if (this.kind !== Track.Kind.Video) {
			return undefined;
		}

		const {width, height} = this._mediaStreamTrack.getSettings();
		if (width && height) {
			return {
				width,
				height,
			};
		}
		return undefined;
	}

	private _isUpstreamPaused: boolean = false;

	get isUpstreamPaused() {
		return this._isUpstreamPaused;
	}

	get isUserProvided() {
		return this.providedByUser;
	}

	override get mediaStreamTrack() {
		return this.processor?.processedTrack ?? this._mediaStreamTrack;
	}

	get isLocal() {
		return true;
	}

	getSourceTrackSettings() {
		return this._mediaStreamTrack.getSettings();
	}

	private addMediaStreamTrackListeners(track: MediaStreamTrack, includeEndedListener = true) {
		if (includeEndedListener) {
			track.addEventListener('ended', this.handleEnded);
		}
		track.addEventListener('mute', this.handleTrackMuteEvent);
		track.addEventListener('unmute', this.handleTrackUnmuteEvent);
	}

	private removeMediaStreamTrackListeners(track: MediaStreamTrack) {
		track.removeEventListener('ended', this.handleEnded);
		track.removeEventListener('mute', this.handleTrackMuteEvent);
		track.removeEventListener('unmute', this.handleTrackUnmuteEvent);
	}

	private async restoreMediaStreamTrackAfterFailure(
		previousTrack: MediaStreamTrack,
		previousConstraints: MediaTrackConstraints,
		previousEnabled: boolean,
		failedTrack: MediaStreamTrack,
		failedProcessedTrack: MediaStreamTrack | undefined,
		previousTrackEndedListenerDeferred: boolean,
	) {
		this.removeMediaStreamTrackListeners(failedTrack);
		for (const element of this.attachedElements) {
			detachTrack(failedTrack, element);
			if (failedProcessedTrack) {
				detachTrack(failedProcessedTrack, element);
			}
		}
		if (previousTrack.readyState !== 'live') {
			throw new TrackInvalidError('unable to restore an ended track after replacement failure');
		}
		this.mediaStream = new MediaStream([previousTrack]);
		this._mediaStreamTrack = previousTrack;
		this._constraints = previousConstraints;
		previousTrack.enabled = previousEnabled;
		this.addMediaStreamTrackListeners(previousTrack, !previousTrackEndedListenerDeferred);
		let restoredProcessedTrack: MediaStreamTrack | undefined;
		if (this.processor) {
			if (this.kind === 'unknown') {
				throw TypeError('cannot restore processor on track of unknown kind');
			}
			if (this.processorElement) {
				attachToElement(previousTrack, this.processorElement);
				this.processorElement.muted = true;
			}
			await this.processor.restart({
				track: previousTrack,
				kind: this.kind,
				element: this.processorElement,
			});
			restoredProcessedTrack = this.processor.processedTrack;
		}
		if (this.sender && this.sender.transport?.state !== 'closed') {
			await this.sender.replaceTrack(restoredProcessedTrack ?? previousTrack);
		}
		await this.resumeUpstream();
		for (const element of this.attachedElements) {
			attachToElement(restoredProcessedTrack ?? previousTrack, element);
		}
	}

	private async setMediaStreamTrack(newTrack: MediaStreamTrack, options: SetMediaStreamTrackOptions) {
		const {deferEndedListener = false, force = false, preservePreviousTrack} = options;
		if (newTrack === this._mediaStreamTrack && !force) {
			return;
		}
		const previousTrack = this._mediaStreamTrack;
		const previousConstraints = this._constraints;
		const previousEnabled = previousTrack.enabled;
		const previousTrackEndedListenerDeferred = this.stagedReplacementTrack === previousTrack;
		const nextTrackEndedListenerDeferred = deferEndedListener || this.stagedReplacementTrack === newTrack;
		let processedTrack: MediaStreamTrack | undefined;
		try {
			this.attachedElements.forEach((el) => {
				detachTrack(previousTrack, el);
			});
			this.debouncedTrackMuteHandler.cancel('new-track');
			this.removeMediaStreamTrackListeners(previousTrack);
			this.mediaStream = new MediaStream([newTrack]);
			this.addMediaStreamTrackListeners(newTrack, !nextTrackEndedListenerDeferred);
			this._constraints = newTrack.getConstraints();
			if (this.processor) {
				this.log.debug('restarting processor', this.logContext);
				if (this.kind === 'unknown') {
					throw TypeError('cannot set processor on track of unknown kind');
				}

				if (this.processorElement) {
					attachToElement(newTrack, this.processorElement);
					this.processorElement.muted = true;
				}
				await this.processor.restart({
					track: newTrack,
					kind: this.kind,
					element: this.processorElement,
				});
				processedTrack = this.processor.processedTrack;
			}
			if (this.sender && this.sender.transport?.state !== 'closed') {
				await this.sender.replaceTrack(processedTrack ?? newTrack);
			}
			this._mediaStreamTrack = newTrack;
			this._mediaStreamTrack.enabled = !this.isMuted;
			await this.resumeUpstream();
			this.attachedElements.forEach((el) => {
				attachToElement(processedTrack ?? newTrack, el);
			});
			if (!preservePreviousTrack && previousTrack !== newTrack) {
				previousTrack.stop();
			}
		} catch (error) {
			try {
				await this.restoreMediaStreamTrackAfterFailure(
					previousTrack,
					previousConstraints,
					previousEnabled,
					newTrack,
					processedTrack,
					previousTrackEndedListenerDeferred,
				);
			} catch (rollbackError) {
				throw new AggregateError([error, rollbackError], 'Track replacement and internal rollback both failed');
			}
			throw error;
		}
	}

	async waitForDimensions(timeout = DEFAULT_DIMENSIONS_TIMEOUT): Promise<Track.Dimensions> {
		if (this.kind === Track.Kind.Audio) {
			throw new Error('cannot get dimensions for audio tracks');
		}

		if (getBrowser()?.os === 'iOS') {
			await sleep(10);
		}

		const started = Date.now();
		while (Date.now() - started < timeout) {
			const dims = this.dimensions;
			if (dims) {
				return dims;
			}
			await sleep(50);
		}
		throw new TrackInvalidError('unable to get track dimensions after timeout');
	}

	async setDeviceId(deviceId: ConstrainDOMString): Promise<boolean> {
		if (
			this._constraints.deviceId === deviceId &&
			this._mediaStreamTrack.getSettings().deviceId === unwrapConstraint(deviceId)
		) {
			return true;
		}

		this._constraints.deviceId = deviceId;

		if (this.isMuted) {
			this.pendingDeviceChange = true;
			return true;
		}

		await this.restartTrack();

		return unwrapConstraint(deviceId) === this._mediaStreamTrack.getSettings().deviceId;
	}

	abstract restartTrack(constraints?: unknown): Promise<void>;

	async getDeviceId(normalize = true): Promise<string | undefined> {
		if (this.source === Track.Source.ScreenShare) {
			return;
		}
		const {deviceId, groupId} = this._mediaStreamTrack.getSettings();
		const kind = this.kind === Track.Kind.Audio ? 'audioinput' : 'videoinput';

		return normalize ? DeviceManager.getInstance().normalizeDeviceId(kind, deviceId, groupId) : deviceId;
	}

	async mute() {
		this.setTrackMuted(true);
		return this;
	}

	async unmute() {
		this.setTrackMuted(false);
		return this;
	}

	async replaceTrack(track: MediaStreamTrack, options?: ReplaceTrackOptions): Promise<typeof this>;
	async replaceTrack(track: MediaStreamTrack, userProvidedTrack?: boolean): Promise<typeof this>;
	async replaceTrack(track: MediaStreamTrack, userProvidedOrOptions: boolean | ReplaceTrackOptions | undefined) {
		const unlock = await this.trackChangeLock.lock();
		const previousProvidedByUser = this.providedByUser;
		let replacementCommitted = false;
		try {
			if (this.stagedReplacementTrack) {
				throw new TrackInvalidError('unable to replace a track while a staged replacement is active');
			}
			if (!this.sender) {
				throw new TrackInvalidError('unable to replace an unpublished track');
			}

			let userProvidedTrack: boolean | undefined;
			let stopProcessor: boolean | undefined;

			if (typeof userProvidedOrOptions === 'boolean') {
				userProvidedTrack = userProvidedOrOptions;
			} else if (userProvidedOrOptions !== undefined) {
				userProvidedTrack = userProvidedOrOptions.userProvidedTrack;
				stopProcessor = userProvidedOrOptions.stopProcessor;
			}

			this.providedByUser = userProvidedTrack ?? true;

			this.log.debug('replace MediaStreamTrack', this.logContext);
			await this.setMediaStreamTrack(track, {preservePreviousTrack: previousProvidedByUser});
			replacementCommitted = true;

			if (stopProcessor && this.processor) {
				await this.internalStopProcessor();
			}
			return this;
		} catch (error) {
			if (!replacementCommitted) {
				this.providedByUser = previousProvidedByUser;
			}
			throw error;
		} finally {
			unlock();
		}
	}

	async runWithTrackChangeLock<T>(operation: () => Promise<T>): Promise<T> {
		const unlock = await this.trackChangeLock.lock();
		try {
			return await operation();
		} finally {
			unlock();
		}
	}

	async stageTrackReplacement(track: MediaStreamTrack): Promise<typeof this> {
		const unlock = await this.trackChangeLock.lock();
		const previousProvidedByUser = this.providedByUser;
		const previousStagedReplacementTrack = this.stagedReplacementTrack;
		try {
			if (!this.sender) {
				throw new TrackInvalidError('unable to stage a replacement for an unpublished track');
			}
			if (previousStagedReplacementTrack && previousStagedReplacementTrack !== this._mediaStreamTrack) {
				throw new TrackInvalidError('staged replacement identity does not match the active source track');
			}
			if (track === this._mediaStreamTrack) {
				throw new TrackInvalidError('unable to stage the active source track as its own replacement');
			}
			if (track.readyState !== 'live') {
				throw new TrackInvalidError('unable to stage an ended replacement track');
			}

			this.providedByUser = true;
			this.log.debug('stage MediaStreamTrack replacement', this.logContext);
			await this.setMediaStreamTrack(track, {
				deferEndedListener: true,
				preservePreviousTrack: true,
			});
			this.stagedReplacementTrack = track;
			return this;
		} catch (error) {
			this.providedByUser = previousProvidedByUser;
			this.stagedReplacementTrack = previousStagedReplacementTrack;
			throw error;
		} finally {
			unlock();
		}
	}

	async commitStagedTrackReplacement(track: MediaStreamTrack, userProvidedTrack: boolean): Promise<typeof this> {
		const unlock = await this.trackChangeLock.lock();
		try {
			if (this.stagedReplacementTrack !== track || this._mediaStreamTrack !== track) {
				throw new TrackInvalidError('unable to commit a replacement that is not the active staged track');
			}
			if (!this.sender) {
				throw new TrackInvalidError('unable to commit a replacement for an unpublished track');
			}
			if (track.readyState !== 'live') {
				throw new TrackInvalidError('unable to commit an ended staged track');
			}

			track.addEventListener('ended', this.handleEnded);
			if (track.readyState !== 'live') {
				track.removeEventListener('ended', this.handleEnded);
				throw new TrackInvalidError('staged track ended while its replacement was committed');
			}
			this.providedByUser = userProvidedTrack;
			this.stagedReplacementTrack = undefined;
			return this;
		} finally {
			unlock();
		}
	}

	protected async restart(constraints?: MediaTrackConstraints) {
		this.manuallyStopped = false;
		const unlock = await this.trackChangeLock.lock();
		const previousProvidedByUser = this.providedByUser;
		let newTrack: MediaStreamTrack | undefined;
		let replacementCommitted = false;

		try {
			if (this.stagedReplacementTrack) {
				throw new TrackInvalidError('unable to restart a track while a staged replacement is active');
			}
			if (!constraints) {
				constraints = this._constraints;
			}
			const {deviceId, facingMode, ...otherConstraints} = constraints;
			this.log.debug('restarting track with constraints', {...this.logContext, constraints});

			const streamConstraints: MediaStreamConstraints = {
				audio: false,
				video: false,
			};

			if (this.kind === Track.Kind.Video) {
				streamConstraints.video = deviceId || facingMode ? {deviceId, facingMode} : true;
			} else {
				streamConstraints.audio = deviceId ? {deviceId, ...otherConstraints} : true;
			}

			this.attachedElements.forEach((el) => {
				detachTrack(this.mediaStreamTrack, el);
			});
			this._mediaStreamTrack.removeEventListener('ended', this.handleEnded);
			this._mediaStreamTrack.stop();

			const mediaStream = await navigator.mediaDevices.getUserMedia(streamConstraints);
			newTrack = mediaStream.getTracks()[0];
			if (!newTrack) {
				throw new TrackInvalidError('getUserMedia returned no track during restart');
			}
			if (this.kind === Track.Kind.Video) {
				await newTrack.applyConstraints(otherConstraints);
			}
			this.log.debug('re-acquired MediaStreamTrack', this.logContext);

			this.providedByUser = false;
			await this.setMediaStreamTrack(newTrack, {preservePreviousTrack: previousProvidedByUser});
			replacementCommitted = true;
			this._constraints = constraints;
			this.pendingDeviceChange = false;
			this.emit(TrackEvent.Restarted, this);
			if (this.manuallyStopped) {
				this.log.warn('track was stopped during a restart, stopping restarted track', this.logContext);
				this.stop();
			}
			return this;
		} catch (error) {
			if (!replacementCommitted) {
				this.providedByUser = previousProvidedByUser;
				newTrack?.stop();
			}
			throw error;
		} finally {
			unlock();
		}
	}

	protected setTrackMuted(muted: boolean) {
		this.log.debug(`setting ${this.kind} track ${muted ? 'muted' : 'unmuted'}`, this.logContext);

		if (this.isMuted === muted && this._mediaStreamTrack.enabled !== muted) {
			return;
		}

		this.isMuted = muted;
		this._mediaStreamTrack.enabled = !muted;
		this.emit(muted ? TrackEvent.Muted : TrackEvent.Unmuted, this);
	}

	protected get needsReAcquisition(): boolean {
		return (
			this._mediaStreamTrack.readyState !== 'live' ||
			this._mediaStreamTrack.muted ||
			!this._mediaStreamTrack.enabled ||
			this.reacquireTrack
		);
	}

	protected override async handleAppVisibilityChanged() {
		await super.handleAppVisibilityChanged();
		if (!isMobile()) return;
		this.log.debug(`visibility changed, is in Background: ${this.isInBackground}`, this.logContext);

		if (!this.isInBackground && this.needsReAcquisition && !this.isUserProvided && !this.isMuted) {
			this.log.debug(`track needs to be reacquired, restarting ${this.source}`, this.logContext);
			await this.restart();
			this.reacquireTrack = false;
		}
	}

	private handleTrackMuteEvent = () =>
		this.debouncedTrackMuteHandler().catch(() =>
			this.log.debug('track mute bounce got cancelled by an unmute event', this.logContext),
		);

	private debouncedTrackMuteHandler = debounce(async () => {
		await this.pauseUpstream();
	}, 5000);

	private handleTrackUnmuteEvent = async () => {
		this.debouncedTrackMuteHandler.cancel('unmute');
		await this.resumeUpstream();
	};

	private handleEnded = () => {
		if (this.isInBackground) {
			this.reacquireTrack = true;
		}
		this._mediaStreamTrack.removeEventListener('mute', this.handleTrackMuteEvent);
		this._mediaStreamTrack.removeEventListener('unmute', this.handleTrackUnmuteEvent);
		this.emit(TrackEvent.Ended, this);
	};

	override stop() {
		this.manuallyStopped = true;
		this.stagedReplacementTrack = undefined;
		const processor = this.processor;
		this.processor = undefined;
		try {
			void processor?.destroy().catch((error) => {
				this.log.error('failed to destroy track processor during stop', {...this.logContext, error});
			});
		} catch (error) {
			this.log.error('failed to destroy track processor during stop', {...this.logContext, error});
		}
		super.stop();

		this._mediaStreamTrack.removeEventListener('ended', this.handleEnded);
		this._mediaStreamTrack.removeEventListener('mute', this.handleTrackMuteEvent);
		this._mediaStreamTrack.removeEventListener('unmute', this.handleTrackUnmuteEvent);
	}

	async pauseUpstream() {
		const unlock = await this.pauseUpstreamLock.lock();
		try {
			if (this._isUpstreamPaused === true) {
				return;
			}
			if (!this.sender) {
				this.log.warn('unable to pause upstream for an unpublished track', this.logContext);
				return;
			}

			this._isUpstreamPaused = true;
			this.emit(TrackEvent.UpstreamPaused, this);
			const browser = getBrowser();
			if (browser?.name === 'Safari' && compareVersions(browser.version, '12.0') < 0) {
				throw new DeviceUnsupportedError('pauseUpstream is not supported on Safari < 12.');
			}
			if (this.sender.transport?.state !== 'closed') {
				await this.sender.replaceTrack(null);
			}
		} finally {
			unlock();
		}
	}

	async resumeUpstream() {
		const unlock = await this.pauseUpstreamLock.lock();
		try {
			if (this._isUpstreamPaused === false) {
				return;
			}
			if (!this.sender) {
				this.log.warn('unable to resume upstream for an unpublished track', this.logContext);
				return;
			}
			this._isUpstreamPaused = false;
			this.emit(TrackEvent.UpstreamResumed, this);

			if (this.sender.transport?.state !== 'closed') {
				await this.sender.replaceTrack(this.mediaStreamTrack);
			}
		} finally {
			unlock();
		}
	}

	async getRTCStatsReport(): Promise<RTCStatsReport | undefined> {
		if (!this.sender?.getStats) {
			return;
		}
		const statsReport = await this.sender.getStats();
		return statsReport;
	}

	async setProcessor(processor: TrackProcessor<TrackKind>, showProcessedStreamLocally = true) {
		const unlock = await this.trackChangeLock.lock();
		try {
			this.log.debug('setting up processor', this.logContext);
			const processorElement = document.createElement(this.kind) as HTMLMediaElement;
			const processorOptions = {
				kind: this.kind,
				track: this._mediaStreamTrack,
				element: processorElement,
				audioContext: this.audioContext,
			};
			try {
				await processor.init(processorOptions);
			} catch (error) {
				try {
					await processor.destroy();
				} catch (cleanupError) {
					throw new AggregateError([error, cleanupError], 'Track processor setup and candidate cleanup both failed');
				}
				throw error;
			}
			this.log.debug('processor initialized', this.logContext);
			const previousProcessor = this.processor;
			if (previousProcessor) {
				try {
					await this.internalStopProcessor(false);
				} catch (error) {
					const cleanupErrors: Array<unknown> = [];
					try {
						await processor.destroy();
					} catch (cleanupError) {
						cleanupErrors.push(cleanupError);
					}
					processorElement.remove();
					const sender = this.sender;
					if (this.processor !== previousProcessor && sender && sender.transport?.state !== 'closed') {
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
						throw new AggregateError(
							[error, ...cleanupErrors],
							'Existing track processor removal and candidate cleanup both failed',
						);
					}
					throw error;
				}
			}
			if (this.kind === 'unknown') {
				let cleanupError: unknown;
				try {
					await processor.destroy();
				} catch (error) {
					cleanupError = error;
				}
				processorElement.remove();
				const kindError = new TypeError('cannot set processor on track of unknown kind');
				if (cleanupError !== undefined) {
					throw new AggregateError([kindError, cleanupError], 'Invalid track processor kind and cleanup both failed');
				}
				throw kindError;
			}
			const processedTrack = processor.processedTrack;
			try {
				attachToElement(this._mediaStreamTrack, processorElement);
				processorElement.muted = true;
				processorElement.play().catch((error) => {
					if (error instanceof DOMException && error.name === 'AbortError') {
						this.log.warn('failed to play processor element, retrying', {
							...this.logContext,
							error,
						});
						setTimeout(() => {
							processorElement.play().catch((err) => {
								this.log.error('failed to play processor element', {...this.logContext, err});
							});
						}, 100);
					} else {
						this.log.error('failed to play processor element', {...this.logContext, error});
					}
				});
				if (processedTrack) {
					for (const el of this.attachedElements) {
						if (showProcessedStreamLocally) {
							detachTrack(this._mediaStreamTrack, el);
							attachToElement(processedTrack, el);
						}
					}
					await this.sender?.replaceTrack(processedTrack);
				}
				this.processor = processor;
				this.processorElement = processorElement;
				this.emit(TrackEvent.TrackProcessorUpdate, processor);
			} catch (error) {
				const cleanupErrors: Array<unknown> = [];
				if (this.processor === processor) this.processor = undefined;
				if (this.processorElement === processorElement) this.processorElement = undefined;
				if (processedTrack) {
					for (const el of this.attachedElements) {
						try {
							detachTrack(processedTrack, el);
							if (this._mediaStreamTrack.readyState === 'live') attachToElement(this._mediaStreamTrack, el);
						} catch (cleanupError) {
							cleanupErrors.push(cleanupError);
						}
					}
				}
				processorElement.remove();
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
					throw new AggregateError([error, ...cleanupErrors], 'Track processor install rollback was incomplete');
				}
				throw error;
			}
		} finally {
			unlock();
		}
	}

	getProcessor() {
		return this.processor;
	}

	async stopProcessor(keepElement = true) {
		const unlock = await this.trackChangeLock.lock();
		try {
			await this.internalStopProcessor(keepElement);
		} finally {
			unlock();
		}
	}

	async stopProcessorIfCurrent(processor: TrackProcessor<TrackKind>, keepElement = true): Promise<boolean> {
		const unlock = await this.trackChangeLock.lock();
		try {
			if (this.processor !== processor) {
				return false;
			}
			await this.internalStopProcessor(keepElement);
			return true;
		} finally {
			unlock();
		}
	}

	protected async internalStopProcessor(keepElement = true) {
		if (!this.processor) return;
		this.log.debug('stopping processor', this.logContext);
		const processor = this.processor;
		const processedTrack = processor.processedTrack;
		const constraints = this._constraints;
		this.processor = undefined;
		if (processedTrack) {
			for (const element of this.attachedElements) {
				detachTrack(processedTrack, element);
			}
		}
		if (!keepElement) {
			this.processorElement?.remove();
			this.processorElement = undefined;
		}
		const cleanupErrors: Array<unknown> = [];
		if (this._mediaStreamTrack.readyState === 'live') {
			try {
				await this.setMediaStreamTrack(this._mediaStreamTrack, {
					force: true,
					preservePreviousTrack: this.providedByUser,
				});
			} catch (error) {
				cleanupErrors.push(error);
			}
			if (this._mediaStreamTrack.readyState === 'live') {
				try {
					await this._mediaStreamTrack.applyConstraints(constraints);
				} catch (error) {
					cleanupErrors.push(error);
				}
			}
		}
		this._constraints = constraints;
		if (processedTrack && processedTrack.readyState !== 'ended') {
			processedTrack.enabled = false;
			try {
				processedTrack.stop();
			} catch (error) {
				cleanupErrors.push(error);
			}
		}
		try {
			await processor.destroy();
		} catch (error) {
			cleanupErrors.push(error);
		}
		const sender = this.sender;
		if (sender && sender.transport?.state !== 'closed') {
			const rawSenderTrack = this._mediaStreamTrack.readyState === 'live' ? this._mediaStreamTrack : null;
			if (sender.track !== rawSenderTrack) {
				try {
					await sender.replaceTrack(rawSenderTrack);
				} catch (error) {
					cleanupErrors.push(error);
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
			throw new AggregateError(cleanupErrors, 'Failed to stop track processor cleanly');
		}
		this.emit(TrackEvent.TrackProcessorUpdate);
	}

	startPreConnectBuffer(timeslice: number = 100) {
		if (!isRecordingSupported()) {
			this.log.warn('MediaRecorder is not available, cannot start preconnect buffer', this.logContext);
			return;
		}

		if (!this.localTrackRecorder) {
			let mimeType = 'audio/webm;codecs=opus';
			if (!MediaRecorder.isTypeSupported(mimeType)) {
				mimeType = 'video/mp4';
			}
			this.localTrackRecorder = new LocalTrackRecorder(this, {
				mimeType,
			});
		} else {
			this.log.warn('preconnect buffer already started');
			return;
		}

		this.localTrackRecorder.start(timeslice);
		this.autoStopPreConnectBuffer = CriticalTimers.setTimeout(() => {
			this.log.warn('preconnect buffer timed out, stopping recording automatically', this.logContext);
			this.stopPreConnectBuffer();
		}, PRE_CONNECT_BUFFER_TIMEOUT);
	}

	stopPreConnectBuffer() {
		CriticalTimers.clearTimeout(this.autoStopPreConnectBuffer);
		if (this.localTrackRecorder) {
			this.localTrackRecorder.stop();
			this.localTrackRecorder = undefined;
		}
	}

	getPreConnectBuffer(): ReadableStream<Uint8Array> | undefined {
		return this.localTrackRecorder?.byteStream;
	}

	getPreConnectBufferMimeType() {
		return this.localTrackRecorder?.mimeType;
	}

	protected abstract monitorSender(): void;
}
