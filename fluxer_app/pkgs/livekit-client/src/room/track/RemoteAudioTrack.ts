// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {TrackEvent} from '../events.ts';
import type {AudioReceiverStats} from '../stats.ts';
import {computeBitrate} from '../stats.ts';
import type {LoggerOptions} from '../types.ts';
import {isReactNative, supportsSetSinkId} from '../utils.ts';
import type {AudioOutputOptions} from './options.ts';
import RemoteTrack from './RemoteTrack.ts';
import {Track} from './Track.ts';

interface ReactNativeVolumeTrack extends MediaStreamTrack {
	_setVolume(volume: number): void;
}

function supportsReactNativeVolume(track: MediaStreamTrack): track is ReactNativeVolumeTrack {
	return '_setVolume' in track && typeof track._setVolume === 'function';
}

export default class RemoteAudioTrack extends RemoteTrack<Track.Kind.Audio> {
	private prevStats?: AudioReceiverStats;

	private elementVolume: number | undefined;

	private audioContext?: AudioContext;

	private gainNode?: GainNode;

	private sourceNode?: MediaStreamAudioSourceNode;

	private webAudioPluginNodes: Array<AudioNode>;

	private sinkId?: string;

	constructor(
		mediaTrack: MediaStreamTrack,
		sid: string,
		receiver: RTCRtpReceiver,
		audioContext?: AudioContext,
		audioOutput?: AudioOutputOptions,
		loggerOptions?: LoggerOptions,
	) {
		super(mediaTrack, sid, Track.Kind.Audio, receiver, loggerOptions);
		this.audioContext = audioContext;
		this.webAudioPluginNodes = [];
		if (audioOutput) {
			this.sinkId = audioOutput.deviceId;
		}
	}

	setVolume(volume: number) {
		for (const el of this.attachedElements) {
			if (this.audioContext) {
				this.gainNode?.gain.setTargetAtTime(volume, 0, 0.1);
			} else {
				el.volume = volume;
			}
		}
		if (isReactNative() && supportsReactNativeVolume(this._mediaStreamTrack)) {
			this._mediaStreamTrack._setVolume(volume);
		}
		this.elementVolume = volume;
	}

	getVolume(): number {
		if (this.elementVolume !== undefined) {
			return this.elementVolume;
		}
		if (isReactNative()) {
			return 1.0;
		}
		let highestVolume = 0;
		this.attachedElements.forEach((element) => {
			if (element.volume > highestVolume) {
				highestVolume = element.volume;
			}
		});
		return highestVolume;
	}

	async setSinkId(deviceId: string) {
		this.sinkId = deviceId;
		await Promise.all(
			this.attachedElements.map((elm) => {
				if (!supportsSetSinkId(elm)) {
					return;
				}
				return elm.setSinkId(deviceId) as Promise<void>;
			}),
		);
	}

	override attach(): HTMLMediaElement;
	override attach(element: HTMLMediaElement): HTMLMediaElement;
	override attach(element?: HTMLMediaElement): HTMLMediaElement {
		const needsNewWebAudioConnection = this.attachedElements.length === 0;
		if (!element) {
			element = super.attach();
		} else {
			super.attach(element);
		}

		if (this.sinkId && supportsSetSinkId(element)) {
			element.setSinkId(this.sinkId).catch((e) => {
				this.log.error('Failed to set sink id on remote audio track', e, this.logContext);
			});
		}
		if (this.audioContext && needsNewWebAudioConnection) {
			this.log.debug('using audio context mapping', this.logContext);
			this.connectWebAudio(this.audioContext, element);
			element.volume = 0;
			element.muted = true;
		}

		if (this.elementVolume !== undefined) {
			this.setVolume(this.elementVolume);
		}

		return element;
	}

	override detach(): Array<HTMLMediaElement>;

	override detach(element: HTMLMediaElement): HTMLMediaElement;
	override detach(element?: HTMLMediaElement): HTMLMediaElement | Array<HTMLMediaElement> {
		let detached: HTMLMediaElement | Array<HTMLMediaElement>;
		if (!element) {
			detached = super.detach();
			this.disconnectWebAudio();
		} else {
			detached = super.detach(element);
			if (this.audioContext) {
				if (this.attachedElements.length > 0) {
					this.connectWebAudio(this.audioContext, this.attachedElements[0]);
				} else {
					this.disconnectWebAudio();
				}
			}
		}
		return detached;
	}

	setAudioContext(audioContext: AudioContext | undefined) {
		this.audioContext = audioContext;
		if (audioContext && this.attachedElements.length > 0) {
			this.connectWebAudio(audioContext, this.attachedElements[0]);
		} else if (!audioContext) {
			this.disconnectWebAudio();
		}
	}

	setWebAudioPlugins(nodes: Array<AudioNode>) {
		this.webAudioPluginNodes = nodes;
		if (this.attachedElements.length > 0 && this.audioContext) {
			this.connectWebAudio(this.audioContext, this.attachedElements[0]);
		}
	}

	private connectWebAudio(context: AudioContext, element: HTMLMediaElement) {
		this.disconnectWebAudio();
		this.sourceNode = context.createMediaStreamSource(element.srcObject as MediaStream);
		let lastNode: AudioNode = this.sourceNode;
		this.webAudioPluginNodes.forEach((node) => {
			lastNode.connect(node);
			lastNode = node;
		});
		this.gainNode = context.createGain();
		lastNode.connect(this.gainNode);
		this.gainNode.connect(context.destination);

		if (this.elementVolume !== undefined) {
			this.gainNode.gain.setTargetAtTime(this.elementVolume, 0, 0.1);
		}

		if (context.state !== 'running') {
			context
				.resume()
				.then(() => {
					if (context.state !== 'running') {
						this.emit(TrackEvent.AudioPlaybackFailed, new Error("Audio Context couldn't be started automatically"));
					}
				})
				.catch((e) => {
					this.emit(TrackEvent.AudioPlaybackFailed, e);
				});
		}
	}

	private disconnectWebAudio() {
		this.gainNode?.disconnect();
		this.sourceNode?.disconnect();
		this.gainNode = undefined;
		this.sourceNode = undefined;
	}

	protected monitorReceiver = async () => {
		if (!this.receiver) {
			this._currentBitrate = 0;
			return;
		}
		const stats = await this.getReceiverStats();

		if (stats && this.prevStats && this.receiver) {
			this._currentBitrate = computeBitrate(stats, this.prevStats);
		}

		this.prevStats = stats;
	};

	async getReceiverStats(): Promise<AudioReceiverStats | undefined> {
		if (!this.receiver || !this.receiver.getStats) {
			return;
		}

		const stats = await this.receiver.getStats();
		let receiverStats: AudioReceiverStats | undefined;
		stats.forEach((v) => {
			if (v.type === 'inbound-rtp') {
				receiverStats = {
					type: 'audio',
					streamId: v.id,
					timestamp: v.timestamp,
					jitter: v.jitter,
					bytesReceived: v.bytesReceived,
					concealedSamples: v.concealedSamples,
					concealmentEvents: v.concealmentEvents,
					silentConcealedSamples: v.silentConcealedSamples,
					silentConcealmentEvents: v.silentConcealmentEvents,
					totalAudioEnergy: v.totalAudioEnergy,
					totalSamplesDuration: v.totalSamplesDuration,
				};
			}
		});
		return receiverStats;
	}
}
