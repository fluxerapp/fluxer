// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import Keybind from '@app/features/input/state/InputKeybind';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	resolveVoiceActivityGateState,
	resolveVoiceActivityThresholdRms,
	updateVoiceActivityNoiseFloorRms,
} from '@app/features/voice/engine/VoiceLocalSpeakingGate';
import {SPEAKING_LOCAL_RELEASE_MS} from '@app/features/voice/engine/VoiceSpeakingThreshold';
import {computeSpeakingDetectorRms} from '@app/features/voice/engine/v2/VoiceEngineV2AppMicrophoneTransaction';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {buildDeepFilterAudioChain, type DeepFilterAudioChain} from '@app/features/voice/utils/DeepFilterNoiseProcessor';
import {getNoiseSuppressionBackendDescriptor} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionBackends';
import {
	buildNoiseSuppressionWorkletChain,
	type NoiseSuppressionWorkletChain,
} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionChain';
import {readEffectiveNoiseSuppression} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionRuntime';
import {applyNoiseSuppressionOverride} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionSelection';
import type {NoiseSuppressionWorkletBackend} from '@app/features/voice/utils/noise_suppression/NoiseSuppressionWorkletTypes';
import {
	getActiveInputDeviceLabel,
	type ResolvedVoiceProcessing,
	resolveVoiceProcessingFromStateForDeviceLabel,
} from '@app/features/voice/utils/VoiceProcessingProfile';
import {inputVoiceVolumePercentToGain} from '@app/features/voice/utils/VoiceVolumeUtils';
import type {AudioProcessorOptions, LocalAudioTrack, ProcessorOptions, Track, TrackProcessor} from 'livekit-client';

const logger = new Logger('VoiceInputProcessor');
const GATE_ANALYSER_FFT_SIZE = 256;
const GATE_TICK_INTERVAL_MS = 50;
const GATE_ATTACK_TIME_CONSTANT = 0.005;
const GATE_RELEASE_TIME_CONSTANT = 0.02;
const DEFAULT_NOISE_SUPPRESSION_PROBE_SAMPLE_RATE = 48000;

class VoiceInputTrackProcessor implements TrackProcessor<Track.Kind.Audio> {
	name = 'fluxer-voice-input-processor';
	processedTrack?: MediaStreamTrack;
	private sourceNode: MediaStreamAudioSourceNode | null = null;
	private gainNode: GainNode | null = null;
	private passthroughDestination: MediaStreamAudioDestinationNode | null = null;
	private deepFilterChain: DeepFilterAudioChain | null = null;
	private workletChain: NoiseSuppressionWorkletChain | null = null;
	private buildGeneration = 0;
	private pendingBuild: AbortController | null = null;
	private cancelled = false;
	private audioContext: AudioContext | null = null;
	private gateNode: GainNode | null = null;
	private gateAnalyserNode: AnalyserNode | null = null;
	private gateSamples: Uint8Array<ArrayBuffer> | null = null;
	private gateTimerId: number | null = null;
	private gateOpen = false;
	private gateSilenceStartedAtMs: number | null = null;
	private gateNoiseFloorRms = 0;

	constructor(
		private inputVolumePercent: number,
		private deepFilterEnabled: boolean,
		private deepFilterNoiseReductionLevel: number,
		private gateEnabled: boolean,
		private workletBackend: NoiseSuppressionWorkletBackend | null,
		private suppressionStrength: number,
	) {}

	matchesMode(
		deepFilterEnabled: boolean,
		deepFilterNoiseReductionLevel: number,
		gateEnabled: boolean,
		workletBackend: NoiseSuppressionWorkletBackend | null,
		suppressionStrength: number,
	): boolean {
		return (
			this.deepFilterEnabled === deepFilterEnabled &&
			this.deepFilterNoiseReductionLevel === deepFilterNoiseReductionLevel &&
			this.gateEnabled === gateEnabled &&
			this.workletBackend === workletBackend &&
			this.suppressionStrength === suppressionStrength
		);
	}

	updateInputVolumePercent(nextPercent: number): void {
		this.inputVolumePercent = nextPercent;
		if (this.gainNode) {
			this.gainNode.gain.value = inputVoiceVolumePercentToGain(nextPercent);
		}
	}

	async init(opts: ProcessorOptions<Track.Kind.Audio>): Promise<void> {
		const audioContext = opts.audioContext ?? this.audioContext;
		assert.ok(audioContext, 'Voice input processor requires an audio context before initialization');
		this.audioContext = audioContext;
		await this.rebuild({...opts, audioContext});
	}

	async restart(opts: ProcessorOptions<Track.Kind.Audio>): Promise<void> {
		await this.init(opts);
	}

	async destroy(): Promise<void> {
		this.cancelPendingBuild();
		await this.teardown();
	}

	cancelPendingBuild(): void {
		this.cancelled = true;
		this.buildGeneration++;
		this.pendingBuild?.abort();
	}

	private async rebuild(opts: AudioProcessorOptions): Promise<void> {
		if (this.cancelled) throw new DOMException('Voice input build cancelled', 'AbortError');
		const teardown = this.teardown();
		const generation = this.buildGeneration;
		await teardown;
		if (generation !== this.buildGeneration) throw new DOMException('Voice input build cancelled', 'AbortError');
		const controller = new AbortController();
		this.pendingBuild = controller;
		try {
			this.sourceNode = opts.audioContext.createMediaStreamSource(new MediaStream([opts.track]));
			this.gainNode = opts.audioContext.createGain();
			this.gainNode.gain.value = inputVoiceVolumePercentToGain(this.inputVolumePercent);
			this.sourceNode.connect(this.gainNode);
			const chainTail = this.gateEnabled ? this.startVoiceActivityGate(opts.audioContext) : this.gainNode;
			if (this.workletBackend != null) {
				const chain = await this.buildWorkletChain(opts, this.workletBackend, generation, controller.signal);
				if (generation !== this.buildGeneration) {
					await chain?.dispose();
					throw new DOMException('Voice input build cancelled', 'AbortError');
				}
				if (chain) {
					this.workletChain = chain;
					chainTail.connect(chain.inputDestination);
					this.processedTrack = chain.processedTrack;
					return;
				}
			}
			if (this.deepFilterEnabled) {
				const chain = await buildDeepFilterAudioChain({
					audioContext: opts.audioContext,
					noiseReductionLevel: this.deepFilterNoiseReductionLevel,
				});
				if (generation !== this.buildGeneration) {
					await chain.dispose();
					throw new DOMException('Voice input build cancelled', 'AbortError');
				}
				this.deepFilterChain = chain;
				chainTail.connect(chain.inputDestination);
				this.processedTrack = chain.processedTrack;
				return;
			}
			this.passthroughDestination = opts.audioContext.createMediaStreamDestination();
			chainTail.connect(this.passthroughDestination);
			const passthroughTrack = this.passthroughDestination.stream.getAudioTracks()[0];
			if (!passthroughTrack) {
				throw new Error('Voice input processor produced no passthrough output track');
			}
			this.processedTrack = passthroughTrack;
		} catch (error) {
			if (generation === this.buildGeneration) await this.teardown();
			throw error;
		} finally {
			if (this.pendingBuild === controller) this.pendingBuild = null;
		}
	}

	private async buildWorkletChain(
		opts: AudioProcessorOptions,
		backend: NoiseSuppressionWorkletBackend,
		generation: number,
		signal: AbortSignal,
	): Promise<NoiseSuppressionWorkletChain | null> {
		try {
			return await buildNoiseSuppressionWorkletChain({
				audioContext: opts.audioContext,
				backend,
				suppressionStrength: this.suppressionStrength,
				signal,
				onRuntimeFailure: (error) => {
					if (generation !== this.buildGeneration) return;
					if (!ownsVoiceInputProcessor(this)) return;
					logger.warn('Noise suppression worklet failed while running; rebuilding without it', {backend, error});
					markNoiseSuppressionBackendFailed(backend);
					void restartVoiceInputProcessorAfterWorkletFailure(this);
				},
			});
		} catch (error) {
			if (generation !== this.buildGeneration) throw error;
			if (signal.aborted) throw error;
			logger.warn('Noise suppression worklet chain build failed; continuing without suppression', {backend, error});
			markNoiseSuppressionBackendFailed(backend);
			return null;
		}
	}

	private startVoiceActivityGate(audioContext: BaseAudioContext): AudioNode {
		const gainNode = this.gainNode;
		if (!gainNode || !this.sourceNode) {
			throw new Error('Voice input processor gate requires a built input chain');
		}
		const gateNode = audioContext.createGain();
		gateNode.gain.value = 0;
		gainNode.connect(gateNode);
		const analyserNode = audioContext.createAnalyser();
		analyserNode.fftSize = GATE_ANALYSER_FFT_SIZE;
		this.sourceNode.connect(analyserNode);
		this.gateNode = gateNode;
		this.gateAnalyserNode = analyserNode;
		this.gateSamples = new Uint8Array(analyserNode.fftSize);
		this.gateOpen = false;
		this.gateSilenceStartedAtMs = null;
		this.gateNoiseFloorRms = 0;
		this.gateTimerId = window.setInterval(() => {
			this.tickVoiceActivityGate(audioContext);
		}, GATE_TICK_INTERVAL_MS);
		return gateNode;
	}

	private tickVoiceActivityGate(audioContext: BaseAudioContext): void {
		const analyserNode = this.gateAnalyserNode;
		const samples = this.gateSamples;
		const gateNode = this.gateNode;
		if (!analyserNode || !samples || !gateNode) return;
		analyserNode.getByteTimeDomainData(samples);
		const rms = computeSpeakingDetectorRms(samples);
		const next = resolveVoiceActivityGateState({
			rms,
			thresholdRms: resolveVoiceActivityThresholdRms({
				autoSensitivity: VoiceSettings.getVadAutoSensitivity(),
				vadThreshold: VoiceSettings.getVadThreshold(),
				noiseFloorRms: this.gateNoiseFloorRms,
			}),
			nowMs: performance.now(),
			silenceStartedAtMs: this.gateSilenceStartedAtMs,
			gateOpen: this.gateOpen,
			releaseDelayMs: SPEAKING_LOCAL_RELEASE_MS,
		});
		this.gateSilenceStartedAtMs = next.silenceStartedAtMs;
		if (!next.open) {
			this.gateNoiseFloorRms = updateVoiceActivityNoiseFloorRms(this.gateNoiseFloorRms, rms);
		}
		if (next.open === this.gateOpen) return;
		this.gateOpen = next.open;
		gateNode.gain.setTargetAtTime(
			next.open ? 1 : 0,
			audioContext.currentTime,
			next.open ? GATE_ATTACK_TIME_CONSTANT : GATE_RELEASE_TIME_CONSTANT,
		);
	}

	private async teardown(): Promise<void> {
		this.buildGeneration++;
		this.pendingBuild?.abort();
		this.pendingBuild = null;
		if (this.gateTimerId !== null) {
			window.clearInterval(this.gateTimerId);
			this.gateTimerId = null;
		}
		this.sourceNode?.disconnect();
		this.gainNode?.disconnect();
		this.gateNode?.disconnect();
		this.gateAnalyserNode?.disconnect();
		this.passthroughDestination?.disconnect();
		const workletChain = this.workletChain;
		const deepFilterChain = this.deepFilterChain;
		const processedTrack = this.processedTrack;
		this.sourceNode = null;
		this.gainNode = null;
		this.gateNode = null;
		this.gateAnalyserNode = null;
		this.gateSamples = null;
		this.gateOpen = false;
		this.gateSilenceStartedAtMs = null;
		this.gateNoiseFloorRms = 0;
		this.passthroughDestination = null;
		this.deepFilterChain = null;
		this.workletChain = null;
		this.processedTrack = undefined;
		if (workletChain) {
			try {
				await workletChain.dispose();
			} catch (error) {
				logger.warn('Failed to dispose noise suppression worklet chain for voice input', error);
			}
		}
		if (deepFilterChain) {
			try {
				await deepFilterChain.dispose();
			} catch (error) {
				logger.warn('Failed to dispose DeepFilter chain for voice input', error);
			}
		}
		if (processedTrack && processedTrack.readyState !== 'ended') {
			try {
				processedTrack.stop();
			} catch (error) {
				logger.warn('Failed to stop processed voice input track', error);
			}
		}
	}
}

interface VoiceInputProcessorBinding {
	track: LocalAudioTrack;
	processor: VoiceInputTrackProcessor;
}

let activeTrack: LocalAudioTrack | null = null;
let activeProcessor: VoiceInputTrackProcessor | null = null;
let pendingProcessor: VoiceInputProcessorBinding | null = null;
let desiredTrack: LocalAudioTrack | null = null;
let synchronizationGeneration = 0;
const failedWorkletBackends = new Set<NoiseSuppressionWorkletBackend>();

function ownsVoiceInputProcessor(processor: VoiceInputTrackProcessor): boolean {
	return processor === activeProcessor || processor === pendingProcessor?.processor;
}

function markNoiseSuppressionBackendFailed(backend: NoiseSuppressionWorkletBackend): void {
	failedWorkletBackends.add(backend);
}

let lastSeenNoiseSuppressionConfigVersion: number | null = null;

function forgetFailedBackendsOnConfigChange(configVersion: number): void {
	if (lastSeenNoiseSuppressionConfigVersion === configVersion) return;
	lastSeenNoiseSuppressionConfigVersion = configVersion;
	failedWorkletBackends.clear();
}

async function restartVoiceInputProcessorAfterWorkletFailure(processor: VoiceInputTrackProcessor): Promise<void> {
	const track = processor === activeProcessor ? activeTrack : pendingProcessor?.track;
	if (!track) return;
	try {
		const removal = removeVoiceInputProcessor(track);
		const generation = synchronizationGeneration;
		await removal;
		if (generation !== synchronizationGeneration) return;
		await syncVoiceInputProcessor(track);
	} catch (error) {
		logger.warn('Failed to rebuild the voice input processor after a noise suppression failure', error);
	}
}

function resolveActiveVoiceProcessing(sampleRate?: number): ResolvedVoiceProcessing {
	const label = getActiveInputDeviceLabel(VoiceSettings);
	const profile = resolveVoiceProcessingFromStateForDeviceLabel(VoiceSettings, label);
	return applyNoiseSuppressionOverride(
		profile,
		readEffectiveNoiseSuppression(sampleRate ?? DEFAULT_NOISE_SUPPRESSION_PROBE_SAMPLE_RATE),
	);
}

function resolveWorkletBackend(profile: ResolvedVoiceProcessing): NoiseSuppressionWorkletBackend | null {
	const descriptor = getNoiseSuppressionBackendDescriptor(profile.noiseSuppressionBackend);
	if (descriptor.engine !== 'worklet') return null;
	const backend = profile.noiseSuppressionBackend as NoiseSuppressionWorkletBackend;
	return failedWorkletBackends.has(backend) ? null : backend;
}

export function isVoiceActivityGateEnabled(): boolean {
	if (Keybind.transmitMode !== 'voice_activity') return false;
	if (Keybind.isPushToMuteEffective()) return false;
	return resolveActiveVoiceProcessing().mode !== 'studio';
}

function shouldUseVoiceInputProcessor(): boolean {
	const profile = resolveActiveVoiceProcessing();
	return (
		profile.deepFilter ||
		resolveWorkletBackend(profile) != null ||
		VoiceSettings.getInputVolume() !== 100 ||
		isVoiceActivityGateEnabled()
	);
}

export async function syncVoiceInputProcessor(track: LocalAudioTrack | null): Promise<void> {
	const generation = ++synchronizationGeneration;
	desiredTrack = track;
	if (!track) {
		await stopCurrentVoiceInputProcessors();
		return;
	}
	const effective = readEffectiveNoiseSuppression(DEFAULT_NOISE_SUPPRESSION_PROBE_SAMPLE_RATE);
	forgetFailedBackendsOnConfigChange(effective.configVersion);
	const profile = resolveActiveVoiceProcessing();
	const deepFilterEnabled = profile.deepFilter;
	const deepFilterNoiseReductionLevel = profile.deepFilterNoiseReductionLevel;
	const workletBackend = resolveWorkletBackend(profile);
	const suppressionStrength = effective.suppressionStrength;
	const inputVolumePercent = VoiceSettings.getInputVolume();
	const gateEnabled = isVoiceActivityGateEnabled();
	if (!shouldUseVoiceInputProcessor()) {
		await stopCurrentVoiceInputProcessors();
		return;
	}
	if (
		activeTrack === track &&
		activeProcessor?.matchesMode(
			deepFilterEnabled,
			deepFilterNoiseReductionLevel,
			gateEnabled,
			workletBackend,
			suppressionStrength,
		)
	) {
		activeProcessor.updateInputVolumePercent(inputVolumePercent);
		return;
	}
	await stopCurrentVoiceInputProcessors();
	if (generation !== synchronizationGeneration) return;
	const processor = new VoiceInputTrackProcessor(
		inputVolumePercent,
		deepFilterEnabled,
		deepFilterNoiseReductionLevel,
		gateEnabled,
		workletBackend,
		suppressionStrength,
	);
	try {
		if (!(await installVoiceInputProcessor({track, processor}, generation))) return;
	} catch (error) {
		logger.warn('Voice input processor install failed; publication remains on raw mic track', {
			error,
			deepFilterEnabled,
			workletBackend,
			inputVolumePercent,
		});
		throw error;
	}
	logger.debug('Applied voice input processor', {
		inputVolumePercent,
		deepFilterEnabled,
		gateEnabled,
		workletBackend,
	});
}

async function installVoiceInputProcessor(binding: VoiceInputProcessorBinding, generation: number): Promise<boolean> {
	const {track, processor} = binding;
	pendingProcessor = binding;
	try {
		await track.setProcessor(processor);
	} catch (error) {
		try {
			await processor.destroy();
		} catch (destroyError) {
			logger.debug('Failed to destroy voice input processor after install failure', destroyError);
		}
		if (generation !== synchronizationGeneration) return false;
		throw error;
	} finally {
		if (pendingProcessor === binding) pendingProcessor = null;
	}
	if (generation !== synchronizationGeneration) {
		await stopVoiceInputProcessor(binding);
		return false;
	}
	activeTrack = track;
	activeProcessor = processor;
	return true;
}

export function updateVoiceInputGain(track: LocalAudioTrack | null): void {
	if (!shouldUseVoiceInputProcessor()) {
		void removeVoiceInputProcessor(track);
		return;
	}
	if (activeTrack === track && activeProcessor) {
		activeProcessor.updateInputVolumePercent(VoiceSettings.getInputVolume());
		return;
	}
	void syncVoiceInputProcessor(track);
}

export async function removeVoiceInputProcessor(track?: LocalAudioTrack | null): Promise<void> {
	if (track != null && desiredTrack !== track) return;
	synchronizationGeneration++;
	desiredTrack = null;
	await stopCurrentVoiceInputProcessors();
}

async function stopVoiceInputProcessor({track, processor}: VoiceInputProcessorBinding): Promise<void> {
	processor.cancelPendingBuild();
	try {
		if (await track.stopProcessorIfCurrent(processor)) return;
	} catch (error) {
		logger.warn('Failed to stop voice input processor', error);
	}
	try {
		await processor.destroy();
	} catch (error) {
		logger.warn('Failed to destroy voice input processor after stop failure', error);
	}
}

async function stopCurrentVoiceInputProcessors(): Promise<void> {
	const pending = pendingProcessor;
	const active = activeTrack && activeProcessor ? {track: activeTrack, processor: activeProcessor} : null;
	pendingProcessor = null;
	activeTrack = null;
	activeProcessor = null;
	if (pending) await stopVoiceInputProcessor(pending);
	if (active) await stopVoiceInputProcessor(active);
}
