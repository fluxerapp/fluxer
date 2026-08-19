// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {createVoiceAudioContext} from '@app/features/voice/engine/VoiceSharedAudioContext';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {DeepFilterNoiseFilterProcessor} from 'deepfilternet3-noise-filter';
import type {LocalAudioTrack} from 'livekit-client';

const logger = new Logger('DeepFilterNoiseProcessor');
const DEEP_FILTER_MODEL_SAMPLE_RATE = 48000;
const DEFAULT_SUPPRESSION_LEVEL = 80;
const HIGH_PASS_FREQUENCY_HZ = 60;
const HIGH_PASS_Q = Math.SQRT1_2;
const LIMITER_THRESHOLD_DB = -3;
const LIMITER_KNEE_DB = 0;
const LIMITER_RATIO = 20;
const LIMITER_ATTACK_SEC = 0.003;
const LIMITER_RELEASE_SEC = 0.05;

let activeProcessor: DeepFilterNoiseFilterProcessor | null = null;
let activeTrack: LocalAudioTrack | null = null;

export function createDeepFilterProcessor(
	noiseReductionLevel = DEFAULT_SUPPRESSION_LEVEL,
): DeepFilterNoiseFilterProcessor {
	const clampedNoiseReductionLevel = Math.max(0, Math.min(100, noiseReductionLevel));
	return new DeepFilterNoiseFilterProcessor({
		sampleRate: DEEP_FILTER_MODEL_SAMPLE_RATE,
		noiseReductionLevel: clampedNoiseReductionLevel,
		enabled: true,
		assetConfig: {
			cdnUrl: `${RuntimeConfig.staticCdnEndpoint}/libs/deepfilternet3`,
		},
	});
}

export function resolveDeepFilterAudioContext(
	sourceContext: AudioContext,
	feedTrack: MediaStreamTrack,
): AudioContext | null {
	const modelRateContext = createVoiceAudioContext({
		latencyHint: 'interactive',
		sampleRate: DEEP_FILTER_MODEL_SAMPLE_RATE,
	});
	if (!modelRateContext) return null;
	if (modelRateContext.sampleRate === sourceContext.sampleRate) return modelRateContext;
	try {
		modelRateContext.createMediaStreamSource(new MediaStream([feedTrack])).disconnect();
		return modelRateContext;
	} catch (error) {
		logger.info('DeepFilter cannot read the capture graph at the model rate; running it at the capture rate', {
			modelSampleRate: modelRateContext.sampleRate,
			captureSampleRate: sourceContext.sampleRate,
			error,
		});
	}
	void modelRateContext.close().catch((error) => {
		logger.debug('Failed to close the rejected DeepFilter AudioContext', error);
	});
	return createVoiceAudioContext({latencyHint: 'interactive', sampleRate: sourceContext.sampleRate});
}

export interface DeepFilterAudioChain {
	processedTrack: MediaStreamTrack;
	inputDestination: MediaStreamAudioDestinationNode;
	dispose: () => Promise<void>;
}

function safeDisconnect(node: AudioNode | null | undefined): void {
	if (!node) return;
	try {
		node.disconnect();
	} catch {}
}

function safeStopTrack(track: MediaStreamTrack | null | undefined): void {
	if (!track) return;
	try {
		track.stop();
	} catch {}
}

export async function buildDeepFilterAudioChain(opts: {
	audioContext: AudioContext;
	noiseReductionLevel?: number;
}): Promise<DeepFilterAudioChain> {
	const {audioContext} = opts;
	const noiseReductionLevel = Math.max(
		0,
		Math.min(100, opts.noiseReductionLevel ?? VoiceSettings.getDeepFilterNoiseSuppressionLevel()),
	);
	const inputDestination = audioContext.createMediaStreamDestination();
	const inputTrack = inputDestination.stream.getAudioTracks()[0];
	if (!inputTrack) {
		throw new Error('buildDeepFilterAudioChain: missing input destination track');
	}
	const hpfSource = audioContext.createMediaStreamSource(new MediaStream([inputTrack]));
	const highPass = audioContext.createBiquadFilter();
	highPass.type = 'highpass';
	highPass.frequency.value = HIGH_PASS_FREQUENCY_HZ;
	highPass.Q.value = HIGH_PASS_Q;
	hpfSource.connect(highPass);
	const deepFilterFeedDestination = audioContext.createMediaStreamDestination();
	highPass.connect(deepFilterFeedDestination);
	const deepFilterFeedTrack = deepFilterFeedDestination.stream.getAudioTracks()[0];
	if (!deepFilterFeedTrack) {
		safeDisconnect(hpfSource);
		safeDisconnect(highPass);
		safeDisconnect(inputDestination);
		safeDisconnect(deepFilterFeedDestination);
		safeStopTrack(inputTrack);
		throw new Error('buildDeepFilterAudioChain: missing DeepFilter feed track');
	}
	const processor = createDeepFilterProcessor(noiseReductionLevel);
	processor.audioContext = resolveDeepFilterAudioContext(audioContext, deepFilterFeedTrack);
	const disposeDeepFilterInputGraph = () => {
		safeDisconnect(inputDestination);
		safeDisconnect(hpfSource);
		safeDisconnect(highPass);
		safeDisconnect(deepFilterFeedDestination);
		safeStopTrack(inputTrack);
		safeStopTrack(deepFilterFeedTrack);
	};
	try {
		await processor.init({track: deepFilterFeedTrack});
	} catch (error) {
		disposeDeepFilterInputGraph();
		try {
			await processor.destroy();
		} catch (destroyError) {
			logger.debug('DeepFilter destroy after init failure threw', destroyError);
		}
		throw error;
	}
	if (!processor.processedTrack) {
		disposeDeepFilterInputGraph();
		try {
			await processor.destroy();
		} catch (destroyError) {
			logger.debug('DeepFilter destroy after missing processedTrack threw', destroyError);
		}
		throw new Error('DeepFilter init produced no processedTrack');
	}
	const limiterSource = audioContext.createMediaStreamSource(new MediaStream([processor.processedTrack]));
	const limiter = audioContext.createDynamicsCompressor();
	limiter.threshold.value = LIMITER_THRESHOLD_DB;
	limiter.knee.value = LIMITER_KNEE_DB;
	limiter.ratio.value = LIMITER_RATIO;
	limiter.attack.value = LIMITER_ATTACK_SEC;
	limiter.release.value = LIMITER_RELEASE_SEC;
	limiterSource.connect(limiter);
	const outputDestination = audioContext.createMediaStreamDestination();
	limiter.connect(outputDestination);
	const processedTrack = outputDestination.stream.getAudioTracks()[0];
	if (!processedTrack) {
		disposeDeepFilterInputGraph();
		safeDisconnect(limiterSource);
		safeDisconnect(limiter);
		safeDisconnect(outputDestination);
		safeStopTrack(processor.processedTrack);
		try {
			await processor.destroy();
		} catch (destroyError) {
			logger.debug('DeepFilter destroy after missing output track threw', destroyError);
		}
		throw new Error('buildDeepFilterAudioChain: missing limiter output track');
	}
	let disposed = false;
	const dispose = async () => {
		if (disposed) return;
		disposed = true;
		safeDisconnect(inputDestination);
		safeDisconnect(hpfSource);
		safeDisconnect(highPass);
		safeDisconnect(deepFilterFeedDestination);
		safeDisconnect(limiterSource);
		safeDisconnect(limiter);
		safeDisconnect(outputDestination);
		try {
			await processor.destroy();
		} catch (error) {
			logger.warn('Failed to destroy DeepFilter processor in chain dispose', error);
		}
		safeStopTrack(inputTrack);
		safeStopTrack(deepFilterFeedTrack);
		safeStopTrack(processor.processedTrack);
		safeStopTrack(processedTrack);
	};
	return {
		processedTrack,
		inputDestination,
		dispose,
	};
}

export async function applyDeepFilterProcessor(
	track: LocalAudioTrack,
	noiseReductionLevel = VoiceSettings.getDeepFilterNoiseSuppressionLevel(),
): Promise<void> {
	if (!VoiceSettings.getDeepFilterNoiseSuppression()) {
		return;
	}
	try {
		await removeDeepFilterProcessor();
		const processor = createDeepFilterProcessor(noiseReductionLevel);
		await track.setProcessor(processor);
		activeTrack = track;
		activeProcessor = processor;
		logger.info('Applied DeepFilterNet3 noise suppression');
	} catch (error) {
		logger.warn('Failed to apply DeepFilterNet3 noise suppression', error);
		activeTrack = null;
		activeProcessor = null;
	}
}

export async function removeDeepFilterProcessor(track?: LocalAudioTrack): Promise<void> {
	if (activeProcessor) {
		const targetTrack = activeTrack ?? track;
		try {
			if (targetTrack) {
				await targetTrack.stopProcessor();
			}
		} catch (error) {
			logger.warn('Failed to stop DeepFilter processor', error);
		}
		try {
			await activeProcessor.destroy();
		} catch (error) {
			logger.warn('Failed to destroy DeepFilter processor', error);
		}
		activeTrack = null;
		activeProcessor = null;
		logger.debug('Removed DeepFilterNet3 noise suppression');
	}
}

export function setDeepFilterEnabled(enabled: boolean): void {
	if (activeProcessor) {
		const result = activeProcessor.setEnabled(enabled);
		void Promise.resolve(result).catch((error) => {
			logger.warn('Failed to set DeepFilter enabled state', error);
		});
		logger.debug('Set DeepFilter enabled', {enabled});
	}
}

export function isDeepFilterActive(): boolean {
	return activeProcessor != null;
}

export function isDeepFilterAppliedToTrack(track?: LocalAudioTrack | null): boolean {
	return activeProcessor != null && activeTrack === (track ?? null);
}
