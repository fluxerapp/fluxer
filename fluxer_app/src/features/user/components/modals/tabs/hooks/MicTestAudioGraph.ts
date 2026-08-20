// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {createVoiceSoftClipNode} from '@app/features/voice/engine/VoiceSharedAudioContext';
import {buildDeepFilterAudioChain, type DeepFilterAudioChain} from '@app/features/voice/utils/DeepFilterNoiseProcessor';

const logger = new Logger('MicTestAudioGraph');

export interface MicTestAudioGraph {
	source: MediaStreamAudioSourceNode;
	analyser: AnalyserNode;
	inputGain: GainNode;
	delay: DelayNode;
	outputGain: GainNode;
	softClipInput: GainNode;
	softClipOutput: AudioNode;
	playbackTarget: AudioNode;
	dispose: () => Promise<void>;
}

interface CreateMicTestAudioGraphOptions {
	audioContext: AudioContext;
	sourceTrack: MediaStreamTrack;
	inputGain: number;
	outputGain: number;
	playbackTarget: AudioNode;
	playbackDelaySeconds: number;
	deepFilter: boolean;
	deepFilterNoiseReductionLevel: number;
}

export async function createMicTestAudioGraph({
	audioContext,
	sourceTrack,
	inputGain,
	outputGain,
	playbackTarget,
	playbackDelaySeconds,
	deepFilter,
	deepFilterNoiseReductionLevel,
}: CreateMicTestAudioGraphOptions): Promise<MicTestAudioGraph> {
	const source = audioContext.createMediaStreamSource(new MediaStream([sourceTrack]));
	const inputGainNode = audioContext.createGain();
	inputGainNode.gain.value = inputGain;
	source.connect(inputGainNode);

	let deepFilterChain: DeepFilterAudioChain | null = null;
	let passthroughDestination: MediaStreamAudioDestinationNode | null = null;
	let processedTrack: MediaStreamTrack | null = null;
	try {
		if (deepFilter) {
			deepFilterChain = await buildDeepFilterAudioChain({
				audioContext,
				noiseReductionLevel: deepFilterNoiseReductionLevel,
			});
			inputGainNode.connect(deepFilterChain.inputDestination);
			processedTrack = deepFilterChain.processedTrack;
		} else {
			passthroughDestination = audioContext.createMediaStreamDestination();
			inputGainNode.connect(passthroughDestination);
			processedTrack = passthroughDestination.stream.getAudioTracks()[0] ?? null;
		}
		if (!processedTrack) {
			throw new Error('Mic test graph produced no processed audio track');
		}
	} catch (error) {
		inputGainNode.disconnect();
		source.disconnect();
		passthroughDestination?.disconnect();
		passthroughDestination?.stream.getTracks().forEach((track) => track.stop());
		if (deepFilterChain) {
			await deepFilterChain.dispose().catch((disposeError) => {
				logger.debug('Failed to dispose DeepFilter chain after mic test graph init failure', disposeError);
			});
		}
		throw error;
	}

	const analyser = audioContext.createAnalyser();
	const delay = audioContext.createDelay(Math.max(1, playbackDelaySeconds + 0.25));
	const outputGainNode = audioContext.createGain();
	analyser.fftSize = 2048;
	analyser.smoothingTimeConstant = 0.2;
	delay.delayTime.value = playbackDelaySeconds;
	outputGainNode.gain.value = outputGain;
	const softClip = createVoiceSoftClipNode(audioContext);
	const softClipInput = softClip?.input ?? outputGainNode;
	const softClipOutput: AudioNode = softClip?.output ?? outputGainNode;
	inputGainNode.connect(analyser);
	analyser.connect(delay);
	delay.connect(outputGainNode);
	if (softClip) outputGainNode.connect(softClip.input);
	softClipOutput.connect(playbackTarget);

	const dispose = async () => {
		source.disconnect();
		analyser.disconnect();
		inputGainNode.disconnect();
		delay.disconnect();
		outputGainNode.disconnect();
		softClipInput.disconnect();
		softClipOutput.disconnect();
		passthroughDestination?.disconnect();
		passthroughDestination?.stream.getTracks().forEach((track) => track.stop());
		if (deepFilterChain) {
			await deepFilterChain.dispose().catch((error) => {
				logger.warn('Failed to dispose DeepFilter chain for mic test graph', error);
			});
		}
	};

	return {
		source,
		analyser,
		inputGain: inputGainNode,
		delay,
		outputGain: outputGainNode,
		softClipInput,
		softClipOutput,
		playbackTarget,
		dispose,
	};
}
