// SPDX-License-Identifier: AGPL-3.0-or-later

import {createVoiceSoftClipNode} from '@app/features/voice/engine/VoiceSharedAudioContext';
import {type VoiceInputConfig, VoiceInputGraph} from '@app/features/voice/utils/VoiceInputProcessor';

export interface MicTestAudioGraph {
	analyser: AnalyserNode;
	outputGain: GainNode;
	softClipOutput: AudioNode;
	playbackTarget: AudioNode;
	configure: () => Promise<void>;
	dispose: () => void;
}

interface CreateMicTestAudioGraphOptions {
	signal: AbortSignal;
	source: MediaStreamAudioSourceNode;
	sourceTrack: MediaStreamTrack;
	resolveConfig: () => VoiceInputConfig;
	outputGain: number;
	playbackTarget: AudioNode;
	playbackDelaySeconds: number;
}

export function createMicTestAudioGraph({
	signal,
	source,
	sourceTrack,
	resolveConfig,
	outputGain,
	playbackTarget,
	playbackDelaySeconds,
}: CreateMicTestAudioGraphOptions): MicTestAudioGraph {
	signal.throwIfAborted();
	const audioContext = source.context as AudioContext;
	const inputGraph = new VoiceInputGraph(audioContext, 1, resolveConfig);
	inputGraph.setSourceNode(sourceTrack, source);
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
	inputGraph.output.connect(analyser);
	analyser.connect(delay);
	delay.connect(outputGainNode);
	if (softClip) outputGainNode.connect(softClip.input);
	softClipOutput.connect(playbackTarget);
	void inputGraph.configure();

	let disposed = false;
	const dispose = () => {
		if (disposed) return;
		disposed = true;
		signal.removeEventListener('abort', dispose);
		inputGraph.dispose();
		analyser.disconnect();
		delay.disconnect();
		outputGainNode.disconnect();
		softClipInput.disconnect();
		softClipOutput.disconnect();
	};
	signal.addEventListener('abort', dispose, {once: true});

	return {
		analyser,
		outputGain: outputGainNode,
		softClipOutput,
		playbackTarget,
		configure: () => inputGraph.configure(),
		dispose,
	};
}
