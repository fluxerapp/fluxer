// SPDX-License-Identifier: AGPL-3.0-or-later

import {VoiceEngineV2HostPortImplementation} from '@fluxer/voice_engine_v2/src/implementations/VoiceEngineV2HostPortImplementation';
import type {VoiceEngineV2HostPorts} from '@fluxer/voice_engine_v2/src/ports';
import {createVoiceEngineV2MemoryEventLogSpillSink} from '@fluxer/voice_engine_v2/src/runtime/eventLogRing';
import {VoiceEngineV2Runtime} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Runtime';
import type {VoiceEngineV2ConformanceSubject} from '@fluxer/voice_engine_v2/src/testing/conformance';
import {runVoiceEngineV2ConformanceSuite, waitForRuntime} from '@fluxer/voice_engine_v2/src/testing/conformance';
import {FakeVoiceEngineV2Driver} from '@fluxer/voice_engine_v2/src/testing/FakeVoiceEngineV2Driver';
import {describe, expect, it} from 'vitest';

function createConformanceHostPorts(driver: FakeVoiceEngineV2Driver): VoiceEngineV2HostPorts {
	return {
		media: {
			prewarm: () => driver.prewarm(),
			connect: (options) => driver.connect(options),
			disconnect: (reason) => driver.disconnect(reason),
			publishMicrophone: (options) => driver.publishMicrophone(options),
			unpublishMicrophone: () => driver.unpublishMicrophone(),
			setMicrophoneEnabled: (enabled) => driver.setMicrophoneEnabled(enabled),
			publishCamera: (options) => driver.publishCamera(options),
			updateCameraEncoding: (options) => driver.updateCameraEncoding(options),
			unpublishCamera: () => driver.unpublishCamera(),
			publishScreen: (options) => driver.publishScreen(options),
			updateScreenEncoding: (options) => driver.updateScreenEncoding(options),
			unpublishScreen: () => driver.unpublishScreen(),
			publishScreenAudio: (options) => driver.publishScreenAudio(options),
			unpublishScreenAudio: () => driver.unpublishScreenAudio(),
			setOutputDevice: (options) => driver.setOutputDevice(options),
			publishData: (options) => driver.publishData(options),
		},
		subscriptions: {
			setParticipantVolume: (options) => driver.setParticipantVolume(options),
			setRemoteTrackSubscription: (options) => driver.setRemoteTrackSubscription(options),
		},
		stats: {
			collectStats: () => driver.collectStats(),
		},
	};
}

function createHostPortSubject(): VoiceEngineV2ConformanceSubject {
	const driver = new FakeVoiceEngineV2Driver();
	return {driver, implementation: new VoiceEngineV2HostPortImplementation(createConformanceHostPorts(driver))};
}

runVoiceEngineV2ConformanceSuite('host-port native', createHostPortSubject);

describe('VoiceEngineV2HostPortImplementation', () => {
	it('reports an unsupported-capability failure when the host port is absent', async () => {
		const runtime = new VoiceEngineV2Runtime(new VoiceEngineV2HostPortImplementation({}), {
			eventLogSpillSink: createVoiceEngineV2MemoryEventLogSpillSink(),
			verifyEventLogInvariantsOnDispatch: true,
		});
		const failures: Array<string> = [];
		runtime.subscribe(({event}) => {
			if (event.type === 'connection.connectFailed') failures.push(event.error.code);
		});

		runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}});
		await waitForRuntime();

		expect(failures).toEqual(['unsupportedCapability']);
	});
});
