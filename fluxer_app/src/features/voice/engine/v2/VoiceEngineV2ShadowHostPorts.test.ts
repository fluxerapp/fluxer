// SPDX-License-Identifier: AGPL-3.0-or-later

import {createVoiceEngineV2AppTestControllerHost} from '@app/features/voice/engine/v2/VoiceEngineV2AppControllerHostTestUtils';
import {
	createVoiceEngineV2ShadowHostPorts,
	type VoiceEngineV2ShadowHostPortCall,
} from '@app/features/voice/engine/v2/VoiceEngineV2ShadowHostPorts';
import {waitForRuntime} from '@fluxer/voice_engine_v2/testing';
import {describe, expect, it} from 'vitest';

describe('VoiceEngineV2ShadowHostPorts', () => {
	it('records v2 command intent while returning deterministic host results', async () => {
		const calls: Array<VoiceEngineV2ShadowHostPortCall> = [];
		const host = createVoiceEngineV2AppTestControllerHost({
			ports: createVoiceEngineV2ShadowHostPorts({
				record(call) {
					calls.push(call);
				},
			}),
		});

		host.controller.connect({url: 'wss://voice.example.test', token: 'token'});
		await waitForRuntime();
		host.controller.checkPermission('microphone');
		await waitForRuntime();
		host.controller.enumerateDevices();
		await waitForRuntime();
		host.controller.queryHardwareEncoderCapabilities();
		await waitForRuntime();
		host.controller.collectStats();
		await waitForRuntime();

		expect(calls.map((call) => call.type)).toEqual([
			'liveKit.connect',
			'permissions.check',
			'devices.enumerate',
			'capabilities.getHardwareEncoderCapabilities',
			'liveKit.collectStats',
		]);
		expect(host.model.connection.connected).toBe(true);
		expect(host.model.permissions.microphone).toEqual({
			name: 'microphone',
			status: 'unknown',
			canPrompt: true,
		});
		expect(host.model.devices).toEqual({
			audioInputs: [],
			audioOutputs: [],
			cameras: [],
			selectedAudioInputId: null,
			selectedAudioOutputId: null,
			selectedCameraId: null,
		});
		expect(host.model.stats).toEqual({rttMs: null, outbound: [], inbound: []});
		expect(host.snapshot.hardwareEncoder.capabilities).toMatchObject({
			available: false,
			backend: 'none',
			reason: 'shadow-host',
		});

		host.dispose();
	});
});
