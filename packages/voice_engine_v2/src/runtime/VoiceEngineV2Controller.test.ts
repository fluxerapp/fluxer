// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	VoiceEngineV2CommandResult,
	VoiceEngineV2Implementation,
} from '@fluxer/voice_engine_v2/src/implementations';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol';
import {createVoiceEngineV2MemoryEventLogSpillSink} from '@fluxer/voice_engine_v2/src/runtime/eventLogRing';
import {VoiceEngineV2Controller} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Controller';
import {VoiceEngineV2Runtime} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Runtime';
import {
	FakeVoiceEngineV2Driver,
	VoiceEngineV2TestImplementation,
	waitForRuntime,
} from '@fluxer/voice_engine_v2/src/testing';
import {describe, expect, it} from 'vitest';

interface DeferredCommand {
	command: VoiceEngineV2Command;
	resolve(result?: VoiceEngineV2CommandResult): void;
}

class DeferredVoiceEngineV2Implementation implements VoiceEngineV2Implementation {
	readonly kind = 'js' as const;
	readonly started: Array<VoiceEngineV2Command> = [];
	readonly deferred: Array<DeferredCommand> = [];

	execute(command: VoiceEngineV2Command): Promise<VoiceEngineV2CommandResult> {
		this.started.push(command);
		if (command.type !== 'microphone.publish') return Promise.resolve({ok: true});
		return new Promise<VoiceEngineV2CommandResult>((resolve) => {
			this.deferred.push({
				command,
				resolve: (result = {ok: true}) => {
					resolve(result);
				},
			});
		});
	}
}

interface ControllerRuntimeTestOptions {
	clock?: {now(): number};
}

function createControllerTestRuntime(
	implementation: VoiceEngineV2Implementation,
	options: ControllerRuntimeTestOptions = {},
): VoiceEngineV2Runtime {
	return new VoiceEngineV2Runtime(implementation, {
		eventLogSpillSink: createVoiceEngineV2MemoryEventLogSpillSink(),
		verifyEventLogInvariantsOnDispatch: true,
		...options,
	});
}

describe('VoiceEngineV2Controller', () => {
	it('exposes explicit methods over the v2 event protocol', async () => {
		const driver = new FakeVoiceEngineV2Driver();
		const controller = new VoiceEngineV2Controller(
			createControllerTestRuntime(new VoiceEngineV2TestImplementation(driver)),
		);

		controller.connect({url: 'wss://voice', token: 'token'});
		await waitForRuntime();
		controller.publishMicrophone({deviceId: 'default'});
		controller.setMicrophoneEnabled(false);
		await waitForRuntime();

		expect(controller.model.connection.connected).toBe(true);
		expect(controller.model.media.microphone).toBe('published');
		expect(driver.calls).toEqual([
			{type: 'connect', options: {url: 'wss://voice', token: 'token'}},
			{type: 'publishMicrophone', options: {deviceId: 'default'}},
			{type: 'setMicrophoneEnabled', enabled: false},
		]);
	});

	it('records an append-only event log with the host clock', () => {
		let now = 1000;
		const driver = new FakeVoiceEngineV2Driver();
		const runtime = createControllerTestRuntime(new VoiceEngineV2TestImplementation(driver), {
			clock: {
				now: () => {
					now += 10;
					return now;
				},
			},
		});

		runtime.dispatch({type: 'implementation.prewarmRequested'});

		expect(runtime.eventLog).toHaveLength(1);
		expect(runtime.eventLog[0]).toMatchObject({
			sequence: 1,
			atMs: 1010,
			event: {type: 'implementation.prewarmRequested'},
			commands: [{type: 'implementation.prewarm', operationId: 1}],
		});
	});

	it('queries hardware encoder capabilities through the v2 command pipeline', async () => {
		const driver = new FakeVoiceEngineV2Driver({
			hardwareEncoderCapabilities: {
				available: true,
				backend: 'nvenc',
				compiled: true,
				runtime: true,
				codecs: ['h264'],
				zeroCopy: true,
				nativeInputs: ['d3d11-texture'],
			},
		});
		const controller = new VoiceEngineV2Controller(
			createControllerTestRuntime(new VoiceEngineV2TestImplementation(driver)),
		);

		controller.queryHardwareEncoderCapabilities();
		await waitForRuntime();

		expect(driver.calls).toEqual([{type: 'getHardwareEncoderCapabilities'}]);
		expect(controller.snapshot.hardwareEncoder.capabilities).toMatchObject({
			available: true,
			backend: 'nvenc',
			zeroCopy: true,
		});
	});

	it('serializes commands by resource and rejects stale completions', async () => {
		const implementation = new DeferredVoiceEngineV2Implementation();
		const runtime = createControllerTestRuntime(implementation);

		runtime.dispatch({type: 'connection.connectRequested', options: {url: 'wss://voice', token: 'token'}});
		await waitForRuntime();
		runtime.dispatch({type: 'microphone.publishRequested', options: {deviceId: 'first'}});
		await waitForRuntime();
		runtime.dispatch({type: 'microphone.unpublishRequested'});
		await waitForRuntime();

		expect(implementation.started.map((command) => command.type)).toEqual(['connection.connect', 'microphone.publish']);
		expect(runtime.commandQueue.map((command) => command.type)).toEqual(['microphone.unpublish']);

		implementation.deferred[0]?.resolve();
		await waitForRuntime();

		expect(implementation.started.map((command) => command.type)).toEqual([
			'connection.connect',
			'microphone.publish',
			'microphone.unpublish',
		]);
		expect(runtime.eventLog.some((entry) => entry.event.type === 'command.staleCompletionRejected')).toBe(true);
		expect(runtime.snapshot.microphone.status).toBe('idle');
	});
});
