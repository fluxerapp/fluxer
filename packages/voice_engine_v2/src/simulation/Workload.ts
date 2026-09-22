// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {
	VoiceEngineV2CameraOptions,
	VoiceEngineV2ConnectOptions,
	VoiceEngineV2MicrophoneOptions,
	VoiceEngineV2NativeCaptureOptions,
	VoiceEngineV2NativeFrameSinkOptions,
	VoiceEngineV2Participant,
	VoiceEngineV2ScreenOptions,
} from '@fluxer/voice_engine_v2/src/protocol/types';

const SIMULATOR_WORKLOAD_STEPS_MAX = 512;
const SIMULATOR_PARTICIPANTS_MAX = 32;

export interface VoiceEngineV2WorkloadStep {
	readonly tick: number;
	readonly event: VoiceEngineV2Event;
}

export interface VoiceEngineV2Workload {
	readonly name: string;
	readonly steps: ReadonlyArray<VoiceEngineV2WorkloadStep>;
	readonly tickCount: number;
}

export class VoiceEngineV2WorkloadBuilder {
	private readonly steps: Array<VoiceEngineV2WorkloadStep> = [];
	private cursorTick = 0;
	private readonly name: string;

	constructor(name: string) {
		assert.ok(typeof name === 'string', 'workload name must be a string');
		assert.ok(name.length > 0, 'workload name must not be empty');
		this.name = name;
	}

	at(tick: number): this {
		assert.ok(Number.isInteger(tick), 'workload tick must be an integer');
		assert.ok(tick >= this.cursorTick, 'workload ticks must be monotonic');
		this.cursorTick = tick;
		return this;
	}

	advance(delta: number): this {
		assert.ok(Number.isInteger(delta), 'workload advance must be an integer');
		assert.ok(delta >= 0, 'workload advance must be non-negative');
		this.cursorTick += delta;
		return this;
	}

	connect(options: VoiceEngineV2ConnectOptions): this {
		return this.emit({type: 'connection.connectRequested', options});
	}

	publishMicrophone(options: VoiceEngineV2MicrophoneOptions = {deviceId: 'default-mic'}): this {
		return this.emit({type: 'microphone.publishRequested', options});
	}

	unpublishMicrophone(): this {
		return this.emit({type: 'microphone.unpublishRequested'});
	}

	publishCamera(options: VoiceEngineV2CameraOptions = {deviceId: 'default-camera'}): this {
		return this.emit({type: 'camera.publishRequested', options});
	}

	publishScreen(options: VoiceEngineV2ScreenOptions): this {
		return this.emit({type: 'screen.publishRequested', options});
	}

	startNativeCapture(options: VoiceEngineV2NativeCaptureOptions): this {
		return this.emit({type: 'nativeCapture.startRequested', options});
	}

	attachNativeFrameSink(options: VoiceEngineV2NativeFrameSinkOptions): this {
		return this.emit({type: 'nativeFrameSink.attachRequested', options});
	}

	joinParticipant(participant: VoiceEngineV2Participant): this {
		return this.emit({type: 'room.participantJoined', participant});
	}

	leaveParticipant(participantIdentity: string, participantSid?: string): this {
		return this.emit({type: 'room.participantLeft', participantIdentity, participantSid});
	}

	disconnect(reason: 'user' | 'server' | 'network' | 'replaced' | 'shutdown'): this {
		return this.emit({type: 'connection.disconnectRequested', reason});
	}

	externallyEstablish(options: VoiceEngineV2ConnectOptions): this {
		assert.ok(options, 'externallyEstablish requires connect options');
		assert.equal(typeof options.url, 'string', 'externallyEstablish url must be a string');
		assert.equal(typeof options.token, 'string', 'externallyEstablish token must be a string');
		return this.emit({type: 'connection.externallyEstablished', options});
	}

	remoteDisconnect(reason: 'user' | 'server' | 'network' | 'replaced' | 'shutdown'): this {
		return this.emit({type: 'connection.remoteDisconnected', reason});
	}

	emit(event: VoiceEngineV2Event): this {
		assert.ok(event, 'workload event must be defined');
		assert.ok(this.steps.length < SIMULATOR_WORKLOAD_STEPS_MAX, 'cannot exceed workload step cap');
		this.steps.push({tick: this.cursorTick, event});
		return this;
	}

	build(): VoiceEngineV2Workload {
		assert.ok(this.steps.length <= SIMULATOR_WORKLOAD_STEPS_MAX, 'workload exceeds step cap');
		const tickCount = this.steps.length === 0 ? 0 : this.steps[this.steps.length - 1].tick + 1;
		return {
			name: this.name,
			steps: [...this.steps],
			tickCount,
		};
	}
}

export function createVoiceEngineV2OneOnOneCallWorkload(): VoiceEngineV2Workload {
	const remote: VoiceEngineV2Participant = {sid: 'sid-remote', identity: 'remote-1', name: 'Remote 1'};
	return new VoiceEngineV2WorkloadBuilder('one-on-one')
		.at(0)
		.connect({url: 'wss://voice.example.test', token: 'tok-1'})
		.advance(2)
		.emit({type: 'connection.connectSucceeded', operationId: 1})
		.advance(1)
		.publishMicrophone()
		.advance(2)
		.joinParticipant(remote)
		.advance(4)
		.disconnect('user')
		.build();
}

export function createVoiceEngineV2ScreenShareWorkload(): VoiceEngineV2Workload {
	const captureOptions: VoiceEngineV2NativeCaptureOptions = {
		captureId: 'cap-1',
		source: {id: 'display-1', kind: 'screen', title: 'Display 1'},
		width: 1280,
		height: 720,
		frameRate: 30,
		includeCursor: true,
		includeAudio: false,
		zeroCopyRequired: true,
	};
	const sinkOptions: VoiceEngineV2NativeFrameSinkOptions = {
		sinkId: 'sink-1',
		captureId: 'cap-1',
		zeroCopyRequired: true,
	};
	const screenOptions: VoiceEngineV2ScreenOptions = {captureId: 'cap-1', width: 1280, height: 720};
	return new VoiceEngineV2WorkloadBuilder('screen-share')
		.at(0)
		.connect({url: 'wss://voice.example.test', token: 'tok-screen'})
		.advance(2)
		.emit({type: 'connection.connectSucceeded', operationId: 1})
		.advance(1)
		.startNativeCapture(captureOptions)
		.advance(2)
		.attachNativeFrameSink(sinkOptions)
		.advance(1)
		.publishScreen(screenOptions)
		.advance(5)
		.disconnect('user')
		.build();
}

export const SIMULATOR_EXTERNAL_ESTABLISH_CYCLES = 3;

export function createVoiceEngineV2ExternalEstablishmentCycleWorkload(): VoiceEngineV2Workload {
	assert.ok(SIMULATOR_EXTERNAL_ESTABLISH_CYCLES >= 1, 'external establishment workload requires at least one cycle');
	assert.ok(SIMULATOR_EXTERNAL_ESTABLISH_CYCLES <= 8, 'external establishment cycle count must stay bounded');
	const remote: VoiceEngineV2Participant = {sid: 'sid-remote-ext', identity: 'remote-ext-1', name: 'Remote Ext 1'};
	const reasons: ReadonlyArray<'network' | 'server' | 'replaced'> = ['network', 'server', 'replaced'];
	const builder = new VoiceEngineV2WorkloadBuilder('external-establishment-cycles').at(0).publishMicrophone();
	for (let cycle = 0; cycle < SIMULATOR_EXTERNAL_ESTABLISH_CYCLES; cycle++) {
		const reason = reasons[cycle % reasons.length];
		assert.ok(reason !== undefined, 'cycle reason selection must not produce holes');
		builder
			.advance(1)
			.externallyEstablish({url: `wss://voice.example.test/external-${cycle}`, token: `tok-ext-${cycle}`})
			.advance(2)
			.publishMicrophone()
			.advance(1)
			.joinParticipant({...remote, sid: `${remote.sid}-${cycle}`, identity: `${remote.identity}-${cycle}`})
			.advance(2)
			.remoteDisconnect(reason);
	}
	return builder
		.advance(2)
		.externallyEstablish({url: 'wss://voice.example.test/external-final', token: 'tok-ext-final'})
		.advance(2)
		.disconnect('user')
		.build();
}

export function createVoiceEngineV2FiveParticipantConferenceWorkload(): VoiceEngineV2Workload {
	const builder = new VoiceEngineV2WorkloadBuilder('five-party-conference')
		.at(0)
		.connect({url: 'wss://voice.example.test', token: 'tok-conf'})
		.advance(2)
		.emit({type: 'connection.connectSucceeded', operationId: 1})
		.advance(1)
		.publishMicrophone();
	const participantCount = 4;
	assert.ok(participantCount <= SIMULATOR_PARTICIPANTS_MAX, 'participant count exceeds cap');
	for (let index = 0; index < participantCount; index++) {
		builder.advance(1).joinParticipant({
			sid: `sid-p${index + 1}`,
			identity: `peer-${index + 1}`,
			name: `Peer ${index + 1}`,
		});
	}
	return builder.advance(6).disconnect('user').build();
}
