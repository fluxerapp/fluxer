// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {transitionVoiceEngineV2} from '@fluxer/voice_engine_v2/src/core/reducer';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	type VoiceEngineV2Snapshot,
} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2AudioControlsPatch} from '@fluxer/voice_engine_v2/src/protocol/types';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const updateVoiceStateExplicit = vi.fn();
const engine: {snapshot: VoiceEngineV2Snapshot | null} = {snapshot: null};

vi.mock('@lingui/core/macro', () => {
	const descriptor = (value: unknown): unknown => (typeof value === 'string' ? {message: value} : value);
	return {msg: descriptor, t: descriptor, plural: () => '', select: () => '', selectOrdinal: () => ''};
});

vi.mock('@lingui/react/macro', () => ({
	Trans: () => null,
	useLingui: () => ({i18n: {_: (descriptor: {message?: string}) => descriptor.message ?? ''}}),
}));

vi.mock('@app/features/gateway/transport/GatewaySessionPool', () => ({
	default: {foregroundConnection: {socket: {updateVoiceStateExplicit}}},
}));

vi.mock('@app/features/voice/engine/VoiceMediaEngineBridge', () => ({
	getAllVoiceStatesInChannelFromMediaEngine: () => ({}),
	getVoiceConnectionContextFromMediaEngine: () => ({connectionId: 'local'}),
	getVoiceEngineV2SnapshotFromMediaEngine: () => engine.snapshot,
}));

vi.mock('@app/features/voice/utils/VoicePermissionUtils', () => ({
	applyVoiceSpeakPermissionToSelfMute: (_guildId: string | null, _channelId: string, selfMute: boolean) => selfMute,
}));

vi.mock('@app/features/voice/components/alerts/VoiceChannelFullModal', () => ({VoiceChannelFullModal: () => null}));
vi.mock('@app/features/voice/components/alerts/VoiceConnectionConfirmModal', () => ({
	VoiceConnectionConfirmModal: () => null,
}));
vi.mock('@app/features/voice/components/alerts/VoiceP2pConsentModal', () => ({VoiceP2pConsentModal: () => null}));

const {syncVoiceStateToServer} = await import('@app/features/voice/engine/VoiceChannelConnector');

function snapshotWith(controls: VoiceEngineV2AudioControlsPatch): VoiceEngineV2Snapshot {
	return transitionVoiceEngineV2(createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities()), {
		type: 'audioControls.changed',
		controls,
	}).snapshot;
}

function lastSelfMute(): boolean {
	const [payload] = updateVoiceStateExplicit.mock.lastCall as [{self_mute: boolean}];
	return payload.self_mute;
}

describe('direct gateway voice state sync with push to talk', () => {
	beforeEach(() => {
		updateVoiceStateExplicit.mockClear();
	});

	it('reports self_mute while the push to talk key is up on a viewer stream key sync', () => {
		engine.snapshot = snapshotWith({mode: 'pushToTalk', pushToTalkActive: false, locallyMuted: false});
		syncVoiceStateToServer('1', '2', 'local', {viewer_stream_keys: []});
		expect(lastSelfMute()).toBe(true);
	});

	it('reports unmuted while the push to talk key is held', () => {
		engine.snapshot = snapshotWith({mode: 'pushToTalk', pushToTalkActive: true, locallyMuted: false});
		syncVoiceStateToServer('1', '2', 'local', {viewer_stream_keys: []});
		expect(lastSelfMute()).toBe(false);
	});

	it('reports self-mute over a held push to talk key', () => {
		engine.snapshot = snapshotWith({mode: 'pushToTalk', pushToTalkActive: true, locallyMuted: true});
		syncVoiceStateToServer('1', '2', 'local', {viewer_stream_keys: []});
		expect(lastSelfMute()).toBe(true);
	});

	it('keeps voice activity unmuted and prefers an explicit self_mute', () => {
		engine.snapshot = snapshotWith({mode: 'voiceActivity', locallyMuted: false});
		syncVoiceStateToServer('1', '2', 'local', {viewer_stream_keys: []});
		expect(lastSelfMute()).toBe(false);
		syncVoiceStateToServer('1', '2', 'local', {self_mute: true});
		expect(lastSelfMute()).toBe(true);
	});
});
