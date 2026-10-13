// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	selectVoiceEngineV2AppEffectiveSelfMuteFromAudioControls,
	selectVoiceEngineV2AppMuteReason,
	selectVoiceEngineV2AppSyncSelfMuteForVoiceStatePayload,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppSelectors';
import {transitionVoiceEngineV2} from '@fluxer/voice_engine_v2/src/core/reducer';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	type VoiceEngineV2Snapshot,
} from '@fluxer/voice_engine_v2/src/core/state';
import type {
	VoiceEngineV2AudioControls,
	VoiceEngineV2AudioControlsPatch,
} from '@fluxer/voice_engine_v2/src/protocol/types';
import {describe, expect, it} from 'vitest';

function audio(overrides: Partial<VoiceEngineV2AudioControls>): VoiceEngineV2AudioControls {
	return {
		mode: 'pushToTalk',
		locallyMuted: false,
		preferredLocallyMuted: false,
		locallyDeafened: false,
		mutedByPermission: false,
		hasUserSetMute: false,
		hasUserSetDeaf: false,
		shouldUnmuteOnUndeafen: false,
		pushToTalkActive: false,
		pushToMuteActive: false,
		inputVolume: 100,
		outputVolume: 100,
		...overrides,
	};
}

function snapshotWith(controls: VoiceEngineV2AudioControlsPatch): VoiceEngineV2Snapshot {
	return transitionVoiceEngineV2(createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities()), {
		type: 'audioControls.changed',
		controls,
	}).snapshot;
}

describe('push to talk self-mute selectors', () => {
	it('reports self-mute over a held push to talk key', () => {
		const controls = audio({locallyMuted: true, pushToTalkActive: true});
		expect(selectVoiceEngineV2AppEffectiveSelfMuteFromAudioControls(controls)).toBe(true);
		expect(selectVoiceEngineV2AppMuteReason({voiceState: null, permissionMuted: false, audio: controls})).toBe('self');
	});

	it('reports the push to talk gate when not self-muted', () => {
		const idle = audio({});
		expect(selectVoiceEngineV2AppEffectiveSelfMuteFromAudioControls(idle)).toBe(true);
		expect(selectVoiceEngineV2AppMuteReason({voiceState: null, permissionMuted: false, audio: idle})).toBe(
			'voice_push_to_talk',
		);
		const held = audio({pushToTalkActive: true});
		expect(selectVoiceEngineV2AppEffectiveSelfMuteFromAudioControls(held)).toBe(false);
		expect(selectVoiceEngineV2AppMuteReason({voiceState: null, permissionMuted: false, audio: held})).toBeNull();
	});

	it('syncs self_mute from self-mute and the push to talk gate', () => {
		expect(selectVoiceEngineV2AppSyncSelfMuteForVoiceStatePayload(snapshotWith({mode: 'pushToTalk'}))).toBe(true);
		expect(
			selectVoiceEngineV2AppSyncSelfMuteForVoiceStatePayload(
				snapshotWith({mode: 'pushToTalk', pushToTalkActive: true}),
			),
		).toBe(false);
		expect(
			selectVoiceEngineV2AppSyncSelfMuteForVoiceStatePayload(
				snapshotWith({mode: 'pushToTalk', pushToTalkActive: true, locallyMuted: true}),
			),
		).toBe(true);
	});

	it('keeps voice activity and permission mutes in the synced self_mute', () => {
		expect(selectVoiceEngineV2AppSyncSelfMuteForVoiceStatePayload(snapshotWith({mode: 'voiceActivity'}))).toBe(false);
		expect(
			selectVoiceEngineV2AppSyncSelfMuteForVoiceStatePayload(snapshotWith({mode: 'voiceActivity', locallyMuted: true})),
		).toBe(true);
		expect(
			selectVoiceEngineV2AppSyncSelfMuteForVoiceStatePayload(
				snapshotWith({mode: 'voiceActivity', mutedByPermission: true}),
			),
		).toBe(true);
		expect(
			selectVoiceEngineV2AppSyncSelfMuteForVoiceStatePayload(
				snapshotWith({mode: 'pushToMute', pushToMuteActive: true}),
			),
		).toBe(true);
	});
});
