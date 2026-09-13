// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	computeVoiceMediaEffectiveAudioState,
	createVoiceMediaSnapshot,
	transitionVoiceMediaSnapshot,
	type VoiceMediaAudioControls,
	type VoiceMediaSnapshot,
} from '@app/features/voice/engine/VoiceMediaStateMachine';
import {describe, expect, it} from 'vitest';

const releasedPtt: VoiceMediaAudioControls = {
	pushToTalkActive: true,
	pushToTalkHeld: false,
	pushToMuteActive: false,
	pushToMuteHeld: false,
};

const heldPtt: VoiceMediaAudioControls = {
	pushToTalkActive: true,
	pushToTalkHeld: true,
	pushToMuteActive: false,
	pushToMuteHeld: false,
};

const heldPtm: VoiceMediaAudioControls = {
	pushToTalkActive: false,
	pushToTalkHeld: false,
	pushToMuteActive: true,
	pushToMuteHeld: true,
};

const noControls: VoiceMediaAudioControls = {
	pushToTalkActive: false,
	pushToTalkHeld: false,
	pushToMuteActive: false,
	pushToMuteHeld: false,
};

function clearCommands(snapshot: VoiceMediaSnapshot): VoiceMediaSnapshot {
	return transitionVoiceMediaSnapshot(snapshot, {type: 'commands.clear'});
}

describe('VoiceMediaStateMachine', () => {
	it('tracks microphone permission warmup grant and denial fallback states', () => {
		let snapshot = createVoiceMediaSnapshot();
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'permission.warmup.start'});
		expect(snapshot.context.permissionWarmup).toBe('checking');
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'permission.warmup.granted'});
		expect(snapshot.context.permissionWarmup).toBe('granted');

		snapshot = createVoiceMediaSnapshot();
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'permission.warmup.denied'});
		expect(snapshot.context.permissionWarmup).toBe('denied');
		expect(snapshot.context.microphone.state).toBe('permissionDenied');
		expect(snapshot.context.commands).toEqual([
			{type: 'microphone.permissionDeniedFallbackSelfMute'},
			{type: 'voiceState.syncSelfMute', selfMute: true},
		]);
	});

	it('does not publish the microphone twice for repeated enable requests while enabling', () => {
		let snapshot = createVoiceMediaSnapshot();
		snapshot = transitionVoiceMediaSnapshot(snapshot, {
			type: 'microphone.enable.request',
			hasPublication: false,
			hasLivePublication: false,
		});
		snapshot = transitionVoiceMediaSnapshot(snapshot, {
			type: 'microphone.enable.request',
			hasPublication: false,
			hasLivePublication: false,
		});
		expect(snapshot.context.microphone.state).toBe('enabling');
		expect(snapshot.context.commands).toEqual([{type: 'microphone.enable'}]);
	});

	it('skips duplicate publication when an existing microphone track is live', () => {
		const snapshot = transitionVoiceMediaSnapshot(createVoiceMediaSnapshot(), {
			type: 'microphone.enable.request',
			hasPublication: true,
			hasLivePublication: true,
		});
		expect(snapshot.context.microphone.state).toBe('enabled');
		expect(snapshot.context.commands).toEqual([]);
	});

	it('recovers an ended microphone publication by requesting republish', () => {
		let snapshot = createVoiceMediaSnapshot();
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'microphone.enable.success'});
		snapshot = clearCommands(snapshot);
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'microphone.publication.ended'});
		snapshot = transitionVoiceMediaSnapshot(snapshot, {
			type: 'microphone.enable.request',
			hasPublication: true,
			hasLivePublication: false,
		});
		expect(snapshot.context.microphone.state).toBe('republishing');
		expect(snapshot.context.microphone.hasLivePublication).toBe(false);
		expect(snapshot.context.commands).toEqual([{type: 'microphone.republish'}]);
	});

	it('falls back to republish when microphone capture restart fails', () => {
		let snapshot = createVoiceMediaSnapshot();
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'microphone.enable.success'});
		snapshot = clearCommands(snapshot);
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'refresh.request', hasPublication: true});
		expect(snapshot.context.refresh.state).toBe('restarting');
		expect(snapshot.context.commands).toEqual([{type: 'microphone.restart'}]);
		snapshot = clearCommands(snapshot);
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'refresh.restart.failure'});
		expect(snapshot.context.refresh.state).toBe('republishing');
		expect(snapshot.context.refresh.lastFailure).toBe('restart');
		expect(snapshot.context.commands).toEqual([{type: 'microphone.republish'}]);
	});

	it('queues capture refresh requests while a refresh is already in flight', () => {
		let snapshot = createVoiceMediaSnapshot();
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'microphone.enable.success'});
		snapshot = clearCommands(snapshot);
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'refresh.request', hasPublication: true});
		snapshot = clearCommands(snapshot);
		snapshot = transitionVoiceMediaSnapshot(snapshot, {
			type: 'refresh.request',
			hasPublication: true,
			forceRepublish: true,
		});
		expect(snapshot.context.refresh.state).toBe('restarting');
		expect(snapshot.context.refresh.queued).toBe(true);
		expect(snapshot.context.refresh.forceRepublishQueued).toBe(true);
		expect(snapshot.context.commands).toEqual([]);

		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'refresh.restart.success'});
		expect(snapshot.context.refresh.state).toBe('republishing');
		expect(snapshot.context.commands).toEqual([{type: 'microphone.republish'}]);
	});

	it('models mute, deaf, push-to-talk, and push-to-mute effective audio transitions', () => {
		let snapshot = createVoiceMediaSnapshot();
		snapshot = transitionVoiceMediaSnapshot(snapshot, {
			type: 'audio.reconcile',
			audioState: computeVoiceMediaEffectiveAudioState({selfMute: true, selfDeaf: false}, noControls),
			controls: noControls,
			permissionMuted: false,
			hasLiveMicrophonePublication: true,
		});
		expect(snapshot.context.effectiveAudioMode).toBe('muted');
		expect(snapshot.context.commands).toEqual([{type: 'microphone.mutePublications', reason: 'voice state update'}]);

		snapshot = clearCommands(snapshot);
		snapshot = transitionVoiceMediaSnapshot(snapshot, {
			type: 'audio.reconcile',
			audioState: computeVoiceMediaEffectiveAudioState({selfMute: false, selfDeaf: true}, noControls),
			controls: noControls,
			permissionMuted: false,
			hasLiveMicrophonePublication: true,
		});
		expect(snapshot.context.effectiveAudioMode).toBe('muted');
		expect(snapshot.context.effectiveAudioState?.effectiveDeaf).toBe(true);

		snapshot = clearCommands(snapshot);
		snapshot = transitionVoiceMediaSnapshot(snapshot, {
			type: 'audio.reconcile',
			audioState: computeVoiceMediaEffectiveAudioState({selfMute: false, selfDeaf: false}, releasedPtt),
			controls: releasedPtt,
			permissionMuted: false,
			hasLiveMicrophonePublication: true,
		});
		expect(snapshot.context.effectiveAudioMode).toBe('muted');

		snapshot = clearCommands(snapshot);
		snapshot = transitionVoiceMediaSnapshot(snapshot, {
			type: 'audio.reconcile',
			audioState: computeVoiceMediaEffectiveAudioState({selfMute: false, selfDeaf: false}, heldPtm),
			controls: heldPtm,
			permissionMuted: false,
			hasLiveMicrophonePublication: true,
		});
		expect(snapshot.context.effectiveAudioMode).toBe('muted');

		snapshot = clearCommands(snapshot);
		snapshot = transitionVoiceMediaSnapshot(snapshot, {
			type: 'audio.reconcile',
			audioState: computeVoiceMediaEffectiveAudioState({selfMute: false, selfDeaf: false}, heldPtt),
			controls: heldPtt,
			permissionMuted: false,
			hasLiveMicrophonePublication: true,
		});
		expect(snapshot.context.effectiveAudioMode).toBe('unmuted');
		expect(snapshot.context.commands).toEqual([{type: 'microphone.unmutePublications', reason: 'voice state update'}]);
	});

	it('requests microphone enable during unmuted reconcile when no live publication exists', () => {
		const snapshot = transitionVoiceMediaSnapshot(createVoiceMediaSnapshot(), {
			type: 'audio.reconcile',
			audioState: computeVoiceMediaEffectiveAudioState({selfMute: false, selfDeaf: false}, noControls),
			controls: noControls,
			permissionMuted: false,
			hasLiveMicrophonePublication: false,
		});
		expect(snapshot.context.effectiveAudioMode).toBe('unmuted');
		expect(snapshot.context.commands).toEqual([{type: 'microphone.enable'}]);
	});

	it('falls back to self mute when speak permission mutes local capture', () => {
		const snapshot = transitionVoiceMediaSnapshot(createVoiceMediaSnapshot(), {
			type: 'audio.reconcile',
			audioState: computeVoiceMediaEffectiveAudioState({selfMute: false, selfDeaf: false}, noControls),
			controls: noControls,
			permissionMuted: true,
			hasLiveMicrophonePublication: true,
		});
		expect(snapshot.context.effectiveAudioMode).toBe('muted');
		expect(snapshot.context.commands).toEqual([
			{type: 'microphone.disable'},
			{type: 'voiceState.syncSelfMute', selfMute: true},
		]);
	});

	it('tracks speaking detector attachment and cleanup/reset', () => {
		let snapshot = createVoiceMediaSnapshot();
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'microphone.enable.success'});
		expect(snapshot.context.speakingDetector).toBe('attached');
		snapshot = clearCommands(snapshot);
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'microphone.disable.request', hasPublication: true});
		expect(snapshot.context.speakingDetector).toBe('detached');
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'microphone.disable.success'});
		expect(snapshot.context.microphone.state).toBe('disabled');
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'media.reset'});
		expect(snapshot.context.permissionWarmup).toBe('unknown');
		expect(snapshot.context.microphone.hasPublication).toBe(false);
		expect(snapshot.context.commands).toEqual([]);
	});

	it('models camera enable, update, disable, and permission denied behavior', () => {
		let snapshot = createVoiceMediaSnapshot();
		snapshot = transitionVoiceMediaSnapshot(snapshot, {
			type: 'camera.setEnabled.request',
			enabled: true,
			currentlyEnabled: false,
		});
		expect(snapshot.context.camera).toBe('enabling');
		expect(snapshot.context.commands).toEqual([{type: 'camera.enable'}]);
		snapshot = clearCommands(snapshot);
		snapshot = transitionVoiceMediaSnapshot(snapshot, {type: 'camera.success', enabled: true});
		expect(snapshot.context.camera).toBe('enabled');

		snapshot = transitionVoiceMediaSnapshot(snapshot, {
			type: 'camera.setEnabled.request',
			enabled: true,
			currentlyEnabled: true,
		});
		expect(snapshot.context.camera).toBe('updating');
		expect(snapshot.context.commands).toEqual([{type: 'camera.update'}]);
		snapshot = clearCommands(snapshot);
		snapshot = transitionVoiceMediaSnapshot(snapshot, {
			type: 'camera.setEnabled.request',
			enabled: false,
			currentlyEnabled: true,
		});
		expect(snapshot.context.camera).toBe('disabling');
		expect(snapshot.context.commands).toEqual([{type: 'camera.disable'}]);

		snapshot = transitionVoiceMediaSnapshot(snapshot, {
			type: 'camera.failure',
			actualEnabled: false,
			permissionDenied: true,
		});
		expect(snapshot.context.camera).toBe('permissionDenied');
	});
});
