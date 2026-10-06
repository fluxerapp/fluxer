// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EffectiveAudioState} from '@app/features/voice/engine/VoiceEffectiveAudioState';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type VoiceMediaPermissionWarmupState =
	| 'unknown'
	| 'checking'
	| 'granted'
	| 'denied'
	| 'unavailable'
	| 'failedContinuing';

export type VoiceMediaMicrophoneState =
	| 'disabled'
	| 'enabling'
	| 'enabled'
	| 'disabling'
	| 'restarting'
	| 'republishing'
	| 'permissionDenied';

export type VoiceMediaSpeakingDetectorState = 'detached' | 'attached';
export type VoiceMediaRefreshState = 'idle' | 'restarting' | 'republishing';
export type VoiceMediaCameraState = 'disabled' | 'enabling' | 'enabled' | 'disabling' | 'updating' | 'permissionDenied';
export type VoiceMediaEffectiveAudioMode = 'unknown' | 'muted' | 'unmuted';

export type VoiceMediaCommand =
	| {type: 'microphone.enable'}
	| {type: 'microphone.disable'}
	| {type: 'microphone.republish'}
	| {type: 'microphone.restart'}
	| {type: 'microphone.mutePublications'; reason: string}
	| {type: 'microphone.unmutePublications'; reason: string}
	| {type: 'microphone.permissionDeniedFallbackSelfMute'}
	| {type: 'voiceState.syncSelfMute'; selfMute: boolean}
	| {type: 'speakingDetector.attach'}
	| {type: 'speakingDetector.detach'}
	| {type: 'camera.enable'}
	| {type: 'camera.disable'}
	| {type: 'camera.update'};

export interface VoiceMediaInputState {
	selfMute: boolean;
	selfDeaf: boolean;
	serverMute?: boolean;
	serverDeaf?: boolean;
}

export interface VoiceMediaAudioControls {
	pushToTalkActive: boolean;
	pushToTalkHeld: boolean;
	pushToMuteActive: boolean;
	pushToMuteHeld: boolean;
}

interface VoiceMediaMicrophoneContext {
	state: VoiceMediaMicrophoneState;
	hasPublication: boolean;
	hasLivePublication: boolean;
}

interface VoiceMediaRefreshContext {
	state: VoiceMediaRefreshState;
	queued: boolean;
	forceRepublishQueued: boolean;
	lastFailure: 'restart' | 'republish' | null;
}

interface VoiceMediaContext {
	permissionWarmup: VoiceMediaPermissionWarmupState;
	microphone: VoiceMediaMicrophoneContext;
	speakingDetector: VoiceMediaSpeakingDetectorState;
	refresh: VoiceMediaRefreshContext;
	camera: VoiceMediaCameraState;
	effectiveAudioMode: VoiceMediaEffectiveAudioMode;
	effectiveAudioState: EffectiveAudioState | null;
	audioControls: VoiceMediaAudioControls;
	commands: ReadonlyArray<VoiceMediaCommand>;
}

export type VoiceMediaEvent =
	| {type: 'permission.warmup.start'}
	| {type: 'permission.warmup.granted'}
	| {type: 'permission.warmup.denied'}
	| {type: 'permission.warmup.unavailable'}
	| {type: 'permission.warmup.failedContinuing'}
	| {
			type: 'microphone.enable.request';
			hasPublication: boolean;
			hasLivePublication: boolean;
			speakPermissionDenied?: boolean;
			permissionDenied?: boolean;
	  }
	| {type: 'microphone.enable.success'; hasPublication?: boolean; hasLivePublication?: boolean}
	| {type: 'microphone.enable.failure'; permissionDenied?: boolean; publicationCreated?: boolean}
	| {type: 'microphone.disable.request'; hasPublication: boolean}
	| {type: 'microphone.disable.success'}
	| {type: 'microphone.disable.failure'}
	| {type: 'microphone.publication.ended'}
	| {type: 'speakingDetector.attach'}
	| {type: 'speakingDetector.detach'}
	| {type: 'refresh.request'; hasPublication: boolean; forceRepublish?: boolean}
	| {type: 'refresh.restart.success'}
	| {type: 'refresh.restart.failure'}
	| {type: 'refresh.republish.success'}
	| {type: 'refresh.republish.failure'}
	| {
			type: 'audio.reconcile';
			audioState: EffectiveAudioState;
			controls: VoiceMediaAudioControls;
			permissionMuted: boolean;
			hasLiveMicrophonePublication: boolean;
	  }
	| {type: 'audio.controls.update'; controls: VoiceMediaAudioControls}
	| {type: 'camera.setEnabled.request'; enabled: boolean; currentlyEnabled: boolean}
	| {type: 'camera.update.request'; currentlyEnabled: boolean}
	| {type: 'camera.success'; enabled: boolean}
	| {type: 'camera.failure'; actualEnabled: boolean; permissionDenied?: boolean}
	| {type: 'media.reset'}
	| {type: 'commands.clear'};

const EMPTY_COMMANDS: ReadonlyArray<VoiceMediaCommand> = [];

const DEFAULT_AUDIO_CONTROLS: VoiceMediaAudioControls = {
	pushToTalkActive: false,
	pushToTalkHeld: false,
	pushToMuteActive: false,
	pushToMuteHeld: false,
};

function initialContext(): VoiceMediaContext {
	return {
		permissionWarmup: 'unknown',
		microphone: {
			state: 'disabled',
			hasPublication: false,
			hasLivePublication: false,
		},
		speakingDetector: 'detached',
		refresh: {
			state: 'idle',
			queued: false,
			forceRepublishQueued: false,
			lastFailure: null,
		},
		camera: 'disabled',
		effectiveAudioMode: 'unknown',
		effectiveAudioState: null,
		audioControls: DEFAULT_AUDIO_CONTROLS,
		commands: EMPTY_COMMANDS,
	};
}

export function computeVoiceMediaEffectiveAudioState(
	audioState: VoiceMediaInputState,
	controls: VoiceMediaAudioControls = DEFAULT_AUDIO_CONTROLS,
): EffectiveAudioState {
	const serverMute = audioState.serverMute ?? false;
	const serverDeaf = audioState.serverDeaf ?? false;
	const effectiveDeaf = serverDeaf || audioState.selfDeaf;
	const effectiveMute =
		effectiveDeaf ||
		serverMute ||
		audioState.selfMute ||
		(controls.pushToTalkActive && !controls.pushToTalkHeld) ||
		(controls.pushToMuteActive && controls.pushToMuteHeld);
	return {
		selfMute: audioState.selfMute,
		selfDeaf: audioState.selfDeaf,
		serverMute,
		serverDeaf,
		effectiveMute,
		effectiveDeaf,
	};
}

function appendCommands(context: VoiceMediaContext, commands: ReadonlyArray<VoiceMediaCommand>): VoiceMediaContext {
	if (commands.length === 0) return context;
	return {
		...context,
		commands: [...context.commands, ...commands],
	};
}

function setMicrophonePublication(
	context: VoiceMediaContext,
	patch: Partial<VoiceMediaMicrophoneContext>,
): VoiceMediaContext {
	return {
		...context,
		microphone: {
			...context.microphone,
			...patch,
		},
	};
}

function setRefresh(context: VoiceMediaContext, patch: Partial<VoiceMediaRefreshContext>): VoiceMediaContext {
	return {
		...context,
		refresh: {
			...context.refresh,
			...patch,
		},
	};
}

function handleWarmupDenied(context: VoiceMediaContext): VoiceMediaContext {
	return appendCommands(
		{
			...context,
			permissionWarmup: 'denied',
			microphone: {
				state: 'permissionDenied',
				hasPublication: false,
				hasLivePublication: false,
			},
			speakingDetector: 'detached',
		},
		[{type: 'microphone.permissionDeniedFallbackSelfMute'}, {type: 'voiceState.syncSelfMute', selfMute: true}],
	);
}

function requestMicrophoneEnable(
	context: VoiceMediaContext,
	event: Extract<VoiceMediaEvent, {type: 'microphone.enable.request'}>,
): VoiceMediaContext {
	if (event.permissionDenied || event.speakPermissionDenied) {
		return appendCommands(
			setMicrophonePublication(context, {
				state: 'permissionDenied',
				hasPublication: event.hasPublication,
				hasLivePublication: false,
			}),
			[{type: 'microphone.permissionDeniedFallbackSelfMute'}, {type: 'voiceState.syncSelfMute', selfMute: true}],
		);
	}
	if (context.microphone.state === 'enabling' || context.microphone.state === 'republishing') {
		return setMicrophonePublication(context, {
			hasPublication: event.hasPublication,
			hasLivePublication: event.hasLivePublication,
		});
	}
	if (event.hasLivePublication) {
		return {
			...context,
			microphone: {
				state: 'enabled',
				hasPublication: true,
				hasLivePublication: true,
			},
		};
	}
	if (event.hasPublication) {
		return appendCommands(
			{
				...context,
				microphone: {
					state: 'republishing',
					hasPublication: true,
					hasLivePublication: false,
				},
			},
			[{type: 'microphone.republish'}],
		);
	}
	return appendCommands(
		{
			...context,
			microphone: {
				state: 'enabling',
				hasPublication: false,
				hasLivePublication: false,
			},
		},
		[{type: 'microphone.enable'}],
	);
}

function completeMicrophoneEnable(
	context: VoiceMediaContext,
	event: Extract<VoiceMediaEvent, {type: 'microphone.enable.success'}>,
): VoiceMediaContext {
	return appendCommands(
		{
			...context,
			microphone: {
				state: 'enabled',
				hasPublication: event.hasPublication ?? true,
				hasLivePublication: event.hasLivePublication ?? true,
			},
			speakingDetector: 'attached',
		},
		[{type: 'speakingDetector.attach'}],
	);
}

function failMicrophoneEnable(
	context: VoiceMediaContext,
	event: Extract<VoiceMediaEvent, {type: 'microphone.enable.failure'}>,
): VoiceMediaContext {
	const commands: Array<VoiceMediaCommand> = [];
	if (event.publicationCreated) {
		commands.push({type: 'microphone.disable'});
	}
	if (event.permissionDenied) {
		commands.push(
			{type: 'microphone.permissionDeniedFallbackSelfMute'},
			{type: 'voiceState.syncSelfMute', selfMute: true},
		);
		return appendCommands(
			{
				...context,
				microphone: {
					state: 'permissionDenied',
					hasPublication: false,
					hasLivePublication: false,
				},
				speakingDetector: 'detached',
			},
			commands,
		);
	}
	return appendCommands(
		{
			...context,
			microphone: {
				state: 'disabled',
				hasPublication: false,
				hasLivePublication: false,
			},
			speakingDetector: 'detached',
		},
		commands,
	);
}

function requestMicrophoneDisable(
	context: VoiceMediaContext,
	event: Extract<VoiceMediaEvent, {type: 'microphone.disable.request'}>,
): VoiceMediaContext {
	if (!event.hasPublication && context.microphone.state === 'disabled') {
		return context;
	}
	return appendCommands(
		{
			...context,
			microphone: {
				state: 'disabling',
				hasPublication: event.hasPublication,
				hasLivePublication: context.microphone.hasLivePublication && event.hasPublication,
			},
			speakingDetector: 'detached',
		},
		[{type: 'microphone.disable'}, {type: 'speakingDetector.detach'}],
	);
}

function requestRefresh(
	context: VoiceMediaContext,
	event: Extract<VoiceMediaEvent, {type: 'refresh.request'}>,
): VoiceMediaContext {
	if (!event.hasPublication) return context;
	if (context.refresh.state !== 'idle') {
		return setRefresh(context, {
			queued: true,
			forceRepublishQueued: context.refresh.forceRepublishQueued || Boolean(event.forceRepublish),
		});
	}
	if (event.forceRepublish) {
		return appendCommands(
			{
				...context,
				refresh: {
					state: 'republishing',
					queued: false,
					forceRepublishQueued: false,
					lastFailure: null,
				},
				microphone: {
					...context.microphone,
					state: 'republishing',
				},
			},
			[{type: 'microphone.republish'}],
		);
	}
	return appendCommands(
		{
			...context,
			refresh: {
				state: 'restarting',
				queued: false,
				forceRepublishQueued: false,
				lastFailure: null,
			},
			microphone: {
				...context.microphone,
				state: 'restarting',
			},
		},
		[{type: 'microphone.restart'}],
	);
}

function finishRefresh(
	context: VoiceMediaContext,
	failure: VoiceMediaRefreshContext['lastFailure'],
): VoiceMediaContext {
	const queued = context.refresh.queued;
	const forceRepublishQueued = context.refresh.forceRepublishQueued;
	if (queued) {
		return requestRefresh(
			{
				...context,
				refresh: {
					state: 'idle',
					queued: false,
					forceRepublishQueued: false,
					lastFailure: failure,
				},
				microphone: {
					state: context.microphone.hasLivePublication ? 'enabled' : 'disabled',
					hasPublication: context.microphone.hasPublication,
					hasLivePublication: context.microphone.hasLivePublication,
				},
			},
			{
				type: 'refresh.request',
				hasPublication: true,
				forceRepublish: forceRepublishQueued,
			},
		);
	}
	return {
		...context,
		refresh: {
			state: 'idle',
			queued: false,
			forceRepublishQueued: false,
			lastFailure: failure,
		},
		microphone: {
			state: context.microphone.hasLivePublication ? 'enabled' : 'disabled',
			hasPublication: context.microphone.hasPublication,
			hasLivePublication: context.microphone.hasLivePublication,
		},
	};
}

function failRefreshRestart(context: VoiceMediaContext): VoiceMediaContext {
	return appendCommands(
		{
			...context,
			refresh: {
				...context.refresh,
				state: 'republishing',
				lastFailure: 'restart',
			},
			microphone: {
				...context.microphone,
				state: 'republishing',
			},
		},
		[{type: 'microphone.republish'}],
	);
}

function reconcileAudio(
	context: VoiceMediaContext,
	event: Extract<VoiceMediaEvent, {type: 'audio.reconcile'}>,
): VoiceMediaContext {
	const effectiveMute = event.permissionMuted || event.audioState.effectiveMute || event.audioState.effectiveDeaf;
	const commands: Array<VoiceMediaCommand> = [];
	if (event.permissionMuted) {
		commands.push({type: 'microphone.disable'}, {type: 'voiceState.syncSelfMute', selfMute: true});
	} else if (effectiveMute) {
		commands.push({type: 'microphone.mutePublications', reason: 'voice state update'});
	} else if (!event.hasLiveMicrophonePublication) {
		commands.push({type: 'microphone.enable'});
	} else {
		commands.push({type: 'microphone.unmutePublications', reason: 'voice state update'});
	}
	return appendCommands(
		{
			...context,
			effectiveAudioMode: effectiveMute ? 'muted' : 'unmuted',
			effectiveAudioState: event.audioState,
			audioControls: event.controls,
		},
		commands,
	);
}

function requestCameraSetEnabled(
	context: VoiceMediaContext,
	event: Extract<VoiceMediaEvent, {type: 'camera.setEnabled.request'}>,
): VoiceMediaContext {
	if (event.enabled) {
		if (context.camera === 'enabling') return context;
		if (event.currentlyEnabled || context.camera === 'enabled') {
			return appendCommands({...context, camera: 'updating'}, [{type: 'camera.update'}]);
		}
		return appendCommands({...context, camera: 'enabling'}, [{type: 'camera.enable'}]);
	}
	if (context.camera === 'disabling') return context;
	if (!event.currentlyEnabled && context.camera === 'disabled') return context;
	return appendCommands({...context, camera: 'disabling'}, [{type: 'camera.disable'}]);
}

function requestCameraUpdate(
	context: VoiceMediaContext,
	event: Extract<VoiceMediaEvent, {type: 'camera.update.request'}>,
): VoiceMediaContext {
	if (!event.currentlyEnabled && context.camera !== 'enabled') return context;
	return appendCommands({...context, camera: 'updating'}, [{type: 'camera.update'}]);
}

export const voiceMediaStateMachine = setup({
	types: {} as {
		context: VoiceMediaContext;
		events: VoiceMediaEvent;
	},
	actions: {
		warmupStart: assign(({context}) => ({...context, permissionWarmup: 'checking'})),
		warmupGranted: assign(({context}) => ({...context, permissionWarmup: 'granted'})),
		warmupDenied: assign(({context}) => handleWarmupDenied(context)),
		warmupUnavailable: assign(({context}) => ({...context, permissionWarmup: 'unavailable'})),
		warmupFailedContinuing: assign(({context}) => ({...context, permissionWarmup: 'failedContinuing'})),
		requestMicrophoneEnable: assign(({context, event}) =>
			event.type === 'microphone.enable.request' ? requestMicrophoneEnable(context, event) : context,
		),
		completeMicrophoneEnable: assign(({context, event}) =>
			event.type === 'microphone.enable.success' ? completeMicrophoneEnable(context, event) : context,
		),
		failMicrophoneEnable: assign(({context, event}) =>
			event.type === 'microphone.enable.failure' ? failMicrophoneEnable(context, event) : context,
		),
		requestMicrophoneDisable: assign(({context, event}) =>
			event.type === 'microphone.disable.request' ? requestMicrophoneDisable(context, event) : context,
		),
		completeMicrophoneDisable: assign(({context}) => ({
			...context,
			microphone: {
				state: 'disabled',
				hasPublication: false,
				hasLivePublication: false,
			},
			speakingDetector: 'detached',
		})),
		failMicrophoneDisable: assign(({context}) => ({
			...context,
			microphone: {
				...context.microphone,
				state: context.microphone.hasLivePublication ? 'enabled' : 'disabled',
			},
		})),
		markPublicationEnded: assign(({context}) => ({
			...context,
			microphone: {
				state: context.microphone.hasPublication ? 'enabled' : 'disabled',
				hasPublication: context.microphone.hasPublication,
				hasLivePublication: false,
			},
			speakingDetector: 'detached',
		})),
		attachSpeakingDetector: assign(({context}) =>
			appendCommands({...context, speakingDetector: 'attached'}, [{type: 'speakingDetector.attach'}]),
		),
		detachSpeakingDetector: assign(({context}) =>
			appendCommands({...context, speakingDetector: 'detached'}, [{type: 'speakingDetector.detach'}]),
		),
		requestRefresh: assign(({context, event}) =>
			event.type === 'refresh.request' ? requestRefresh(context, event) : context,
		),
		completeRestart: assign(({context}) =>
			finishRefresh(
				{
					...context,
					microphone: {
						state: 'enabled',
						hasPublication: true,
						hasLivePublication: true,
					},
					speakingDetector: 'attached',
				},
				null,
			),
		),
		failRestart: assign(({context}) => failRefreshRestart(context)),
		completeRepublish: assign(({context}) =>
			finishRefresh(
				{
					...context,
					microphone: {
						state: 'enabled',
						hasPublication: true,
						hasLivePublication: true,
					},
					speakingDetector: 'attached',
				},
				context.refresh.lastFailure,
			),
		),
		failRepublish: assign(({context}) =>
			finishRefresh(
				{
					...context,
					microphone: {
						state: 'disabled',
						hasPublication: false,
						hasLivePublication: false,
					},
					speakingDetector: 'detached',
				},
				'republish',
			),
		),
		reconcileAudio: assign(({context, event}) =>
			event.type === 'audio.reconcile' ? reconcileAudio(context, event) : context,
		),
		updateAudioControls: assign(({context, event}) =>
			event.type === 'audio.controls.update' ? {...context, audioControls: event.controls} : context,
		),
		requestCameraSetEnabled: assign(({context, event}) =>
			event.type === 'camera.setEnabled.request' ? requestCameraSetEnabled(context, event) : context,
		),
		requestCameraUpdate: assign(({context, event}) =>
			event.type === 'camera.update.request' ? requestCameraUpdate(context, event) : context,
		),
		completeCamera: assign(({context, event}) =>
			event.type === 'camera.success' ? {...context, camera: event.enabled ? 'enabled' : 'disabled'} : context,
		),
		failCamera: assign(({context, event}) => {
			if (event.type !== 'camera.failure') return context;
			return {
				...context,
				camera: event.permissionDenied ? 'permissionDenied' : event.actualEnabled ? 'enabled' : 'disabled',
			};
		}),
		reset: assign(() => initialContext()),
		clearCommands: assign(({context}) =>
			context.commands.length === 0 ? context : {...context, commands: EMPTY_COMMANDS},
		),
	},
}).createMachine({
	id: 'voiceMedia',
	context: () => initialContext(),
	initial: 'tracking',
	states: {
		tracking: {
			on: {
				'permission.warmup.start': {actions: 'warmupStart'},
				'permission.warmup.granted': {actions: 'warmupGranted'},
				'permission.warmup.denied': {actions: 'warmupDenied'},
				'permission.warmup.unavailable': {actions: 'warmupUnavailable'},
				'permission.warmup.failedContinuing': {actions: 'warmupFailedContinuing'},
				'microphone.enable.request': {actions: 'requestMicrophoneEnable'},
				'microphone.enable.success': {actions: 'completeMicrophoneEnable'},
				'microphone.enable.failure': {actions: 'failMicrophoneEnable'},
				'microphone.disable.request': {actions: 'requestMicrophoneDisable'},
				'microphone.disable.success': {actions: 'completeMicrophoneDisable'},
				'microphone.disable.failure': {actions: 'failMicrophoneDisable'},
				'microphone.publication.ended': {actions: 'markPublicationEnded'},
				'speakingDetector.attach': {actions: 'attachSpeakingDetector'},
				'speakingDetector.detach': {actions: 'detachSpeakingDetector'},
				'refresh.request': {actions: 'requestRefresh'},
				'refresh.restart.success': {actions: 'completeRestart'},
				'refresh.restart.failure': {actions: 'failRestart'},
				'refresh.republish.success': {actions: 'completeRepublish'},
				'refresh.republish.failure': {actions: 'failRepublish'},
				'audio.reconcile': {actions: 'reconcileAudio'},
				'audio.controls.update': {actions: 'updateAudioControls'},
				'camera.setEnabled.request': {actions: 'requestCameraSetEnabled'},
				'camera.update.request': {actions: 'requestCameraUpdate'},
				'camera.success': {actions: 'completeCamera'},
				'camera.failure': {actions: 'failCamera'},
				'media.reset': {actions: 'reset'},
				'commands.clear': {actions: 'clearCommands'},
			},
		},
	},
});

export type VoiceMediaSnapshot = SnapshotFrom<typeof voiceMediaStateMachine>;

export function createVoiceMediaSnapshot(): VoiceMediaSnapshot {
	return initialTransition(voiceMediaStateMachine)[0];
}

export function transitionVoiceMediaSnapshot(snapshot: VoiceMediaSnapshot, event: VoiceMediaEvent): VoiceMediaSnapshot {
	return transition(voiceMediaStateMachine, snapshot, event)[0] as VoiceMediaSnapshot;
}
