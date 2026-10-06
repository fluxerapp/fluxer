// SPDX-License-Identifier: AGPL-3.0-or-later

import {normalizeVoiceMediaGraphViewerStreamKeys} from '@app/features/voice/engine/VoiceMediaGraph';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export interface LocalVoiceConnectionState {
	selfMute: boolean;
	selfDeaf: boolean;
	selfVideo: boolean;
	selfStream: boolean;
	viewerStreamKeys: Array<string>;
	hasUserSetMute: boolean;
	hasUserSetDeaf: boolean;
	mutedByPermission: boolean;
	shouldUnmuteOnUndeafen: boolean;
}

export type LocalVoiceStateSeed = Partial<LocalVoiceConnectionState>;

export interface LocalVoicePersistedDefaults {
	selfMute: boolean;
	selfDeaf: boolean;
	hasUserSetMute: boolean;
	hasUserSetDeaf: boolean;
}

export interface LocalVoiceStateContext {
	fallback: LocalVoiceConnectionState;
	persistedDefaults: LocalVoicePersistedDefaults;
	microphonePermissionGranted: boolean | null;
	connections: Record<string, LocalVoiceConnectionState>;
}

export interface LocalVoiceStateInput {
	microphonePermissionGranted: boolean | null;
	persistedDefaults?: Partial<LocalVoicePersistedDefaults>;
	fallback?: Partial<LocalVoiceConnectionState>;
	connections?: Record<string, LocalVoiceStateSeed>;
}

export type LocalVoiceStateEvent =
	| {
			type: 'defaults.apply';
			activeConnectionId: string | null;
			persistedDefaults: Partial<LocalVoicePersistedDefaults>;
	  }
	| {
			type: 'permission.sync';
			activeConnectionId: string | null;
			microphoneGranted: boolean;
			defaultMuteInitialized: boolean;
	  }
	| {type: 'permission.deny'; activeConnectionId: string | null}
	| {type: 'permission.grant'; activeConnectionId: string | null}
	| {type: 'viewer.replace'; activeConnectionId: string | null; keys: ReadonlyArray<string>}
	| {type: 'mute.toggle'; activeConnectionId: string | null}
	| {type: 'deaf.toggle'; activeConnectionId: string | null}
	| {type: 'video.toggle'; activeConnectionId: string | null}
	| {type: 'stream.toggle'; activeConnectionId: string | null}
	| {type: 'mute.clearUserSet'; activeConnectionId: string | null}
	| {type: 'mute.update'; activeConnectionId: string | null; muted: boolean}
	| {type: 'deaf.update'; activeConnectionId: string | null; deafened: boolean}
	| {type: 'video.update'; activeConnectionId: string | null; video: boolean}
	| {type: 'stream.update'; activeConnectionId: string | null; streaming: boolean}
	| {type: 'preferences.reset'}
	| {type: 'connection.seed'; connectionId: string; seed: LocalVoiceStateSeed}
	| {type: 'connection.sync'; connectionId: string; seed: LocalVoiceStateSeed}
	| {type: 'connection.clear'; connectionId: string | null};

const DEFAULT_PERSISTED_DEFAULTS: LocalVoicePersistedDefaults = {
	selfMute: false,
	selfDeaf: false,
	hasUserSetMute: false,
	hasUserSetDeaf: false,
};

function createLocalVoiceConnectionState(
	defaults: LocalVoicePersistedDefaults,
	seed: Partial<LocalVoiceConnectionState> = {},
): LocalVoiceConnectionState {
	return {
		selfMute: seed.selfMute ?? defaults.selfMute,
		selfDeaf: seed.selfDeaf ?? defaults.selfDeaf,
		selfVideo: seed.selfVideo ?? false,
		selfStream: seed.selfStream ?? false,
		viewerStreamKeys: normalizeVoiceMediaGraphViewerStreamKeys(seed.viewerStreamKeys ?? []),
		hasUserSetMute: seed.hasUserSetMute ?? defaults.hasUserSetMute,
		hasUserSetDeaf: seed.hasUserSetDeaf ?? defaults.hasUserSetDeaf,
		mutedByPermission: seed.mutedByPermission ?? false,
		shouldUnmuteOnUndeafen: seed.shouldUnmuteOnUndeafen ?? false,
	};
}

function cloneState(state: LocalVoiceConnectionState): LocalVoiceConnectionState {
	return {
		...state,
		viewerStreamKeys: [...state.viewerStreamKeys],
	};
}

function normalizeDefaults(defaults?: Partial<LocalVoicePersistedDefaults>): LocalVoicePersistedDefaults {
	return {
		...DEFAULT_PERSISTED_DEFAULTS,
		...defaults,
	};
}

function createConnectionStateFromContext(
	context: LocalVoiceStateContext,
	seed: LocalVoiceStateSeed = {},
): LocalVoiceConnectionState {
	return createLocalVoiceConnectionState(context.persistedDefaults, {
		selfVideo: context.fallback.selfVideo,
		selfStream: context.fallback.selfStream,
		viewerStreamKeys: context.fallback.viewerStreamKeys,
		mutedByPermission: context.fallback.mutedByPermission,
		shouldUnmuteOnUndeafen: context.fallback.shouldUnmuteOnUndeafen,
		...seed,
	});
}

function withConnection(
	context: LocalVoiceStateContext,
	connectionId: string,
	state: LocalVoiceConnectionState,
): LocalVoiceStateContext {
	return {
		...context,
		connections: {
			...context.connections,
			[connectionId]: state,
		},
	};
}

function getActiveStateForRead(
	context: LocalVoiceStateContext,
	activeConnectionId: string | null,
): LocalVoiceConnectionState {
	return activeConnectionId ? (context.connections[activeConnectionId] ?? context.fallback) : context.fallback;
}

function updateActiveState(
	context: LocalVoiceStateContext,
	activeConnectionId: string | null,
	updater: (state: LocalVoiceConnectionState) => LocalVoiceConnectionState,
): LocalVoiceStateContext {
	if (!activeConnectionId) {
		return {
			...context,
			fallback: updater(cloneState(context.fallback)),
		};
	}
	const current = context.connections[activeConnectionId] ?? createConnectionStateFromContext(context);
	return withConnection(context, activeConnectionId, updater(cloneState(current)));
}

function persistMuteAndDeafenDefaults(
	context: LocalVoiceStateContext,
	state: LocalVoiceConnectionState,
): LocalVoiceStateContext {
	return {
		...context,
		fallback: {
			...context.fallback,
			selfMute: state.selfMute,
			selfDeaf: state.selfDeaf,
			hasUserSetMute: state.hasUserSetMute,
			hasUserSetDeaf: state.hasUserSetDeaf,
			shouldUnmuteOnUndeafen: state.shouldUnmuteOnUndeafen,
		},
		persistedDefaults: {
			selfMute: state.selfMute,
			selfDeaf: state.selfDeaf,
			hasUserSetMute: state.hasUserSetMute,
			hasUserSetDeaf: state.hasUserSetDeaf,
		},
	};
}

function updateActiveAndPersistAudio(
	context: LocalVoiceStateContext,
	activeConnectionId: string | null,
	updater: (state: LocalVoiceConnectionState) => LocalVoiceConnectionState,
): LocalVoiceStateContext {
	let nextActiveState: LocalVoiceConnectionState | null = null;
	const nextContext = updateActiveState(context, activeConnectionId, (state) => {
		nextActiveState = updater(state);
		return nextActiveState;
	});
	return persistMuteAndDeafenDefaults(
		nextContext,
		nextActiveState ?? getActiveStateForRead(nextContext, activeConnectionId),
	);
}

function restorePersistedMutePreferenceAfterPermissionGrant(
	state: LocalVoiceConnectionState,
	defaults: LocalVoicePersistedDefaults,
): LocalVoiceConnectionState {
	return {
		...state,
		selfMute: defaults.hasUserSetMute ? defaults.selfMute : false,
		hasUserSetMute: defaults.hasUserSetMute,
	};
}

function applyPersistedDefaultsToFallbackState(
	context: LocalVoiceStateContext,
	persistedDefaults: Partial<LocalVoicePersistedDefaults>,
	activeConnectionId: string | null,
): LocalVoiceStateContext {
	const nextDefaults = normalizeDefaults({...context.persistedDefaults, ...persistedDefaults});
	let nextContext: LocalVoiceStateContext = {
		...context,
		persistedDefaults: nextDefaults,
	};
	if (activeConnectionId) return nextContext;
	const micGranted = context.microphonePermissionGranted ?? true;
	const fallback = {
		...context.fallback,
		selfMute: micGranted ? nextDefaults.selfMute : true,
		selfDeaf: nextDefaults.selfDeaf,
		hasUserSetMute: nextDefaults.hasUserSetMute,
		hasUserSetDeaf: nextDefaults.hasUserSetDeaf,
		mutedByPermission: !micGranted,
	};
	nextContext = {
		...nextContext,
		fallback: {
			...fallback,
			shouldUnmuteOnUndeafen: fallback.selfDeaf ? fallback.shouldUnmuteOnUndeafen : false,
		},
	};
	return nextContext;
}

function applyTransientPermissionMute(
	context: LocalVoiceStateContext,
	activeConnectionId: string | null,
): LocalVoiceStateContext {
	return updateActiveState(
		{
			...context,
			microphonePermissionGranted: false,
		},
		activeConnectionId,
		(state) => ({
			...state,
			selfMute: true,
			mutedByPermission: true,
		}),
	);
}

function applyPermissionGrant(
	context: LocalVoiceStateContext,
	activeConnectionId: string | null,
	restorePersistedMutePreference: boolean,
): LocalVoiceStateContext {
	return updateActiveAndPersistAudio(
		{
			...context,
			microphonePermissionGranted: true,
		},
		activeConnectionId,
		(state) => {
			const restored = restorePersistedMutePreference
				? restorePersistedMutePreferenceAfterPermissionGrant(state, context.persistedDefaults)
				: state;
			return {
				...restored,
				mutedByPermission: false,
			};
		},
	);
}

function syncPermission(
	context: LocalVoiceStateContext,
	activeConnectionId: string | null,
	microphoneGranted: boolean,
	defaultMuteInitialized: boolean,
): LocalVoiceStateContext {
	if (!microphoneGranted) {
		return applyTransientPermissionMute(context, activeConnectionId);
	}
	const state = getActiveStateForRead(context, activeConnectionId);
	if (state.mutedByPermission) {
		return applyPermissionGrant(context, activeConnectionId, true);
	}
	const shouldApplyDefaultUnmute = !defaultMuteInitialized && !state.hasUserSetMute && state.selfMute;
	return updateActiveAndPersistAudio(
		{
			...context,
			microphonePermissionGranted: true,
		},
		activeConnectionId,
		(activeState) => ({
			...activeState,
			selfMute: shouldApplyDefaultUnmute ? false : activeState.selfMute,
			mutedByPermission: false,
		}),
	);
}

function toggleSelfMute(context: LocalVoiceStateContext, activeConnectionId: string | null): LocalVoiceStateContext {
	const micDenied = context.microphonePermissionGranted === false;
	return updateActiveAndPersistAudio(context, activeConnectionId, (state) => {
		const newSelfMute = !state.selfMute;
		if (state.selfDeaf && !newSelfMute) {
			if (micDenied) {
				return {
					...state,
					selfDeaf: false,
					hasUserSetMute: true,
					hasUserSetDeaf: true,
					mutedByPermission: true,
					shouldUnmuteOnUndeafen: false,
				};
			}
			return {
				...state,
				selfMute: false,
				selfDeaf: false,
				hasUserSetMute: true,
				hasUserSetDeaf: true,
				shouldUnmuteOnUndeafen: false,
			};
		}
		if (micDenied && !newSelfMute) {
			return {
				...state,
				hasUserSetMute: true,
				mutedByPermission: true,
			};
		}
		return {
			...state,
			selfMute: newSelfMute,
			hasUserSetMute: true,
			shouldUnmuteOnUndeafen: state.selfDeaf ? state.shouldUnmuteOnUndeafen : false,
		};
	});
}

function toggleSelfDeaf(context: LocalVoiceStateContext, activeConnectionId: string | null): LocalVoiceStateContext {
	const micDenied = context.microphonePermissionGranted === false;
	return updateActiveAndPersistAudio(context, activeConnectionId, (state) => {
		const newSelfDeaf = !state.selfDeaf;
		if (newSelfDeaf) {
			const wasMutedBefore = state.selfMute || micDenied;
			return {
				...state,
				selfDeaf: true,
				selfMute: true,
				hasUserSetDeaf: true,
				shouldUnmuteOnUndeafen: !wasMutedBefore,
			};
		}
		return {
			...state,
			selfDeaf: false,
			selfMute: state.shouldUnmuteOnUndeafen && !micDenied ? false : state.selfMute,
			hasUserSetDeaf: true,
			shouldUnmuteOnUndeafen: false,
		};
	});
}

function updateSelfMute(
	context: LocalVoiceStateContext,
	activeConnectionId: string | null,
	muted: boolean,
): LocalVoiceStateContext {
	const micDenied = context.microphonePermissionGranted === false;
	return updateActiveAndPersistAudio(context, activeConnectionId, (state) => {
		if (micDenied && !muted) {
			return {
				...state,
				selfMute: true,
				mutedByPermission: true,
			};
		}
		return {
			...state,
			selfMute: muted,
		};
	});
}

function updateSelfDeaf(
	context: LocalVoiceStateContext,
	activeConnectionId: string | null,
	deafened: boolean,
): LocalVoiceStateContext {
	return updateActiveAndPersistAudio(context, activeConnectionId, (state) => ({
		...state,
		selfDeaf: deafened,
		shouldUnmuteOnUndeafen: deafened ? state.shouldUnmuteOnUndeafen : false,
	}));
}

function seedConnectionState(
	context: LocalVoiceStateContext,
	connectionId: string,
	seed: LocalVoiceStateSeed,
): LocalVoiceStateContext {
	if (context.connections[connectionId]) return context;
	const state = createConnectionStateFromContext(context, seed);
	return persistMuteAndDeafenDefaults(withConnection(context, connectionId, state), state);
}

function syncConnectionState(
	context: LocalVoiceStateContext,
	connectionId: string,
	seed: LocalVoiceStateSeed,
): LocalVoiceStateContext {
	const state = {
		...(context.connections[connectionId] ?? createConnectionStateFromContext(context, seed)),
		...(seed.selfMute === undefined ? {} : {selfMute: seed.selfMute}),
		...(seed.selfDeaf === undefined ? {} : {selfDeaf: seed.selfDeaf}),
		...(seed.selfVideo === undefined ? {} : {selfVideo: seed.selfVideo}),
		...(seed.selfStream === undefined ? {} : {selfStream: seed.selfStream}),
		...(seed.viewerStreamKeys === undefined
			? {}
			: {viewerStreamKeys: normalizeVoiceMediaGraphViewerStreamKeys(seed.viewerStreamKeys)}),
		...(seed.hasUserSetMute === undefined ? {} : {hasUserSetMute: seed.hasUserSetMute}),
		...(seed.hasUserSetDeaf === undefined ? {} : {hasUserSetDeaf: seed.hasUserSetDeaf}),
		...(seed.mutedByPermission === undefined ? {} : {mutedByPermission: seed.mutedByPermission}),
		...(seed.shouldUnmuteOnUndeafen === undefined ? {} : {shouldUnmuteOnUndeafen: seed.shouldUnmuteOnUndeafen}),
	};
	return persistMuteAndDeafenDefaults(withConnection(context, connectionId, state), state);
}

function clearConnectionState(context: LocalVoiceStateContext, connectionId: string | null): LocalVoiceStateContext {
	if (!connectionId) return context;
	const connections = {...context.connections};
	delete connections[connectionId];
	return {
		...context,
		connections,
		fallback: {
			...context.fallback,
			selfVideo: false,
			selfStream: false,
			viewerStreamKeys: [],
		},
	};
}

function resetPreferences(context: LocalVoiceStateContext): LocalVoiceStateContext {
	const persistedDefaults = DEFAULT_PERSISTED_DEFAULTS;
	return {
		...context,
		persistedDefaults,
		connections: {},
		fallback: createLocalVoiceConnectionState(persistedDefaults, {
			selfMute: context.microphonePermissionGranted === false,
			mutedByPermission: context.microphonePermissionGranted === false,
		}),
	};
}

export const localVoiceStateMachine = setup({
	types: {} as {
		context: LocalVoiceStateContext;
		events: LocalVoiceStateEvent;
		input: LocalVoiceStateInput;
	},
	actions: {
		applyDefaults: assign(({context, event}) =>
			event.type === 'defaults.apply'
				? applyPersistedDefaultsToFallbackState(context, event.persistedDefaults, event.activeConnectionId)
				: context,
		),
		syncPermission: assign(({context, event}) =>
			event.type === 'permission.sync'
				? syncPermission(context, event.activeConnectionId, event.microphoneGranted, event.defaultMuteInitialized)
				: context,
		),
		denyPermission: assign(({context, event}) =>
			event.type === 'permission.deny' ? applyTransientPermissionMute(context, event.activeConnectionId) : context,
		),
		grantPermission: assign(({context, event}) =>
			event.type === 'permission.grant' ? applyPermissionGrant(context, event.activeConnectionId, true) : context,
		),
		replaceViewerKeys: assign(({context, event}) =>
			event.type === 'viewer.replace'
				? updateActiveState(context, event.activeConnectionId, (state) => ({
						...state,
						viewerStreamKeys: normalizeVoiceMediaGraphViewerStreamKeys(event.keys),
					}))
				: context,
		),
		toggleMute: assign(({context, event}) =>
			event.type === 'mute.toggle' ? toggleSelfMute(context, event.activeConnectionId) : context,
		),
		toggleDeaf: assign(({context, event}) =>
			event.type === 'deaf.toggle' ? toggleSelfDeaf(context, event.activeConnectionId) : context,
		),
		toggleVideo: assign(({context, event}) =>
			event.type === 'video.toggle'
				? updateActiveState(context, event.activeConnectionId, (state) => ({...state, selfVideo: !state.selfVideo}))
				: context,
		),
		toggleStream: assign(({context, event}) =>
			event.type === 'stream.toggle'
				? updateActiveState(context, event.activeConnectionId, (state) => ({
						...state,
						selfStream: !state.selfStream,
					}))
				: context,
		),
		clearUserSetMute: assign(({context, event}) =>
			event.type === 'mute.clearUserSet'
				? updateActiveAndPersistAudio(context, event.activeConnectionId, (state) => ({
						...state,
						hasUserSetMute: false,
					}))
				: context,
		),
		updateMute: assign(({context, event}) =>
			event.type === 'mute.update' ? updateSelfMute(context, event.activeConnectionId, event.muted) : context,
		),
		updateDeaf: assign(({context, event}) =>
			event.type === 'deaf.update' ? updateSelfDeaf(context, event.activeConnectionId, event.deafened) : context,
		),
		updateVideo: assign(({context, event}) =>
			event.type === 'video.update'
				? updateActiveState(context, event.activeConnectionId, (state) => ({...state, selfVideo: event.video}))
				: context,
		),
		updateStream: assign(({context, event}) =>
			event.type === 'stream.update'
				? updateActiveState(context, event.activeConnectionId, (state) => ({...state, selfStream: event.streaming}))
				: context,
		),
		resetPreferences: assign(({context}) => resetPreferences(context)),
		seedConnection: assign(({context, event}) =>
			event.type === 'connection.seed' ? seedConnectionState(context, event.connectionId, event.seed) : context,
		),
		syncConnection: assign(({context, event}) =>
			event.type === 'connection.sync' ? syncConnectionState(context, event.connectionId, event.seed) : context,
		),
		clearConnection: assign(({context, event}) =>
			event.type === 'connection.clear' ? clearConnectionState(context, event.connectionId) : context,
		),
	},
}).createMachine({
	id: 'localVoiceState',
	context: ({input}) => {
		const persistedDefaults = normalizeDefaults(input.persistedDefaults);
		const fallback = createLocalVoiceConnectionState(persistedDefaults, input.fallback);
		const connections: Record<string, LocalVoiceConnectionState> = {};
		for (const [connectionId, seed] of Object.entries(input.connections ?? {})) {
			connections[connectionId] = createLocalVoiceConnectionState(persistedDefaults, seed);
		}
		return {
			fallback,
			persistedDefaults,
			microphonePermissionGranted: input.microphonePermissionGranted,
			connections,
		};
	},
	initial: 'tracking',
	states: {
		tracking: {
			on: {
				'defaults.apply': {actions: 'applyDefaults'},
				'permission.sync': {actions: 'syncPermission'},
				'permission.deny': {actions: 'denyPermission'},
				'permission.grant': {actions: 'grantPermission'},
				'viewer.replace': {actions: 'replaceViewerKeys'},
				'mute.toggle': {actions: 'toggleMute'},
				'deaf.toggle': {actions: 'toggleDeaf'},
				'video.toggle': {actions: 'toggleVideo'},
				'stream.toggle': {actions: 'toggleStream'},
				'mute.clearUserSet': {actions: 'clearUserSetMute'},
				'mute.update': {actions: 'updateMute'},
				'deaf.update': {actions: 'updateDeaf'},
				'video.update': {actions: 'updateVideo'},
				'stream.update': {actions: 'updateStream'},
				'preferences.reset': {actions: 'resetPreferences'},
				'connection.seed': {actions: 'seedConnection'},
				'connection.sync': {actions: 'syncConnection'},
				'connection.clear': {actions: 'clearConnection'},
			},
		},
	},
});

export type LocalVoiceStateSnapshot = SnapshotFrom<typeof localVoiceStateMachine>;

export function createLocalVoiceStateSnapshot(input: LocalVoiceStateInput): LocalVoiceStateSnapshot {
	return initialTransition(localVoiceStateMachine, input)[0];
}

export function transitionLocalVoiceStateSnapshot(
	snapshot: LocalVoiceStateSnapshot,
	event: LocalVoiceStateEvent,
): LocalVoiceStateSnapshot {
	return transition(localVoiceStateMachine, snapshot, event)[0] as LocalVoiceStateSnapshot;
}

export function getActiveLocalVoiceState(
	snapshot: LocalVoiceStateSnapshot,
	activeConnectionId: string | null,
): LocalVoiceConnectionState {
	return cloneState(getActiveStateForRead(snapshot.context, activeConnectionId));
}

export function hasLocalVoiceConnectionState(snapshot: LocalVoiceStateSnapshot, connectionId: string | null): boolean {
	return connectionId ? snapshot.context.connections[connectionId] !== undefined : false;
}
