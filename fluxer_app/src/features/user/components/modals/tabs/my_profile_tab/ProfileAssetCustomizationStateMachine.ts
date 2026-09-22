// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TransientUploadFieldValue} from '@app/lib/forms/TransientUploadFields';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type ProfileAssetMode = 'inherit' | 'custom' | 'unset';

export interface ProfileAssetRemoteState {
	readonly identityKey: string | null;
	readonly mode: ProfileAssetMode;
	readonly hasCustomAsset: boolean;
}

interface ProfileAssetCustomizationContext extends ProfileAssetRemoteState {
	readonly initialMode: ProfileAssetMode;
	readonly previewUrl: string | null;
	readonly hasCleared: boolean;
}

export type ProfileAssetCustomizationEvent =
	| {
			type: 'asset.remoteApplied';
			remoteState: ProfileAssetRemoteState;
			force?: boolean;
	  }
	| {
			type: 'asset.modeSelected';
			mode: ProfileAssetMode;
	  }
	| {
			type: 'asset.uploaded';
			previewUrl: string;
	  }
	| {
			type: 'asset.cleared';
	  }
	| {
			type: 'asset.committed';
			remoteState: ProfileAssetRemoteState;
	  };

export type ProfileAssetCustomizationStateValue = 'unhydrated' | 'clean' | 'dirty' | 'committed';

export interface ProfileAssetCustomizationState extends ProfileAssetRemoteState {
	readonly state: ProfileAssetCustomizationStateValue;
	readonly initialMode: ProfileAssetMode;
	readonly previewUrl: string | null;
	readonly hasCleared: boolean;
	readonly hasAsset: boolean;
	readonly isDirty: boolean;
}

export interface ProfileAssetUploadPatch {
	readonly value: TransientUploadFieldValue;
}

function createContext(): ProfileAssetCustomizationContext {
	return {
		identityKey: null,
		mode: 'inherit',
		initialMode: 'inherit',
		hasCustomAsset: false,
		previewUrl: null,
		hasCleared: false,
	};
}

function isDirtyContext(context: ProfileAssetCustomizationContext): boolean {
	return context.mode !== context.initialMode || context.previewUrl !== null || context.hasCleared;
}

function applyRemoteState(remoteState: ProfileAssetRemoteState): ProfileAssetCustomizationContext {
	return {
		...remoteState,
		initialMode: remoteState.mode,
		previewUrl: null,
		hasCleared: false,
	};
}

function remoteIdentityChanged(
	context: ProfileAssetCustomizationContext,
	remoteState: ProfileAssetRemoteState,
): boolean {
	return context.identityKey !== remoteState.identityKey;
}

function remoteMatchesCurrent(
	context: ProfileAssetCustomizationContext,
	remoteState: ProfileAssetRemoteState,
): boolean {
	return (
		context.identityKey === remoteState.identityKey &&
		context.mode === remoteState.mode &&
		context.hasCustomAsset === remoteState.hasCustomAsset
	);
}

export function createProfileAssetRemoteState(params: {
	identityKey: string | null;
	mode: ProfileAssetMode;
	hasCustomAsset: boolean;
}): ProfileAssetRemoteState {
	return params;
}

export function createProfileAssetRemoteStateFromFlags(params: {
	identityKey: string | null;
	hasCustomAsset: boolean;
	isUnset: boolean;
}): ProfileAssetRemoteState {
	const mode: ProfileAssetMode = params.isUnset ? 'unset' : params.hasCustomAsset ? 'custom' : 'inherit';
	return createProfileAssetRemoteState({
		identityKey: params.identityKey,
		mode,
		hasCustomAsset: params.hasCustomAsset && mode === 'custom',
	});
}

export function createGlobalProfileAssetRemoteState(params: {
	identityKey: string | null;
	hasCustomAsset: boolean;
}): ProfileAssetRemoteState {
	return createProfileAssetRemoteState({
		identityKey: params.identityKey,
		mode: params.hasCustomAsset ? 'custom' : 'unset',
		hasCustomAsset: params.hasCustomAsset,
	});
}

export const profileAssetCustomizationStateMachine = setup({
	types: {} as {
		context: ProfileAssetCustomizationContext;
		events: ProfileAssetCustomizationEvent;
	},
	actions: {
		applyRemote: assign(({event}) => {
			if (event.type !== 'asset.remoteApplied') return {};
			return applyRemoteState(event.remoteState);
		}),
		commitRemote: assign(({event}) => {
			if (event.type !== 'asset.committed') return {};
			return applyRemoteState(event.remoteState);
		}),
		selectMode: assign(({context, event}) => {
			if (event.type !== 'asset.modeSelected') return {};
			if (event.mode === 'custom') {
				return {
					mode: 'custom' as const,
					hasCleared: false,
				};
			}
			return {
				mode: event.mode,
				hasCustomAsset: false,
				previewUrl: null,
				hasCleared:
					event.mode === 'unset' &&
					(context.initialMode !== 'unset' || context.hasCustomAsset || context.previewUrl !== null),
			};
		}),
		applyUpload: assign(({event}) => {
			if (event.type !== 'asset.uploaded') return {};
			return {
				mode: 'custom' as const,
				hasCustomAsset: true,
				previewUrl: event.previewUrl,
				hasCleared: false,
			};
		}),
		clearAsset: assign({
			mode: 'unset',
			hasCustomAsset: false,
			previewUrl: null,
			hasCleared: true,
		}),
	},
	guards: {
		isDirty: ({context}) => isDirtyContext(context),
		remoteIdentityChanged: ({context, event}) =>
			(event.type === 'asset.remoteApplied' || event.type === 'asset.committed') &&
			remoteIdentityChanged(context, event.remoteState),
		forceRemoteApply: ({event}) => event.type === 'asset.remoteApplied' && event.force === true,
		remoteMatchesCurrent: ({context, event}) =>
			(event.type === 'asset.remoteApplied' || event.type === 'asset.committed') &&
			remoteMatchesCurrent(context, event.remoteState),
	},
}).createMachine({
	id: 'profileAssetCustomization',
	context: () => createContext(),
	initial: 'unhydrated',
	states: {
		unhydrated: {
			on: {
				'asset.remoteApplied': {target: 'clean', actions: 'applyRemote'},
				'asset.committed': {target: 'committed', actions: 'commitRemote'},
				'asset.modeSelected': {target: 'routing', actions: 'selectMode'},
				'asset.uploaded': {target: 'routing', actions: 'applyUpload'},
				'asset.cleared': {target: 'routing', actions: 'clearAsset'},
			},
		},
		routing: {
			always: [{guard: 'isDirty', target: 'dirty'}, {target: 'clean'}],
		},
		clean: {
			on: {
				'asset.remoteApplied': {target: 'clean', actions: 'applyRemote'},
				'asset.committed': {target: 'committed', actions: 'commitRemote'},
				'asset.modeSelected': {target: 'routing', actions: 'selectMode'},
				'asset.uploaded': {target: 'routing', actions: 'applyUpload'},
				'asset.cleared': {target: 'routing', actions: 'clearAsset'},
			},
		},
		dirty: {
			on: {
				'asset.remoteApplied': [
					{guard: 'forceRemoteApply', target: 'clean', actions: 'applyRemote'},
					{guard: 'remoteIdentityChanged', target: 'clean', actions: 'applyRemote'},
					{target: 'dirty'},
				],
				'asset.committed': {target: 'committed', actions: 'commitRemote'},
				'asset.modeSelected': {target: 'routing', actions: 'selectMode'},
				'asset.uploaded': {target: 'routing', actions: 'applyUpload'},
				'asset.cleared': {target: 'routing', actions: 'clearAsset'},
			},
		},
		committed: {
			on: {
				'asset.remoteApplied': [
					{guard: 'forceRemoteApply', target: 'clean', actions: 'applyRemote'},
					{guard: 'remoteIdentityChanged', target: 'clean', actions: 'applyRemote'},
					{guard: 'remoteMatchesCurrent', target: 'clean', actions: 'applyRemote'},
					{target: 'committed'},
				],
				'asset.committed': {target: 'committed', actions: 'commitRemote'},
				'asset.modeSelected': {target: 'routing', actions: 'selectMode'},
				'asset.uploaded': {target: 'routing', actions: 'applyUpload'},
				'asset.cleared': {target: 'routing', actions: 'clearAsset'},
			},
		},
	},
});

export type ProfileAssetCustomizationSnapshot = SnapshotFrom<typeof profileAssetCustomizationStateMachine>;

export function createProfileAssetCustomizationSnapshot(): ProfileAssetCustomizationSnapshot {
	return initialTransition(profileAssetCustomizationStateMachine)[0];
}

export function transitionProfileAssetCustomizationSnapshot(
	snapshot: ProfileAssetCustomizationSnapshot,
	event: ProfileAssetCustomizationEvent,
): ProfileAssetCustomizationSnapshot {
	return transition(profileAssetCustomizationStateMachine, snapshot, event)[0] as ProfileAssetCustomizationSnapshot;
}

export function getProfileAssetCustomizationStateValue(
	snapshot: ProfileAssetCustomizationSnapshot,
): ProfileAssetCustomizationStateValue {
	return typeof snapshot.value === 'string' ? (snapshot.value as ProfileAssetCustomizationStateValue) : 'unhydrated';
}

export function selectProfileAssetCustomizationState(
	snapshot: ProfileAssetCustomizationSnapshot,
): ProfileAssetCustomizationState {
	const state = getProfileAssetCustomizationStateValue(snapshot);
	return {
		...snapshot.context,
		state,
		hasAsset: snapshot.context.hasCustomAsset || snapshot.context.previewUrl !== null,
		isDirty: state === 'dirty',
	};
}

export function getProfileAssetUploadPatch(state: ProfileAssetCustomizationState): ProfileAssetUploadPatch {
	if (state.hasCleared) return {value: null};
	if (state.previewUrl !== null) return {value: state.previewUrl};
	if ((state.mode === 'inherit' || state.mode === 'unset') && state.mode !== state.initialMode) {
		return {value: null};
	}
	return {value: undefined};
}

export function assignProfileAssetUploadPatch<TPayload extends object>(
	payload: TPayload,
	key: string,
	state: ProfileAssetCustomizationState,
): void {
	const {value} = getProfileAssetUploadPatch(state);
	if (value === undefined) return;
	(payload as Record<string, TransientUploadFieldValue>)[key] = value;
}
