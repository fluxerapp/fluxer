// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	assignProfileAssetUploadPatch,
	createGlobalProfileAssetRemoteState,
	createProfileAssetCustomizationSnapshot,
	createProfileAssetRemoteStateFromFlags,
	getProfileAssetUploadPatch,
	type ProfileAssetCustomizationEvent,
	type ProfileAssetCustomizationSnapshot,
	type ProfileAssetMode,
	type ProfileAssetRemoteState,
	selectProfileAssetCustomizationState,
	transitionProfileAssetCustomizationSnapshot,
} from '@app/features/user/components/modals/tabs/my_profile_tab/ProfileAssetCustomizationStateMachine';
import {describe, expect, it} from 'vitest';

const IDENTITY = 'user-1:guild-1';
const OTHER_IDENTITY = 'user-1:guild-2';
const DATA_URL = 'data:image/png;base64,AAA';

function remote(mode: ProfileAssetMode, hasCustomAsset: boolean, identityKey = IDENTITY): ProfileAssetRemoteState {
	return {identityKey, mode, hasCustomAsset};
}

function transition(
	snapshot: ProfileAssetCustomizationSnapshot,
	event: ProfileAssetCustomizationEvent,
): ProfileAssetCustomizationSnapshot {
	return transitionProfileAssetCustomizationSnapshot(snapshot, event);
}

function hydrate(remoteState: ProfileAssetRemoteState): ProfileAssetCustomizationSnapshot {
	return transition(createProfileAssetCustomizationSnapshot(), {type: 'asset.remoteApplied', remoteState});
}

describe('ProfileAssetCustomizationStateMachine', () => {
	it('derives per-community asset modes from custom hashes and unset flags', () => {
		expect(
			createProfileAssetRemoteStateFromFlags({
				identityKey: IDENTITY,
				hasCustomAsset: false,
				isUnset: false,
			}),
		).toEqual(remote('inherit', false));
		expect(
			createProfileAssetRemoteStateFromFlags({
				identityKey: IDENTITY,
				hasCustomAsset: true,
				isUnset: false,
			}),
		).toEqual(remote('custom', true));
		expect(
			createProfileAssetRemoteStateFromFlags({
				identityKey: IDENTITY,
				hasCustomAsset: true,
				isUnset: true,
			}),
		).toEqual(remote('unset', false));
	});

	it('tracks an accepted upload as a dirty custom asset patch', () => {
		let snapshot = hydrate(remote('inherit', false));
		snapshot = transition(snapshot, {type: 'asset.uploaded', previewUrl: DATA_URL});

		const state = selectProfileAssetCustomizationState(snapshot);
		expect(state).toMatchObject({
			state: 'dirty',
			mode: 'custom',
			initialMode: 'inherit',
			hasCustomAsset: true,
			previewUrl: DATA_URL,
			hasCleared: false,
			hasAsset: true,
			isDirty: true,
		});
		expect(getProfileAssetUploadPatch(state)).toEqual({value: DATA_URL});
	});

	it('sends null when a custom asset is changed back to inherit or unset', () => {
		let snapshot = hydrate(remote('custom', true));
		snapshot = transition(snapshot, {type: 'asset.modeSelected', mode: 'inherit'});
		let state = selectProfileAssetCustomizationState(snapshot);
		expect(state).toMatchObject({state: 'dirty', mode: 'inherit', hasCleared: false});
		expect(getProfileAssetUploadPatch(state)).toEqual({value: null});

		snapshot = hydrate(remote('custom', true));
		snapshot = transition(snapshot, {type: 'asset.modeSelected', mode: 'unset'});
		state = selectProfileAssetCustomizationState(snapshot);
		expect(state).toMatchObject({state: 'dirty', mode: 'unset', hasCleared: true});
		expect(getProfileAssetUploadPatch(state)).toEqual({value: null});
	});

	it('does not revert an accepted custom upload when a stale same-identity remote snapshot says inherit', () => {
		let snapshot = hydrate(remote('inherit', false));
		snapshot = transition(snapshot, {type: 'asset.uploaded', previewUrl: DATA_URL});
		snapshot = transition(snapshot, {type: 'asset.committed', remoteState: remote('custom', true)});

		let state = selectProfileAssetCustomizationState(snapshot);
		expect(state).toMatchObject({
			state: 'committed',
			mode: 'custom',
			initialMode: 'custom',
			hasCustomAsset: true,
			previewUrl: null,
			isDirty: false,
		});

		snapshot = transition(snapshot, {type: 'asset.remoteApplied', remoteState: remote('inherit', false)});
		state = selectProfileAssetCustomizationState(snapshot);
		expect(state).toMatchObject({
			state: 'committed',
			mode: 'custom',
			hasCustomAsset: true,
			isDirty: false,
		});
		expect(getProfileAssetUploadPatch(state)).toEqual({value: undefined});

		snapshot = transition(snapshot, {type: 'asset.remoteApplied', remoteState: remote('custom', true)});
		state = selectProfileAssetCustomizationState(snapshot);
		expect(state).toMatchObject({state: 'clean', mode: 'custom', hasCustomAsset: true});
	});

	it('protects dirty local edits from same-identity remote refreshes but accepts identity changes', () => {
		let snapshot = hydrate(remote('inherit', false));
		snapshot = transition(snapshot, {type: 'asset.uploaded', previewUrl: DATA_URL});
		snapshot = transition(snapshot, {type: 'asset.remoteApplied', remoteState: remote('inherit', false)});

		let state = selectProfileAssetCustomizationState(snapshot);
		expect(state).toMatchObject({state: 'dirty', mode: 'custom', previewUrl: DATA_URL});

		snapshot = transition(snapshot, {
			type: 'asset.remoteApplied',
			remoteState: remote('inherit', false, OTHER_IDENTITY),
		});
		state = selectProfileAssetCustomizationState(snapshot);
		expect(state).toMatchObject({
			state: 'clean',
			identityKey: OTHER_IDENTITY,
			mode: 'inherit',
			previewUrl: null,
			isDirty: false,
		});
	});

	it('allows explicit same-identity resets to discard dirty local edits', () => {
		let snapshot = hydrate(remote('inherit', false));
		snapshot = transition(snapshot, {type: 'asset.uploaded', previewUrl: DATA_URL});
		snapshot = transition(snapshot, {
			type: 'asset.remoteApplied',
			remoteState: remote('inherit', false),
			force: true,
		});

		const state = selectProfileAssetCustomizationState(snapshot);
		expect(state).toMatchObject({
			state: 'clean',
			mode: 'inherit',
			previewUrl: null,
			hasCleared: false,
			isDirty: false,
		});
	});

	it('assigns upload patches without writing undefined fields', () => {
		const cleanState = selectProfileAssetCustomizationState(hydrate(remote('inherit', false)));
		const dirtyState = selectProfileAssetCustomizationState(
			transition(hydrate(remote('inherit', false)), {type: 'asset.uploaded', previewUrl: DATA_URL}),
		);
		const payload: {avatar?: string | null} = {};
		assignProfileAssetUploadPatch(payload, 'avatar', cleanState);
		expect(payload).toEqual({});
		assignProfileAssetUploadPatch(payload, 'avatar', dirtyState);
		expect(payload).toEqual({avatar: DATA_URL});
	});

	it('models global profile assets as custom or unset instead of inherit', () => {
		expect(createGlobalProfileAssetRemoteState({identityKey: 'user-1:global', hasCustomAsset: true})).toEqual({
			identityKey: 'user-1:global',
			mode: 'custom',
			hasCustomAsset: true,
		});
		expect(createGlobalProfileAssetRemoteState({identityKey: 'user-1:global', hasCustomAsset: false})).toEqual({
			identityKey: 'user-1:global',
			mode: 'unset',
			hasCustomAsset: false,
		});
	});
});
