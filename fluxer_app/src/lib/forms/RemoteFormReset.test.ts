// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createRemoteFormResetMachineSnapshot,
	getRemoteFormResetDecision,
	getRemoteFormResetMachineStateValue,
	type RemoteFormResetMachineEvent,
	type RemoteFormResetMachineSnapshot,
	transitionRemoteFormResetSnapshot,
} from '@app/lib/forms/RemoteFormReset';
import {describe, expect, it} from 'vitest';

function transition(
	snapshot: RemoteFormResetMachineSnapshot,
	event: RemoteFormResetMachineEvent,
): RemoteFormResetMachineSnapshot {
	return transitionRemoteFormResetSnapshot(snapshot, event);
}

describe('remoteFormResetStateMachine', () => {
	it('starts unhydrated and applies the first remote values regardless of dirty state', () => {
		let snapshot = createRemoteFormResetMachineSnapshot();
		expect(getRemoteFormResetMachineStateValue(snapshot)).toBe('unhydrated');
		snapshot = transition(snapshot, {
			type: 'remote.inspect',
			identityChanged: false,
			remoteValuesChanged: false,
			isDirty: true,
		});
		expect(getRemoteFormResetMachineStateValue(snapshot)).toBe('hydrated');
		expect(snapshot.context.decision).toEqual({shouldReset: true, reason: 'initial'});
	});

	it('keeps dirty same-identity remote refreshes from resetting the form', () => {
		let snapshot = createRemoteFormResetMachineSnapshot();
		snapshot = transition(snapshot, {type: 'remote.commit'});
		snapshot = transition(snapshot, {
			type: 'remote.inspect',
			identityChanged: false,
			remoteValuesChanged: true,
			isDirty: true,
		});
		expect(snapshot.context.decision).toEqual({shouldReset: false});
	});

	it('allows identity changes, clean remote refreshes, explicit resets, and commits', () => {
		let snapshot = createRemoteFormResetMachineSnapshot();
		snapshot = transition(snapshot, {type: 'remote.commit'});
		snapshot = transition(snapshot, {
			type: 'remote.inspect',
			identityChanged: true,
			remoteValuesChanged: true,
			isDirty: true,
		});
		expect(snapshot.context.decision).toEqual({shouldReset: true, reason: 'identity-change'});
		snapshot = transition(snapshot, {
			type: 'remote.inspect',
			identityChanged: false,
			remoteValuesChanged: true,
			isDirty: false,
		});
		expect(snapshot.context.decision).toEqual({shouldReset: true, reason: 'remote-clean'});
		snapshot = transition(snapshot, {type: 'remote.explicitReset'});
		expect(snapshot.context.decision).toEqual({shouldReset: true, reason: 'explicit-reset'});
		snapshot = transition(snapshot, {type: 'remote.commit'});
		expect(snapshot.context.decision).toEqual({shouldReset: true, reason: 'commit'});
	});
});

describe('getRemoteFormResetDecision', () => {
	it('hydrates before any remote values have been applied', () => {
		expect(
			getRemoteFormResetDecision({
				hasAppliedRemoteValues: false,
				identityChanged: false,
				remoteValuesChanged: false,
				isDirty: true,
			}),
		).toEqual({shouldReset: true, reason: 'initial'});
	});

	it('resets on identity changes even when the previous entity is dirty', () => {
		expect(
			getRemoteFormResetDecision({
				hasAppliedRemoteValues: true,
				identityChanged: true,
				remoteValuesChanged: true,
				isDirty: true,
			}),
		).toEqual({shouldReset: true, reason: 'identity-change'});
	});

	it('accepts remote refreshes while the form is clean', () => {
		expect(
			getRemoteFormResetDecision({
				hasAppliedRemoteValues: true,
				identityChanged: false,
				remoteValuesChanged: true,
				isDirty: false,
			}),
		).toEqual({shouldReset: true, reason: 'remote-clean'});
	});

	it('does not overwrite dirty local edits for the same identity', () => {
		expect(
			getRemoteFormResetDecision({
				hasAppliedRemoteValues: true,
				identityChanged: false,
				remoteValuesChanged: true,
				isDirty: true,
			}),
		).toEqual({shouldReset: false});
	});

	it('does nothing when the same remote values are seen again', () => {
		expect(
			getRemoteFormResetDecision({
				hasAppliedRemoteValues: true,
				identityChanged: false,
				remoteValuesChanged: false,
				isDirty: false,
			}),
		).toEqual({shouldReset: false});
	});
});
