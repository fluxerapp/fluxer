// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createSyncedFieldMachineSnapshot,
	type SyncedFieldMachineEvent,
	type SyncedFieldMachineSnapshot,
	selectSyncedFieldMachineModel,
	transitionSyncedFieldMachineSnapshot,
} from '@app/features/user/state/SyncedFieldStateMachine';
import {describe, expect, it} from 'vitest';

const REMOTE_SNAPSHOT = Object.freeze({source: 'remote'});
const LOCAL_CANDIDATE = Object.freeze({source: 'local'});

function transition(snapshot: SyncedFieldMachineSnapshot, event: SyncedFieldMachineEvent): SyncedFieldMachineSnapshot {
	return transitionSyncedFieldMachineSnapshot(snapshot, event);
}

describe('SyncedFieldStateMachine', () => {
	it('starts active and idle without commands', () => {
		const model = selectSyncedFieldMachineModel(createSyncedFieldMachineSnapshot());
		expect(model.state).toBe('idle');
		expect(model.isActive).toBe(true);
		expect(model.command).toBeNull();
		expect(model.suppressPush).toBe(false);
		expect(model.failure).toBeNull();
	});

	it('skips missing or disabled remote snapshots', () => {
		let snapshot = createSyncedFieldMachineSnapshot();
		snapshot = transition(snapshot, {
			type: 'sync.remoteObserved',
			enabled: true,
			snapshot: undefined,
		});
		expect(selectSyncedFieldMachineModel(snapshot).command).toBeNull();

		snapshot = transition(snapshot, {
			type: 'sync.remoteObserved',
			enabled: false,
			snapshot: REMOTE_SNAPSHOT,
		});
		const model = selectSyncedFieldMachineModel(snapshot);
		expect(model.state).toBe('idle');
		expect(model.command).toBeNull();
	});

	it('compares a remote snapshot before deciding whether to apply it', () => {
		let snapshot = createSyncedFieldMachineSnapshot();
		snapshot = transition(snapshot, {
			type: 'sync.remoteObserved',
			enabled: true,
			snapshot: REMOTE_SNAPSHOT,
		});

		let model = selectSyncedFieldMachineModel(snapshot);
		expect(model.state).toBe('comparingRemote');
		expect(model.command).toEqual({type: 'compareRemote', snapshot: REMOTE_SNAPSHOT});

		snapshot = transition(snapshot, {type: 'sync.remoteMatched'});
		model = selectSyncedFieldMachineModel(snapshot);
		expect(model.state).toBe('idle');
		expect(model.command).toBeNull();
	});

	it('suppresses local pushes while a remote snapshot is being applied', () => {
		let snapshot = createSyncedFieldMachineSnapshot();
		snapshot = transition(snapshot, {
			type: 'sync.remoteObserved',
			enabled: true,
			snapshot: REMOTE_SNAPSHOT,
		});
		snapshot = transition(snapshot, {type: 'sync.remoteNeedsApply'});

		let model = selectSyncedFieldMachineModel(snapshot);
		expect(model.state).toBe('applyingRemote');
		expect(model.suppressPush).toBe(true);
		expect(model.command).toEqual({type: 'applyRemote', snapshot: REMOTE_SNAPSHOT});

		snapshot = transition(snapshot, {
			type: 'sync.localObserved',
			enabled: true,
			candidate: LOCAL_CANDIDATE,
		});
		model = selectSyncedFieldMachineModel(snapshot);
		expect(model.state).toBe('applyingRemote');
		expect(model.command).toEqual({type: 'applyRemote', snapshot: REMOTE_SNAPSHOT});

		snapshot = transition(snapshot, {type: 'sync.applyFinished'});
		model = selectSyncedFieldMachineModel(snapshot);
		expect(model.state).toBe('idle');
		expect(model.suppressPush).toBe(false);
		expect(model.command).toBeNull();
	});

	it('prepares and pushes a changed local candidate', () => {
		let snapshot = createSyncedFieldMachineSnapshot();
		snapshot = transition(snapshot, {
			type: 'sync.localObserved',
			enabled: true,
			candidate: LOCAL_CANDIDATE,
		});

		let model = selectSyncedFieldMachineModel(snapshot);
		expect(model.state).toBe('preparingPush');
		expect(model.command).toEqual({type: 'preparePush', candidate: LOCAL_CANDIDATE});

		snapshot = transition(snapshot, {type: 'sync.localReadyToPush'});
		model = selectSyncedFieldMachineModel(snapshot);
		expect(model.state).toBe('pushing');
		expect(model.command).toEqual({type: 'pushLocal', candidate: LOCAL_CANDIDATE});

		snapshot = transition(snapshot, {type: 'sync.commandHandled'});
		model = selectSyncedFieldMachineModel(snapshot);
		expect(model.state).toBe('idle');
		expect(model.command).toBeNull();
	});

	it('skips local candidates that are disabled, absent, or already synced', () => {
		let snapshot = createSyncedFieldMachineSnapshot();
		snapshot = transition(snapshot, {
			type: 'sync.localObserved',
			enabled: false,
			candidate: LOCAL_CANDIDATE,
		});
		expect(selectSyncedFieldMachineModel(snapshot).command).toBeNull();

		snapshot = transition(snapshot, {
			type: 'sync.localObserved',
			enabled: true,
			candidate: null,
		});
		expect(selectSyncedFieldMachineModel(snapshot).command).toBeNull();

		snapshot = transition(snapshot, {
			type: 'sync.localObserved',
			enabled: true,
			candidate: LOCAL_CANDIDATE,
		});
		snapshot = transition(snapshot, {type: 'sync.localAlreadySynced'});

		const model = selectSyncedFieldMachineModel(snapshot);
		expect(model.state).toBe('idle');
		expect(model.command).toBeNull();
	});

	it('suspends on terminal failures and ignores further sync events until reset', () => {
		let snapshot = createSyncedFieldMachineSnapshot();
		snapshot = transition(snapshot, {
			type: 'sync.failed',
			failure: {
				reason: 'payload-too-large',
				message: 'too large',
			},
		});

		let model = selectSyncedFieldMachineModel(snapshot);
		expect(model.state).toBe('suspended');
		expect(model.isActive).toBe(false);
		expect(model.failure?.reason).toBe('payload-too-large');
		expect(model.command).toBeNull();

		snapshot = transition(snapshot, {
			type: 'sync.localObserved',
			enabled: true,
			candidate: LOCAL_CANDIDATE,
		});
		expect(selectSyncedFieldMachineModel(snapshot).state).toBe('suspended');
		expect(selectSyncedFieldMachineModel(snapshot).command).toBeNull();

		snapshot = transition(snapshot, {type: 'sync.reset'});
		model = selectSyncedFieldMachineModel(snapshot);
		expect(model.state).toBe('idle');
		expect(model.isActive).toBe(true);
		expect(model.failure).toBeNull();
	});
});
