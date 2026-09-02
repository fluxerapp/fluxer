// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ScreenShareRemoteMigrationState,
	transitionRemoteScreenShareMigrationState,
} from '@app/features/voice/engine/ScreenSharePublicationMigrationStateMachine';
import {describe, expect, it} from 'vitest';

function candidate(overrides: Partial<ScreenShareRemoteMigrationState> = {}): ScreenShareRemoteMigrationState {
	return {
		migrationId: 'migration-a',
		generation: 1,
		previousTrackSid: 'old-track',
		candidateTrackSid: 'new-track',
		committedTrackSid: 'old-track',
		codec: 'av1',
		phase: 'candidate',
		readySent: false,
		...overrides,
	};
}

describe('ScreenSharePublicationMigrationStateMachine remote migration', () => {
	it('enters breaking state before a replacement publication exists', () => {
		const breaking = transitionRemoteScreenShareMigrationState(null, {
			type: 'migration.break',
			migrationId: 'migration-a',
			generation: 1,
			previousTrackSid: 'old-track',
			codec: 'av1',
		});

		expect(breaking).toEqual({
			migrationId: 'migration-a',
			generation: 1,
			previousTrackSid: 'old-track',
			candidateTrackSid: null,
			committedTrackSid: 'old-track',
			codec: 'av1',
			phase: 'breaking',
			readySent: true,
		});
	});

	it('keeps buffering when the previous publication disappears during break-before-make', () => {
		const breaking = transitionRemoteScreenShareMigrationState(null, {
			type: 'migration.break',
			migrationId: 'migration-a',
			generation: 1,
			previousTrackSid: 'old-track',
			codec: 'av1',
		});

		const stillBreaking = transitionRemoteScreenShareMigrationState(breaking, {
			type: 'migration.committedUnpublished',
			trackSid: 'old-track',
		});

		expect(stillBreaking).toMatchObject({
			migrationId: 'migration-a',
			phase: 'breaking',
			previousTrackSid: null,
			candidateTrackSid: null,
			committedTrackSid: null,
		});
	});

	it('commits a replacement after break-before-make', () => {
		const breaking = transitionRemoteScreenShareMigrationState(null, {
			type: 'migration.break',
			migrationId: 'migration-a',
			generation: 1,
			previousTrackSid: 'old-track',
			codec: 'av1',
		});

		const committed = transitionRemoteScreenShareMigrationState(breaking, {
			type: 'migration.commit',
			migrationId: 'migration-a',
			generation: 1,
			previousTrackSid: 'old-track',
			candidateTrackSid: 'new-track',
		});

		expect(committed).toMatchObject({
			migrationId: 'migration-a',
			phase: 'committed',
			candidateTrackSid: null,
			committedTrackSid: 'new-track',
			readySent: true,
		});
	});

	it('rolls back or clears breaking state on abort without a candidate SID', () => {
		const breaking = transitionRemoteScreenShareMigrationState(null, {
			type: 'migration.break',
			migrationId: 'migration-a',
			generation: 1,
			previousTrackSid: 'old-track',
			codec: 'av1',
		});

		expect(
			transitionRemoteScreenShareMigrationState(breaking, {
				type: 'migration.abort',
				migrationId: 'migration-a',
				candidateTrackSid: null,
			}),
		).toMatchObject({
			phase: 'committed',
			candidateTrackSid: null,
			committedTrackSid: 'old-track',
			readySent: true,
		});

		const oldTrackGone = transitionRemoteScreenShareMigrationState(breaking, {
			type: 'migration.committedUnpublished',
			trackSid: 'old-track',
		});
		expect(
			transitionRemoteScreenShareMigrationState(oldTrackGone, {
				type: 'migration.abort',
				migrationId: 'migration-a',
				candidateTrackSid: null,
			}),
		).toBeNull();
	});

	it('moves from candidate to committed without selecting the candidate early', () => {
		const pending = transitionRemoteScreenShareMigrationState(null, {
			type: 'migration.candidate',
			migrationId: 'migration-a',
			generation: 1,
			previousTrackSid: 'old-track',
			candidateTrackSid: 'new-track',
			codec: 'av1',
		});

		expect(pending).toEqual(candidate());

		const committed = transitionRemoteScreenShareMigrationState(pending, {
			type: 'migration.commit',
			migrationId: 'migration-a',
			generation: 1,
			previousTrackSid: 'old-track',
			candidateTrackSid: 'new-track',
		});

		expect(committed).toMatchObject({
			migrationId: 'migration-a',
			phase: 'committed',
			candidateTrackSid: null,
			committedTrackSid: 'new-track',
			readySent: true,
		});
	});

	it('ignores stale candidates and stale mismatched commits', () => {
		const committed = candidate({
			migrationId: 'migration-b',
			generation: 2,
			candidateTrackSid: null,
			committedTrackSid: 'newer-track',
			phase: 'committed',
			readySent: true,
		});

		expect(
			transitionRemoteScreenShareMigrationState(committed, {
				type: 'migration.candidate',
				migrationId: 'migration-a',
				generation: 1,
				previousTrackSid: 'old-track',
				candidateTrackSid: 'stale-candidate',
				codec: 'h264',
			}),
		).toBe(committed);
		expect(
			transitionRemoteScreenShareMigrationState(committed, {
				type: 'migration.commit',
				migrationId: 'migration-c',
				generation: 2,
				previousTrackSid: 'old-track',
				candidateTrackSid: 'wrong-track',
			}),
		).toBe(committed);
	});

	it('allows a newer generation to supersede an existing committed publication', () => {
		const committed = candidate({
			migrationId: 'migration-a',
			generation: 1,
			candidateTrackSid: null,
			committedTrackSid: 'new-track',
			phase: 'committed',
			readySent: true,
		});

		const next = transitionRemoteScreenShareMigrationState(committed, {
			type: 'migration.candidate',
			migrationId: 'migration-b',
			generation: 2,
			previousTrackSid: 'new-track',
			candidateTrackSid: 'newer-track',
			codec: 'h264',
		});

		expect(next).toMatchObject({
			migrationId: 'migration-b',
			generation: 2,
			previousTrackSid: 'new-track',
			candidateTrackSid: 'newer-track',
			committedTrackSid: 'new-track',
			phase: 'candidate',
			readySent: false,
		});
	});

	it('accepts an out-of-order commit without first seeing the candidate announcement', () => {
		const committed = transitionRemoteScreenShareMigrationState(null, {
			type: 'migration.commit',
			migrationId: 'migration-a',
			generation: 1,
			previousTrackSid: 'old-track',
			candidateTrackSid: 'new-track',
		});

		expect(committed).toMatchObject({
			migrationId: 'migration-a',
			generation: 1,
			phase: 'committed',
			candidateTrackSid: null,
			committedTrackSid: 'new-track',
			readySent: true,
		});
	});

	it('rolls back to the previous publication on matching abort', () => {
		const pending = candidate();
		const rolledBack = transitionRemoteScreenShareMigrationState(pending, {
			type: 'migration.abort',
			migrationId: 'migration-a',
			candidateTrackSid: 'new-track',
		});

		expect(rolledBack).toMatchObject({
			phase: 'committed',
			candidateTrackSid: null,
			committedTrackSid: 'old-track',
			readySent: true,
		});
		expect(
			transitionRemoteScreenShareMigrationState(pending, {
				type: 'migration.abort',
				migrationId: 'migration-a',
				candidateTrackSid: 'different-track',
			}),
		).toBe(pending);
	});

	it('clears candidate or committed state when the corresponding publication disappears', () => {
		expect(
			transitionRemoteScreenShareMigrationState(candidate(), {
				type: 'migration.candidateUnpublished',
				trackSid: 'new-track',
			}),
		).toMatchObject({
			phase: 'committed',
			candidateTrackSid: null,
			committedTrackSid: 'old-track',
		});
		expect(
			transitionRemoteScreenShareMigrationState(candidate({phase: 'committed', committedTrackSid: 'old-track'}), {
				type: 'migration.committedUnpublished',
				trackSid: 'old-track',
			}),
		).toBeNull();
	});

	it('marks candidate readiness idempotently', () => {
		const pending = candidate();
		const ready = transitionRemoteScreenShareMigrationState(pending, {type: 'migration.readySent'});

		expect(ready).toMatchObject({readySent: true});
		expect(transitionRemoteScreenShareMigrationState(ready, {type: 'migration.readySent'})).toBe(ready);
	});
});
