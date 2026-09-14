// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ExperimentAssignmentsResponse,
	INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE,
	readTypingIndicatorReworkAssignment,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {
	INERT_TYPING_INDICATOR_REWORK_ASSIGNMENT,
	type TypingIndicatorReworkAssignmentResponse,
} from '@fluxer/schema/src/domains/experiment/TypingIndicatorReworkSchemas';
import {runInAction} from 'mobx';
import {afterEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = vi.fn();
	},
}));

vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {get: vi.fn(), post: vi.fn()},
}));

const {ExperimentAssignments} = await import('@app/features/experiment/state/ExperimentAssignments');
const {TypingIndicatorReworkRollout} = await import('@app/features/typing/state/TypingIndicatorReworkRollout');

const TARGETED_ASSIGNMENT: TypingIndicatorReworkAssignmentResponse = {
	enabled: true,
	config_version: 4,
	user_targeted: true,
	source: 'canary',
};

function responseWith(assignment: TypingIndicatorReworkAssignmentResponse | undefined): ExperimentAssignmentsResponse {
	return {
		poll_interval_seconds: 300,
		poll_jitter_percent: 15,
		assignments: assignment === undefined ? {} : {typing_indicator_rework: assignment},
	};
}

function publish(assignment: TypingIndicatorReworkAssignmentResponse | undefined): void {
	runInAction(() => {
		ExperimentAssignments.response = responseWith(assignment);
	});
}

afterEach(() => {
	runInAction(() => {
		ExperimentAssignments.response = INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE;
	});
});

describe('TypingIndicatorReworkRollout', () => {
	it('reads the inert assignment before any response has landed', () => {
		expect(TypingIndicatorReworkRollout.assignment).toEqual(INERT_TYPING_INDICATOR_REWORK_ASSIGNMENT);
		expect(TypingIndicatorReworkRollout.usesRollingTyping).toBe(false);
	});

	it('treats a missing experiment key as off', () => {
		publish(undefined);

		expect(readTypingIndicatorReworkAssignment(responseWith(undefined))).toBe(INERT_TYPING_INDICATOR_REWORK_ASSIGNMENT);
		expect(TypingIndicatorReworkRollout.assignment).toEqual(INERT_TYPING_INDICATOR_REWORK_ASSIGNMENT);
		expect(TypingIndicatorReworkRollout.usesRollingTyping).toBe(false);
	});

	it('uses rolling typing for a targeted account', () => {
		publish(TARGETED_ASSIGNMENT);

		expect(TypingIndicatorReworkRollout.assignment).toEqual(TARGETED_ASSIGNMENT);
		expect(TypingIndicatorReworkRollout.usesRollingTyping).toBe(true);
	});

	it('keeps legacy typing for an account outside the rollout', () => {
		publish({...TARGETED_ASSIGNMENT, user_targeted: false, source: null});

		expect(TypingIndicatorReworkRollout.usesRollingTyping).toBe(false);
	});

	it('keeps legacy typing while the rollout is disabled instance-wide', () => {
		publish({...TARGETED_ASSIGNMENT, enabled: false});

		expect(TypingIndicatorReworkRollout.usesRollingTyping).toBe(false);
	});

	it('keeps legacy typing for an excluded account', () => {
		publish({enabled: true, config_version: 4, user_targeted: false, source: null});

		expect(TypingIndicatorReworkRollout.assignment.enabled).toBe(true);
		expect(TypingIndicatorReworkRollout.usesRollingTyping).toBe(false);
	});

	it('follows a later poll that turns the rollout off', () => {
		publish(TARGETED_ASSIGNMENT);
		expect(TypingIndicatorReworkRollout.usesRollingTyping).toBe(true);

		publish({...TARGETED_ASSIGNMENT, enabled: false, user_targeted: false, source: null});

		expect(TypingIndicatorReworkRollout.usesRollingTyping).toBe(false);
	});
});
