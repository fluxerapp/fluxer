// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ExperimentAssignmentsResponse,
	INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {
	type GuildHeaderCollapseAssignmentResponse,
	INERT_GUILD_HEADER_COLLAPSE_ASSIGNMENT,
} from '@fluxer/schema/src/domains/experiment/GuildHeaderCollapseSchemas';
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
const {GuildHeaderCollapseRollout} = await import('@app/features/app/state/GuildHeaderCollapseRollout');

const TARGETED_ASSIGNMENT: GuildHeaderCollapseAssignmentResponse = {
	enabled: true,
	config_version: 3,
	user_targeted: true,
	source: 'canary',
};

function publish(assignment: GuildHeaderCollapseAssignmentResponse | undefined): void {
	const response: ExperimentAssignmentsResponse = {
		poll_interval_seconds: 300,
		poll_jitter_percent: 15,
		assignments: assignment === undefined ? {} : {guild_header_collapse: assignment},
	};
	runInAction(() => {
		ExperimentAssignments.response = response;
	});
}

afterEach(() => {
	runInAction(() => {
		ExperimentAssignments.response = INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE;
	});
});

describe('GuildHeaderCollapseRollout', () => {
	it('reads the inert assignment before any response has landed', () => {
		expect(GuildHeaderCollapseRollout.assignment).toEqual(INERT_GUILD_HEADER_COLLAPSE_ASSIGNMENT);
		expect(GuildHeaderCollapseRollout.collapsesOnScroll).toBe(false);
	});

	it('treats a missing experiment key as off', () => {
		publish(undefined);

		expect(GuildHeaderCollapseRollout.assignment).toEqual(INERT_GUILD_HEADER_COLLAPSE_ASSIGNMENT);
		expect(GuildHeaderCollapseRollout.collapsesOnScroll).toBe(false);
	});

	it('collapses on scroll for a targeted account', () => {
		publish(TARGETED_ASSIGNMENT);

		expect(GuildHeaderCollapseRollout.assignment).toEqual(TARGETED_ASSIGNMENT);
		expect(GuildHeaderCollapseRollout.collapsesOnScroll).toBe(true);
	});

	it('keeps the old header for an account outside the rollout', () => {
		publish({...TARGETED_ASSIGNMENT, user_targeted: false, source: null});

		expect(GuildHeaderCollapseRollout.collapsesOnScroll).toBe(false);
	});

	it('keeps the old header while the rollout is disabled instance-wide', () => {
		publish({...TARGETED_ASSIGNMENT, enabled: false});

		expect(GuildHeaderCollapseRollout.collapsesOnScroll).toBe(false);
	});

	it('follows a later poll that turns the rollout off', () => {
		publish(TARGETED_ASSIGNMENT);
		expect(GuildHeaderCollapseRollout.collapsesOnScroll).toBe(true);

		publish({...TARGETED_ASSIGNMENT, enabled: false, user_targeted: false, source: null});

		expect(GuildHeaderCollapseRollout.collapsesOnScroll).toBe(false);
	});
});
