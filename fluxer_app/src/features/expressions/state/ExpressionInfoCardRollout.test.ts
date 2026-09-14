// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ExperimentAssignmentsResponse,
	INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {
	type ExpressionInfoCardAssignmentResponse,
	INERT_EXPRESSION_INFO_CARD_ASSIGNMENT,
} from '@fluxer/schema/src/domains/experiment/ExpressionInfoCardSchemas';
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
const {ExpressionInfoCardRollout} = await import('@app/features/expressions/state/ExpressionInfoCardRollout');

function publish(assignment: ExpressionInfoCardAssignmentResponse): void {
	const response: ExperimentAssignmentsResponse = {
		poll_interval_seconds: 300,
		poll_jitter_percent: 15,
		assignments: {expression_info_card: assignment},
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

describe('ExpressionInfoCardRollout', () => {
	it('reads the inert assignment out of the inert envelope', () => {
		expect(ExpressionInfoCardRollout.assignment).toBe(INERT_EXPRESSION_INFO_CARD_ASSIGNMENT);
		expect(ExpressionInfoCardRollout.enabled).toBe(false);
	});

	it('stays on the control arm while the rollout is disabled', () => {
		publish({enabled: false, config_version: 4, user_targeted: false, source: null});
		expect(ExpressionInfoCardRollout.enabled).toBe(false);
	});

	it('stays on the control arm for an account the rollout did not target', () => {
		publish({enabled: true, config_version: 4, user_targeted: false, source: null});
		expect(ExpressionInfoCardRollout.enabled).toBe(false);
	});

	it('moves to the experiment arm for a targeted account', () => {
		publish({enabled: true, config_version: 4, user_targeted: true, source: 'canary'});
		expect(ExpressionInfoCardRollout.enabled).toBe(true);
	});

	it('follows the envelope back to the control arm when the store is reset', () => {
		publish({enabled: true, config_version: 4, user_targeted: true, source: 'user_rule'});
		expect(ExpressionInfoCardRollout.enabled).toBe(true);
		ExperimentAssignments.reset();
		expect(ExpressionInfoCardRollout.enabled).toBe(false);
	});
});
