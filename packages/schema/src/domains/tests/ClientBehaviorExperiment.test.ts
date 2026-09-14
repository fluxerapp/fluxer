// SPDX-License-Identifier: AGPL-3.0-or-later

import {createClientBehaviorExperiment} from '@fluxer/schema/src/domains/experiment/ClientBehaviorExperiment';
import {describe, expect, it} from 'vitest';

const TARGETED_USER_CAP = 1000;

function userIds(count: number): Array<string> {
	return Array.from({length: count}, (_, index) => String(index + 1));
}

const rollout = createClientBehaviorExperiment({defaultRolloutSalt: 'client-behaviour-cap-test'});

describe('createClientBehaviorExperiment targeted user lists', () => {
	it('accepts an included list at the thousand entry cap', () => {
		const config = rollout.ConfigSchema.parse({included_user_ids: userIds(TARGETED_USER_CAP)});

		expect(config.included_user_ids).toHaveLength(TARGETED_USER_CAP);
	});

	it('rejects an included list above the thousand entry cap', () => {
		expect(() => rollout.ConfigSchema.parse({included_user_ids: userIds(TARGETED_USER_CAP + 1)})).toThrow();
	});

	it('accepts an excluded list at the thousand entry cap', () => {
		const config = rollout.ConfigSchema.parse({excluded_user_ids: userIds(TARGETED_USER_CAP)});

		expect(config.excluded_user_ids).toHaveLength(TARGETED_USER_CAP);
	});

	it('rejects an excluded list above the thousand entry cap', () => {
		expect(() => rollout.ConfigSchema.parse({excluded_user_ids: userIds(TARGETED_USER_CAP + 1)})).toThrow();
	});
});
