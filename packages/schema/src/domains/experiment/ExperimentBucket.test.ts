// SPDX-License-Identifier: AGPL-3.0-or-later

import {experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {describe, expect, test} from 'vitest';

const TARGETED_USER_ID = '1000000000000000001';
const OTHER_USER_ID = '1000000000000000002';

function syntheticUserIds(count: number): Array<string> {
	const ids: Array<string> = [];
	for (let index = 0; index < count; index++) {
		ids.push((1400000000000000000n + BigInt(index)).toString());
	}
	return ids;
}

describe('experimentBucket', () => {
	test('stays inside the basis point range for every synthetic id', () => {
		for (const userId of syntheticUserIds(500)) {
			const bucket = experimentBucket(userId, 'voice-ns-v1');
			expect(Number.isInteger(bucket)).toBe(true);
			expect(bucket).toBeGreaterThanOrEqual(0);
			expect(bucket).toBeLessThan(10000);
		}
	});

	test.each([
		{userId: TARGETED_USER_ID, salt: 'voice-ns-v1', expected: 8241},
		{userId: TARGETED_USER_ID, salt: 'voice-ns-v2', expected: 8500},
		{userId: OTHER_USER_ID, salt: 'voice-ns-v1', expected: 5384},
		{userId: OTHER_USER_ID, salt: 'voice-ns-v2', expected: 1357},
	])('preserves the assignment bucket for $userId with salt $salt', ({userId, salt, expected}) => {
		expect(experimentBucket(userId, salt)).toBe(expected);
	});

	test('changes with the salt for at least most user ids', () => {
		const userIds = syntheticUserIds(200);
		const changed = userIds.filter(
			(userId) => experimentBucket(userId, 'voice-ns-v1') !== experimentBucket(userId, 'voice-ns-v2'),
		);
		expect(changed.length).toBeGreaterThan(userIds.length - 5);
	});
});
