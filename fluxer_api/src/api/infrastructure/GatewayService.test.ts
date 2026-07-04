// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, test} from 'vitest';
import {createUserID} from '../BrandedTypes';
import {GatewayService} from './GatewayService';

describe('GatewayService.getCurrentActivities', () => {
	test('returns only activities that match the public schema', async () => {
		const service = new GatewayService();
		(service as any).call = async (method: string, params: Record<string, unknown>) => {
			expect(method).toBe('presence.get_current_activities');
			expect(params).toEqual({user_id: '123'});
			return {
				activities: [
					{name: 'Test Activity', type: 0, details: 'Queueing'},
					{name: 'Broken URL', type: 0, details_url: {}},
					{name: 'Broken Buttons', type: 0, buttons: [1]},
					null,
				],
			};
		};

		const activities = await service.getCurrentActivities(createUserID(123n));

		expect(activities).toEqual([{name: 'Test Activity', type: 0, details: 'Queueing'}]);
	});

	test('returns an empty list when the gateway response is not an array', async () => {
		const service = new GatewayService();
		(service as any).call = async () => ({activities: {name: 'Test Activity', type: 0}});

		const activities = await service.getCurrentActivities(createUserID(123n));

		expect(activities).toEqual([]);
	});
});
