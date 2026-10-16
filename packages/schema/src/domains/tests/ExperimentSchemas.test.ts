import {
	DEFAULT_EXPERIMENT_DELIVERY_CONFIG,
	ExperimentAssignmentsResponse,
	ExperimentDeliveryConfigSchema,
	ExperimentDeliveryConfigUpdateRequest,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {describe, expect, it} from 'vitest';

describe('experiment delivery schemas', () => {
	it('derives complete configuration defaults without filling partial updates', () => {
		expect(ExperimentDeliveryConfigSchema.parse({})).toEqual(DEFAULT_EXPERIMENT_DELIVERY_CONFIG);
		expect(ExperimentDeliveryConfigUpdateRequest.parse({})).toEqual({});
		expect(ExperimentDeliveryConfigUpdateRequest.parse({poll_interval_seconds: 3600})).toEqual({
			poll_interval_seconds: 3600,
		});
	});

	it.each([
		{poll_interval_seconds: 59},
		{poll_interval_seconds: 86401},
		{poll_jitter_percent: -1},
		{poll_jitter_percent: 51},
	])('uses the same delivery bounds in configuration and updates: %j', (value) => {
		expect(ExperimentDeliveryConfigSchema.safeParse(value).success).toBe(false);
		expect(ExperimentDeliveryConfigUpdateRequest.safeParse(value).success).toBe(false);
	});

	it.each([1, 999999])('preserves server poll intervals for client-side clamping: %i', (interval) => {
		expect(
			ExperimentAssignmentsResponse.parse({
				...DEFAULT_EXPERIMENT_DELIVERY_CONFIG,
				poll_interval_seconds: interval,
				assignments: {},
			}).poll_interval_seconds,
		).toBe(interval);
	});
});
