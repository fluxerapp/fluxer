// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveWorkerLanes, validateLaneCompleteness, WORKER_LANES} from '@app/api/worker/WorkerLaneConfig';
import {describe, expect, it} from 'vitest';

describe('WorkerLaneConfig', () => {
	it('keeps each retired subject on the lane that used to run it', () => {
		const lanes = resolveWorkerLanes({
			mode: 'all_lanes',
			laneConcurrencyOverrides: {},
		});
		const retired = Object.fromEntries(lanes.map((lane) => [lane.name, lane.retiredTaskTypes]));
		expect(retired).toEqual({
			realtime: [],
			unfurl: [],
			lifecycle: ['sendScheduledMessage', 'finalizeNcmecAttachmentReport'],
			batch: [],
			crosspost: [],
		});
		for (const lane of lanes) {
			for (const task of lane.retiredTaskTypes) expect(lane.taskTypes).not.toContain(task);
		}
	});
	it('never claims a retired subject from a single_task lane', () => {
		const lanes = resolveWorkerLanes({
			mode: 'single_task',
			taskName: 'processStripeWebhook',
			laneConcurrencyOverrides: {},
		});
		expect(lanes[0]!.retiredTaskTypes).toEqual([]);
	});
	it('rejects a registry that brings a retired task name back', () => {
		const registry = Object.fromEntries(WORKER_LANES.flatMap((lane) => lane.taskTypes).map((task) => [task, () => {}]));
		expect(() => validateLaneCompleteness(registry)).not.toThrow();
		expect(() => validateLaneCompleteness({...registry, sendScheduledMessage: () => {}})).toThrow(
			/Retired tasks registered again: sendScheduledMessage/,
		);
	});
});
