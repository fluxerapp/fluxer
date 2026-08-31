// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {resolveCronSchedulerEnabled, resolveWorkerLanes, WORKER_LANES} from '../WorkerLaneConfig';

describe('WorkerLaneConfig', () => {
	it('returns all lanes in all_lanes mode', () => {
		const lanes = resolveWorkerLanes({
			mode: 'all_lanes',
			laneConcurrencyOverrides: {},
		});
		expect(lanes.map((lane) => lane.name)).toEqual(WORKER_LANES.map((lane) => lane.name));
	});
	it('returns a single lane in single_lane mode', () => {
		const lanes = resolveWorkerLanes({
			mode: 'single_lane',
			laneName: 'batch',
			laneConcurrencyOverrides: {},
		});
		expect(lanes).toHaveLength(1);
		expect(lanes[0]!.name).toBe('batch');
	});
	it('resolves Stripe webhook processing in lifecycle lane when running single_task mode', () => {
		const lanes = resolveWorkerLanes({
			mode: 'single_task',
			taskName: 'processStripeWebhook',
			laneConcurrencyOverrides: {},
		});
		expect(lanes).toHaveLength(1);
		expect(lanes[0]!.name).toBe('lifecycle');
		expect(lanes[0]!.taskTypes).toEqual(['processStripeWebhook']);
	});
	it('keeps mention processing in realtime and routes embed extraction to the dedicated unfurl lane', () => {
		const mentionLane = resolveWorkerLanes({
			mode: 'single_task',
			taskName: 'handleMentions',
			laneConcurrencyOverrides: {},
		});
		const mentionChunkLane = resolveWorkerLanes({
			mode: 'single_task',
			taskName: 'handleMentionChunk',
			laneConcurrencyOverrides: {},
		});
		const embedLane = resolveWorkerLanes({
			mode: 'single_task',
			taskName: 'extractEmbeds',
			laneConcurrencyOverrides: {},
		});
		expect(mentionLane).toHaveLength(1);
		expect(mentionLane[0]!.name).toBe('realtime');
		expect(mentionLane[0]!.taskTypes).toEqual(['handleMentions']);
		expect(mentionChunkLane).toHaveLength(1);
		expect(mentionChunkLane[0]!.name).toBe('realtime');
		expect(mentionChunkLane[0]!.taskTypes).toEqual(['handleMentionChunk']);
		expect(embedLane).toHaveLength(1);
		expect(embedLane[0]!.name).toBe('unfurl');
		expect(embedLane[0]!.taskTypes).toEqual(['extractEmbeds']);
	});
	it('throws when single_lane mode has no lane configured', () => {
		expect(() =>
			resolveWorkerLanes({
				mode: 'single_lane',
				laneConcurrencyOverrides: {},
			}),
		).toThrow(/required when worker mode is "single_lane"/);
	});
	it('throws when a lane override is invalid', () => {
		expect(() =>
			resolveWorkerLanes({
				mode: 'single_lane',
				laneName: 'realtime',
				laneConcurrencyOverrides: {
					realtime: 0,
				},
			}),
		).toThrow(/Invalid concurrency override/);
	});
	it('applies per-lane concurrency overrides', () => {
		const lanes = resolveWorkerLanes({
			mode: 'all_lanes',
			laneConcurrencyOverrides: {
				realtime: 99,
			},
		});
		const realtimeLane = lanes.find((lane) => lane.name === 'realtime');
		const lifecycleLane = lanes.find((lane) => lane.name === 'lifecycle');
		expect(realtimeLane?.concurrency).toBe(99);
		expect(lifecycleLane?.concurrency).toBe(8);
	});
	it('enables cron by default in all_lanes mode', () => {
		expect(resolveCronSchedulerEnabled('all_lanes', undefined)).toBe(true);
	});
	it('disables cron by default in single_lane mode', () => {
		expect(resolveCronSchedulerEnabled('single_lane', undefined)).toBe(false);
	});
	it('respects explicit cron scheduler config', () => {
		expect(resolveCronSchedulerEnabled('single_lane', true)).toBe(true);
		expect(resolveCronSchedulerEnabled('all_lanes', false)).toBe(false);
	});
});
