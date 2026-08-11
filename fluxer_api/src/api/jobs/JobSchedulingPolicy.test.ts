// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {SCHEDULED_MESSAGE_TTL_SECONDS} from '../channel/services/ScheduledMessageService';
import {
	MAX_JOB_SCHEDULE_DELAY_MS,
	WORKER_QUEUE_MAX_AGE_MS,
	WORKER_QUEUE_RECOVERY_WINDOW_MS,
} from './JobSchedulingPolicy';

describe('job scheduling retention policy', () => {
	it('retains scheduled-message authority for the complete queue schedule and recovery horizon', () => {
		expect(SCHEDULED_MESSAGE_TTL_SECONDS * 1000).toBe(WORKER_QUEUE_MAX_AGE_MS);
		expect(WORKER_QUEUE_MAX_AGE_MS).toBe(MAX_JOB_SCHEDULE_DELAY_MS + WORKER_QUEUE_RECOVERY_WINDOW_MS);
	});
});
