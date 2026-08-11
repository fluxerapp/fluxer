// SPDX-License-Identifier: AGPL-3.0-or-later

import {ms} from 'itty-time';

export const MAX_JOB_SCHEDULE_DELAY_MS = ms('30 days');
export const WORKER_QUEUE_RECOVERY_WINDOW_MS = ms('7 days');
export const WORKER_QUEUE_MAX_AGE_MS = MAX_JOB_SCHEDULE_DELAY_MS + WORKER_QUEUE_RECOVERY_WINDOW_MS;
