// SPDX-License-Identifier: AGPL-3.0-or-later

export const CAPTURE_SUBJECT_PREFIX = 'cassandra.capture';

export type CaptureProcess = 'api' | 'worker' | 'users';
export type CaptureOutcome = 'applied' | 'failed' | 'unknown' | 'not_applied';
export type CaptureScope = 'row' | 'partition' | 'prefix';
