// SPDX-License-Identifier: AGPL-3.0-or-later

import {classifyWriteError} from '@app/api/database/capture/CaptureOutcomes';
import cassandra from 'cassandra-driver';
import {describe, expect, it} from 'vitest';

describe('write error classification', () => {
	const refused = Object.assign(new Error('connect ECONNREFUSED'), {code: 'ECONNREFUSED'});
	const cases: Array<[string, unknown, 'failed' | 'unknown']> = [
		['unavailable', new cassandra.errors.ResponseError(0x1000, ''), 'failed'],
		['overloaded', new cassandra.errors.ResponseError(0x1001, ''), 'failed'],
		['bootstrapping', new cassandra.errors.ResponseError(0x1002, ''), 'failed'],
		['invalid', new cassandra.errors.ResponseError(0x2200, ''), 'failed'],
		['syntax', new cassandra.errors.ResponseError(0x2000, ''), 'failed'],
		['unauthorized', new cassandra.errors.ResponseError(0x2100, ''), 'failed'],
		['config', new cassandra.errors.ResponseError(0x2300, ''), 'failed'],
		['unprepared', new cassandra.errors.ResponseError(0x2500, ''), 'failed'],
		['write timeout', new cassandra.errors.ResponseError(0x1100, ''), 'unknown'],
		['write failure', new cassandra.errors.ResponseError(0x1500, ''), 'unknown'],
		['server error', new cassandra.errors.ResponseError(0x0000, ''), 'unknown'],
		['client timeout', new cassandra.errors.OperationTimedOutError('timed out'), 'unknown'],
		['busy connection', new cassandra.errors.BusyConnectionError('h:9042', 2048, 4), 'failed'],
		['no host, refused', new cassandra.errors.NoHostAvailableError({'h:9042': refused}), 'failed'],
		[
			'no host, timed out',
			new cassandra.errors.NoHostAvailableError({'h:9042': new cassandra.errors.OperationTimedOutError('t')}),
			'unknown',
		],
		['anything else', new Error('socket hang up'), 'unknown'],
	];
	for (const [name, error, outcome] of cases) {
		it(`treats ${name} as ${outcome}`, () => {
			expect(classifyWriteError(error)).toBe(outcome);
		});
	}
});
