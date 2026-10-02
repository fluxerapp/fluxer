// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	executeGroupedBatches,
	mapCassandraDriverError,
	setCassandraQueryExecutorForTesting,
} from '@app/api/database/CassandraQueryExecution';
import {prepared} from '@app/api/database/CassandraTypes';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import cassandra from 'cassandra-driver';
import {afterEach, describe, expect, it} from 'vitest';

function busyConnectionError(): cassandra.errors.BusyConnectionError {
	return new cassandra.errors.BusyConnectionError('127.0.0.1:9042', 2048, 4);
}

describe('mapCassandraDriverError', () => {
	it('sheds a busy connection error as a 503', () => {
		const mapped = mapCassandraDriverError(busyConnectionError());
		expect(mapped).toBeInstanceOf(ServiceUnavailableError);
		expect((mapped as ServiceUnavailableError).status).toBe(503);
	});

	it('sheds a busy connection error nested in a no host available error as a 503', () => {
		const mapped = mapCassandraDriverError(
			new cassandra.errors.NoHostAvailableError({'127.0.0.1:9042': busyConnectionError()}),
		);
		expect(mapped).toBeInstanceOf(ServiceUnavailableError);
		expect((mapped as ServiceUnavailableError).status).toBe(503);
	});

	it('returns other no host available errors unchanged', () => {
		const err = new cassandra.errors.NoHostAvailableError({'127.0.0.1:9042': new Error('connection refused')});
		expect(mapCassandraDriverError(err)).toBe(err);
	});

	it('returns unrelated errors unchanged', () => {
		const err = new cassandra.errors.ResponseError(0x2200, 'invalid query');
		expect(mapCassandraDriverError(err)).toBe(err);
	});
});

describe('executeGroupedBatches', () => {
	afterEach(() => {
		setCassandraQueryExecutorForTesting(null);
	});

	function recordBatches(): Array<Array<string>> {
		const batches: Array<Array<string>> = [];
		setCassandraQueryExecutorForTesting({
			executeQuery: async () => [],
			executeBatch: async (queries) => {
				batches.push(queries.map((query) => (query.params as {id: string}).id));
			},
		});
		return batches;
	}

	function group(id: number, size: number) {
		return Array.from({length: size}, (_, index) => prepared('DELETE FROM t WHERE id = ?', {id: `${id}.${index}`}));
	}

	it('keeps every group in one batch and never exceeds the statement limit', async () => {
		const batches = recordBatches();
		await executeGroupedBatches(
			Array.from({length: 10}, (_, id) => group(id, 3)),
			7,
		);
		expect(batches.map((batch) => batch.length)).toEqual([6, 6, 6, 6, 6]);
		for (const batch of batches) {
			const groups = new Set(batch.map((id) => id.split('.')[0]));
			for (const id of groups) {
				expect(batch.filter((entry) => entry.startsWith(`${id}.`))).toHaveLength(3);
			}
		}
	});

	it('sends nothing for no groups', async () => {
		const batches = recordBatches();
		await executeGroupedBatches([]);
		expect(batches).toEqual([]);
	});
});
