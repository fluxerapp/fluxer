// SPDX-License-Identifier: AGPL-3.0-or-later

import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import cassandra from 'cassandra-driver';
import {describe, expect, it} from 'vitest';
import {mapCassandraDriverError} from './CassandraQueryExecution';

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
