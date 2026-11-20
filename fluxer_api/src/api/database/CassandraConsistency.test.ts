// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseCassandraConsistency, parseCassandraSerialConsistency} from '@pkgs/cassandra/src/Client';
import {describe, expect, it} from 'vitest';

describe('Cassandra consistency settings', () => {
	it('defaults to local levels so requests stay in the local data centre', () => {
		expect(parseCassandraConsistency(undefined)).toBe('localQuorum');
		expect(parseCassandraConsistency('  ')).toBe('localQuorum');
		expect(parseCassandraSerialConsistency(undefined)).toBe('localSerial');
	});

	it('accepts snake case, upper case and driver names', () => {
		expect(parseCassandraConsistency('local_one')).toBe('localOne');
		expect(parseCassandraConsistency('EACH_QUORUM')).toBe('eachQuorum');
		expect(parseCassandraConsistency('localQuorum')).toBe('localQuorum');
		expect(parseCassandraSerialConsistency('SERIAL')).toBe('serial');
		expect(parseCassandraSerialConsistency('local_serial')).toBe('localSerial');
	});

	it('rejects levels that do not exist or do not fit the setting', () => {
		expect(() => parseCassandraConsistency('two')).toThrow(/Unsupported Cassandra consistency/);
		expect(() => parseCassandraConsistency('serial')).toThrow(/Unsupported Cassandra consistency/);
		expect(() => parseCassandraSerialConsistency('quorum')).toThrow(/Unsupported Cassandra serial consistency/);
	});
});
