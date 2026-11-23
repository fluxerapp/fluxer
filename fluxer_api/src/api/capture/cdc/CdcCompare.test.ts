// SPDX-License-Identifier: AGPL-3.0-or-later

import {compareColumns, countsAgree} from '@app/api/capture/cdc/CdcCompare';
import {canonicalStoredValue} from '@app/api/capture/cdc/CdcStoredValues';
import {describe, expect, it} from 'vitest';

describe('compare columns', () => {
	it('hashes the shared scalar columns in name order and checks collections apart', () => {
		const types = new Map([
			['user_id', 'bigint'],
			['username', 'text'],
			['password_hash', 'text'],
			['totp_secret', 'text'],
			['flags', 'bigint'],
			['pronouns', 'set<text>'],
			['custom_status', 'frozen<custom_status>'],
		]);
		expect(
			compareColumns(
				'users',
				['username', 'user_id', 'password_hash', 'totp_secret', 'pronouns', 'flags', 'custom_status', 'legacy'],
				['custom_status', 'flags', 'pronouns', 'user_id', 'username', 'new_column'],
				types,
			),
		).toEqual({hashed: ['flags', 'user_id', 'username'], collections: ['custom_status', 'pronouns']});
	});

	it('accepts counts within 0.1 percent', () => {
		expect(countsAgree(100_000, 100_100)).toBe(true);
		expect(countsAgree(100_000, 100_101)).toBe(false);
		expect(countsAgree(0, 0)).toBe(true);
	});
});

describe('stored collection values', () => {
	it('reads Python repr text and the CDC JSON as the same value', () => {
		const pairs: Array<[string, string]> = [
			['SortedSet([5, 9007199254740993])', '[5, 9007199254740993]'],
			["OrderedMapSerializedKey([('a', 'b\\'c'), ('d', 1)])", '{"a": "b\'c", "d": 1}'],
			["OrderedMapSerializedKey([(7, 'x')])", '{"7": "x"}'],
			[
				'mute_config(end_time=datetime.datetime(2026, 1, 2, 3, 4, 5, 678000), selected_time_window=3, muted=False)',
				'{"end_time": "2026-01-02 03:04:05.678", "muted": 0, "selected_time_window": 3}',
			],
			["(1, 'a', None)", '[1, "a", null]'],
			["SortedSet([UUID('6a9f0d9e-2b8a-4c1e-9c55-1f3a2b4c5d6e')])", '["6a9f0d9e-2b8a-4c1e-9c55-1f3a2b4c5d6e"]'],
			['[1.5, 2.0]', '[1.5, 2]'],
			['', ''],
		];
		for (const [repr, json] of pairs) expect(canonicalStoredValue(repr)).toBe(canonicalStoredValue(json));
	});

	it('tells different values apart', () => {
		expect(canonicalStoredValue('SortedSet([1, 2])')).not.toBe(canonicalStoredValue('[1, 3]'));
		expect(canonicalStoredValue("['1']")).not.toBe(canonicalStoredValue('[1]'));
	});

	it('refuses text it cannot read', () => {
		expect(() => canonicalStoredValue('SortedSet([1, ')).toThrow();
	});
});
