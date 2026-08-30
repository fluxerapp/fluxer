// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {coerceUnsafeIntegersToStrings, parseJsonPreservingLargeIntegers} from '../LosslessJsonParser';

describe('parseJsonPreservingLargeIntegers', () => {
	it('keeps safe integers as numbers', () => {
		const parsed = parseJsonPreservingLargeIntegers('{"id":9007199254740991}') as {
			id: unknown;
		};
		expect(parsed.id).toBe(9007199254740991);
		expect(typeof parsed.id).toBe('number');
	});
	it('converts unsafe integers to strings', () => {
		const parsed = parseJsonPreservingLargeIntegers('{"id":9007199254740992}') as {
			id: unknown;
		};
		expect(parsed.id).toBe('9007199254740992');
		expect(typeof parsed.id).toBe('string');
	});
	it('preserves floating point numbers', () => {
		const parsed = parseJsonPreservingLargeIntegers('{"took":0.062,"id":1472109478688579732}') as {
			took: unknown;
			id: unknown;
		};
		expect(parsed.took).toBe(0.062);
		expect(typeof parsed.took).toBe('number');
		expect(parsed.id).toBe('1472109478688579732');
	});
	it('does not touch numbers inside strings', () => {
		const parsed = parseJsonPreservingLargeIntegers('{"id":"1472109478688579732"}') as {
			id: unknown;
		};
		expect(parsed.id).toBe('1472109478688579732');
	});
	it('handles arrays of values', () => {
		const parsed = parseJsonPreservingLargeIntegers('{"arr":[1,1472109478688579732]}') as {
			arr: Array<unknown>;
		};
		expect(parsed.arr[0]).toBe(1);
		expect(parsed.arr[1]).toBe('1472109478688579732');
	});
});

const CORPUS: Array<string> = [
	'{"content":"hey","nonce":"1472109478688579732","channel_id":"1472109478688579731"}',
	'{"message_ids":["1472109478688579732","1472109478688579733","1472109478688579734"]}',
	'{"id":9007199254740991}',
	'{"id":9007199254740992}',
	'{"id":-9007199254740991}',
	'{"id":-9007199254740992}',
	'{"id":-1472109478688579732}',
	'{"id":999999999999999}',
	'{"id":1000000000000000}',
	'{"id":9999999999999999}',
	'{"id":99999999999999999}',
	'{"id":123456789012345}',
	'{"id":1234567890123456}',
	'{"id":12345678901234567}',
	'{"id":0}',
	'{"id":-0}',
	'{"id":00}',
	'{"id":0.00000000000000012345}',
	'{"id":1.2345678901234567890}',
	'{"id":123456789012345678.9}',
	'{"id":1e20}',
	'{"id":1E+20}',
	'{"id":12345678901234567890e-5}',
	'{"id":-1.7976931348623157e308}',
	'{"a":1234567890,"b":12345678,"c":9012345678}',
	'[1234567890123456789,1,2,3]',
	'{"12345678901234567890":1}',
	'{"content":"call me at 15551234567890123456789 ok"}',
	'{"content":"he said \\"1472109478688579732\\" loudly","id":1472109478688579732}',
	'{"content":"trailing backslash \\\\","id":9007199254740993}',
	'{"content":"0001472109478688579732"}',
	'{"id":0001472109478688579732}',
	'{"nested":{"arr":[{"id":1472109478688579732},{"id":15}]},"n":15}',
	'{"content":"no digits at all"}',
	'{}',
	'[]',
	'null',
	'"1472109478688579732"',
	'1472109478688579732',
	'{"took":0.062,"id":1472109478688579732}',
];

type ParseResult = {ok: true; value: unknown} | {ok: false; error: string};

function toResult(parse: () => unknown): ParseResult {
	try {
		return {ok: true, value: parse()};
	} catch (error) {
		return {ok: false, error: String(error)};
	}
}

function parseWithDigitRunGate(jsonText: string, digitRun: number): ParseResult {
	const gate = new RegExp(String.raw`\d{${digitRun}}`);
	return toResult(() => JSON.parse(gate.test(jsonText) ? coerceUnsafeIntegersToStrings(jsonText) : jsonText));
}

function parseUngated(jsonText: string): ParseResult {
	return toResult(() => JSON.parse(coerceUnsafeIntegersToStrings(jsonText)));
}

describe('parseJsonPreservingLargeIntegers digit-run gate', () => {
	it('matches the ungated scanner across the corpus', () => {
		for (const body of CORPUS) {
			expect(
				toResult(() => parseJsonPreservingLargeIntegers(body)),
				body,
			).toEqual(parseUngated(body));
		}
	});
	it('exercises both the fast path and the scan', () => {
		const gate = /\d{16}/;
		expect(CORPUS.some((body) => !gate.test(body))).toBe(true);
		expect(CORPUS.some((body) => gate.test(body))).toBe(true);
	});
	it('rejects malformed bodies identically on both paths', () => {
		for (const body of ['{"id":00}', '{"id":0001472109478688579732}']) {
			const gated = toResult(() => parseJsonPreservingLargeIntegers(body));
			expect(gated.ok).toBe(false);
			expect(gated, body).toEqual(parseUngated(body));
		}
	});
	it('diverges from the ungated scanner when the gate is loosened to 17 digits', () => {
		const broken = CORPUS.filter((body) => {
			const loosened = parseWithDigitRunGate(body, 17);
			const ungated = parseUngated(body);
			return JSON.stringify(loosened) !== JSON.stringify(ungated);
		});
		expect(broken).toContain('{"id":9007199254740992}');
		expect(broken).toContain('{"id":9999999999999999}');
		expect(broken).toContain('{"id":-9007199254740992}');
	});
	it('agrees with the ungated scanner when the gate is tightened to 15 digits', () => {
		for (const body of CORPUS) {
			expect(parseWithDigitRunGate(body, 15), body).toEqual(parseUngated(body));
		}
	});
});
