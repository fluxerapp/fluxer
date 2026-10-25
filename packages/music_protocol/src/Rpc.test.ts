// SPDX-License-Identifier: AGPL-3.0-or-later

import {decodeRpcPayload, decodeRpcResult, encodeRpcResult} from '@fluxer/music_protocol/src/Rpc';
import {describe, expect, test} from 'vitest';

describe('Rpc', () => {
	test('rpc results round-trip', () => {
		expect(decodeRpcResult(encodeRpcResult({ok: true, value: {added: 3}}))).toEqual({ok: true, value: {added: 3}});
		expect(decodeRpcResult(encodeRpcResult({ok: false, error: 'QUEUE_FULL'}))).toEqual({
			ok: false,
			error: 'QUEUE_FULL',
		});
		expect(decodeRpcResult('{]')).toEqual({ok: false, error: 'SOURCE_FAILED'});
	});
	test('unknown error code becomes SOURCE_FAILED', () => {
		expect(decodeRpcResult('{"error":"WAT"}')).toEqual({ok: false, error: 'SOURCE_FAILED'});
	});
	test('payload validation', () => {
		expect(decodeRpcPayload('music.seek', '{"ms":5000}')).toEqual({ms: 5000});
		expect(decodeRpcPayload('music.seek', '{"ms":"5"}')).toBeNull();
		expect(decodeRpcPayload('music.loop', '{"mode":"forever"}')).toBeNull();
		expect(decodeRpcPayload('music.queue', '{"offset":0,"limit":999}')).toEqual({offset: 0, limit: 20});
		expect(decodeRpcPayload('music.add', '{"input":""}')).toBeNull();
	});
	test('payload-less methods accept {}', () => {
		expect(decodeRpcPayload('music.pause', '{}')).toEqual({});
		expect(decodeRpcPayload('music.stop', 'nope')).toBeNull();
	});
});
