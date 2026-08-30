// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {KVClient} from '@pkgs/kv_client/src/KVClient';
import {KVClientErrorCode} from '@pkgs/kv_client/src/KVClientError';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const {evalshaMock, evalMock} = vi.hoisted(() => ({
	evalshaMock: vi.fn(),
	evalMock: vi.fn(),
}));

vi.mock('ioredis', () => {
	class MockRedis {
		evalsha = evalshaMock;
		eval = evalMock;
	}
	return {default: MockRedis, Cluster: MockRedis};
});

const RATE_LIMIT_REPLY = JSON.stringify({
	allowed: true,
	limit: 5,
	remaining: 4,
	resetAfterMs: 200,
	resetAtMs: 1717171717,
	retryAfterMs: 0,
});

const EXPECTED_RATE_LIMIT_RESULT = {
	allowed: true,
	limit: 5,
	remaining: 4,
	resetAfterMs: 200,
	resetAtMs: 1717171717,
	retryAfterMs: 0,
};

function createClient(): KVClient {
	return new KVClient('redis://127.0.0.1:6379');
}

function noScriptError(): Error {
	return new Error('NOSCRIPT No matching script. Please use EVAL.');
}

function getCallArguments(mock: typeof evalshaMock, index: number): Array<unknown> {
	const call = mock.mock.calls[index];
	if (!call) {
		throw new Error(`Expected a call at index ${index}`);
	}
	return call;
}

describe('KVClient script execution', () => {
	beforeEach(() => {
		evalshaMock.mockReset();
		evalMock.mockReset();
	});

	it('sends EVALSHA instead of EVAL for the leaky bucket rate limit script', async () => {
		evalshaMock.mockResolvedValue(RATE_LIMIT_REPLY);
		const result = await createClient().checkLeakyBucketLimit('rate_limit:bucket', 5, 1000, 1);
		expect(evalMock).not.toHaveBeenCalled();
		expect(evalshaMock).toHaveBeenCalledTimes(1);
		const [sha, keyCount, key, , limit, windowMs, cost] = getCallArguments(evalshaMock, 0);
		expect(sha).toMatch(/^[0-9a-f]{40}$/);
		expect(keyCount).toBe(1);
		expect(key).toBe('rate_limit:bucket');
		expect(limit).toBe(5);
		expect(windowMs).toBe(1000);
		expect(cost).toBe(1);
		expect(result).toEqual(EXPECTED_RATE_LIMIT_RESULT);
	});

	it('falls back to EVAL with the original script and identical arguments on NOSCRIPT', async () => {
		evalshaMock.mockRejectedValue(noScriptError());
		evalMock.mockResolvedValue(RATE_LIMIT_REPLY);
		const result = await createClient().checkLeakyBucketLimit('rate_limit:bucket', 5, 1000, 1);
		expect(evalshaMock).toHaveBeenCalledTimes(1);
		expect(evalMock).toHaveBeenCalledTimes(1);
		const [sha, ...evalshaRest] = getCallArguments(evalshaMock, 0);
		const [script, ...evalRest] = getCallArguments(evalMock, 0);
		expect(createHash('sha1').update(String(script)).digest('hex')).toBe(sha);
		expect(evalRest).toEqual(evalshaRest);
		expect(String(script)).toContain("local rawState = redis.call('GET', key)");
		expect(result).toEqual(EXPECTED_RATE_LIMIT_RESULT);
	});

	it('keeps using EVALSHA with the same digest after a NOSCRIPT fallback', async () => {
		evalshaMock.mockRejectedValueOnce(noScriptError()).mockResolvedValue(RATE_LIMIT_REPLY);
		evalMock.mockResolvedValue(RATE_LIMIT_REPLY);
		const client = createClient();
		const first = await client.checkLeakyBucketLimit('rate_limit:bucket', 5, 1000, 1);
		const second = await client.checkLeakyBucketLimit('rate_limit:bucket', 5, 1000, 1);
		expect(evalshaMock).toHaveBeenCalledTimes(2);
		expect(evalMock).toHaveBeenCalledTimes(1);
		expect(getCallArguments(evalshaMock, 1)[0]).toBe(getCallArguments(evalshaMock, 0)[0]);
		expect(first).toEqual(EXPECTED_RATE_LIMIT_RESULT);
		expect(second).toEqual(EXPECTED_RATE_LIMIT_RESULT);
	});

	it('sends EVALSHA for every scripted command', async () => {
		const cases: Array<{name: string; reply: unknown; keyCount: number; run: (client: KVClient) => Promise<unknown>}> =
			[
				{
					name: 'releaseLock',
					reply: 1,
					keyCount: 1,
					run: async (client) => client.releaseLock('lock:key', 'token'),
				},
				{
					name: 'extendLock',
					reply: 1,
					keyCount: 1,
					run: async (client) => client.extendLock('lock:key', 'token', 30),
				},
				{
					name: 'renewSnowflakeNode',
					reply: 1,
					keyCount: 1,
					run: async (client) => client.renewSnowflakeNode('snowflake:1', 'instance', 30),
				},
				{
					name: 'checkLeakyBucketLimit',
					reply: RATE_LIMIT_REPLY,
					keyCount: 1,
					run: async (client) => client.checkLeakyBucketLimit('rate_limit:bucket', 5, 1000, 1),
				},
				{
					name: 'tryConsumeTokens',
					reply: 2,
					keyCount: 1,
					run: async (client) => client.tryConsumeTokens('tokens:key', 2, 10, 1, 1000),
				},
				{
					name: 'scheduleBulkDeletion',
					reply: 1,
					keyCount: 2,
					run: async (client) => client.scheduleBulkDeletion('queue:key', 'secondary:key', 1, 'value'),
				},
				{
					name: 'removeBulkDeletion',
					reply: 1,
					keyCount: 2,
					run: async (client) => client.removeBulkDeletion('queue:key', 'secondary:key'),
				},
				{
					name: 'dequeuePurgeBatch',
					reply: JSON.stringify({urls: ['https://fluxer.test/a.png'], tokens: 1}),
					keyCount: 2,
					run: async (client) => client.dequeuePurgeBatch('queue:key', 'bucket:key', 10, 10, 1, 1000),
				},
				{
					name: 'evalScript',
					reply: 1,
					keyCount: 1,
					run: async (client) => client.evalScript('customScript', "return redis.call('GET', KEYS[1])", 1, 'key'),
				},
			];
		const digests = new Set<unknown>();
		for (const scriptCase of cases) {
			evalshaMock.mockReset();
			evalMock.mockReset();
			evalshaMock.mockResolvedValue(scriptCase.reply);
			await scriptCase.run(createClient());
			expect(evalMock, scriptCase.name).not.toHaveBeenCalled();
			expect(evalshaMock, scriptCase.name).toHaveBeenCalledTimes(1);
			const [sha, keyCount] = getCallArguments(evalshaMock, 0);
			expect(sha, scriptCase.name).toMatch(/^[0-9a-f]{40}$/);
			expect(keyCount, scriptCase.name).toBe(scriptCase.keyCount);
			digests.add(sha);
		}
		expect(digests.size).toBe(cases.length);
	});

	it('does not retry with EVAL when the script fails for another reason', async () => {
		evalshaMock.mockRejectedValue(new Error('Connection is closed.'));
		await expect(createClient().releaseLock('lock:key', 'token')).rejects.toMatchObject({
			code: KVClientErrorCode.REQUEST_FAILED,
			message: 'KV request failed (releaseLock): Connection is closed.',
		});
		expect(evalMock).not.toHaveBeenCalled();
	});

	it('normalizes timeouts raised by the EVALSHA attempt', async () => {
		evalshaMock.mockRejectedValue(new Error('Command timed out'));
		await expect(createClient().releaseLock('lock:key', 'token')).rejects.toMatchObject({
			code: KVClientErrorCode.TIMEOUT,
			message: 'KV request timed out: releaseLock',
		});
	});

	it('normalizes failures raised by the EVAL fallback', async () => {
		evalshaMock.mockRejectedValue(noScriptError());
		evalMock.mockRejectedValue(new Error('Connection is closed.'));
		await expect(createClient().releaseLock('lock:key', 'token')).rejects.toMatchObject({
			code: KVClientErrorCode.REQUEST_FAILED,
			message: 'KV request failed (releaseLock): Connection is closed.',
		});
	});

	it('reports invalid JSON from a scripted command', async () => {
		evalshaMock.mockResolvedValue('not json');
		await expect(createClient().checkLeakyBucketLimit('rate_limit:bucket', 5, 1000, 1)).rejects.toMatchObject({
			code: KVClientErrorCode.INVALID_RESPONSE,
		});
	});
});
