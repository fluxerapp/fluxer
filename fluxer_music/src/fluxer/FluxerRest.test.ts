// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';
import {FluxerRest} from './FluxerRest';

const TOKEN = 'secret-bot-token-123';
type Call = {url: string; init: RequestInit};

function json(status: number, body: unknown, headers: Record<string, string> = {}): Response {
	return new Response(status === 204 ? null : JSON.stringify(body), {status, headers});
}

function make(handler: (call: Call, n: number) => Response) {
	const calls: Array<Call> = [];
	const fetchImpl = (async (url: string | URL, init?: RequestInit) => {
		const call = {url: String(url), init: init ?? {}};
		calls.push(call);
		return handler(call, calls.length);
	}) as unknown as typeof fetch;
	const rest = new FluxerRest({baseUrl: 'https://fx.example', token: TOKEN, fetch: fetchImpl});
	return {rest, calls};
}

const discovery = () => json(200, {endpoints: {api_public: 'https://api.fx.example'}});

describe('FluxerRest', () => {
	it('sends Bot auth and allowed_mentions', async () => {
		const {rest, calls} = make((call) => (call.url.includes('well-known') ? discovery() : json(200, {id: 'm1'})));
		await rest.discover();
		expect(calls[0]?.url).toBe('https://fx.example/.well-known/fluxer');
		const res = await rest.sendMessage('c1', {content: 'hi', message_reference: {message_id: 'x'}});
		expect(res).toEqual({id: 'm1'});
		const call = calls[1];
		expect(call?.url).toBe('https://api.fx.example/v1/channels/c1/messages');
		expect(call?.init.method).toBe('POST');
		expect(((call as Call).init.headers as Record<string, string>).Authorization).toBe(`Bot ${TOKEN}`);
		expect(JSON.parse(String(call?.init.body))).toEqual({
			content: 'hi',
			message_reference: {message_id: 'x'},
			allowed_mentions: {parse: []},
		});
		await rest.editMessage('c1', 'm1', {content: 'yo'});
		expect(calls[2]?.init.method).toBe('PATCH');
		expect(calls[2]?.url).toBe('https://api.fx.example/v1/channels/c1/messages/m1');
		expect(JSON.parse(String(calls[2]?.init.body)).allowed_mentions).toEqual({parse: []});
		await rest.deleteMessage('c1', 'm1');
		expect(calls[3]?.init.method).toBe('DELETE');
	});

	it('discovers lazily, and reads the gateway url and channels', async () => {
		const {rest, calls} = make((call) => {
			if (call.url.includes('well-known')) return discovery();
			if (call.url.endsWith('/gateway/bot')) return json(200, {url: 'wss://gw.example', shards: 1});
			return json(200, {id: 'c1', guild_id: 'g1', type: 2, bitrate: 64000, name: 'x'});
		});
		expect(await rest.gatewayUrl()).toBe('wss://gw.example');
		expect(calls.map((c) => c.url)).toEqual([
			'https://fx.example/.well-known/fluxer',
			'https://api.fx.example/v1/gateway/bot',
		]);
		expect(await rest.getChannel('c1')).toEqual({id: 'c1', guildId: 'g1', type: 2, bitrate: 64000});
	});

	it('maps a channel without guild or bitrate to nulls', async () => {
		const {rest} = make((call) => (call.url.includes('well-known') ? discovery() : json(200, {id: 'c2', type: 1})));
		expect(await rest.getChannel('c2')).toEqual({id: 'c2', guildId: null, type: 1, bitrate: null});
	});

	it('retries 429 after retry_after', async () => {
		vi.useFakeTimers();
		try {
			const {rest, calls} = make((call, n) => {
				if (call.url.includes('well-known')) return discovery();
				return n === 2 ? json(429, {code: 'RATE_LIMITED', retry_after: 0.5, global: false}) : json(200, {id: 'm1'});
			});
			const p = rest.sendMessage('c1', {content: 'x'});
			await vi.advanceTimersByTimeAsync(400);
			expect(calls.filter((c) => c.url.includes('/messages'))).toHaveLength(1);
			await vi.advanceTimersByTimeAsync(200);
			await expect(p).resolves.toEqual({id: 'm1'});
			expect(calls.filter((c) => c.url.includes('/messages'))).toHaveLength(2);
		} finally {
			vi.useRealTimers();
		}
	});

	it('gives up after 3 tries on 429', async () => {
		const {rest, calls} = make((call) =>
			call.url.includes('well-known') ? discovery() : json(429, {code: 'RATE_LIMITED', retry_after: 0.001}),
		);
		await expect(rest.deleteMessage('c1', 'm1')).rejects.toThrow(/429/);
		expect(calls.filter((c) => c.url.includes('/messages'))).toHaveLength(3);
	});

	it('error message never contains the token', async () => {
		const {rest} = make((call) =>
			call.url.includes('well-known') ? discovery() : json(403, {code: 'MISSING_PERMISSIONS', message: 'nope'}),
		);
		const err = await rest.sendMessage('c1', {content: 'x'}).catch((e: Error) => e);
		expect(err).toBeInstanceOf(Error);
		expect(String((err as Error).message)).not.toContain(TOKEN);
		expect(String((err as Error).stack)).not.toContain(TOKEN);
		expect((err as Error).message).toContain('403');
		expect((err as Error).message).toContain('MISSING_PERMISSIONS');
	});

	it('encodes emoji and passes session_id on reaction removal', async () => {
		const {rest, calls} = make((call) => (call.url.includes('well-known') ? discovery() : json(204, null)));
		await rest.addReaction('c1', 'm1', '⏯');
		expect(calls[1]?.init.method).toBe('PUT');
		expect(calls[1]?.url).toBe(
			`https://api.fx.example/v1/channels/c1/messages/m1/reactions/${encodeURIComponent('⏯')}/@me`,
		);
		await rest.removeUserReaction('c1', 'm1', '⏯', 'u7', 'sess1');
		expect(calls[2]?.init.method).toBe('DELETE');
		expect(calls[2]?.url).toBe(
			`https://api.fx.example/v1/channels/c1/messages/m1/reactions/${encodeURIComponent('⏯')}/u7?session_id=sess1`,
		);
		await rest.removeUserReaction('c1', 'm1', '⏯', 'u7');
		expect(calls[3]?.url.endsWith('/u7')).toBe(true);
	});
});
