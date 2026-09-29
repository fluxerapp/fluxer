// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {
	errorForPhoneReply,
	natsPhoneRpcTransport,
	type PhoneRpcTransport,
	PhoneRpcUnreachableError,
	PhoneVerificationClient,
	phoneIdempotencyKey,
	renderPhoneRpcMetrics,
} from '@app/api/auth/PhoneVerificationClient';
import {HEADER, RPC_PHONE_CHECK, RPC_PHONE_START} from '@app/api/infrastructure/activity/Contract.generated';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {PhoneAlreadyUsedError} from '@fluxer/errors/src/domains/auth/PhoneAlreadyUsedError';
import {SmsVerificationUnavailableError} from '@fluxer/errors/src/domains/auth/SmsVerificationUnavailableError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {connect, type NatsConnection} from '@nats-io/transport-node';
import {afterAll, beforeAll, describe, expect, it} from 'vitest';

interface RecordedRequest {
	subject: string;
	payload: Record<string, unknown>;
	timeoutMs: number;
	headers: Record<string, string>;
}

class ScriptedTransport implements PhoneRpcTransport {
	readonly requests: Array<RecordedRequest> = [];
	constructor(private readonly script: Array<string | Error>) {}

	async request(subject: string, payload: string, options: {timeoutMs: number; headers: Record<string, string>}) {
		this.requests.push({subject, payload: JSON.parse(payload), timeoutMs: options.timeoutMs, headers: options.headers});
		const next = this.script.shift();
		if (next === undefined) throw new Error('no scripted reply');
		if (next instanceof Error) throw next;
		return next;
	}
}

const INFO = {userId: '1174109840998400001', phone: '+15555550123', requestId: 'req-1'};
const START = {
	user_id: INFO.userId,
	user_flags: '0',
	has_verified_phone: false,
	phone: INFO.phone,
	requested_channel: null,
	client_ip: '203.0.113.9',
	captcha_passed: false,
};

describe('phone verification client', () => {
	it('sends an absolute deadline, a stable idempotency key and the request id', async () => {
		let now = 1_000_000;
		const transport = new ScriptedTransport([JSON.stringify({result: 'sms_sent'})]);
		const client = new PhoneVerificationClient(
			transport,
			() => now,
			() => 'nonce-1',
		);
		const reply = await client.start(START, INFO);
		expect(reply).toEqual({result: 'sms_sent'});
		const [request] = transport.requests;
		expect(request.subject).toBe(RPC_PHONE_START);
		expect(request.payload).toEqual({v: 1, ...START});
		expect(request.headers[HEADER.deadline]).toBe(String(now + 12_000));
		expect(request.headers[HEADER.requestId]).toBe('req-1');
		expect(request.headers[HEADER.idempotencyKey]).toBe(
			createHash('sha256').update(`start\n${INFO.userId}\n${INFO.phone}\nnonce-1`).digest('base64url'),
		);
		expect(request.timeoutMs).toBe(12_250);
		now += 1;
		expect(phoneIdempotencyKey('check', INFO, 'n')).not.toBe(phoneIdempotencyKey('start', INFO, 'n'));
	});

	it('never derives the idempotency key from the client request id', async () => {
		const transport = new ScriptedTransport([
			JSON.stringify({result: 'sms_sent'}),
			JSON.stringify({result: 'sms_sent'}),
		]);
		const client = new PhoneVerificationClient(transport);
		await client.start(START, INFO);
		await client.start(START, INFO);
		const [first, second] = transport.requests;
		expect(first.headers[HEADER.requestId]).toBe(second.headers[HEADER.requestId]);
		expect(first.headers[HEADER.idempotencyKey]).not.toBe(second.headers[HEADER.idempotencyKey]);
	});

	it('counts error replies by their code so unavailable is alertable', async () => {
		const transport = new ScriptedTransport([JSON.stringify({result: 'error', code: 'unavailable'})]);
		await new PhoneVerificationClient(transport).start(START, INFO);
		expect(renderPhoneRpcMetrics()).toContain('fluxer_api_phone_rpc_total{op="start",result="unavailable"}');
	});

	it('retries once with the same key when the service did not answer and enough time remains', async () => {
		let now = 0;
		const transport = new ScriptedTransport([
			new PhoneRpcUnreachableError('no_responders'),
			JSON.stringify({result: 'sms_sent'}),
		]);
		const client = new PhoneVerificationClient(transport, () => now);
		const pending = client.start(START, INFO);
		now = 1_000;
		expect(await pending).toEqual({result: 'sms_sent'});
		expect(transport.requests).toHaveLength(2);
		expect(transport.requests[1].headers[HEADER.idempotencyKey]).toBe(
			transport.requests[0].headers[HEADER.idempotencyKey],
		);
		expect(transport.requests[1].headers[HEADER.deadline]).toBe(transport.requests[0].headers[HEADER.deadline]);
	});

	it('gives up without a retry when too little of the deadline is left', async () => {
		let now = 0;
		const transport: PhoneRpcTransport = {
			request: async () => {
				now = 9_000;
				throw new PhoneRpcUnreachableError('timeout');
			},
		};
		const client = new PhoneVerificationClient(transport, () => now);
		await expect(
			client.check({user_id: INFO.userId, user_flags: '0', phone: INFO.phone, code: '123456', client_ip: ''}, INFO),
		).rejects.toBeInstanceOf(SmsVerificationUnavailableError);
	});

	it('answers unavailable when the service is not configured or replies with garbage', async () => {
		await expect(new PhoneVerificationClient(null).start(START, INFO)).rejects.toBeInstanceOf(
			SmsVerificationUnavailableError,
		);
		const garbled = new PhoneVerificationClient(new ScriptedTransport(['{"nope":1}']));
		await expect(garbled.start(START, INFO)).rejects.toBeInstanceOf(SmsVerificationUnavailableError);
	});

	it('maps service errors to the existing client errors', () => {
		const limited = errorForPhoneReply({
			code: 'rate_limited',
			retry_after_s: 600,
			rate_limit_scope: 'shared',
			limit: 3,
			message: 'Too many verification texts were sent recently. Try again later.',
		});
		expect(limited).toBeInstanceOf(RateLimitError);
		expect((limited as RateLimitError).code).toBe(APIErrorCodes.PHONE_RATE_LIMIT_EXCEEDED);
		const blank = {retry_after_s: null, rate_limit_scope: null, limit: null, message: null};
		expect(errorForPhoneReply({code: 'already_used', ...blank})).toBeInstanceOf(PhoneAlreadyUsedError);
		for (const code of ['unavailable', 'deadline_exceeded', 'unsupported_contract'] as const) {
			expect(errorForPhoneReply({code, ...blank})).toBeInstanceOf(SmsVerificationUnavailableError);
		}
	});
});

const NATS_URL = process.env.FLUXER_TEST_ACTIVITY_NATS_URL;

describe.skipIf(!NATS_URL)('phone verification client over NATS', () => {
	let nc: NatsConnection;
	const seen: Array<{subject: string; headers: Record<string, string>}> = [];
	beforeAll(async () => {
		nc = await connect({servers: NATS_URL, token: process.env.FLUXER_TEST_ACTIVITY_NATS_TOKEN});
		nc.subscribe('rpc.phone.v1.*', {
			queue: 'phone',
			callback: (_error, msg) => {
				const headers: Record<string, string> = {};
				for (const key of msg.headers?.keys() ?? []) headers[key] = msg.headers!.get(key);
				seen.push({subject: msg.subject, headers});
				msg.respond(
					JSON.stringify({
						result: 'verified',
						action: {
							v: 1,
							id: 'phone:k',
							key: INFO.userId,
							issued_at_ms: 1,
							expires_at_ms: 2,
							type: 'phone_verified',
							user_id: INFO.userId,
							method: 'outbound',
							clear_suspicious: 4,
						},
					}),
				);
			},
		});
		await nc.flush();
	});
	afterAll(async () => {
		await nc?.close();
	});

	it('carries the headers to the responder and returns its reply', async () => {
		const client = new PhoneVerificationClient(natsPhoneRpcTransport(() => nc));
		const reply = await client.check(
			{user_id: INFO.userId, user_flags: '0', phone: INFO.phone, code: '123456', client_ip: '203.0.113.9'},
			INFO,
		);
		expect(reply.result).toBe('verified');
		expect(seen.at(-1)?.subject).toBe(RPC_PHONE_CHECK);
		expect(seen.at(-1)?.headers[HEADER.requestId]).toBe('req-1');
		expect(Number(seen.at(-1)?.headers[HEADER.deadline])).toBeGreaterThan(Date.now());
	});

	it('reports a subject nobody serves as unreachable', async () => {
		const other = await connect({servers: NATS_URL, token: process.env.FLUXER_TEST_ACTIVITY_NATS_TOKEN});
		try {
			const transport = natsPhoneRpcTransport(() => other);
			await expect(transport.request('rpc.phone.v9.start', '{}', {timeoutMs: 1000, headers: {}})).rejects.toMatchObject(
				{reason: 'no_responders'},
			);
		} finally {
			await other.close();
		}
	});
});
