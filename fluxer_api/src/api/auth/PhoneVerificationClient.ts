// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash, randomUUID} from 'node:crypto';
import type {
	ChallengeReply,
	ChallengeReq,
	CheckReply,
	CheckReq,
	PhoneError,
	StartReply,
	StartReq,
} from '@app/api/infrastructure/activity/Contract.generated';
import {
	HEADER,
	PHONE_CLIENT_TIMEOUT_SLACK_MS,
	PHONE_DEADLINE_MS,
	PHONE_RETRY_MIN_REMAINING_MS,
	PHONE_RPC_VERSION,
	RPC_PHONE_CHALLENGE,
	RPC_PHONE_CHECK,
	RPC_PHONE_START,
} from '@app/api/infrastructure/activity/Contract.generated';
import {Logger} from '@app/api/Logger';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {InvalidPhoneNumberError} from '@fluxer/errors/src/domains/auth/InvalidPhoneNumberError';
import {InvalidPhoneVerificationCodeError} from '@fluxer/errors/src/domains/auth/InvalidPhoneVerificationCodeError';
import {PhoneAlreadyUsedError} from '@fluxer/errors/src/domains/auth/PhoneAlreadyUsedError';
import {PhoneCountryNotSupportedError} from '@fluxer/errors/src/domains/auth/PhoneCountryNotSupportedError';
import {PhoneInboundVerificationRequiredError} from '@fluxer/errors/src/domains/auth/PhoneInboundVerificationRequiredError';
import {PhoneLookupUnavailableError} from '@fluxer/errors/src/domains/auth/PhoneLookupUnavailableError';
import {PhoneNumberNotInServiceError} from '@fluxer/errors/src/domains/auth/PhoneNumberNotInServiceError';
import {PhoneNumberNotMobileError} from '@fluxer/errors/src/domains/auth/PhoneNumberNotMobileError';
import {PhoneVerificationNeedsReviewError} from '@fluxer/errors/src/domains/auth/PhoneVerificationNeedsReviewError';
import {SmsVerificationUnavailableError} from '@fluxer/errors/src/domains/auth/SmsVerificationUnavailableError';
import {CaptchaVerificationRequiredError} from '@fluxer/errors/src/domains/core/CaptchaVerificationRequiredError';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import type {FluxerError} from '@fluxer/errors/src/FluxerError';
import {type NatsConnection, headers as natsHeaders, RequestError, TimeoutError} from '@nats-io/transport-node';

type PhoneOp = keyof typeof PHONE_DEADLINE_MS;

export interface PhoneRpcTransport {
	request(
		subject: string,
		payload: string,
		options: {timeoutMs: number; headers: Record<string, string>},
	): Promise<string>;
}

export class PhoneRpcUnreachableError extends Error {
	constructor(
		readonly reason: 'no_responders' | 'timeout' | 'not_configured',
		options?: {cause?: unknown},
	) {
		super(`Phone verification service unreachable: ${reason}`, options);
		this.name = 'PhoneRpcUnreachableError';
	}
}

export interface PhoneRpcRequestInfo {
	userId: string;
	phone: string;
	requestId: string;
}

const SUBJECTS: Record<PhoneOp, string> = {
	start: RPC_PHONE_START,
	check: RPC_PHONE_CHECK,
	challenge: RPC_PHONE_CHALLENGE,
};
const rpcCounts = new Map<string, number>();

function countRpc(op: PhoneOp, result: string): void {
	const key = `op="${op}",result="${result}"`;
	rpcCounts.set(key, (rpcCounts.get(key) ?? 0) + 1);
}

export function renderPhoneRpcMetrics(): string {
	return [
		'# HELP fluxer_api_phone_rpc_total Phone verification requests by operation and result',
		'# TYPE fluxer_api_phone_rpc_total counter',
		...[...rpcCounts].map(([labels, value]) => `fluxer_api_phone_rpc_total{${labels}} ${value}`),
	].join('\n');
}

export function phoneIdempotencyKey(op: PhoneOp, info: PhoneRpcRequestInfo, nonce: string): string {
	return createHash('sha256').update(`${op}\n${info.userId}\n${info.phone}\n${nonce}`).digest('base64url');
}

function replyLabel(reply: {result: string; code?: unknown}): string {
	return reply.result === 'error' && typeof reply.code === 'string' ? reply.code : reply.result;
}

export function natsPhoneRpcTransport(connection: () => NatsConnection | null): PhoneRpcTransport {
	return {
		async request(subject, payload, options) {
			const nc = connection();
			if (!nc) throw new PhoneRpcUnreachableError('not_configured');
			const h = natsHeaders();
			for (const [name, value] of Object.entries(options.headers)) h.set(name, value);
			try {
				const reply = await nc.request(subject, payload, {timeout: options.timeoutMs, headers: h});
				return reply.string();
			} catch (error) {
				if (error instanceof RequestError && error.isNoResponders()) {
					throw new PhoneRpcUnreachableError('no_responders', {cause: error});
				}
				if (error instanceof TimeoutError || error instanceof RequestError) {
					throw new PhoneRpcUnreachableError('timeout', {cause: error});
				}
				throw error;
			}
		},
	};
}

export class PhoneVerificationClient {
	constructor(
		private readonly transport: PhoneRpcTransport | null,
		private readonly now: () => number = Date.now,
		private readonly nonce: () => string = randomUUID,
	) {}

	start(req: Omit<StartReq, 'v'>, info: PhoneRpcRequestInfo): Promise<StartReply> {
		return this.call<StartReply>('start', {v: PHONE_RPC_VERSION, ...req}, info);
	}

	check(req: Omit<CheckReq, 'v'>, info: PhoneRpcRequestInfo): Promise<CheckReply> {
		return this.call<CheckReply>('check', {v: PHONE_RPC_VERSION, ...req}, info);
	}

	challenge(req: Omit<ChallengeReq, 'v'>, info: PhoneRpcRequestInfo): Promise<ChallengeReply> {
		return this.call<ChallengeReply>('challenge', {v: PHONE_RPC_VERSION, ...req}, info);
	}

	private async call<T extends {result: string}>(op: PhoneOp, body: object, info: PhoneRpcRequestInfo): Promise<T> {
		if (!this.transport) {
			countRpc(op, 'not_configured');
			throw new SmsVerificationUnavailableError();
		}
		const deadline = this.now() + PHONE_DEADLINE_MS[op];
		const headers = {
			[HEADER.deadline]: String(deadline),
			[HEADER.idempotencyKey]: phoneIdempotencyKey(op, info, this.nonce()),
			[HEADER.requestId]: info.requestId,
		};
		const payload = JSON.stringify(body);
		for (let attempt = 0; ; attempt++) {
			const remaining = deadline - this.now();
			try {
				const raw = await this.transport.request(SUBJECTS[op], payload, {
					timeoutMs: Math.max(1, remaining + PHONE_CLIENT_TIMEOUT_SLACK_MS),
					headers,
				});
				const reply = parseReply<T>(raw);
				if (!reply) {
					countRpc(op, 'invalid_reply');
					Logger.error({op}, 'Phone verification service returned an unreadable reply');
					throw new SmsVerificationUnavailableError();
				}
				countRpc(op, replyLabel(reply));
				return reply;
			} catch (error) {
				if (!(error instanceof PhoneRpcUnreachableError)) throw error;
				const left = deadline - this.now();
				if (attempt === 0 && error.reason !== 'not_configured' && left >= PHONE_RETRY_MIN_REMAINING_MS) {
					continue;
				}
				countRpc(op, error.reason);
				Logger.warn({op, reason: error.reason}, 'Phone verification service unreachable');
				throw new SmsVerificationUnavailableError();
			}
		}
	}
}

function parseReply<T extends {result: string}>(raw: string): T | null {
	try {
		const value = JSON.parse(raw) as unknown;
		if (typeof value === 'object' && value !== null && typeof (value as {result?: unknown}).result === 'string') {
			return value as T;
		}
	} catch {
		return null;
	}
	return null;
}

export function errorForPhoneReply(error: PhoneError): FluxerError {
	switch (error.code) {
		case 'invalid_phone':
			return new InvalidPhoneNumberError();
		case 'country_not_supported':
			return new PhoneCountryNotSupportedError();
		case 'lookup_unavailable':
			return new PhoneLookupUnavailableError();
		case 'not_in_service':
			return new PhoneNumberNotInServiceError();
		case 'not_mobile':
			return new PhoneNumberNotMobileError();
		case 'needs_review':
			return new PhoneVerificationNeedsReviewError();
		case 'inbound_required':
			return new PhoneInboundVerificationRequiredError();
		case 'already_used':
			return new PhoneAlreadyUsedError();
		case 'captcha_required':
			return new CaptchaVerificationRequiredError();
		case 'invalid_code':
			return new InvalidPhoneVerificationCodeError();
		case 'rate_limited': {
			const retryAfter = error.retry_after_s ?? 60;
			return new RateLimitError({
				code: APIErrorCodes.PHONE_RATE_LIMIT_EXCEEDED,
				message: error.message ?? undefined,
				retryAfter,
				retryAfterDecimal: retryAfter,
				limit: error.limit ?? 1,
				resetTime: new Date(Date.now() + retryAfter * 1000),
				resetAfterDecimal: retryAfter,
				scope: error.rate_limit_scope ?? 'user',
			});
		}
		case 'unsupported_contract':
			Logger.error({code: error.code}, 'Phone verification service rejected the request contract');
			return new SmsVerificationUnavailableError();
		case 'unavailable':
		case 'deadline_exceeded':
			return new SmsVerificationUnavailableError();
	}
}

let phoneConnection: (() => NatsConnection | null) | null = null;

export function setPhoneRpcConnection(connection: (() => NatsConnection | null) | null): void {
	phoneConnection = connection;
}

export function getPhoneVerificationClient(): PhoneVerificationClient {
	return new PhoneVerificationClient(phoneConnection ? natsPhoneRpcTransport(phoneConnection) : null);
}
