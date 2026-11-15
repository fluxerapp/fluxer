// SPDX-License-Identifier: AGPL-3.0-or-later

import type {EngineMessagePort} from '@electron/engine/HostChannel';
import {
	ACCOUNT_ENGINE_LIMITS,
	type AccountEngineClientConfig,
	type AccountEngineDemotion,
	AccountEngineErrorCode,
	type AccountEngineEvent,
	type AccountEngineGuildSubscriptionRelease,
	AccountEngineRequestError,
	type AccountEngineRequestMethod,
	type AccountEngineRequests,
	type AccountEngineResumeSession,
	type AccountEngineSnapshotCapture,
	type AccountEngineSnapshotCursor,
	type AccountEngineSubscriptionRelease,
} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import {isObjectLike, isRecord} from '@fluxer/record_utils/src/RecordGuards';

const SESSION_ID_MAX_LENGTH = 4096;
const SNOWFLAKE_PATTERN = /^\d{1,20}$/u;
const MAX_PROPERTIES_BYTES = 16 * 1024;

export type AccountEngineHandlers = {
	[M in AccountEngineRequestMethod]: (
		params: AccountEngineRequests[M]['params'],
	) => Promise<AccountEngineRequests[M]['result']> | AccountEngineRequests[M]['result'];
};

function invalid(message: string): never {
	throw new AccountEngineRequestError(AccountEngineErrorCode.INVALID_REQUEST, message);
}

function record(value: unknown, label: string): Record<string, unknown> {
	if (!isRecord(value)) {
		invalid(`${label} must be an object`);
	}
	return value;
}

function accountKey(value: unknown): string {
	if (typeof value !== 'string' || value.length === 0 || value.length > ACCOUNT_ENGINE_LIMITS.accountKeyMaxChars) {
		invalid('accountKey must be a non-empty string');
	}
	return value;
}

function snowflake(value: unknown, label: string): string {
	if (typeof value !== 'string' || !SNOWFLAKE_PATTERN.test(value)) {
		invalid(`${label} must be a snowflake`);
	}
	return value;
}

function boundedInteger(value: unknown, label: string, max: number): number {
	if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < 0 || value > max) {
		invalid(`${label} must be an integer from 0 to ${max}`);
	}
	return value;
}

function parseResumeSession(value: unknown): AccountEngineResumeSession | null {
	if (value === null) {
		return null;
	}
	const session = record(value, 'session');
	if (
		typeof session.sessionId !== 'string' ||
		session.sessionId.length === 0 ||
		session.sessionId.length > SESSION_ID_MAX_LENGTH
	) {
		invalid('session.sessionId must be a string');
	}
	if (typeof session.sequence !== 'number' || !Number.isSafeInteger(session.sequence) || session.sequence < 0) {
		invalid('session.sequence must be a non-negative integer');
	}
	if (typeof session.gatewayUrl !== 'string' || session.gatewayUrl.length > ACCOUNT_ENGINE_LIMITS.endpointMaxChars) {
		invalid('session.gatewayUrl must be a string');
	}
	return {sessionId: session.sessionId, sequence: session.sequence, gatewayUrl: session.gatewayUrl};
}

const RELEASE_GUILDS_MAX = 512;

function emptyList(value: unknown, label: string): readonly [] {
	if (!Array.isArray(value) || value.length !== 0) {
		invalid(`${label} must be an empty list`);
	}
	return [];
}

function parseGuildRelease(value: unknown, label: string): AccountEngineGuildSubscriptionRelease {
	const guild = record(value, label);
	const release: {
		member_list_channels?: Record<string, readonly []>;
		members?: readonly [];
		threads?: false;
		thread_member_lists?: readonly [];
		focused_channel_id?: null;
	} = {};
	for (const [key, field] of Object.entries(guild)) {
		switch (key) {
			case 'member_list_channels': {
				const channels = record(field, `${label}.member_list_channels`);
				const entries = Object.entries(channels);
				if (entries.length !== 1) {
					invalid(`${label}.member_list_channels must name one channel`);
				}
				release.member_list_channels = Object.fromEntries(
					entries.map(([channelId, ranges]) => [
						snowflake(channelId, `${label}.member_list_channels`),
						emptyList(ranges, `${label}.member_list_channels.${channelId}`),
					]),
				);
				break;
			}
			case 'members':
				release.members = emptyList(field, `${label}.members`);
				break;
			case 'thread_member_lists':
				release.thread_member_lists = emptyList(field, `${label}.thread_member_lists`);
				break;
			case 'threads':
				if (field !== false) {
					invalid(`${label}.threads can only be released`);
				}
				release.threads = false;
				break;
			case 'focused_channel_id':
				if (field !== null) {
					invalid(`${label}.focused_channel_id can only be released`);
				}
				release.focused_channel_id = null;
				break;
			default:
				invalid(`${label}.${key} is not a releasable subscription`);
		}
	}
	return release;
}

function parseRelease(value: unknown): AccountEngineSubscriptionRelease | null {
	if (value === null) {
		return null;
	}
	const guilds = Object.entries(record(value, 'release'));
	if (guilds.length > RELEASE_GUILDS_MAX) {
		invalid(`release must name at most ${RELEASE_GUILDS_MAX} communities`);
	}
	return Object.fromEntries(
		guilds.map(([guildId, guild]) => [snowflake(guildId, 'release'), parseGuildRelease(guild, `release.${guildId}`)]),
	);
}

function parseCursor(value: unknown): AccountEngineSnapshotCursor {
	const cursor = record(value, 'capture.cursor');
	if (cursor.sessionId !== null && typeof cursor.sessionId !== 'string') {
		invalid('capture.cursor.sessionId must be a string or null');
	}
	if (
		cursor.schemaEpoch !== null &&
		(typeof cursor.schemaEpoch !== 'number' || !Number.isSafeInteger(cursor.schemaEpoch))
	) {
		invalid('capture.cursor.schemaEpoch must be an integer or null');
	}
	if (typeof cursor.updatedAt !== 'number' || !Number.isFinite(cursor.updatedAt) || cursor.updatedAt < 0) {
		invalid('capture.cursor.updatedAt must be a timestamp');
	}
	return {
		sessionId: cursor.sessionId,
		schemaEpoch: cursor.schemaEpoch,
		updatedAt: cursor.updatedAt,
	};
}

function parseCapture(value: unknown): AccountEngineSnapshotCapture | null {
	if (value === null) {
		return null;
	}
	const capture = record(value, 'capture');
	const entries = record(capture.entries, 'capture.entries');
	for (const [entity, rows] of Object.entries(entries)) {
		if (entity.length === 0 || entity.length > 64) {
			invalid('capture entity names must be short strings');
		}
		for (const [key, row] of Object.entries(record(rows, `capture.entries.${entity}`))) {
			if (key.length === 0 || typeof row !== 'string') {
				invalid('capture rows must map keys to serialized strings');
			}
		}
	}
	return {entries: entries as AccountEngineSnapshotCapture['entries'], cursor: parseCursor(capture.cursor)};
}

function parseMentionCounts(value: unknown): Readonly<Record<string, number>> | null {
	if (value === null) {
		return null;
	}
	const counts = record(value, 'mentionCounts');
	const parsed: Record<string, number> = {};
	for (const [channelId, count] of Object.entries(counts)) {
		snowflake(channelId, 'mentionCounts key');
		parsed[channelId] = boundedInteger(count, 'mention count', Number.MAX_SAFE_INTEGER);
	}
	return parsed;
}

function parseConfig(value: unknown): AccountEngineClientConfig {
	const config = record(value, 'params');
	const properties = record(config.properties, 'properties');
	if (JSON.stringify(properties).length > MAX_PROPERTIES_BYTES) {
		invalid('properties are too large');
	}
	return {
		properties,
		apiVersion: boundedInteger(config.apiVersion, 'apiVersion', 1000),
		identifyFlags: boundedInteger(config.identifyFlags, 'identifyFlags', 2 ** 31 - 1),
	};
}

type Validators = {[M in AccountEngineRequestMethod]: (params: unknown) => AccountEngineRequests[M]['params']};

const VALIDATORS: Validators = {
	configure: parseConfig,
	claimForeground: (params) => {
		const value = record(params, 'params');
		return {accountKey: value.accountKey === null ? null : accountKey(value.accountKey)};
	},
	getSummaries: () => null,
	getActivity: (params) => {
		const value = record(params, 'params');
		let accountKeys: Array<string> | null = null;
		if (value.accountKeys !== null) {
			if (!Array.isArray(value.accountKeys) || value.accountKeys.length > 10_000) {
				invalid('accountKeys must be an array or null');
			}
			accountKeys = value.accountKeys.map(accountKey);
		}
		return {
			accountKeys,
			channelLimit: boundedInteger(value.channelLimit, 'channelLimit', ACCOUNT_ENGINE_LIMITS.activityChannelLimitMax),
			mentionLimit: boundedInteger(value.mentionLimit, 'mentionLimit', ACCOUNT_ENGINE_LIMITS.activityMentionLimitMax),
		};
	},
	beginPromotion: (params) => ({accountKey: accountKey(record(params, 'params').accountKey)}),
	completePromotion: (params) => ({accountKey: accountKey(record(params, 'params').accountKey)}),
	abortPromotion: (params) => ({accountKey: accountKey(record(params, 'params').accountKey)}),
	demote: (params): AccountEngineDemotion => {
		const value = record(params, 'params');
		return {
			accountKey: accountKey(value.accountKey),
			session: parseResumeSession(value.session ?? null),
			release: parseRelease(value.release ?? null),
			capture: parseCapture(value.capture ?? null),
			mentionCounts: parseMentionCounts(value.mentionCounts ?? null),
		};
	},
	ackChannel: (params) => {
		const value = record(params, 'params');
		return {
			accountKey: accountKey(value.accountKey),
			channelId: snowflake(value.channelId, 'channelId'),
			messageId: snowflake(value.messageId, 'messageId'),
		};
	},
	reconnect: (params) => ({accountKey: accountKey(record(params, 'params').accountKey)}),
	setUnreadBadgeCustomization: (params) => {
		const value = record(params, 'params');
		if (
			!Array.isArray(value.enabledAccountKeys) ||
			value.enabledAccountKeys.length > ACCOUNT_ENGINE_LIMITS.providedAccountsMax
		) {
			invalid('enabledAccountKeys must be an array');
		}
		return {enabledAccountKeys: value.enabledAccountKeys.map(accountKey)};
	},
};

function isMethod(value: unknown): value is AccountEngineRequestMethod {
	return typeof value === 'string' && Object.hasOwn(VALIDATORS, value);
}

function errorResponse(error: unknown): {code: string; message: string} {
	if (error instanceof AccountEngineRequestError) {
		return {code: error.code, message: error.message};
	}
	return {
		code: AccountEngineErrorCode.UNAVAILABLE,
		message: error instanceof Error ? error.message : 'The account engine failed to answer',
	};
}

export class RendererPortServer {
	private readonly port: EngineMessagePort;
	private readonly handlers: AccountEngineHandlers;
	private closed = false;

	constructor(port: EngineMessagePort, handlers: AccountEngineHandlers, onClose: () => void) {
		this.port = port;
		this.handlers = handlers;
		port.onMessage((message) => {
			void this.handleMessage(message);
		});
		port.onClose(() => {
			this.closed = true;
			onClose();
		});
	}

	emit(event: AccountEngineEvent): void {
		this.post({kind: 'event', event});
	}

	close(): void {
		if (this.closed) {
			return;
		}
		this.closed = true;
		try {
			this.port.close();
		} catch {}
	}

	private async handleMessage(message: unknown): Promise<void> {
		if (!isObjectLike(message)) {
			return;
		}
		const {kind, id, method, params} = message;
		if (kind !== 'request' || typeof id !== 'number' || !Number.isSafeInteger(id)) {
			return;
		}
		try {
			if (!isMethod(method)) {
				invalid(`Unknown method ${String(method)}`);
			}
			const validated = VALIDATORS[method](params);
			const handler = this.handlers[method] as (value: unknown) => unknown;
			const result = await handler(validated);
			this.post({kind: 'response', id, ok: true, result: result ?? null});
		} catch (error) {
			this.post({kind: 'response', id, ok: false, error: errorResponse(error)});
		}
	}

	private post(message: unknown): void {
		if (this.closed) {
			return;
		}
		try {
			this.port.postMessage(message);
		} catch {
			this.closed = true;
		}
	}
}
