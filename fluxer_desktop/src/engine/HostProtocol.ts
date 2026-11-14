// SPDX-License-Identifier: AGPL-3.0-or-later

import {ACCOUNT_ENGINE_LIMITS, type AccountEngineAccountSummary} from '@fluxer/desktop_ipc/src/AccountEngineContract';
import {isObjectLike} from '@fluxer/record_utils/src/RecordGuards';

export const ACCOUNT_ENGINE_HOST_PROTOCOL_VERSION = 1;

export interface EngineAccountRecord {
	readonly accountKey: string;
	readonly userId: string;
	readonly instanceKey: string;
	readonly token: string;
	readonly gatewayEndpoint: string;
	readonly apiEndpoint: string;
	readonly isValid: boolean;
	readonly lastActive: number;
}

export type EnginePowerEvent = 'suspend' | 'resume' | 'lock-screen' | 'unlock-screen' | 'online' | 'offline';

export interface EngineInitMessage {
	readonly type: 'init';
	readonly protocol: number;
	readonly cachePath: string;
	readonly foregroundHint: string | null;
	readonly accounts: ReadonlyArray<EngineAccountRecord>;
	readonly accountsComplete: boolean;
	readonly preferWebSocket: boolean;
	readonly trustedCertificates: ReadonlyArray<string>;
}

export interface AdmittedGateway {
	readonly url: string;
	readonly address: string | null;
	readonly proxy: string | null;
}

export type HostToEngineMessage =
	| EngineInitMessage
	| {readonly type: 'accounts'; readonly accounts: ReadonlyArray<EngineAccountRecord>}
	| {readonly type: 'attachRenderer'}
	| {readonly type: 'detachRenderer'}
	| {
			readonly type: 'admitResult';
			readonly requestId: number;
			readonly ok: boolean;
			readonly url: string | null;
			readonly address: string | null;
			readonly proxy: string | null;
			readonly error: string | null;
	  }
	| {
			readonly type: 'httpResult';
			readonly requestId: number;
			readonly ok: boolean;
			readonly status: number;
			readonly body: string | null;
			readonly error: string | null;
	  }
	| {
			readonly type: 'tokenPersisted';
			readonly accountKey: string;
			readonly previousToken: string;
			readonly token: string;
			readonly outcome: EngineTokenPersistOutcome;
	  }
	| {readonly type: 'power'; readonly event: EnginePowerEvent}
	| {readonly type: 'shutdown'};

export type EngineTokenPersistOutcome = 'persisted' | 'rejected' | 'renderer';

export interface EngineSessionCounts {
	readonly sessions: number;
	readonly ready: number;
	readonly connecting: number;
	readonly backoff: number;
	readonly foreground: number;
	readonly authFailed: number;
}

export type EngineLogLevel = 'debug' | 'info' | 'warn' | 'error';

export type EngineToHostMessage =
	| {readonly type: 'ready'; readonly transport: 'native' | 'websocket'; readonly transportError: string | null}
	| {readonly type: 'admit'; readonly requestId: number; readonly accountKey: string; readonly url: string}
	| {
			readonly type: 'http';
			readonly requestId: number;
			readonly accountKey: string;
			readonly method: 'POST';
			readonly path: string;
			readonly body: string | null;
	  }
	| {readonly type: 'tokenRotated'; readonly accountKey: string; readonly previousToken: string; readonly token: string}
	| {readonly type: 'summaries'; readonly summaries: ReadonlyArray<AccountEngineAccountSummary>}
	| {readonly type: 'removed'; readonly accountKeys: ReadonlyArray<string>}
	| {
			readonly type: 'health';
			readonly rss: number;
			readonly heapUsed: number;
			readonly counts: EngineSessionCounts;
	  }
	| {readonly type: 'log'; readonly level: EngineLogLevel; readonly message: string; readonly detail: string | null}
	| {readonly type: 'shutdownComplete'};

const TOKEN_MAX_LENGTH = 4096;

function isBoundedString(value: unknown, max: number): value is string {
	return typeof value === 'string' && value.length > 0 && value.length <= max;
}

export function parseEngineAccountRecord(value: unknown): EngineAccountRecord | null {
	if (!isObjectLike(value)) {
		return null;
	}
	const record = value;
	if (
		!isBoundedString(record.accountKey, ACCOUNT_ENGINE_LIMITS.accountKeyMaxChars) ||
		!isBoundedString(record.userId, 64) ||
		!isBoundedString(record.instanceKey, ACCOUNT_ENGINE_LIMITS.accountKeyMaxChars) ||
		!isBoundedString(record.token, TOKEN_MAX_LENGTH) ||
		!isBoundedString(record.gatewayEndpoint, ACCOUNT_ENGINE_LIMITS.endpointMaxChars) ||
		!isBoundedString(record.apiEndpoint, ACCOUNT_ENGINE_LIMITS.endpointMaxChars) ||
		typeof record.isValid !== 'boolean' ||
		typeof record.lastActive !== 'number' ||
		!Number.isFinite(record.lastActive)
	) {
		return null;
	}
	return {
		accountKey: record.accountKey,
		userId: record.userId,
		instanceKey: record.instanceKey,
		token: record.token,
		gatewayEndpoint: record.gatewayEndpoint,
		apiEndpoint: record.apiEndpoint,
		isValid: record.isValid,
		lastActive: record.lastActive,
	};
}
