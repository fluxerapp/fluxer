// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DOMAIN_MIGRATION_SOURCE_TO_TARGET,
	DOMAIN_MIGRATION_TARGET_TO_SOURCE,
} from '@app/features/app/domain_migration/DomainMigrationCore';
import type {
	AuthenticationResponseJSON,
	PublicKeyCredentialCreationOptionsJSON,
	PublicKeyCredentialRequestOptionsJSON,
	RegistrationResponseJSON,
} from '@simplewebauthn/browser';

export const PASSKEY_BRIDGE_PATH = '/passkey-bridge';
export const PASSKEY_BRIDGE_VERSION = 1;
export const PASSKEY_BRIDGE_RP_ID = 'fluxer.app';
export const PASSKEY_BRIDGE_TIMEOUT_MS = 5 * 60 * 1000;
export const PASSKEY_BRIDGE_READY_TYPE = 'fluxer:passkey-bridge:ready';
export const PASSKEY_BRIDGE_REQUEST_TYPE = 'fluxer:passkey-bridge:request';
export const PASSKEY_BRIDGE_RESULT_TYPE = 'fluxer:passkey-bridge:result';

export type PasskeyBridgeKind = 'authenticate' | 'register';

export interface PasskeyBridgeOptionsMap {
	authenticate: PublicKeyCredentialRequestOptionsJSON;
	register: PublicKeyCredentialCreationOptionsJSON;
}

export interface PasskeyBridgeResponseMap {
	authenticate: AuthenticationResponseJSON;
	register: RegistrationResponseJSON;
}

export type PasskeyBridgeRequest = {
	[K in PasskeyBridgeKind]: {
		type: typeof PASSKEY_BRIDGE_REQUEST_TYPE;
		v: typeof PASSKEY_BRIDGE_VERSION;
		id: string;
		kind: K;
		options: PasskeyBridgeOptionsMap[K];
	};
}[PasskeyBridgeKind];

export interface PasskeyBridgeErrorPayload {
	name: string;
	message: string;
}

export type PasskeyBridgeResult =
	| {
			type: typeof PASSKEY_BRIDGE_RESULT_TYPE;
			v: typeof PASSKEY_BRIDGE_VERSION;
			id: string;
			ok: true;
			response: PasskeyBridgeResponseMap[PasskeyBridgeKind];
	  }
	| {
			type: typeof PASSKEY_BRIDGE_RESULT_TYPE;
			v: typeof PASSKEY_BRIDGE_VERSION;
			id: string;
			ok: false;
			error: PasskeyBridgeErrorPayload;
	  };

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function isNonEmptyString(value: unknown): value is string {
	return typeof value === 'string' && value.length > 0;
}

export function resolvePasskeyBridgeOpenerOrigin(origin: string, pathname: string): string | null {
	if (pathname !== PASSKEY_BRIDGE_PATH) {
		return null;
	}
	return DOMAIN_MIGRATION_SOURCE_TO_TARGET[origin] ?? null;
}

export function resolvePasskeyBridgeLegacyOrigin(origin: string): string | null {
	return DOMAIN_MIGRATION_TARGET_TO_SOURCE[origin] ?? null;
}

export function isPasskeyBridgeReady(data: unknown): boolean {
	return isRecord(data) && data.type === PASSKEY_BRIDGE_READY_TYPE && data.v === PASSKEY_BRIDGE_VERSION;
}

export function readPasskeyBridgeRequestId(data: unknown): string | null {
	return isRecord(data) && isNonEmptyString(data.id) ? data.id : null;
}

function readRequestRpId(kind: unknown, options: Record<string, unknown>): unknown {
	if (kind === 'authenticate') {
		return options.rpId;
	}
	if (kind === 'register') {
		return isRecord(options.rp) ? options.rp.id : undefined;
	}
	return undefined;
}

export function parsePasskeyBridgeRequest(data: unknown): PasskeyBridgeRequest | null {
	if (
		!isRecord(data) ||
		data.type !== PASSKEY_BRIDGE_REQUEST_TYPE ||
		data.v !== PASSKEY_BRIDGE_VERSION ||
		!isNonEmptyString(data.id) ||
		!isRecord(data.options) ||
		!isNonEmptyString(data.options.challenge)
	) {
		return null;
	}
	if (readRequestRpId(data.kind, data.options) !== PASSKEY_BRIDGE_RP_ID) {
		return null;
	}
	if (data.kind === 'register' && !isRecord(data.options.user)) {
		return null;
	}
	return data as unknown as PasskeyBridgeRequest;
}

export function parsePasskeyBridgeResult(data: unknown, id: string): PasskeyBridgeResult | null {
	if (
		!isRecord(data) ||
		data.type !== PASSKEY_BRIDGE_RESULT_TYPE ||
		data.v !== PASSKEY_BRIDGE_VERSION ||
		data.id !== id
	) {
		return null;
	}
	if (data.ok === true) {
		return isRecord(data.response) && isNonEmptyString(data.response.id) && isRecord(data.response.response)
			? (data as unknown as PasskeyBridgeResult)
			: null;
	}
	if (data.ok === false && isRecord(data.error)) {
		return {
			type: PASSKEY_BRIDGE_RESULT_TYPE,
			v: PASSKEY_BRIDGE_VERSION,
			id,
			ok: false,
			error: {
				name: typeof data.error.name === 'string' ? data.error.name : 'UnknownError',
				message: typeof data.error.message === 'string' ? data.error.message : '',
			},
		};
	}
	return null;
}

export function toPasskeyBridgeErrorPayload(error: unknown): PasskeyBridgeErrorPayload {
	if (error instanceof Error) {
		return {name: error.name, message: error.message};
	}
	return {name: 'UnknownError', message: String(error)};
}
