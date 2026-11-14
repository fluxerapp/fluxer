// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {requestSvcJson} from '@app/api/infrastructure/SvcErrorReply';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import {Logger} from '@app/api/Logger';
import {awaitAll} from '@app/api/utils/ConcurrencyUtils';
import {readOptionalEnv, readOptionalIntegerEnv, requireIntegerInRange} from '@app/api/utils/IntegerOptions';
import {isJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {compareText} from '@app/api/utils/StringComparison';
import {
	type AppAttestChallengePurpose,
	type AttestedAgeFloor,
	isAttestedAgeFloor,
} from '@fluxer/constants/src/AgeRangeConstants';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import {NatsConnectionManager} from '@pkgs/nats/src/NatsConnectionManager';

const USERS_SERVICE_SUBJECT = readOptionalEnv('FLUXER_USERS_SERVICE_SUBJECT') ?? 'svc.users';
const DEFAULT_USERS_SERVICE_TIMEOUT_MS = 6000;
const DEFAULT_USERS_SERVICE_INFLIGHT_MAX_ENTRIES = 10000;
const MAX_REQUEST_TIMEOUT_MS = 2_147_483_647;

export interface IUsersServiceClient {
	getUserPartialResponses(userIds: Array<UserID>): Promise<Map<UserID, UserPartialResponse>>;
	invalidateUserCache(userId: UserID): Promise<void>;
	appAttestCreateChallenge(
		userId: UserID,
		purpose: AppAttestChallengePurpose,
	): Promise<AppAttestOutcome<AppAttestChallengeIssued>>;
	appAttestRegisterKey(params: {
		userId: UserID;
		keyId: string;
		attestationBase64: string;
		challenge: string;
	}): Promise<AppAttestOutcome<true>>;
	appAttestSubmitAge(params: {
		userId: UserID;
		keyId: string;
		assertionBase64: string;
		clientDataBase64: string;
	}): Promise<AppAttestOutcome<AppAttestAgeAccepted>>;
	playIntegrityCreateChallenge(userId: UserID): Promise<PlayIntegrityOutcome<AppAttestChallengeIssued>>;
	playIntegritySubmitAge(params: {
		userId: UserID;
		packageName: string;
		integrityToken: string;
		clientDataBase64: string;
	}): Promise<PlayIntegrityOutcome<AppAttestAgeAccepted>>;
}

const APP_ATTEST_REJECTION_CODES = [
	'not_configured',
	'invalid_request',
	'invalid_challenge',
	'invalid_attestation',
	'key_in_use',
	'unknown_key',
	'invalid_assertion',
] as const;

export type AppAttestRejectionCode = (typeof APP_ATTEST_REJECTION_CODES)[number];

export type AppAttestOutcome<T> = {ok: true; value: T} | {ok: false; code: AppAttestRejectionCode};

const PLAY_INTEGRITY_REJECTION_CODES = [
	'not_configured',
	'invalid_request',
	'invalid_challenge',
	'invalid_token',
	'app_not_recognized',
	'device_not_trusted',
	'source_not_verified',
	'unavailable',
] as const;

export type PlayIntegrityRejectionCode = (typeof PLAY_INTEGRITY_REJECTION_CODES)[number];

export type PlayIntegrityOutcome<T> = {ok: true; value: T} | {ok: false; code: PlayIntegrityRejectionCode};

export interface AppAttestChallengeIssued {
	challenge: string;
	expires_at: string;
}

export interface AppAttestAgeAccepted {
	age_floor: AttestedAgeFloor;
	attested_at: string;
}

type PendingUserPartials = Promise<Map<UserID, UserPartialResponse>>;

type UsersServiceRequest =
	| {
			op: 'GetApiPartialsByIds';
			user_ids: Array<string>;
			instance_name: string;
	  }
	| {
			op: 'Invalidate';
			user_id: string;
	  }
	| {
			op: 'AppAttestCreateChallenge';
			user_id: string;
			purpose: AppAttestChallengePurpose;
	  }
	| {
			op: 'AppAttestRegisterKey';
			user_id: string;
			key_id: string;
			attestation_base64: string;
			challenge: string;
	  }
	| {
			op: 'AppAttestSubmitAge';
			user_id: string;
			key_id: string;
			assertion_base64: string;
			client_data_base64: string;
	  }
	| {
			op: 'PlayIntegrityCreateChallenge';
			user_id: string;
	  }
	| {
			op: 'PlayIntegritySubmitAge';
			user_id: string;
			package_name: string;
			integrity_token: string;
			client_data_base64: string;
	  };

interface UserPartialsResponse {
	FoundApiPartials: Array<UserPartialResponse>;
}

function isUserPartialResponse(value: unknown): value is UserPartialResponse {
	return isJsonRecord(value) && typeof value.id === 'string';
}

function isUserPartialsResponse(value: unknown): value is UserPartialsResponse {
	return (
		isJsonRecord(value) && Array.isArray(value.FoundApiPartials) && value.FoundApiPartials.every(isUserPartialResponse)
	);
}

function rejectionReader<C extends string>(variant: string, codes: ReadonlyArray<C>): (value: unknown) => C | null {
	return (value) => {
		if (!isJsonRecord(value)) return null;
		const rejected = value[variant];
		if (!isJsonRecord(rejected)) return null;
		const code = rejected.code;
		return codes.find((candidate) => candidate === code) ?? null;
	};
}

const appAttestRejection = rejectionReader('AppAttestRejected', APP_ATTEST_REJECTION_CODES);
const playIntegrityRejection = rejectionReader('PlayIntegrityRejected', PLAY_INTEGRITY_REJECTION_CODES);

function acceptChallenge(value: unknown): AppAttestChallengeIssued | null {
	if (!isJsonRecord(value) || !isJsonRecord(value.AppAttestChallenge)) return null;
	const {challenge, expires_at} = value.AppAttestChallenge;
	return typeof challenge === 'string' && typeof expires_at === 'string' ? {challenge, expires_at} : null;
}

function acceptKeyRegistered(value: unknown): true | null {
	return value === 'AppAttestKeyRegistered' ? true : null;
}

function acceptAge(value: unknown): AppAttestAgeAccepted | null {
	if (!isJsonRecord(value) || !isJsonRecord(value.AppAttestAgeAccepted)) return null;
	const {age_floor, attested_at} = value.AppAttestAgeAccepted;
	return isAttestedAgeFloor(age_floor) && typeof attested_at === 'string' ? {age_floor, attested_at} : null;
}

export class NatsUsersServiceClient implements IUsersServiceClient {
	private readonly inflightPartials = new Map<UserID, PendingUserPartials>();

	constructor(
		private readonly connectionManager: INatsConnectionManager,
		private readonly requestTimeoutMs = DEFAULT_USERS_SERVICE_TIMEOUT_MS,
		private readonly subject = USERS_SERVICE_SUBJECT,
		private readonly maxInflightEntries = DEFAULT_USERS_SERVICE_INFLIGHT_MAX_ENTRIES,
	) {
		requireIntegerInRange('FLUXER_USERS_SERVICE_TIMEOUT_MS', requestTimeoutMs, 1, MAX_REQUEST_TIMEOUT_MS);
		requireIntegerInRange('FLUXER_USERS_SERVICE_INFLIGHT_MAX_ENTRIES', maxInflightEntries, 0, Number.MAX_SAFE_INTEGER);
	}

	async getUserPartialResponses(userIds: Array<UserID>): Promise<Map<UserID, UserPartialResponse>> {
		const uniqueUserIds = uniqueSortedUserIds(userIds);
		if (uniqueUserIds.length === 0) {
			return new Map();
		}
		const result = new Map<UserID, UserPartialResponse>();
		const lookups = new Map<PendingUserPartials, Array<UserID>>();
		const misses: Array<UserID> = [];
		for (const userId of uniqueUserIds) {
			const batch = this.inflightPartials.get(userId);
			if (!batch) {
				misses.push(userId);
				continue;
			}
			const assignedUserIds = lookups.get(batch);
			if (assignedUserIds) assignedUserIds.push(userId);
			else lookups.set(batch, [userId]);
		}
		const capacity = Math.max(0, this.maxInflightEntries - this.inflightPartials.size);
		const coalesced = misses.slice(0, capacity);
		const direct = misses.slice(capacity);
		if (coalesced.length > 0) {
			lookups.set(this.fetchCoalescedPartials(coalesced), coalesced);
		}
		if (direct.length > 0) {
			lookups.set(this.fetchUserPartialResponses(direct), direct);
		}
		await awaitAll(
			Array.from(lookups, async ([batch, assignedUserIds]) => {
				const partials = await batch;
				for (const userId of assignedUserIds) {
					const partial = partials.get(userId);
					if (partial) result.set(userId, partial);
				}
			}),
			'[users-service] failed to fetch user partials',
		);
		return result;
	}

	private async fetchUserPartialResponses(userIds: Array<UserID>): Promise<Map<UserID, UserPartialResponse>> {
		const requestedUserIds = new Map(userIds.map((userId) => [userId.toString(), userId]));
		const response = await this.request(
			{
				op: 'GetApiPartialsByIds',
				user_ids: Array.from(requestedUserIds.keys()),
				instance_name: getInstanceProductName(),
			},
			isUserPartialsResponse,
		);
		const result = new Map<UserID, UserPartialResponse>();
		for (const partial of response.FoundApiPartials) {
			const userId = requestedUserIds.get(partial.id);
			if (userId === undefined) {
				throw new Error('[users-service] response contains an invalid or unrequested user ID');
			}
			if (result.has(userId)) {
				throw new Error('[users-service] response contains a duplicate user ID');
			}
			result.set(userId, partial);
		}
		return result;
	}

	private fetchCoalescedPartials(userIds: Array<UserID>): PendingUserPartials {
		const batch = this.fetchUserPartialResponses(userIds).finally(() => {
			for (const userId of userIds) {
				if (this.inflightPartials.get(userId) === batch) {
					this.inflightPartials.delete(userId);
				}
			}
		});
		for (const userId of userIds) {
			this.inflightPartials.set(userId, batch);
		}
		return batch;
	}

	async invalidateUserCache(userId: UserID): Promise<void> {
		this.inflightPartials.delete(userId);
		try {
			await this.request(
				{
					op: 'Invalidate',
					user_id: userId.toString(),
				},
				(value): value is 'Invalidated' => value === 'Invalidated',
			);
		} finally {
			this.inflightPartials.delete(userId);
		}
	}

	async appAttestCreateChallenge(
		userId: UserID,
		purpose: AppAttestChallengePurpose,
	): Promise<AppAttestOutcome<AppAttestChallengeIssued>> {
		return this.attestationRequest(
			{op: 'AppAttestCreateChallenge', user_id: userId.toString(), purpose},
			acceptChallenge,
			appAttestRejection,
		);
	}

	async appAttestRegisterKey(params: {
		userId: UserID;
		keyId: string;
		attestationBase64: string;
		challenge: string;
	}): Promise<AppAttestOutcome<true>> {
		return this.attestationRequest(
			{
				op: 'AppAttestRegisterKey',
				user_id: params.userId.toString(),
				key_id: params.keyId,
				attestation_base64: params.attestationBase64,
				challenge: params.challenge,
			},
			acceptKeyRegistered,
			appAttestRejection,
		);
	}

	async appAttestSubmitAge(params: {
		userId: UserID;
		keyId: string;
		assertionBase64: string;
		clientDataBase64: string;
	}): Promise<AppAttestOutcome<AppAttestAgeAccepted>> {
		return this.attestationRequest(
			{
				op: 'AppAttestSubmitAge',
				user_id: params.userId.toString(),
				key_id: params.keyId,
				assertion_base64: params.assertionBase64,
				client_data_base64: params.clientDataBase64,
			},
			acceptAge,
			appAttestRejection,
		);
	}

	async playIntegrityCreateChallenge(userId: UserID): Promise<PlayIntegrityOutcome<AppAttestChallengeIssued>> {
		return this.attestationRequest(
			{op: 'PlayIntegrityCreateChallenge', user_id: userId.toString()},
			acceptChallenge,
			playIntegrityRejection,
		);
	}

	async playIntegritySubmitAge(params: {
		userId: UserID;
		packageName: string;
		integrityToken: string;
		clientDataBase64: string;
	}): Promise<PlayIntegrityOutcome<AppAttestAgeAccepted>> {
		return this.attestationRequest(
			{
				op: 'PlayIntegritySubmitAge',
				user_id: params.userId.toString(),
				package_name: params.packageName,
				integrity_token: params.integrityToken,
				client_data_base64: params.clientDataBase64,
			},
			acceptAge,
			playIntegrityRejection,
		);
	}

	private async attestationRequest<T, C extends string>(
		payload: UsersServiceRequest,
		accept: (value: unknown) => T | null,
		reject: (value: unknown) => C | null,
	): Promise<{ok: true; value: T} | {ok: false; code: C}> {
		const reply = await this.request(
			payload,
			(value): value is unknown => reject(value) !== null || accept(value) !== null,
		);
		const code = reject(reply);
		if (code !== null) return {ok: false, code};
		const accepted = accept(reply);
		if (accepted === null) throw new Error('[users-service] invalid age range response payload');
		return {ok: true, value: accepted};
	}

	private async request<T>(payload: UsersServiceRequest, responseGuard: (value: unknown) => value is T): Promise<T> {
		try {
			return await requestSvcJson(this.connectionManager, this.subject, payload, {
				timeoutMs: this.requestTimeoutMs,
				guard: responseGuard,
				service: 'users-service',
				invalidPayloadError: () => new Error('[users-service] invalid response payload'),
			});
		} catch (error) {
			Logger.warn({error, op: payload.op}, '[users-service] request failed');
			throw error;
		}
	}
}

let usersServiceClient: IUsersServiceClient | undefined;
let injectedUsersServiceClient: IUsersServiceClient | undefined;

export function setInjectedUsersServiceClient(client: IUsersServiceClient | undefined): void {
	injectedUsersServiceClient = client;
	usersServiceClient = undefined;
}

export function createUsersServiceClient(): IUsersServiceClient {
	if (injectedUsersServiceClient !== undefined) {
		return injectedUsersServiceClient;
	}
	if (usersServiceClient !== undefined) {
		return usersServiceClient;
	}
	const manager = new NatsConnectionManager({
		url: Config.nats.coreUrl,
		token: Config.nats.authToken || undefined,
		name: readOptionalEnv('FLUXER_USERS_SERVICE_NATS_CLIENT_NAME') ?? 'fluxer-api-users',
	});
	usersServiceClient = new NatsUsersServiceClient(
		manager,
		readOptionalIntegerEnv('FLUXER_USERS_SERVICE_TIMEOUT_MS'),
		USERS_SERVICE_SUBJECT,
		readOptionalIntegerEnv('FLUXER_USERS_SERVICE_INFLIGHT_MAX_ENTRIES'),
	);
	void manager.connect().catch((error) => {
		Logger.warn({error}, '[users-service] Failed to establish NATS connection');
	});
	return usersServiceClient;
}

function uniqueSortedUserIds(userIds: Array<UserID>): Array<UserID> {
	const seen = new Map<string, UserID>();
	for (const userId of userIds) {
		seen.set(userId.toString(), userId);
	}
	return Array.from(seen.entries())
		.sort(([left], [right]) => compareText(left, right))
		.map(([, userId]) => userId);
}
