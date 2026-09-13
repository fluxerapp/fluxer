// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {throwForSvcErrorReply} from '@app/api/infrastructure/SvcErrorReply';
import {Logger} from '@app/api/Logger';
import {awaitAll} from '@app/api/utils/ConcurrencyUtils';
import {readOptionalIntegerEnv, requireIntegerInRange} from '@app/api/utils/IntegerOptions';
import {isJsonRecord, parseJsonRecord, parseJsonWithGuard} from '@app/api/utils/JsonBoundaryUtils';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import {NatsConnectionManager} from '@pkgs/nats/src/NatsConnectionManager';
import {StringCodec} from 'nats';

const USERS_SERVICE_SUBJECT = process.env.FLUXER_USERS_SERVICE_SUBJECT || 'svc.users';
const DEFAULT_USERS_SERVICE_TIMEOUT_MS = 6000;
const DEFAULT_USERS_SERVICE_INFLIGHT_MAX_ENTRIES = 10000;
const MAX_REQUEST_TIMEOUT_MS = 2_147_483_647;

export interface IUsersServiceClient {
	getUserPartialResponses(userIds: Array<UserID>): Promise<Map<UserID, UserPartialResponse>>;
	invalidateUserCache(userId: UserID): Promise<void>;
}

type PendingUserPartials = Promise<Map<UserID, UserPartialResponse>>;

type UsersServiceRequest =
	| {
			op: 'GetApiPartialsByIds';
			user_ids: Array<string>;
	  }
	| {
			op: 'Invalidate';
			user_id: string;
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

export class NatsUsersServiceClient implements IUsersServiceClient {
	private readonly codec = StringCodec();
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

	private async request<T>(payload: UsersServiceRequest, responseGuard: (value: unknown) => value is T): Promise<T> {
		try {
			if (this.connectionManager.isClosed()) {
				await this.connectionManager.connect();
			}
			const connection = this.connectionManager.getConnection();
			const response = await connection.request(this.subject, this.codec.encode(JSON.stringify(payload)), {
				timeout: this.requestTimeoutMs,
			});
			const decoded = this.codec.decode(response.data);
			const parsed = parseJsonWithGuard(decoded, responseGuard);
			if (parsed === null) {
				throwForSvcErrorReply('users-service', parseJsonRecord(decoded));
				throw new Error('[users-service] invalid response payload');
			}
			return parsed;
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
		name: process.env.FLUXER_USERS_SERVICE_NATS_CLIENT_NAME || 'fluxer-api-users',
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
		.sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
		.map(([, userId]) => userId);
}
