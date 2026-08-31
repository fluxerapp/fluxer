// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import {NatsConnectionManager} from '@pkgs/nats/src/NatsConnectionManager';
import {StringCodec} from 'nats';
import {createUserID, type UserID} from '../BrandedTypes';
import {Config} from '../Config';
import {Logger} from '../Logger';
import {isJsonRecord, parseJsonRecord, parseJsonWithGuard} from '../utils/JsonBoundaryUtils';
import {throwForSvcErrorReply} from './SvcErrorReply';

const USERS_SERVICE_SUBJECT = process.env.FLUXER_USERS_SERVICE_SUBJECT || 'svc.users';
const DEFAULT_USERS_SERVICE_TIMEOUT_MS = 6000;
const DEFAULT_USERS_SERVICE_INFLIGHT_MAX_ENTRIES = 10000;

export interface IUsersServiceClient {
	getUserPartialResponses(userIds: Array<UserID>): Promise<Map<UserID, UserPartialResponse>>;
	invalidateUserCache(userId: UserID): Promise<void>;
}

type UsersServiceResponse =
	| {
			FoundApiPartials: Array<UserPartialResponse>;
	  }
	| {
			FoundApiPartial: UserPartialResponse;
	  }
	| 'NotFound'
	| 'Invalidated';

function isUserPartialResponse(value: unknown): value is UserPartialResponse {
	return isJsonRecord(value) && typeof value.id === 'string';
}

function isUsersServiceResponse(value: unknown): value is UsersServiceResponse {
	if (value === 'NotFound' || value === 'Invalidated') return true;
	if (!isJsonRecord(value)) return false;
	if ('FoundApiPartials' in value) {
		return Array.isArray(value.FoundApiPartials) && value.FoundApiPartials.every(isUserPartialResponse);
	}
	if ('FoundApiPartial' in value) {
		return isUserPartialResponse(value.FoundApiPartial);
	}
	return false;
}

export class NatsUsersServiceClient implements IUsersServiceClient {
	private readonly codec = StringCodec();
	private readonly inflightPartials = new Map<string, Promise<UserPartialResponse | undefined>>();

	constructor(
		private readonly connectionManager: INatsConnectionManager,
		private readonly requestTimeoutMs = DEFAULT_USERS_SERVICE_TIMEOUT_MS,
		private readonly subject = USERS_SERVICE_SUBJECT,
		private readonly maxInflightEntries = DEFAULT_USERS_SERVICE_INFLIGHT_MAX_ENTRIES,
	) {}

	async getUserPartialResponses(userIds: Array<UserID>): Promise<Map<UserID, UserPartialResponse>> {
		const uniqueUserIds = uniqueSortedUserIds(userIds);
		if (uniqueUserIds.length === 0) {
			return new Map();
		}
		const result = new Map<UserID, UserPartialResponse>();
		const existing: Array<Promise<void>> = [];
		const misses: Array<UserID> = [];
		for (const userId of uniqueUserIds) {
			const key = userId.toString();
			const inflight = this.inflightPartials.get(key);
			if (inflight) {
				existing.push(this.copyInflightPartial(userId, inflight, result));
				continue;
			}
			misses.push(userId);
		}
		const newlyFetched = this.fetchAndCoalesceMissingPartials(misses, result);
		await Promise.all([...existing, newlyFetched]);
		return result;
	}

	private async fetchUserPartialResponses(userIds: Array<UserID>): Promise<Map<UserID, UserPartialResponse>> {
		const response = await this.request({
			op: 'GetApiPartialsByIds',
			user_ids: userIds.map((userId) => userId.toString()),
		});
		const partials =
			typeof response === 'object' && 'FoundApiPartials' in response
				? response.FoundApiPartials
				: typeof response === 'object' && 'FoundApiPartial' in response
					? [response.FoundApiPartial]
					: [];
		const result = new Map<UserID, UserPartialResponse>();
		for (const partial of partials) {
			try {
				const userId = createUserID(BigInt(partial.id));
				result.set(userId, partial);
			} catch (error) {
				Logger.warn({userId: partial.id, error}, '[users-service] invalid user partial id');
			}
		}
		return result;
	}

	private async fetchAndCoalesceMissingPartials(
		userIds: Array<UserID>,
		result: Map<UserID, UserPartialResponse>,
	): Promise<void> {
		if (userIds.length === 0) {
			return;
		}
		const capacity = Math.max(0, this.maxInflightEntries - this.inflightPartials.size);
		const coalesced = userIds.slice(0, capacity);
		const direct = userIds.slice(capacity);
		const tasks: Array<Promise<void>> = [];
		if (coalesced.length > 0) {
			const batch = this.fetchUserPartialResponses(coalesced);
			for (const userId of coalesced) {
				const key = userId.toString();
				const partial = batch
					.then((partials) => partials.get(userId))
					.finally(() => {
						this.inflightPartials.delete(key);
					});
				this.inflightPartials.set(key, partial);
				tasks.push(this.copyInflightPartial(userId, partial, result));
			}
		}
		if (direct.length > 0) {
			tasks.push(
				this.fetchUserPartialResponses(direct).then((partials) => {
					for (const [userId, partial] of partials) {
						result.set(userId, partial);
					}
				}),
			);
		}
		await Promise.all(tasks);
	}

	private async copyInflightPartial(
		userId: UserID,
		partialPromise: Promise<UserPartialResponse | undefined>,
		result: Map<UserID, UserPartialResponse>,
	): Promise<void> {
		const partial = await partialPromise;
		if (partial) {
			result.set(userId, partial);
		}
	}

	async invalidateUserCache(userId: UserID): Promise<void> {
		await this.request({
			op: 'Invalidate',
			user_id: userId.toString(),
		});
	}

	private async request(payload: Record<string, unknown>): Promise<UsersServiceResponse> {
		try {
			if (this.connectionManager.isClosed()) {
				await this.connectionManager.connect();
			}
			const connection = this.connectionManager.getConnection();
			const response = await connection.request(this.subject, this.codec.encode(JSON.stringify(payload)), {
				timeout: this.requestTimeoutMs,
			});
			const decoded = this.codec.decode(response.data);
			const parsed = parseJsonWithGuard(decoded, isUsersServiceResponse);
			if (!parsed) {
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
	void manager.connect().catch((error) => {
		Logger.warn({error}, '[users-service] Failed to establish NATS connection');
	});
	usersServiceClient = new NatsUsersServiceClient(
		manager,
		readPositiveIntegerEnv('FLUXER_USERS_SERVICE_TIMEOUT_MS'),
		USERS_SERVICE_SUBJECT,
		readPositiveIntegerEnv('FLUXER_USERS_SERVICE_INFLIGHT_MAX_ENTRIES', DEFAULT_USERS_SERVICE_INFLIGHT_MAX_ENTRIES),
	);
	return usersServiceClient;
}

function readPositiveIntegerEnv(name: string, fallback = DEFAULT_USERS_SERVICE_TIMEOUT_MS): number {
	const value = process.env[name];
	if (!value) {
		return fallback;
	}
	const parsed = Number(value);
	return Number.isInteger(parsed) && parsed > 0 ? parsed : fallback;
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
