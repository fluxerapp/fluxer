/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {ChannelID, GuildID, RoleID, UserID} from '@fluxer/api/src/BrandedTypes';
import {createChannelID, createRoleID, createUserID} from '@fluxer/api/src/BrandedTypes';
import type {GatewayDispatchEvent} from '@fluxer/api/src/constants/Gateway';
import {GatewayRpcClient} from '@fluxer/api/src/infrastructure/GatewayRpcClient';
import {GatewayRpcMethodError, GatewayRpcMethodErrorCodes} from '@fluxer/api/src/infrastructure/GatewayRpcError';
import type {CallData} from '@fluxer/api/src/infrastructure/IGatewayService';
import {Logger} from '@fluxer/api/src/Logger';
import {CallAlreadyExistsError} from '@fluxer/errors/src/domains/channel/CallAlreadyExistsError';
import {InvalidChannelTypeForCallError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeForCallError';
import {NoActiveCallError} from '@fluxer/errors/src/domains/channel/NoActiveCallError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {BadGatewayError} from '@fluxer/errors/src/domains/core/BadGatewayError';
import {GatewayTimeoutError} from '@fluxer/errors/src/domains/core/GatewayTimeoutError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {UserNotInVoiceError} from '@fluxer/errors/src/domains/user/UserNotInVoiceError';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {ms} from 'itty-time';

const GATEWAY_ERROR_TO_DOMAIN_ERROR: Record<string, () => Error> = {
	[GatewayRpcMethodErrorCodes.GUILD_NOT_FOUND]: () => new UnknownGuildError(),
	[GatewayRpcMethodErrorCodes.FORBIDDEN]: () => new MissingPermissionsError(),
	[GatewayRpcMethodErrorCodes.CHANNEL_NOT_FOUND]: () => new UnknownChannelError(),
	[GatewayRpcMethodErrorCodes.CHANNEL_NOT_VOICE]: () => new InvalidChannelTypeForCallError(),
	[GatewayRpcMethodErrorCodes.CALL_ALREADY_EXISTS]: () => new CallAlreadyExistsError(),
	[GatewayRpcMethodErrorCodes.CALL_NOT_FOUND]: () => new NoActiveCallError(),
	[GatewayRpcMethodErrorCodes.USER_NOT_IN_VOICE]: () => new UserNotInVoiceError(),
	[GatewayRpcMethodErrorCodes.CONNECTION_NOT_FOUND]: () => new UserNotInVoiceError(),
	[GatewayRpcMethodErrorCodes.MODERATOR_MISSING_CONNECT]: () => new MissingPermissionsError(),
	[GatewayRpcMethodErrorCodes.TARGET_MISSING_CONNECT]: () => new MissingPermissionsError(),
};

interface DispatchGuildParams {
	guildId: GuildID;
	event: GatewayDispatchEvent;
	data: unknown;
}

interface DispatchPresenceParams {
	userId: UserID;
	event: GatewayDispatchEvent;
	data: unknown;
}

interface InvalidatePushBadgeCountParams {
	userId: UserID;
}

interface GuildDataParams {
	guildId: GuildID;
	userId: UserID;
}

interface GuildMemberParams {
	guildId: GuildID;
	userId: UserID;
}

interface HasMemberParams {
	guildId: GuildID;
	userId: UserID;
}

interface GuildMemoryInfo {
	guild_id: string | null;
	guild_name: string;
	guild_icon: string | null;
	memory: string;
	member_count: number;
	session_count: number;
	presence_count: number;
}

interface UserPermissionsParams {
	guildId: GuildID;
	userId: UserID;
	channelId?: ChannelID;
}

interface CheckPermissionParams {
	guildId: GuildID;
	userId: UserID;
	permission: bigint;
	channelId?: ChannelID;
}

interface CanManageRolesParams {
	guildId: GuildID;
	userId: UserID;
	targetUserId: UserID;
	roleId: RoleID;
}

interface AssignableRolesParams {
	guildId: GuildID;
	userId: UserID;
}

interface MaxRolePositionParams {
	guildId: GuildID;
	userId: UserID;
}

interface MembersWithRoleParams {
	guildId: GuildID;
	roleId: RoleID;
}

interface CheckTargetMemberParams {
	guildId: GuildID;
	userId: UserID;
	targetUserId: UserID;
}

interface ViewableChannelsParams {
	guildId: GuildID;
	userId: UserID;
}

interface CategoryChannelCountParams {
	guildId: GuildID;
	categoryId: ChannelID;
}

interface ChannelCountParams {
	guildId: GuildID;
}

interface UsersToMentionByRolesParams {
	guildId: GuildID;
	channelId: ChannelID;
	roleIds: Array<RoleID>;
	authorId: UserID;
}

interface UsersToMentionByUserIdsParams {
	guildId: GuildID;
	channelId: ChannelID;
	userIds: Array<UserID>;
	authorId: UserID;
}

interface AllUsersToMentionParams {
	guildId: GuildID;
	channelId: ChannelID;
	authorId: UserID;
}

interface ResolveAllMentionsParams {
	guildId: GuildID;
	channelId: ChannelID;
	authorId: UserID;
	mentionEveryone: boolean;
	mentionHere: boolean;
	roleIds: Array<RoleID>;
	userIds: Array<UserID>;
}

interface JoinGuildParams {
	userId: UserID;
	guildId: GuildID;
}

interface LeaveGuildParams {
	userId: UserID;
	guildId: GuildID;
}

interface TerminateSessionParams {
	userId: UserID;
	sessionIdHashes: Array<string>;
}

interface TerminateAllSessionsParams {
	userId: UserID;
}

interface UpdateMemberVoiceParams {
	guildId: GuildID;
	userId: UserID;
	mute: boolean;
	deaf: boolean;
}

interface DisconnectVoiceUserParams {
	guildId: GuildID;
	userId: UserID;
	connectionId: string | null;
}

interface MoveMemberParams {
	guildId: GuildID;
	moderatorId: UserID;
	userId: UserID;
	channelId: ChannelID | null;
	connectionId: string | null;
}

interface GuildMemberRpcResponse {
	success: boolean;
	member_data?: GuildMemberResponse;
}

type PendingRequest<T> = {
	resolve: (value: T) => void;
	reject: (error: Error) => void;
};

export class GatewayService {
	private rpcClient: GatewayRpcClient;
	private pendingGuildDataRequests = new Map<string, Array<PendingRequest<GuildResponse>>>();
	private pendingGuildMemberRequests = new Map<
		string,
		Array<PendingRequest<{success: boolean; memberData?: GuildMemberResponse}>>
	>();
	private pendingPermissionRequests = new Map<string, Array<PendingRequest<boolean>>>();
	private pendingBatchRequestCount = 0;
	private batchTimeout: NodeJS.Timeout | null = null;
	private isBatchProcessing = false;
	private readonly BATCH_DELAY_MS = ms('5 milliseconds');
	private readonly MAX_PENDING_BATCH_REQUESTS = 2000;
	private readonly MAX_BATCH_CONCURRENCY = 50;

	private circuitBreakerConsecutiveFailures = 0;
	private circuitBreakerOpenUntilMs = 0;
	private readonly CIRCUIT_BREAKER_FAILURE_THRESHOLD = 5;
	private readonly CIRCUIT_BREAKER_COOLDOWN_MS = ms('10 seconds');

	constructor() {
		this.rpcClient = GatewayRpcClient.getInstance();
	}

	private isCircuitBreakerOpen(): boolean {
		if (this.circuitBreakerConsecutiveFailures < this.CIRCUIT_BREAKER_FAILURE_THRESHOLD) {
			return false;
		}
		if (Date.now() >= this.circuitBreakerOpenUntilMs) {
			this.circuitBreakerConsecutiveFailures = 0;
			this.circuitBreakerOpenUntilMs = 0;
			return false;
		}
		return true;
	}

	private recordCircuitBreakerSuccess(): void {
		this.circuitBreakerConsecutiveFailures = 0;
	}

	private recordCircuitBreakerFailure(): void {
		this.circuitBreakerConsecutiveFailures += 1;
		if (this.circuitBreakerConsecutiveFailures === this.CIRCUIT_BREAKER_FAILURE_THRESHOLD) {
			this.circuitBreakerOpenUntilMs = Date.now() + this.CIRCUIT_BREAKER_COOLDOWN_MS;
			Logger.warn(
				{
					consecutiveFailures: this.circuitBreakerConsecutiveFailures,
					cooldownMs: this.CIRCUIT_BREAKER_COOLDOWN_MS,
				},
				'[gateway] Circuit breaker opened due to consecutive failures',
			);
		}
	}

	private shouldRecordCircuitBreakerFailure(error: unknown): boolean {
		if (!(error instanceof GatewayRpcMethodError)) {
			return true;
		}
		if (GATEWAY_ERROR_TO_DOMAIN_ERROR[error.code] != null) {
			return false;
		}
		return true;
	}

	private async call<T>(method: string, params: Record<string, unknown>): Promise<T> {
		if (this.isCircuitBreakerOpen()) {
			throw new ServiceUnavailableError();
		}
		try {
			const result = await this.rpcClient.call<T>(method, params);
			this.recordCircuitBreakerSuccess();
			return result;
		} catch (error) {
			if (this.shouldRecordCircuitBreakerFailure(error)) {
				this.recordCircuitBreakerFailure();
			} else {
				this.recordCircuitBreakerSuccess();
			}
			throw this.transformGatewayError(error);
		}
	}

	private transformGatewayError(error: unknown): Error {
		if (error instanceof GatewayRpcMethodError) {
			const createError = GATEWAY_ERROR_TO_DOMAIN_ERROR[error.code];
			if (createError) {
				return createError();
			}
			if (error.code === GatewayRpcMethodErrorCodes.TIMEOUT) {
				return new GatewayTimeoutError();
			}
			if (error.code === GatewayRpcMethodErrorCodes.OVERLOADED) {
				return new ServiceUnavailableError();
			}
			if (error.code === GatewayRpcMethodErrorCodes.NO_RESPONDERS) {
				return new ServiceUnavailableError();
			}
			if (error.code === GatewayRpcMethodErrorCodes.INTERNAL_ERROR) {
				return new BadGatewayError();
			}
			return new BadGatewayError();
		}

		if (error instanceof Error && error.name === 'TimeoutError') {
			return new GatewayTimeoutError();
		}

		return error instanceof Error ? error : new Error(String(error));
	}

	private logBatchFailures(method: string, failures: Array<PromiseRejectedResult>): void {
		if (failures.length === 0) {
			return;
		}

		const reasonCounts = new Map<string, number>();
		for (const failure of failures) {
			const reason = failure.reason instanceof Error ? failure.reason.message : String(failure.reason);
			reasonCounts.set(reason, (reasonCounts.get(reason) ?? 0) + 1);
		}

		const reasonSummary = Array.from(reasonCounts.entries())
			.sort(([, a], [, b]) => b - a)
			.slice(0, 3)
			.map(([reason, count]) => ({reason, count}));

		Logger.error(
			{method, failureCount: failures.length, reasonSummary},
			`[gateway-batch] ${method} batch operation failed`,
		);
	}

	private scheduleBatch(): void {
		if (this.batchTimeout || this.isBatchProcessing) {
			return;
		}

		this.batchTimeout = setTimeout(() => {
			this.batchTimeout = null;
			void this.processBatch();
		}, this.BATCH_DELAY_MS);
	}

	private async processBatch(): Promise<void> {
		if (this.isBatchProcessing) {
			return;
		}

		this.isBatchProcessing = true;
		try {
			while (true) {
				const guildDataRequests = new Map(this.pendingGuildDataRequests);
				const guildMemberRequests = new Map(this.pendingGuildMemberRequests);
				const permissionRequests = new Map(this.pendingPermissionRequests);

				const totalGuildDataRequests = Array.from(guildDataRequests.values()).reduce(
					(sum, pending) => sum + pending.length,
					0,
				);
				const totalGuildMemberRequests = Array.from(guildMemberRequests.values()).reduce(
					(sum, pending) => sum + pending.length,
					0,
				);
				const totalPermissionRequests = Array.from(permissionRequests.values()).reduce(
					(sum, pending) => sum + pending.length,
					0,
				);
				this.pendingBatchRequestCount = 0;

				if (totalGuildDataRequests === 0 && totalGuildMemberRequests === 0 && totalPermissionRequests === 0) {
					return;
				}

				if (totalGuildDataRequests > 0 || totalGuildMemberRequests > 0 || totalPermissionRequests > 0) {
					Logger.debug(
						`[gateway-batch] Processing batch: ${guildDataRequests.size} unique guild.get_data requests (${totalGuildDataRequests} total), ${guildMemberRequests.size} unique guild.get_member requests (${totalGuildMemberRequests} total), ${permissionRequests.size} unique guild.check_permission requests (${totalPermissionRequests} total)`,
					);
				}

				this.pendingGuildDataRequests.clear();
				this.pendingGuildMemberRequests.clear();
				this.pendingPermissionRequests.clear();

				if (guildDataRequests.size > 0) {
					await this.processGuildDataBatch(guildDataRequests);
				}

				if (guildMemberRequests.size > 0) {
					await this.processGuildMemberBatch(guildMemberRequests);
				}

				if (permissionRequests.size > 0) {
					await this.processPermissionBatch(permissionRequests);
				}
			}
		} finally {
			this.isBatchProcessing = false;

			if (
				this.pendingGuildDataRequests.size > 0 ||
				this.pendingGuildMemberRequests.size > 0 ||
				this.pendingPermissionRequests.size > 0
			) {
				this.scheduleBatch();
			}
		}
	}

	private rejectAllPendingBatchRequests(error: Error): void {
		for (const pendingRequests of this.pendingGuildDataRequests.values()) {
			for (const {reject} of pendingRequests) {
				reject(error);
			}
		}
		for (const pendingRequests of this.pendingGuildMemberRequests.values()) {
			for (const {reject} of pendingRequests) {
				reject(error);
			}
		}
		for (const pendingRequests of this.pendingPermissionRequests.values()) {
			for (const {reject} of pendingRequests) {
				reject(error);
			}
		}
		this.pendingGuildDataRequests.clear();
		this.pendingGuildMemberRequests.clear();
		this.pendingPermissionRequests.clear();
		this.pendingBatchRequestCount = 0;
	}

	private async processInChunks<T>(
		items: Array<T>,
		handler: (item: T) => Promise<void>,
		method: string,
	): Promise<void> {
		const allFailures: Array<PromiseRejectedResult> = [];

		for (let i = 0; i < items.length; i += this.MAX_BATCH_CONCURRENCY) {
			const chunk = items.slice(i, i + this.MAX_BATCH_CONCURRENCY);
			const results = await Promise.allSettled(chunk.map(handler));
			const failures = results.filter((r): r is PromiseRejectedResult => r.status === 'rejected');
			allFailures.push(...failures);
		}

		this.logBatchFailures(method, allFailures);
	}

	private async processGuildDataBatch(requests: Map<string, Array<PendingRequest<GuildResponse>>>): Promise<void> {
		const entries = Array.from(requests.entries());

		await this.processInChunks(
			entries,
			async ([key, pending]) => {
				try {
					const [guildIdStr, userIdStr, skipCheck] = key.split('-');
					const guildId = BigInt(guildIdStr) as GuildID;
					const userId = BigInt(userIdStr) as UserID;
					const skipMembershipCheck = skipCheck === 'skip';

					const guildResponse = await this.call<GuildResponse>('guild.get_data', {
						guild_id: guildId.toString(),
						user_id: skipMembershipCheck ? null : userId.toString(),
					});
					pending.forEach(({resolve}) => resolve(guildResponse));
				} catch (error) {
					const transformedError = this.transformGatewayError(error);
					pending.forEach(({reject}) => reject(transformedError));
					throw error;
				}
			},
			'guild.get_data',
		);
	}

	private async processGuildMemberBatch(
		requests: Map<string, Array<PendingRequest<{success: boolean; memberData?: GuildMemberResponse}>>>,
	): Promise<void> {
		const entries = Array.from(requests.entries());

		await this.processInChunks(
			entries,
			async ([key, pending]) => {
				try {
					const [guildIdStr, userIdStr] = key.split('-');
					const guildId = BigInt(guildIdStr) as GuildID;
					const userId = BigInt(userIdStr) as UserID;

					const rpcResult = await this.call<GuildMemberRpcResponse | null>('guild.get_member', {
						guild_id: guildId.toString(),
						user_id: userId.toString(),
					});

					if (rpcResult?.success && rpcResult.member_data) {
						const result = {success: true, memberData: rpcResult.member_data};
						pending.forEach(({resolve}) => resolve(result));
					} else {
						pending.forEach(({resolve}) => resolve({success: false}));
					}
				} catch (error) {
					const transformedError = this.transformGatewayError(error);
					pending.forEach(({reject}) => reject(transformedError));
					throw error;
				}
			},
			'guild.get_member',
		);
	}

	private async processPermissionBatch(requests: Map<string, Array<PendingRequest<boolean>>>): Promise<void> {
		const entries = Array.from(requests.entries());

		await this.processInChunks(
			entries,
			async ([key, pending]) => {
				try {
					const [guildIdStr, userIdStr, permissionStr, channelIdStr] = key.split('-');
					const guildId = BigInt(guildIdStr) as GuildID;
					const userId = BigInt(userIdStr) as UserID;
					const permission = BigInt(permissionStr);
					const channelId = channelIdStr !== '0' ? (BigInt(channelIdStr) as ChannelID) : undefined;

					const result = await this.call<{has_permission: boolean}>('guild.check_permission', {
						guild_id: guildId.toString(),
						user_id: userId.toString(),
						permission: permission.toString(),
						channel_id: channelId ? channelId.toString() : '0',
					});

					pending.forEach(({resolve}) => resolve(result.has_permission));
				} catch (error) {
					const transformedError = this.transformGatewayError(error);
					pending.forEach(({reject}) => reject(transformedError));
					throw error;
				}
			},
			'guild.check_permission',
		);
	}

	async dispatchGuild({guildId, event, data}: DispatchGuildParams): Promise<void> {
		await this.call('guild.dispatch', {
			guild_id: guildId.toString(),
			event,
			data,
		});
	}

	async dispatchPresence({userId, event, data}: DispatchPresenceParams): Promise<void> {
		await this.call('presence.dispatch', {
			user_id: userId.toString(),
			event,
			data,
		});
	}

	async invalidatePushBadgeCount({userId}: InvalidatePushBadgeCountParams): Promise<void> {
		await this.call('push.invalidate_badge_count', {
			user_id: userId.toString(),
		});
	}

	async getGuildCounts(guildId: GuildID): Promise<{memberCount: number; presenceCount: number}> {
		const result = await this.call<{member_count: number; presence_count: number}>('guild.get_counts', {
			guild_id: guildId.toString(),
		});
		return {
			memberCount: result.member_count,
			presenceCount: result.presence_count,
		};
	}

	async getChannelCount({guildId}: ChannelCountParams): Promise<number> {
		const result = await this.call<{count: number}>('guild.get_channel_count', {
			guild_id: guildId.toString(),
		});
		return result.count;
	}

	async getCategoryChannelCount({guildId, categoryId}: CategoryChannelCountParams): Promise<number> {
		const result = await this.call<{count: number}>('guild.get_category_channel_count', {
			guild_id: guildId.toString(),
			category_id: categoryId.toString(),
		});
		return result.count;
	}

	async getGuildData({
		guildId,
		userId,
		skipMembershipCheck,
	}: GuildDataParams & {skipMembershipCheck?: boolean}): Promise<GuildResponse> {
		const key = `${guildId.toString()}-${userId.toString()}-${skipMembershipCheck ? 'skip' : 'check'}`;

		return new Promise<GuildResponse>((resolve, reject) => {
			if (this.pendingBatchRequestCount >= this.MAX_PENDING_BATCH_REQUESTS) {
				reject(new ServiceUnavailableError());
				return;
			}

			const pending = this.pendingGuildDataRequests.get(key) || [];
			pending.push({resolve, reject});
			this.pendingGuildDataRequests.set(key, pending);
			this.pendingBatchRequestCount += 1;

			Logger.debug(
				`[gateway-batch] Queued guild.get_data request for guild ${guildId.toString()}, user ${userId.toString()}, total pending: ${pending.length}`,
			);

			this.scheduleBatch();
		});
	}

	async getGuildMember({
		guildId,
		userId,
	}: GuildMemberParams): Promise<{success: boolean; memberData?: GuildMemberResponse}> {
		const key = `${guildId.toString()}-${userId.toString()}`;

		return new Promise<{success: boolean; memberData?: GuildMemberResponse}>((resolve, reject) => {
			if (this.pendingBatchRequestCount >= this.MAX_PENDING_BATCH_REQUESTS) {
				reject(new ServiceUnavailableError());
				return;
			}

			const pending = this.pendingGuildMemberRequests.get(key) || [];
			pending.push({resolve, reject});
			this.pendingGuildMemberRequests.set(key, pending);
			this.pendingBatchRequestCount += 1;

			Logger.debug(
				`[gateway-batch] Queued guild.get_member request for guild ${guildId.toString()}, user ${userId.toString()}, total pending: ${pending.length}`,
			);

			this.scheduleBatch();
		});
	}

	async hasGuildMember({guildId, userId}: HasMemberParams): Promise<boolean> {
		const result = await this.call<{has_member: boolean}>('guild.has_member', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
		});
		return result.has_member;
	}

	async listGuildMembers({
		guildId,
		limit,
		offset,
	}: {
		guildId: GuildID;
		limit: number;
		offset: number;
	}): Promise<{members: Array<GuildMemberResponse>; total: number}> {
		const result = await this.call<{members?: Array<GuildMemberResponse>; total?: number}>('guild.list_members', {
			guild_id: guildId.toString(),
			limit,
			offset,
		});
		return {
			members: result.members ?? [],
			total: result.total ?? 0,
		};
	}

	async listGuildMembersCursor({
		guildId,
		limit,
		after,
	}: {
		guildId: GuildID;
		limit: number;
		after?: UserID;
	}): Promise<{members: Array<GuildMemberResponse>; total: number}> {
		const result = await this.call<{members?: Array<GuildMemberResponse>; total?: number}>(
			'guild.list_members_cursor',
			{
				guild_id: guildId.toString(),
				limit,
				...(after !== undefined && {after: after.toString()}),
			},
		);
		return {
			members: result.members ?? [],
			total: result.total ?? 0,
		};
	}

	async startGuild(guildId: GuildID): Promise<void> {
		await this.call('guild.start', {
			guild_id: guildId.toString(),
		});
	}

	async stopGuild(guildId: GuildID): Promise<void> {
		await this.call('guild.stop', {
			guild_id: guildId.toString(),
		});
	}

	async reloadGuild(guildId: GuildID): Promise<void> {
		await this.call('guild.reload', {
			guild_id: guildId.toString(),
		});
	}

	async reloadAllGuilds(guildIds: Array<GuildID>): Promise<{count: number}> {
		const result = await this.call<{count: number}>('guild.reload_all', {
			guild_ids: guildIds.map((id) => id.toString()),
		});
		return {count: result.count};
	}

	async shutdownGuild(guildId: GuildID): Promise<void> {
		await this.call('guild.shutdown', {
			guild_id: guildId.toString(),
		});
	}

	async getGuildMemoryStats(limit: number): Promise<{guilds: Array<GuildMemoryInfo>}> {
		const result = await this.call<{guilds: Array<GuildMemoryInfo>}>('process.memory_stats', {
			limit: limit.toString(),
		});
		return {
			guilds: result.guilds,
		};
	}

	async getUserPermissions({guildId, userId, channelId}: UserPermissionsParams): Promise<bigint> {
		const result = await this.call<{permissions: string}>('guild.get_user_permissions', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
			channel_id: channelId ? channelId.toString() : '0',
		});
		return BigInt(result.permissions);
	}

	async getUserPermissionsBatch({
		guildIds,
		userId,
		channelId,
	}: {
		guildIds: Array<GuildID>;
		userId: UserID;
		channelId?: ChannelID;
	}): Promise<Map<GuildID, bigint>> {
		const permissionsMap = new Map<GuildID, bigint>();
		if (guildIds.length === 0) {
			return permissionsMap;
		}

		const result = await this.call<{permissions: Array<{guild_id: string; permissions: string}>}>(
			'guild.get_user_permissions_batch',
			{
				guild_ids: guildIds.map((id) => id.toString()),
				user_id: userId.toString(),
				channel_id: channelId ? channelId.toString() : '0',
			},
		);

		for (const item of result.permissions) {
			const guildId = BigInt(item.guild_id) as GuildID;
			permissionsMap.set(guildId, BigInt(item.permissions));
		}

		return permissionsMap;
	}

	async checkPermission({guildId, userId, permission, channelId}: CheckPermissionParams): Promise<boolean> {
		const key = `${guildId.toString()}-${userId.toString()}-${permission.toString()}-${channelId?.toString() || '0'}`;

		return new Promise<boolean>((resolve, reject) => {
			if (this.pendingBatchRequestCount >= this.MAX_PENDING_BATCH_REQUESTS) {
				reject(new ServiceUnavailableError());
				return;
			}

			const pending = this.pendingPermissionRequests.get(key) || [];
			pending.push({resolve, reject});
			this.pendingPermissionRequests.set(key, pending);
			this.pendingBatchRequestCount += 1;

			Logger.debug(
				`[gateway-batch] Queued guild.check_permission request for guild ${guildId.toString()}, user ${userId.toString()}, channel ${channelId?.toString() || 'none'}, permission ${permission.toString()}, total pending: ${pending.length}`,
			);

			this.scheduleBatch();
		});
	}

	async canManageRoles({guildId, userId, targetUserId, roleId}: CanManageRolesParams): Promise<boolean> {
		const result = await this.call<{can_manage: boolean}>('guild.can_manage_roles', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
			target_user_id: targetUserId.toString(),
			role_id: roleId.toString(),
		});
		return result.can_manage;
	}

	async canManageRole({guildId, userId, roleId}: {guildId: GuildID; userId: UserID; roleId: RoleID}): Promise<boolean> {
		const result = await this.call<{can_manage: boolean}>('guild.can_manage_role', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
			role_id: roleId.toString(),
		});
		return result.can_manage;
	}

	async getAssignableRoles({guildId, userId}: AssignableRolesParams): Promise<Array<RoleID>> {
		const result = await this.call<{role_ids: Array<string>}>('guild.get_assignable_roles', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
		});
		return result.role_ids.map((id: string) => createRoleID(BigInt(id)));
	}

	async getUserMaxRolePosition({guildId, userId}: MaxRolePositionParams): Promise<number> {
		const result = await this.call<{position: number}>('guild.get_user_max_role_position', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
		});
		return result.position;
	}

	async getMembersWithRole({guildId, roleId}: MembersWithRoleParams): Promise<Array<UserID>> {
		const result = await this.call<{user_ids: Array<string>}>('guild.get_members_with_role', {
			guild_id: guildId.toString(),
			role_id: roleId.toString(),
		});
		return result.user_ids.map((id: string) => createUserID(BigInt(id)));
	}

	async checkTargetMember({guildId, userId, targetUserId}: CheckTargetMemberParams): Promise<boolean> {
		const result = await this.call<{can_manage: boolean}>('guild.check_target_member', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
			target_user_id: targetUserId.toString(),
		});
		return result.can_manage;
	}

	async getViewableChannels({guildId, userId}: ViewableChannelsParams): Promise<Array<ChannelID>> {
		const result = await this.call<{channel_ids: Array<string>}>('guild.get_viewable_channels', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
		});
		return result.channel_ids.map((id: string) => createChannelID(BigInt(id)));
	}

	async getUsersToMentionByRoles({
		guildId,
		channelId,
		roleIds,
		authorId,
	}: UsersToMentionByRolesParams): Promise<Array<UserID>> {
		const result = await this.call<{user_ids: Array<string>}>('guild.get_users_to_mention_by_roles', {
			guild_id: guildId.toString(),
			channel_id: channelId.toString(),
			role_ids: roleIds.map((id) => id.toString()),
			author_id: authorId.toString(),
		});
		return result.user_ids.map((id: string) => createUserID(BigInt(id)));
	}

	async getUsersToMentionByUserIds({
		guildId,
		channelId,
		userIds,
		authorId,
	}: UsersToMentionByUserIdsParams): Promise<Array<UserID>> {
		const result = await this.call<{user_ids: Array<string>}>('guild.get_users_to_mention_by_user_ids', {
			guild_id: guildId.toString(),
			channel_id: channelId.toString(),
			user_ids: userIds.map((id) => id.toString()),
			author_id: authorId.toString(),
		});
		return result.user_ids.map((id: string) => createUserID(BigInt(id)));
	}

	async getAllUsersToMention({guildId, channelId, authorId}: AllUsersToMentionParams): Promise<Array<UserID>> {
		const result = await this.call<{user_ids: Array<string>}>('guild.get_all_users_to_mention', {
			guild_id: guildId.toString(),
			channel_id: channelId.toString(),
			author_id: authorId.toString(),
		});
		return result.user_ids.map((id: string) => createUserID(BigInt(id)));
	}

	async resolveAllMentions({
		guildId,
		channelId,
		authorId,
		mentionEveryone,
		mentionHere,
		roleIds,
		userIds,
	}: ResolveAllMentionsParams): Promise<Array<UserID>> {
		const result = await this.call<{user_ids: Array<string>}>('guild.resolve_all_mentions', {
			guild_id: guildId.toString(),
			channel_id: channelId.toString(),
			author_id: authorId.toString(),
			mention_everyone: mentionEveryone,
			mention_here: mentionHere,
			role_ids: roleIds.map((id) => id.toString()),
			user_ids: userIds.map((id) => id.toString()),
		});
		return result.user_ids.map((id: string) => createUserID(BigInt(id)));
	}

	async getVanityUrlChannel(guildId: GuildID): Promise<ChannelID | null> {
		const result = await this.call<{channel_id: string | null}>('guild.get_vanity_url_channel', {
			guild_id: guildId.toString(),
		});
		return result.channel_id ? createChannelID(BigInt(result.channel_id)) : null;
	}

	async getFirstViewableTextChannel(guildId: GuildID): Promise<ChannelID | null> {
		const result = await this.call<{channel_id: string | null}>('guild.get_first_viewable_text_channel', {
			guild_id: guildId.toString(),
		});
		return result.channel_id ? createChannelID(BigInt(result.channel_id)) : null;
	}

	async joinGuild({userId, guildId}: JoinGuildParams): Promise<void> {
		await this.call('presence.join_guild', {
			user_id: userId.toString(),
			guild_id: guildId.toString(),
		});
	}

	async leaveGuild({userId, guildId}: LeaveGuildParams): Promise<void> {
		await this.call('presence.leave_guild', {
			user_id: userId.toString(),
			guild_id: guildId.toString(),
		});
	}

	async terminateSession({userId, sessionIdHashes}: TerminateSessionParams): Promise<void> {
		await this.call('presence.terminate_sessions', {
			user_id: userId.toString(),
			session_id_hashes: sessionIdHashes,
		});
	}

	async terminateAllSessionsForUser({userId}: TerminateAllSessionsParams): Promise<void> {
		await this.call('presence.terminate_all_sessions', {
			user_id: userId.toString(),
		});
	}

	async updateMemberVoice({guildId, userId, mute, deaf}: UpdateMemberVoiceParams): Promise<{success: boolean}> {
		const result = await this.call<{success: boolean}>('guild.update_member_voice', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
			mute,
			deaf,
		});
		return {success: result.success};
	}

	async disconnectVoiceUser({guildId, userId, connectionId}: DisconnectVoiceUserParams): Promise<void> {
		await this.call('guild.disconnect_voice_user', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
			connection_id: connectionId,
		});
	}

	async disconnectVoiceUserIfInChannel({
		guildId,
		userId,
		channelId,
		connectionId,
	}: {
		guildId?: GuildID;
		channelId: ChannelID;
		userId: UserID;
		connectionId?: string;
	}): Promise<{success: boolean; ignored?: boolean}> {
		const params: Record<string, string> = {
			channel_id: channelId.toString(),
			user_id: userId.toString(),
		};
		if (guildId !== undefined) {
			params['guild_id'] = guildId.toString();
		}
		if (connectionId) {
			params['connection_id'] = connectionId;
		}
		const result = await this.call<{success: boolean; ignored?: boolean}>(
			'voice.disconnect_user_if_in_channel',
			params,
		);
		return {
			success: result.success,
			ignored: result.ignored,
		};
	}

	async getVoiceState({
		guildId,
		userId,
	}: {
		guildId: GuildID;
		userId: UserID;
	}): Promise<{channel_id: string | null} | null> {
		const result = await this.call<{voice_state: {channel_id: string | null} | null}>('guild.get_voice_state', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
		});
		return result.voice_state;
	}

	async moveMember({guildId, moderatorId, userId, channelId, connectionId}: MoveMemberParams): Promise<{
		success?: boolean;
		error?: string;
	}> {
		const result = await this.call<{success?: boolean; error?: string}>('guild.move_member', {
			guild_id: guildId.toString(),
			moderator_id: moderatorId.toString(),
			user_id: userId.toString(),
			channel_id: channelId ? channelId.toString() : null,
			connection_id: connectionId,
		});
		return result;
	}

	async hasActivePresence(userId: UserID): Promise<boolean> {
		const result = await this.call<{has_active: boolean}>('presence.has_active', {
			user_id: userId.toString(),
		});
		return result.has_active;
	}

	async addTemporaryGuild({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<void> {
		await this.call('presence.add_temporary_guild', {
			user_id: userId.toString(),
			guild_id: guildId.toString(),
		});
	}

	async removeTemporaryGuild({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<void> {
		try {
			await this.call('presence.remove_temporary_guild', {
				user_id: userId.toString(),
				guild_id: guildId.toString(),
			});
		} catch (error) {
			const errorMessage = error instanceof Error ? error.message : String(error);
			Logger.error(
				`[GatewayService] Failed to remove temporary guild for user ${userId.toString()}, guild ${guildId.toString()}: ${errorMessage}`,
			);
		}
	}

	async syncGroupDmRecipients({
		userId,
		recipientsByChannel,
	}: {
		userId: UserID;
		recipientsByChannel: Record<string, Array<string>>;
	}): Promise<void> {
		try {
			await this.call('presence.sync_group_dm_recipients', {
				user_id: userId.toString(),
				recipients_by_channel: recipientsByChannel,
			});
		} catch (error) {
			const errorMessage = error instanceof Error ? error.message : String(error);
			const channelCount = Object.keys(recipientsByChannel).length;
			Logger.error(
				`[GatewayService] Failed to sync group DM recipients for user ${userId.toString()} (${channelCount} channels): ${errorMessage}`,
			);
		}
	}

	async switchVoiceRegion({guildId, channelId}: {guildId: GuildID; channelId: ChannelID}): Promise<void> {
		await this.call('guild.switch_voice_region', {
			guild_id: guildId.toString(),
			channel_id: channelId.toString(),
		});
	}

	async disconnectAllVoiceUsersInChannel({
		guildId,
		channelId,
	}: {
		guildId: GuildID;
		channelId: ChannelID;
	}): Promise<{success: boolean; disconnectedCount: number}> {
		const result = await this.call<{success: boolean; disconnected_count: number}>(
			'guild.disconnect_all_voice_users_in_channel',
			{
				guild_id: guildId.toString(),
				channel_id: channelId.toString(),
			},
		);
		return {
			success: result.success,
			disconnectedCount: result.disconnected_count,
		};
	}

	async confirmVoiceConnection({
		guildId,
		channelId,
		connectionId,
		tokenNonce,
	}: {
		guildId?: GuildID;
		channelId: ChannelID;
		connectionId: string;
		tokenNonce: string;
	}): Promise<{success: boolean; error?: string}> {
		const params: Record<string, string> = {
			channel_id: channelId.toString(),
			connection_id: connectionId,
			token_nonce: tokenNonce,
		};
		if (guildId !== undefined) {
			params['guild_id'] = guildId.toString();
		}
		const result = await this.call<{success: boolean; error?: string}>('voice.confirm_connection', params);
		return {
			success: result.success,
			error: result.error,
		};
	}

	async getVoiceStatesForChannel({
		guildId,
		channelId,
	}: {
		guildId?: GuildID;
		channelId: ChannelID;
	}): Promise<{voiceStates: Array<{connectionId: string; userId: string; channelId: string}>}> {
		const params: Record<string, string> = {channel_id: channelId.toString()};
		if (guildId !== undefined) {
			params['guild_id'] = guildId.toString();
		}
		const result = await this.call<{voice_states: Array<{connection_id: string; user_id: string; channel_id: string}>}>(
			'voice.get_voice_states_for_channel',
			params,
		);
		return {
			voiceStates: (result.voice_states ?? []).map((vs) => ({
				connectionId: vs.connection_id,
				userId: vs.user_id,
				channelId: vs.channel_id,
			})),
		};
	}

	async getPendingJoinsForChannel({
		guildId,
		channelId,
	}: {
		guildId?: GuildID;
		channelId: ChannelID;
	}): Promise<{pendingJoins: Array<{connectionId: string; userId: string; tokenNonce: string; expiresAt: number}>}> {
		const params: Record<string, string> = {channel_id: channelId.toString()};
		if (guildId !== undefined) {
			params['guild_id'] = guildId.toString();
		}
		const result = await this.call<{
			pending_joins: Array<{connection_id: string; user_id: string; token_nonce: string; expires_at: number}>;
		}>('voice.get_pending_joins_for_channel', params);
		return {
			pendingJoins: (result.pending_joins ?? []).map((pj) => ({
				connectionId: pj.connection_id,
				userId: pj.user_id,
				tokenNonce: pj.token_nonce,
				expiresAt: pj.expires_at,
			})),
		};
	}

	async getCall(channelId: ChannelID): Promise<CallData | null> {
		return this.call<CallData | null>('call.get', {channel_id: channelId.toString()});
	}

	async createCall(
		channelId: ChannelID,
		messageId: string,
		region: string,
		ringing: Array<string>,
		recipients: Array<string>,
	): Promise<CallData> {
		return this.call<CallData>('call.create', {
			channel_id: channelId.toString(),
			message_id: messageId,
			region,
			ringing,
			recipients,
		});
	}

	async updateCallRegion(channelId: ChannelID, region: string | null): Promise<boolean> {
		return this.call<boolean>('call.update_region', {channel_id: channelId.toString(), region});
	}

	async ringCallRecipients(channelId: ChannelID, recipients: Array<string>): Promise<boolean> {
		return this.call<boolean>('call.ring', {channel_id: channelId.toString(), recipients});
	}

	async stopRingingCallRecipients(channelId: ChannelID, recipients: Array<string>): Promise<boolean> {
		return this.call<boolean>('call.stop_ringing', {channel_id: channelId.toString(), recipients});
	}

	async deleteCall(channelId: ChannelID): Promise<boolean> {
		return this.call<boolean>('call.delete', {channel_id: channelId.toString()});
	}

	async getDiscoveryOnlineCounts(guildIds: Array<GuildID>): Promise<Map<GuildID, number>> {
		const result = await this.call<{online_counts: Array<{guild_id: string; online_count: number}>}>(
			'guild.get_online_counts_batch',
			{
				guild_ids: guildIds.map(String),
			},
		);
		const counts = new Map<GuildID, number>();
		for (const entry of result.online_counts) {
			counts.set(BigInt(entry.guild_id) as GuildID, entry.online_count);
		}
		return counts;
	}

	async getDiscoveryGuildCounts(
		guildIds: Array<GuildID>,
	): Promise<Map<GuildID, {memberCount: number; onlineCount: number}>> {
		const result = await this.call<{
			online_counts: Array<{guild_id: string; member_count: number; online_count: number}>;
		}>('guild.get_online_counts_batch', {
			guild_ids: guildIds.map(String),
		});
		const counts = new Map<GuildID, {memberCount: number; onlineCount: number}>();
		for (const entry of result.online_counts) {
			counts.set(BigInt(entry.guild_id) as GuildID, {
				memberCount: entry.member_count,
				onlineCount: entry.online_count,
			});
		}
		return counts;
	}

	async getNodeStats(): Promise<{
		status: string;
		sessions: number;
		guilds: number;
		presences: number;
		calls: number;
		memory: {
			total: string;
			processes: string;
			system: string;
		};
		process_count: number;
		process_limit: number;
		uptime_seconds: number;
	}> {
		return this.call<{
			status: string;
			sessions: number;
			guilds: number;
			presences: number;
			calls: number;
			memory: {total: string; processes: string; system: string};
			process_count: number;
			process_limit: number;
			uptime_seconds: number;
		}>('process.node_stats', {});
	}

	destroy(): void {
		if (this.batchTimeout) {
			clearTimeout(this.batchTimeout);
			this.batchTimeout = null;
		}
		this.rejectAllPendingBatchRequests(new ServiceUnavailableError());
	}
}
