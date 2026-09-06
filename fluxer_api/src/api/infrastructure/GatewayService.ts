// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {CallAlreadyExistsError} from '@fluxer/errors/src/domains/channel/CallAlreadyExistsError';
import {InvalidChannelTypeForCallError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeForCallError';
import {NoActiveCallError} from '@fluxer/errors/src/domains/channel/NoActiveCallError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {BadGatewayError} from '@fluxer/errors/src/domains/core/BadGatewayError';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import {GatewayTimeoutError} from '@fluxer/errors/src/domains/core/GatewayTimeoutError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {UserNotInVoiceError} from '@fluxer/errors/src/domains/user/UserNotInVoiceError';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {ms} from 'itty-time';
import type {ChannelID, GuildID, MessageID, RoleID, UserID} from '../BrandedTypes';
import {createChannelID, createGuildID, createRoleID, createUserID} from '../BrandedTypes';
import {SYSTEM_USER_ID} from '../constants/Core';
import type {GatewayDispatchEvent} from '../constants/Gateway';
import {Logger} from '../Logger';
import {GatewayRpcClient} from './GatewayRpcClient';
import {GatewayRpcMethodError, GatewayRpcMethodErrorCodes} from './GatewayRpcError';
import type {
	CallData,
	GatewayActiveVoiceRooms,
	GatewayChannelMention,
	GatewayGuildMemoryStats,
	GatewayMentionSources,
	GatewayMentionSourcesPage,
	GatewayNodeStats,
	GatewayVoiceStateCounts,
	GatewayVoiceStateEntry,
	GuildChannelAuthContext,
} from './IGatewayService';

const PUSH_BADGE_COUNT_BATCH_SIZE = 100;

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
	[GatewayRpcMethodErrorCodes.INVALID_PARAMS]: () => new BadRequestError({code: APIErrorCodes.INVALID_FORM_BODY}),
	[GatewayRpcMethodErrorCodes.BATCH_TOO_LARGE]: () => new BadRequestError({code: APIErrorCodes.INVALID_FORM_BODY}),
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

interface InvalidatePushBadgeCountsParams {
	userIds: Array<UserID>;
}

interface InvalidatePushSubscriptionsParams {
	userId: UserID;
}

interface ClearPushChannelNotificationsParams {
	userId: UserID;
	channelId: ChannelID;
	messageId: MessageID;
}

interface GuildDataParams {
	guildId: GuildID;
	userId: UserID;
}

interface GuildAuthContextParams {
	guildId: GuildID;
	userId: UserID;
	channelId?: ChannelID;
}

interface GuildMemberParams {
	guildId: GuildID;
	userId: UserID;
}

interface HasMemberParams {
	guildId: GuildID;
	userId: UserID;
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

interface ResolveMentionSourcesPageParams extends ResolveAllMentionsParams {
	limit: number;
	cursor?: string;
}

interface ResolveChannelMentionsParams {
	guildId: GuildID;
	channelIds: Array<ChannelID>;
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

interface GuildAuthContextRpcResponse {
	guild: GuildResponse;
	parent_channel?: ChannelResponse | null;
}

type PendingRequest<T> = {
	resolve: (value: T) => void;
	reject: (error: Error) => void;
	settled: boolean;
	timeoutId: NodeJS.Timeout | null;
};

export class GatewayService {
	private rpcClient: GatewayRpcClient;
	private pendingGuildDataRequests = new Map<string, Array<PendingRequest<GuildResponse>>>();
	private pendingGuildMemberRequests = new Map<
		string,
		Array<
			PendingRequest<{
				success: boolean;
				memberData?: GuildMemberResponse;
			}>
		>
	>();
	private pendingPermissionRequests = new Map<string, Array<PendingRequest<boolean>>>();
	private pendingAuthContextRequests = new Map<string, Array<PendingRequest<GuildChannelAuthContext>>>();
	private pendingBatchRequestCount = 0;
	private guildDataBatchTimeout: NodeJS.Timeout | null = null;
	private guildMemberBatchTimeout: NodeJS.Timeout | null = null;
	private permissionBatchTimeout: NodeJS.Timeout | null = null;
	private authContextBatchTimeout: NodeJS.Timeout | null = null;
	private activeGuildDataRequests = 0;
	private activeGuildMemberRequests = 0;
	private activePermissionRequests = 0;
	private activeAuthContextRequests = 0;
	private authContextUnsupportedUntil = 0;
	private readonly BATCH_DELAY_MS = ms('5 milliseconds');
	private readonly MAX_PENDING_BATCH_REQUESTS = 2000;
	private readonly MAX_BATCH_CONCURRENCY = 50;
	private readonly PENDING_REQUEST_TIMEOUT_MS = ms('30 seconds');
	private readonly AUTH_CONTEXT_FALLBACK_MS = ms('5 minutes');
	private readonly BADGE_COUNTS_FALLBACK_MS = ms('5 minutes');
	private badgeCountsUnsupportedUntil = 0;

	constructor() {
		this.rpcClient = GatewayRpcClient.getInstance();
	}

	private async call<T>(method: string, params: Record<string, unknown>): Promise<T> {
		try {
			return await this.rpcClient.call<T>(method, params);
		} catch (error) {
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

	private settlePendingRequest<T>(request: PendingRequest<T>, settle: () => void): void {
		if (request.settled) {
			return;
		}
		request.settled = true;
		if (request.timeoutId) {
			clearTimeout(request.timeoutId);
			request.timeoutId = null;
		}
		this.pendingBatchRequestCount = Math.max(0, this.pendingBatchRequestCount - 1);
		settle();
	}

	private resolvePendingRequests<T>(requests: Array<PendingRequest<T>>, value: T): void {
		for (const request of requests) {
			this.settlePendingRequest(request, () => request.resolve(value));
		}
	}

	private rejectPendingRequests<T>(requests: Array<PendingRequest<T>>, error: Error): void {
		for (const request of requests) {
			this.settlePendingRequest(request, () => request.reject(error));
		}
	}

	private scheduleGuildDataBatch(): void {
		if (this.guildDataBatchTimeout || this.activeGuildDataRequests >= this.MAX_BATCH_CONCURRENCY) {
			return;
		}
		this.guildDataBatchTimeout = setTimeout(() => {
			this.guildDataBatchTimeout = null;
			this.processGuildDataQueue();
		}, this.BATCH_DELAY_MS);
	}

	private scheduleGuildMemberBatch(): void {
		if (this.guildMemberBatchTimeout || this.activeGuildMemberRequests >= this.MAX_BATCH_CONCURRENCY) {
			return;
		}
		this.guildMemberBatchTimeout = setTimeout(() => {
			this.guildMemberBatchTimeout = null;
			this.processGuildMemberQueue();
		}, this.BATCH_DELAY_MS);
	}

	private schedulePermissionBatch(): void {
		if (this.permissionBatchTimeout || this.activePermissionRequests >= this.MAX_BATCH_CONCURRENCY) {
			return;
		}
		this.permissionBatchTimeout = setTimeout(() => {
			this.permissionBatchTimeout = null;
			this.processPermissionQueue();
		}, this.BATCH_DELAY_MS);
	}

	private scheduleAuthContextBatch(): void {
		if (this.authContextBatchTimeout || this.activeAuthContextRequests >= this.MAX_BATCH_CONCURRENCY) {
			return;
		}
		this.authContextBatchTimeout = setTimeout(() => {
			this.authContextBatchTimeout = null;
			this.processAuthContextQueue();
		}, this.BATCH_DELAY_MS);
	}

	private takePendingEntries<T>(
		requests: Map<string, Array<PendingRequest<T>>>,
		limit: number,
	): Array<[string, Array<PendingRequest<T>>]> {
		const entries: Array<[string, Array<PendingRequest<T>>]> = [];
		for (const [key, pending] of requests) {
			entries.push([key, pending]);
			requests.delete(key);
			if (entries.length >= limit) {
				break;
			}
		}
		return entries;
	}

	private processGuildDataQueue(): void {
		const availableSlots = this.MAX_BATCH_CONCURRENCY - this.activeGuildDataRequests;
		if (availableSlots <= 0) {
			return;
		}
		const entries = this.takePendingEntries(this.pendingGuildDataRequests, availableSlots);
		if (entries.length === 0) {
			return;
		}
		const totalGuildDataRequests = entries.reduce((sum, [, pending]) => sum + pending.length, 0);
		Logger.debug(
			`[gateway-batch] Processing guild.get_data batch: ${entries.length} unique requests (${totalGuildDataRequests} total)`,
		);
		for (const entry of entries) {
			this.activeGuildDataRequests += 1;
			void this.processGuildDataEntry(entry).finally(() => {
				this.activeGuildDataRequests = Math.max(0, this.activeGuildDataRequests - 1);
				if (this.pendingGuildDataRequests.size > 0) {
					this.scheduleGuildDataBatch();
				}
			});
		}
		if (this.pendingGuildDataRequests.size > 0) {
			this.scheduleGuildDataBatch();
		}
	}

	private processGuildMemberQueue(): void {
		const availableSlots = this.MAX_BATCH_CONCURRENCY - this.activeGuildMemberRequests;
		if (availableSlots <= 0) {
			return;
		}
		const entries = this.takePendingEntries(this.pendingGuildMemberRequests, availableSlots);
		if (entries.length === 0) {
			return;
		}
		const totalGuildMemberRequests = entries.reduce((sum, [, pending]) => sum + pending.length, 0);
		Logger.debug(
			`[gateway-batch] Processing guild.get_member batch: ${entries.length} unique requests (${totalGuildMemberRequests} total)`,
		);
		for (const entry of entries) {
			this.activeGuildMemberRequests += 1;
			void this.processGuildMemberEntry(entry).finally(() => {
				this.activeGuildMemberRequests = Math.max(0, this.activeGuildMemberRequests - 1);
				if (this.pendingGuildMemberRequests.size > 0) {
					this.scheduleGuildMemberBatch();
				}
			});
		}
		if (this.pendingGuildMemberRequests.size > 0) {
			this.scheduleGuildMemberBatch();
		}
	}

	private processPermissionQueue(): void {
		const availableSlots = this.MAX_BATCH_CONCURRENCY - this.activePermissionRequests;
		if (availableSlots <= 0) {
			return;
		}
		const entries = this.takePendingEntries(this.pendingPermissionRequests, availableSlots);
		if (entries.length === 0) {
			return;
		}
		const totalPermissionRequests = entries.reduce((sum, [, pending]) => sum + pending.length, 0);
		Logger.debug(
			`[gateway-batch] Processing guild.check_permission batch: ${entries.length} unique requests (${totalPermissionRequests} total)`,
		);
		for (const entry of entries) {
			this.activePermissionRequests += 1;
			void this.processPermissionEntry(entry).finally(() => {
				this.activePermissionRequests = Math.max(0, this.activePermissionRequests - 1);
				if (this.pendingPermissionRequests.size > 0) {
					this.schedulePermissionBatch();
				}
			});
		}
		if (this.pendingPermissionRequests.size > 0) {
			this.schedulePermissionBatch();
		}
	}

	private rejectAllPendingBatchRequests(error: Error): void {
		for (const pendingRequests of this.pendingGuildDataRequests.values()) {
			this.rejectPendingRequests(pendingRequests, error);
		}
		for (const pendingRequests of this.pendingGuildMemberRequests.values()) {
			this.rejectPendingRequests(pendingRequests, error);
		}
		for (const pendingRequests of this.pendingPermissionRequests.values()) {
			this.rejectPendingRequests(pendingRequests, error);
		}
		for (const pendingRequests of this.pendingAuthContextRequests.values()) {
			this.rejectPendingRequests(pendingRequests, error);
		}
		this.pendingGuildDataRequests.clear();
		this.pendingGuildMemberRequests.clear();
		this.pendingPermissionRequests.clear();
		this.pendingAuthContextRequests.clear();
		this.pendingBatchRequestCount = 0;
	}

	private processAuthContextQueue(): void {
		const availableSlots = this.MAX_BATCH_CONCURRENCY - this.activeAuthContextRequests;
		if (availableSlots <= 0) {
			return;
		}
		const entries = this.takePendingEntries(this.pendingAuthContextRequests, availableSlots);
		if (entries.length === 0) {
			return;
		}
		const totalAuthContextRequests = entries.reduce((sum, [, pending]) => sum + pending.length, 0);
		Logger.debug(
			`[gateway-batch] Processing guild.get_auth_context batch: ${entries.length} unique requests (${totalAuthContextRequests} total)`,
		);
		for (const entry of entries) {
			this.activeAuthContextRequests += 1;
			void this.processAuthContextEntry(entry).finally(() => {
				this.activeAuthContextRequests = Math.max(0, this.activeAuthContextRequests - 1);
				if (this.pendingAuthContextRequests.size > 0) {
					this.scheduleAuthContextBatch();
				}
			});
		}
		if (this.pendingAuthContextRequests.size > 0) {
			this.scheduleAuthContextBatch();
		}
	}

	private async processGuildDataEntry([key, pending]: [string, Array<PendingRequest<GuildResponse>>]): Promise<void> {
		try {
			const [guildIdStr, userIdStr, skipCheck] = key.split('-');
			const guildId = BigInt(guildIdStr) as GuildID;
			const userId = BigInt(userIdStr) as UserID;
			const skipMembershipCheck = skipCheck === 'skip';
			const guildResponse = await this.call<GuildResponse>('guild.get_data', {
				guild_id: guildId.toString(),
				user_id: skipMembershipCheck ? null : userId.toString(),
			});
			this.resolvePendingRequests(pending, guildResponse);
		} catch (error) {
			const transformedError = this.transformGatewayError(error);
			this.rejectPendingRequests(pending, transformedError);
			this.logBatchFailures('guild.get_data', [{status: 'rejected', reason: error}]);
		}
	}

	private async processGuildMemberEntry([key, pending]: [
		string,
		Array<
			PendingRequest<{
				success: boolean;
				memberData?: GuildMemberResponse;
			}>
		>,
	]): Promise<void> {
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
				this.resolvePendingRequests(pending, result);
			} else {
				this.resolvePendingRequests(pending, {success: false});
			}
		} catch (error) {
			const transformedError = this.transformGatewayError(error);
			this.rejectPendingRequests(pending, transformedError);
			this.logBatchFailures('guild.get_member', [{status: 'rejected', reason: error}]);
		}
	}

	private async processPermissionEntry([key, pending]: [string, Array<PendingRequest<boolean>>]): Promise<void> {
		try {
			const [guildIdStr, userIdStr, permissionStr, channelIdStr] = key.split('-');
			const guildId = BigInt(guildIdStr) as GuildID;
			const userId = BigInt(userIdStr) as UserID;
			const permission = BigInt(permissionStr);
			const channelId = channelIdStr !== '0' ? (BigInt(channelIdStr) as ChannelID) : undefined;
			const result = await this.call<{
				has_permission: boolean;
			}>('guild.check_permission', {
				guild_id: guildId.toString(),
				user_id: userId.toString(),
				permission: permission.toString(),
				channel_id: channelId ? channelId.toString() : '0',
			});
			this.resolvePendingRequests(pending, result.has_permission);
		} catch (error) {
			const transformedError = this.transformGatewayError(error);
			this.rejectPendingRequests(pending, transformedError);
			this.logBatchFailures('guild.check_permission', [{status: 'rejected', reason: error}]);
		}
	}

	private async processAuthContextEntry([key, pending]: [
		string,
		Array<PendingRequest<GuildChannelAuthContext>>,
	]): Promise<void> {
		const [guildIdStr, userIdStr, channelIdStr] = key.split('-');
		const guildId = BigInt(guildIdStr) as GuildID;
		const userId = BigInt(userIdStr) as UserID;
		const channelId = channelIdStr !== '0' ? (BigInt(channelIdStr) as ChannelID) : undefined;
		try {
			const result = await this.call<GuildAuthContextRpcResponse>('guild.get_auth_context', {
				guild_id: guildId.toString(),
				user_id: userId.toString(),
				channel_id: channelId ? channelId.toString() : null,
			});
			this.resolvePendingRequests(pending, {
				guild: result.guild,
				parentChannel: result.parent_channel ?? null,
			});
		} catch (error) {
			const transformedError = this.transformGatewayError(error);
			if (!this.isAuthContextUnsupportedError(transformedError)) {
				this.rejectPendingRequests(pending, transformedError);
				this.logBatchFailures('guild.get_auth_context', [{status: 'rejected', reason: error}]);
				return;
			}
			this.authContextUnsupportedUntil = Date.now() + this.AUTH_CONTEXT_FALLBACK_MS;
			Logger.warn({error}, '[gateway-rpc] guild.get_auth_context unavailable, falling back to guild.get_data');
			await this.settleAuthContextFromGuildData({guildId, userId, channelId}, pending);
		}
	}

	private isAuthContextUnsupportedError(error: Error): boolean {
		return error instanceof BadGatewayError || error instanceof GatewayTimeoutError;
	}

	private async settleAuthContextFromGuildData(
		params: GuildAuthContextParams,
		pending: Array<PendingRequest<GuildChannelAuthContext>>,
	): Promise<void> {
		try {
			this.resolvePendingRequests(pending, await this.getGuildAuthContextFromGuildData(params));
		} catch (error) {
			const transformedError = this.transformGatewayError(error);
			this.rejectPendingRequests(pending, transformedError);
			this.logBatchFailures('guild.get_data', [{status: 'rejected', reason: error}]);
		}
	}

	private async getGuildAuthContextFromGuildData({
		guildId,
		userId,
		channelId,
	}: GuildAuthContextParams): Promise<GuildChannelAuthContext> {
		const guild = await this.getGuildData({guildId, userId});
		const parentId = channelId?.toString();
		const parentChannel = parentId ? (guild.channels?.find((channel) => channel.id === parentId) ?? null) : null;
		return {guild, parentChannel};
	}

	async dispatchGuild({guildId, event, data}: DispatchGuildParams): Promise<void> {
		await this.call('guild.dispatch', {
			guild_id: guildId.toString(),
			event,
			data,
		});
	}

	async dispatchPresence({userId, event, data}: DispatchPresenceParams): Promise<void> {
		if (userId === SYSTEM_USER_ID) {
			return;
		}
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

	async invalidatePushBadgeCounts({userIds}: InvalidatePushBadgeCountsParams): Promise<void> {
		if (Date.now() < this.badgeCountsUnsupportedUntil) {
			await this.invalidatePushBadgeCountsIndividually(userIds);
			return;
		}
		const batches: Array<Array<UserID>> = [];
		for (let index = 0; index < userIds.length; index += PUSH_BADGE_COUNT_BATCH_SIZE) {
			batches.push(userIds.slice(index, index + PUSH_BADGE_COUNT_BATCH_SIZE));
		}
		try {
			await Promise.all(
				batches.map((batch) =>
					this.call('push.invalidate_badge_counts', {user_ids: batch.map((userId) => userId.toString())}),
				),
			);
		} catch (error) {
			const transformedError = this.transformGatewayError(error);
			if (!this.isAuthContextUnsupportedError(transformedError)) {
				throw transformedError;
			}
			this.badgeCountsUnsupportedUntil = Date.now() + this.BADGE_COUNTS_FALLBACK_MS;
			Logger.warn({error}, '[gateway-rpc] push.invalidate_badge_counts unavailable, falling back to per-user calls');
			await this.invalidatePushBadgeCountsIndividually(userIds);
		}
	}

	private async invalidatePushBadgeCountsIndividually(userIds: ReadonlyArray<UserID>): Promise<void> {
		await Promise.all(userIds.map((userId) => this.invalidatePushBadgeCount({userId})));
	}

	async invalidatePushSubscriptions({userId}: InvalidatePushSubscriptionsParams): Promise<void> {
		await this.call('push.invalidate_subscriptions', {
			user_id: userId.toString(),
		});
	}

	async clearPushChannelNotifications({
		userId,
		channelId,
		messageId,
	}: ClearPushChannelNotificationsParams): Promise<void> {
		await this.call('push.clear_channel_notifications', {
			user_id: userId.toString(),
			channel_id: channelId.toString(),
			message_id: messageId.toString(),
		});
	}

	async syncPushUserGuildSettings({
		userId,
		guildId,
		settings,
	}: {
		userId: UserID;
		guildId: GuildID;
		settings: unknown;
	}): Promise<void> {
		await this.call('push.sync_user_guild_settings', {
			user_id: userId.toString(),
			guild_id: guildId.toString(),
			user_guild_settings: settings,
		});
	}

	async getGuildCounts(guildId: GuildID): Promise<{
		memberCount: number;
		presenceCount: number;
	}> {
		const result = await this.call<{
			member_count?: unknown;
			presence_count?: unknown;
			online_count?: unknown;
		}>('guild.get_counts', {
			guild_id: guildId.toString(),
		});
		if (typeof result.member_count !== 'number') {
			throw new BadGatewayError();
		}
		let presenceCount: number | null = null;
		if (typeof result.presence_count === 'number') {
			presenceCount = result.presence_count;
		} else if (typeof result.online_count === 'number') {
			presenceCount = result.online_count;
		}
		if (presenceCount === null) {
			throw new BadGatewayError();
		}
		return {
			memberCount: result.member_count,
			presenceCount,
		};
	}

	async getChannelCount({guildId}: ChannelCountParams): Promise<number> {
		const result = await this.call<{
			count: number;
		}>('guild.get_channel_count', {
			guild_id: guildId.toString(),
		});
		return result.count;
	}

	async getCategoryChannelCount({guildId, categoryId}: CategoryChannelCountParams): Promise<number> {
		const result = await this.call<{
			count: number;
		}>('guild.get_category_channel_count', {
			guild_id: guildId.toString(),
			category_id: categoryId.toString(),
		});
		return result.count;
	}

	async getGuildData({
		guildId,
		userId,
		skipMembershipCheck,
	}: GuildDataParams & {
		skipMembershipCheck?: boolean;
	}): Promise<GuildResponse> {
		const key = `${guildId.toString()}-${userId.toString()}-${skipMembershipCheck ? 'skip' : 'check'}`;
		return new Promise<GuildResponse>((resolve, reject) => {
			if (this.pendingBatchRequestCount >= this.MAX_PENDING_BATCH_REQUESTS) {
				reject(new ServiceUnavailableError());
				return;
			}
			const pendingRequest: PendingRequest<GuildResponse> = {
				resolve,
				reject,
				settled: false,
				timeoutId: null,
			};
			pendingRequest.timeoutId = setTimeout(() => {
				const error = new GatewayTimeoutError();
				this.rejectPendingRequests([pendingRequest], error);
				this.removePendingGuildDataRequest(key, pendingRequest);
			}, this.PENDING_REQUEST_TIMEOUT_MS);
			const pending = this.pendingGuildDataRequests.get(key) || [];
			pending.push(pendingRequest);
			this.pendingGuildDataRequests.set(key, pending);
			this.pendingBatchRequestCount += 1;
			Logger.debug(
				`[gateway-batch] Queued guild.get_data request for guild ${guildId.toString()}, user ${userId.toString()}, total pending: ${pending.length}`,
			);
			this.scheduleGuildDataBatch();
		});
	}

	async getGuildAuthContext({guildId, userId, channelId}: GuildAuthContextParams): Promise<GuildChannelAuthContext> {
		if (Date.now() < this.authContextUnsupportedUntil) {
			return await this.getGuildAuthContextFromGuildData({guildId, userId, channelId});
		}
		const key = `${guildId.toString()}-${userId.toString()}-${channelId ? channelId.toString() : '0'}`;
		return new Promise<GuildChannelAuthContext>((resolve, reject) => {
			if (this.pendingBatchRequestCount >= this.MAX_PENDING_BATCH_REQUESTS) {
				reject(new ServiceUnavailableError());
				return;
			}
			const pendingRequest: PendingRequest<GuildChannelAuthContext> = {
				resolve,
				reject,
				settled: false,
				timeoutId: null,
			};
			pendingRequest.timeoutId = setTimeout(() => {
				const error = new GatewayTimeoutError();
				this.rejectPendingRequests([pendingRequest], error);
				this.removePendingAuthContextRequest(key, pendingRequest);
			}, this.PENDING_REQUEST_TIMEOUT_MS);
			const pending = this.pendingAuthContextRequests.get(key) || [];
			pending.push(pendingRequest);
			this.pendingAuthContextRequests.set(key, pending);
			this.pendingBatchRequestCount += 1;
			this.scheduleAuthContextBatch();
		});
	}

	private removePendingAuthContextRequest(key: string, request: PendingRequest<GuildChannelAuthContext>): void {
		const pending = this.pendingAuthContextRequests.get(key);
		if (pending) {
			const index = pending.indexOf(request);
			if (index >= 0) {
				pending.splice(index, 1);
				if (pending.length === 0) {
					this.pendingAuthContextRequests.delete(key);
				}
			}
		}
	}

	private removePendingGuildDataRequest(key: string, request: PendingRequest<GuildResponse>): void {
		const pending = this.pendingGuildDataRequests.get(key);
		if (pending) {
			const index = pending.indexOf(request);
			if (index >= 0) {
				pending.splice(index, 1);
				if (pending.length === 0) {
					this.pendingGuildDataRequests.delete(key);
				}
			}
		}
	}

	async getGuildMember({guildId, userId}: GuildMemberParams): Promise<{
		success: boolean;
		memberData?: GuildMemberResponse;
	}> {
		const key = `${guildId.toString()}-${userId.toString()}`;
		return new Promise<{
			success: boolean;
			memberData?: GuildMemberResponse;
		}>((resolve, reject) => {
			if (this.pendingBatchRequestCount >= this.MAX_PENDING_BATCH_REQUESTS) {
				reject(new ServiceUnavailableError());
				return;
			}
			const pendingRequest: PendingRequest<{
				success: boolean;
				memberData?: GuildMemberResponse;
			}> = {
				resolve,
				reject,
				settled: false,
				timeoutId: null,
			};
			pendingRequest.timeoutId = setTimeout(() => {
				const error = new GatewayTimeoutError();
				this.rejectPendingRequests([pendingRequest], error);
				this.removePendingGuildMemberRequest(key, pendingRequest);
			}, this.PENDING_REQUEST_TIMEOUT_MS);
			const pending = this.pendingGuildMemberRequests.get(key) || [];
			pending.push(pendingRequest);
			this.pendingGuildMemberRequests.set(key, pending);
			this.pendingBatchRequestCount += 1;
			Logger.debug(
				`[gateway-batch] Queued guild.get_member request for guild ${guildId.toString()}, user ${userId.toString()}, total pending: ${pending.length}`,
			);
			this.scheduleGuildMemberBatch();
		});
	}

	private removePendingGuildMemberRequest(
		key: string,
		request: PendingRequest<{
			success: boolean;
			memberData?: GuildMemberResponse;
		}>,
	): void {
		const pending = this.pendingGuildMemberRequests.get(key);
		if (pending) {
			const index = pending.indexOf(request);
			if (index >= 0) {
				pending.splice(index, 1);
				if (pending.length === 0) {
					this.pendingGuildMemberRequests.delete(key);
				}
			}
		}
	}

	async hasGuildMember({guildId, userId}: HasMemberParams): Promise<boolean> {
		const result = await this.call<{
			has_member: boolean;
		}>('guild.has_member', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
		});
		return result.has_member;
	}

	async listGuildMembers({guildId, limit, offset}: {guildId: GuildID; limit: number; offset: number}): Promise<{
		members: Array<GuildMemberResponse>;
		total: number;
	}> {
		const result = await this.call<{
			members?: Array<GuildMemberResponse>;
			total?: number;
		}>('guild.list_members', {
			guild_id: guildId.toString(),
			limit,
			offset,
		});
		return {
			members: result.members ?? [],
			total: result.total ?? 0,
		};
	}

	async listGuildMembersCursor({guildId, limit, after}: {guildId: GuildID; limit: number; after?: UserID}): Promise<{
		members: Array<GuildMemberResponse>;
		total: number;
	}> {
		const result = await this.call<{
			members?: Array<GuildMemberResponse>;
			total?: number;
		}>('guild.list_members_cursor', {
			guild_id: guildId.toString(),
			limit,
			...(after !== undefined && {after: after.toString()}),
		});
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

	async reloadAllGuilds(guildIds: Array<GuildID>): Promise<{
		count: number;
	}> {
		const result = await this.call<{
			count: number;
		}>('guild.reload_all', {
			guild_ids: guildIds.map((id) => id.toString()),
		});
		return {count: result.count};
	}

	async shutdownGuild(guildId: GuildID): Promise<void> {
		await this.call('guild.shutdown', {
			guild_id: guildId.toString(),
		});
	}

	async getGuildMemoryStats(limit: number): Promise<GatewayGuildMemoryStats> {
		const result = await this.call<GatewayGuildMemoryStats>('process.memory_stats', {
			limit: limit.toString(),
		});
		return {
			guilds: result.guilds,
		};
	}

	async getVoiceStateCounts(): Promise<GatewayVoiceStateCounts> {
		const result = await this.call<GatewayVoiceStateCounts>('process.voice_state_counts', {});
		return {
			total_voice_states: result.total_voice_states ?? 0,
			regions: result.regions ?? [],
			servers: result.servers ?? [],
		};
	}

	async getActiveVoiceRooms(): Promise<GatewayActiveVoiceRooms> {
		const result = await this.call<{
			rooms?: Array<{
				guild_id?: string | null;
				channel_id: string;
				voice_state_count?: number;
			}>;
			node_count?: number;
		}>('process.active_voice_rooms', {});
		return {
			nodeCount: result.node_count ?? 0,
			rooms: (result.rooms ?? []).map((room) => ({
				guildId:
					room.guild_id === undefined || room.guild_id === null ? undefined : createGuildID(BigInt(room.guild_id)),
				channelId: createChannelID(BigInt(room.channel_id)),
				voiceStateCount: room.voice_state_count ?? 0,
			})),
		};
	}

	async getUserPermissions({guildId, userId, channelId}: UserPermissionsParams): Promise<bigint> {
		const result = await this.call<{
			permissions: string;
		}>('guild.get_user_permissions', {
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
		const result = await this.call<{
			permissions: Array<{
				guild_id: string;
				permissions: string;
			}>;
		}>('guild.get_user_permissions_batch', {
			guild_ids: guildIds.map((id) => id.toString()),
			user_id: userId.toString(),
			channel_id: channelId ? channelId.toString() : '0',
		});
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
			const pendingRequest: PendingRequest<boolean> = {
				resolve,
				reject,
				settled: false,
				timeoutId: null,
			};
			pendingRequest.timeoutId = setTimeout(() => {
				const error = new GatewayTimeoutError();
				this.rejectPendingRequests([pendingRequest], error);
				this.removePendingPermissionRequest(key, pendingRequest);
			}, this.PENDING_REQUEST_TIMEOUT_MS);
			const pending = this.pendingPermissionRequests.get(key) || [];
			pending.push(pendingRequest);
			this.pendingPermissionRequests.set(key, pending);
			this.pendingBatchRequestCount += 1;
			Logger.debug(
				`[gateway-batch] Queued guild.check_permission request for guild ${guildId.toString()}, user ${userId.toString()}, channel ${channelId?.toString() || 'none'}, permission ${permission.toString()}, total pending: ${pending.length}`,
			);
			this.schedulePermissionBatch();
		});
	}

	private removePendingPermissionRequest(key: string, request: PendingRequest<boolean>): void {
		const pending = this.pendingPermissionRequests.get(key);
		if (pending) {
			const index = pending.indexOf(request);
			if (index >= 0) {
				pending.splice(index, 1);
				if (pending.length === 0) {
					this.pendingPermissionRequests.delete(key);
				}
			}
		}
	}

	async canManageRoles({guildId, userId, targetUserId, roleId}: CanManageRolesParams): Promise<boolean> {
		const result = await this.call<{
			can_manage: boolean;
		}>('guild.can_manage_roles', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
			target_user_id: targetUserId.toString(),
			role_id: roleId.toString(),
		});
		return result.can_manage;
	}

	async canManageRole({guildId, userId, roleId}: {guildId: GuildID; userId: UserID; roleId: RoleID}): Promise<boolean> {
		const result = await this.call<{
			can_manage: boolean;
		}>('guild.can_manage_role', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
			role_id: roleId.toString(),
		});
		return result.can_manage;
	}

	async getAssignableRoles({guildId, userId}: AssignableRolesParams): Promise<Array<RoleID>> {
		const result = await this.call<{
			role_ids: Array<string>;
		}>('guild.get_assignable_roles', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
		});
		return result.role_ids.map((id: string) => createRoleID(BigInt(id)));
	}

	async getUserMaxRolePosition({guildId, userId}: MaxRolePositionParams): Promise<number> {
		const result = await this.call<{
			position: number;
		}>('guild.get_user_max_role_position', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
		});
		return result.position;
	}

	async getMembersWithRole({guildId, roleId}: MembersWithRoleParams): Promise<Array<UserID>> {
		const result = await this.call<{
			user_ids: Array<string>;
		}>('guild.get_members_with_role', {
			guild_id: guildId.toString(),
			role_id: roleId.toString(),
		});
		return result.user_ids.map((id: string) => createUserID(BigInt(id)));
	}

	async checkTargetMember({guildId, userId, targetUserId}: CheckTargetMemberParams): Promise<boolean> {
		const result = await this.call<{
			can_manage: boolean;
		}>('guild.check_target_member', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
			target_user_id: targetUserId.toString(),
		});
		return result.can_manage;
	}

	async getViewableChannels({guildId, userId}: ViewableChannelsParams): Promise<Array<ChannelID>> {
		const result = await this.call<{
			channel_ids: Array<string>;
		}>('guild.get_viewable_channels', {
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
		const result = await this.call<{
			user_ids: Array<string>;
		}>('guild.get_users_to_mention_by_roles', {
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
		const result = await this.call<{
			user_ids: Array<string>;
		}>('guild.get_users_to_mention_by_user_ids', {
			guild_id: guildId.toString(),
			channel_id: channelId.toString(),
			user_ids: userIds.map((id) => id.toString()),
			author_id: authorId.toString(),
		});
		return result.user_ids.map((id: string) => createUserID(BigInt(id)));
	}

	async getAllUsersToMention({guildId, channelId, authorId}: AllUsersToMentionParams): Promise<Array<UserID>> {
		const result = await this.call<{
			user_ids: Array<string>;
		}>('guild.get_all_users_to_mention', {
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
		const result = await this.call<{
			user_ids: Array<string>;
		}>('guild.resolve_all_mentions', {
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

	async resolveMentionSources({
		guildId,
		channelId,
		authorId,
		mentionEveryone,
		mentionHere,
		roleIds,
		userIds,
	}: ResolveAllMentionsParams): Promise<GatewayMentionSources> {
		const result = await this.call<{
			direct_user_ids: Array<string>;
			role_user_ids: Array<string>;
			everyone_user_ids: Array<string>;
		}>('guild.resolve_mention_sources', {
			guild_id: guildId.toString(),
			channel_id: channelId.toString(),
			author_id: authorId.toString(),
			mention_everyone: mentionEveryone,
			mention_here: mentionHere,
			role_ids: roleIds.map((id) => id.toString()),
			user_ids: userIds.map((id) => id.toString()),
		});
		return {
			directUserIds: result.direct_user_ids.map((id: string) => createUserID(BigInt(id))),
			roleUserIds: result.role_user_ids.map((id: string) => createUserID(BigInt(id))),
			everyoneUserIds: result.everyone_user_ids.map((id: string) => createUserID(BigInt(id))),
		};
	}

	async resolveMentionSourcesPage({
		guildId,
		channelId,
		authorId,
		mentionEveryone,
		mentionHere,
		roleIds,
		userIds,
		limit,
		cursor,
	}: ResolveMentionSourcesPageParams): Promise<GatewayMentionSourcesPage> {
		const result = await this.call<{
			mentions?: Array<{
				user_id: string;
				direct?: boolean;
				role?: boolean;
				everyone?: boolean;
			}>;
			next_cursor?: string | null;
		}>('guild.resolve_mention_sources_page', {
			guild_id: guildId.toString(),
			channel_id: channelId.toString(),
			author_id: authorId.toString(),
			mention_everyone: mentionEveryone,
			mention_here: mentionHere,
			role_ids: roleIds.map((id) => id.toString()),
			user_ids: userIds.map((id) => id.toString()),
			limit,
			...(cursor !== undefined && {cursor}),
		});
		return {
			mentions: (result.mentions ?? []).map((entry) => ({
				userId: createUserID(BigInt(entry.user_id)),
				direct: entry.direct === true,
				role: entry.role === true,
				everyone: entry.everyone === true,
			})),
			nextCursor: result.next_cursor ?? null,
		};
	}

	async resolveChannelMentions({
		guildId,
		channelIds,
	}: ResolveChannelMentionsParams): Promise<Array<GatewayChannelMention>> {
		if (channelIds.length === 0) {
			return [];
		}
		const result = await this.call<{
			channels?: Array<GatewayChannelMention>;
		}>('guild.resolve_channel_mentions', {
			guild_id: guildId.toString(),
			channel_ids: Array.from(new Set(channelIds)).map((id) => id.toString()),
		});
		return result.channels ?? [];
	}

	async getVanityUrlChannel(guildId: GuildID): Promise<ChannelID | null> {
		const result = await this.call<{
			channel_id: string | null;
		}>('guild.get_vanity_url_channel', {
			guild_id: guildId.toString(),
		});
		return result.channel_id ? createChannelID(BigInt(result.channel_id)) : null;
	}

	async getFirstViewableTextChannel(guildId: GuildID): Promise<ChannelID | null> {
		const result = await this.call<{
			channel_id: string | null;
		}>('guild.get_first_viewable_text_channel', {
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

	async updateMemberVoice({guildId, userId, mute, deaf}: UpdateMemberVoiceParams): Promise<{
		success: boolean;
	}> {
		const result = await this.call<{
			success: boolean;
		}>('guild.update_member_voice', {
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
	}): Promise<{
		success: boolean;
		ignored?: boolean;
	}> {
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
		const result = await this.call<{
			success: boolean;
			ignored?: boolean;
		}>('voice.disconnect_user_if_in_channel', params);
		return {
			success: result.success,
			ignored: result.ignored,
		};
	}

	async getVoiceState({guildId, userId}: {guildId: GuildID; userId: UserID}): Promise<{
		channel_id: string | null;
	} | null> {
		const result = await this.call<{
			voice_state: {
				channel_id: string | null;
			} | null;
		}>('guild.get_voice_state', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
		});
		return result.voice_state;
	}

	async moveMember({guildId, moderatorId, userId, channelId, connectionId}: MoveMemberParams): Promise<{
		success?: boolean;
		error?: string;
	}> {
		const result = await this.call<{
			success?: boolean;
			error?: string;
		}>('guild.move_member', {
			guild_id: guildId.toString(),
			moderator_id: moderatorId.toString(),
			user_id: userId.toString(),
			channel_id: channelId ? channelId.toString() : null,
			connection_id: connectionId,
		});
		return result;
	}

	async hasActivePresence(userId: UserID): Promise<boolean> {
		const result = await this.call<{
			has_active: boolean;
		}>('presence.has_active', {
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

	async disconnectAllVoiceUsersInChannel({guildId, channelId}: {guildId: GuildID; channelId: ChannelID}): Promise<{
		success: boolean;
		disconnectedCount: number;
	}> {
		const result = await this.call<{
			success: boolean;
			disconnected_count: number;
		}>('guild.disconnect_all_voice_users_in_channel', {
			guild_id: guildId.toString(),
			channel_id: channelId.toString(),
		});
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
		tokenNonce?: string;
	}): Promise<{
		success: boolean;
		error?: string;
	}> {
		const params: Record<string, string> = {
			channel_id: channelId.toString(),
			connection_id: connectionId,
		};
		if (guildId !== undefined) {
			params['guild_id'] = guildId.toString();
		}
		if (tokenNonce !== undefined) {
			params['token_nonce'] = tokenNonce;
		}
		const result = await this.call<{
			success: boolean;
			error?: string;
		}>('voice.confirm_connection', params);
		return {
			success: result.success,
			error: result.error,
		};
	}

	async repairVoiceStateFromCache({
		guildId,
		channelId,
		userId,
		connectionId,
	}: {
		guildId?: GuildID;
		channelId: ChannelID;
		userId: UserID;
		connectionId: string;
	}): Promise<{
		success: boolean;
		repaired?: boolean;
		error?: string;
	}> {
		const params: Record<string, string> = {
			channel_id: channelId.toString(),
			user_id: userId.toString(),
			connection_id: connectionId,
		};
		if (guildId !== undefined) {
			params['guild_id'] = guildId.toString();
		}
		const result = await this.call<{
			success: boolean;
			repaired?: boolean;
			error?: string;
		}>('voice.repair_state_from_cache', params);
		return {
			success: result.success,
			repaired: result.repaired,
			error: result.error,
		};
	}

	async getVoiceStatesForChannel({guildId, channelId}: {guildId?: GuildID; channelId: ChannelID}): Promise<{
		voiceStates: Array<GatewayVoiceStateEntry>;
	}> {
		const params: Record<string, string> = {channel_id: channelId.toString()};
		if (guildId !== undefined) {
			params['guild_id'] = guildId.toString();
		}
		const result = await this.call<{
			voice_states: Array<{
				connection_id: string;
				user_id: string;
				channel_id: string;
				region_id?: string;
				server_id?: string;
			}>;
		}>('voice.get_voice_states_for_channel', params);
		return {
			voiceStates: (result.voice_states ?? []).map((vs) => ({
				connectionId: vs.connection_id,
				userId: vs.user_id,
				channelId: vs.channel_id,
				regionId: vs.region_id,
				serverId: vs.server_id,
			})),
		};
	}

	async getPendingJoinsForChannel({guildId, channelId}: {guildId?: GuildID; channelId: ChannelID}): Promise<{
		pendingJoins: Array<{
			connectionId: string;
			userId: string;
			tokenNonce: string;
			expiresAt: number;
		}>;
	}> {
		const params: Record<string, string> = {channel_id: channelId.toString()};
		if (guildId !== undefined) {
			params['guild_id'] = guildId.toString();
		}
		const result = await this.call<{
			pending_joins: Array<{
				connection_id: string;
				user_id: string;
				token_nonce: string;
				expires_at: number;
			}>;
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
		const result = await this.call<{
			online_counts: Array<{
				guild_id: string;
				online_count: number;
			}>;
		}>('guild.get_online_counts_batch', {
			guild_ids: guildIds.map(String),
		});
		const counts = new Map<GuildID, number>();
		for (const entry of result.online_counts) {
			counts.set(BigInt(entry.guild_id) as GuildID, entry.online_count);
		}
		return counts;
	}

	async getDiscoveryGuildCounts(guildIds: Array<GuildID>): Promise<
		Map<
			GuildID,
			{
				memberCount: number;
				onlineCount: number;
			}
		>
	> {
		const result = await this.call<{
			online_counts: Array<{
				guild_id: string;
				member_count: number;
				online_count: number;
			}>;
		}>('guild.get_online_counts_batch', {
			guild_ids: guildIds.map(String),
		});
		const counts = new Map<
			GuildID,
			{
				memberCount: number;
				onlineCount: number;
			}
		>();
		for (const entry of result.online_counts) {
			counts.set(BigInt(entry.guild_id) as GuildID, {
				memberCount: entry.member_count,
				onlineCount: entry.online_count,
			});
		}
		return counts;
	}

	async getNodeStats(): Promise<GatewayNodeStats> {
		return this.call<GatewayNodeStats>('process.node_stats', {});
	}

	destroy(): void {
		if (this.guildDataBatchTimeout) {
			clearTimeout(this.guildDataBatchTimeout);
			this.guildDataBatchTimeout = null;
		}
		if (this.guildMemberBatchTimeout) {
			clearTimeout(this.guildMemberBatchTimeout);
			this.guildMemberBatchTimeout = null;
		}
		if (this.permissionBatchTimeout) {
			clearTimeout(this.permissionBatchTimeout);
			this.permissionBatchTimeout = null;
		}
		if (this.authContextBatchTimeout) {
			clearTimeout(this.authContextBatchTimeout);
			this.authContextBatchTimeout = null;
		}
		this.rejectAllPendingBatchRequests(new ServiceUnavailableError());
	}
}
