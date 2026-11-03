// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, MessageID, RoleID, UserID} from '@app/api/BrandedTypes';
import {createChannelID, createUserID} from '@app/api/BrandedTypes';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import type {GatewayDispatchEvent} from '@app/api/constants/Gateway';
import {GatewayRpcClient} from '@app/api/infrastructure/GatewayRpcClient';
import {GatewayRpcMethodError, GatewayRpcMethodErrorCodes} from '@app/api/infrastructure/GatewayRpcError';
import {
	type CallCaller,
	type CallData,
	callCallerRpcParams,
	type GatewayChannelMention,
	type GatewayGuildMemoryStats,
	type GatewayMentionSourcesPage,
	type GatewayNodeStats,
	type GatewayVoiceStateCounts,
	type GatewayVoiceStateEntry,
	type GuildChannelAuthContext,
} from '@app/api/infrastructure/IGatewayService';
import {Logger} from '@app/api/Logger';
import {chunkArray} from '@app/api/utils/ArrayUtils';
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
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {ms} from 'itty-time';

const USER_PERMISSIONS_BATCH_SIZE = 100;

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
	[GatewayRpcMethodErrorCodes.TIMEOUT]: () => new GatewayTimeoutError(),
	[GatewayRpcMethodErrorCodes.GUILD_OVERLOADED]: () => new ServiceUnavailableError({headers: {'Retry-After': '1'}}),
	[GatewayRpcMethodErrorCodes.OVERLOADED]: () => new ServiceUnavailableError(),
	[GatewayRpcMethodErrorCodes.NO_RESPONDERS]: () => new ServiceUnavailableError(),
};

interface DispatchGuildParams {
	guildId: GuildID;
	event: GatewayDispatchEvent;
	data: unknown;
}

interface DispatchGuildManyParams {
	guildId: GuildID;
	events: Array<{event: GatewayDispatchEvent; data: unknown}>;
}

interface DispatchPresenceParams {
	userId: UserID;
	event: GatewayDispatchEvent;
	data: unknown;
}

interface ClearPushChannelNotificationsParams {
	userId: UserID;
	channelId: ChannelID;
	messageId: MessageID;
}

interface GuildUserParams {
	guildId: GuildID;
	userId: UserID;
}

interface GuildAuthContextParams {
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

interface MembersWithRoleParams {
	guildId: GuildID;
	roleId: RoleID;
}

interface CheckTargetMemberParams {
	guildId: GuildID;
	userId: UserID;
	targetUserId: UserID;
}

interface CategoryChannelCountParams {
	guildId: GuildID;
	categoryId: ChannelID;
}

interface ChannelCountParams {
	guildId: GuildID;
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

type SettleableRequest = Omit<PendingRequest<unknown>, 'resolve'>;

type BatchEntry<T> = [string, Array<PendingRequest<T>>];

interface BatchLane<T> {
	readonly method: string;
	readonly pending: Map<string, Array<PendingRequest<T>>>;
	timeout: NodeJS.Timeout | null;
	active: number;
	readonly run: (entry: BatchEntry<T>) => Promise<void>;
}

interface GuildMemberResult {
	success: boolean;
	memberData?: GuildMemberResponse;
}

export class GatewayService {
	private rpcClient: GatewayRpcClient;
	private readonly guildDataLane = this.createLane<GuildResponse>('guild.get_data', (entry) =>
		this.processGuildDataEntry(entry),
	);
	private readonly guildMemberLane = this.createLane<GuildMemberResult>('guild.get_member', (entry) =>
		this.processGuildMemberEntry(entry),
	);
	private readonly permissionLane = this.createLane<boolean>('guild.check_permission', (entry) =>
		this.processPermissionEntry(entry),
	);
	private readonly authContextLane = this.createLane<GuildChannelAuthContext>('guild.get_auth_context', (entry) =>
		this.processAuthContextEntry(entry),
	);
	private readonly lanes = [this.guildDataLane, this.guildMemberLane, this.permissionLane, this.authContextLane];
	private pendingBatchRequestCount = 0;
	private authContextUnsupportedUntil = 0;
	private readonly BATCH_DELAY_MS = ms('5 milliseconds');
	private readonly MAX_PENDING_BATCH_REQUESTS = 2000;
	private readonly MAX_BATCH_CONCURRENCY = 50;
	private readonly PENDING_REQUEST_TIMEOUT_MS = ms('30 seconds');
	private readonly AUTH_CONTEXT_FALLBACK_MS = ms('5 minutes');

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
			return new BadGatewayError();
		}
		if (error instanceof Error && error.name === 'TimeoutError') {
			return new GatewayTimeoutError();
		}
		return error instanceof Error ? error : new Error(String(error));
	}

	private logBatchFailure(method: string, key: string, error: Error): void {
		if (error instanceof FluxerError && error.status < 500) {
			return;
		}
		Logger.error({method, key, error}, `[gateway-batch] ${method} batch operation failed`);
	}

	private settlePendingRequest(request: SettleableRequest, settle: () => void): void {
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

	private rejectPendingRequests(requests: ReadonlyArray<SettleableRequest>, error: Error): void {
		for (const request of requests) {
			this.settlePendingRequest(request, () => request.reject(error));
		}
	}

	private createLane<T>(method: string, run: (entry: BatchEntry<T>) => Promise<void>): BatchLane<T> {
		return {method, pending: new Map(), timeout: null, active: 0, run};
	}

	private scheduleBatch<T>(lane: BatchLane<T>): void {
		if (lane.timeout || lane.active >= this.MAX_BATCH_CONCURRENCY) {
			return;
		}
		lane.timeout = setTimeout(() => {
			lane.timeout = null;
			this.processQueue(lane);
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

	private processQueue<T>(lane: BatchLane<T>): void {
		const availableSlots = this.MAX_BATCH_CONCURRENCY - lane.active;
		if (availableSlots <= 0) {
			return;
		}
		const entries = this.takePendingEntries(lane.pending, availableSlots);
		if (entries.length === 0) {
			return;
		}
		const totalRequests = entries.reduce((sum, [, pending]) => sum + pending.length, 0);
		Logger.debug(
			`[gateway-batch] Processing ${lane.method} batch: ${entries.length} unique requests (${totalRequests} total)`,
		);
		for (const entry of entries) {
			lane.active += 1;
			void lane.run(entry).finally(() => {
				lane.active = Math.max(0, lane.active - 1);
				if (lane.pending.size > 0) {
					this.scheduleBatch(lane);
				}
			});
		}
		if (lane.pending.size > 0) {
			this.scheduleBatch(lane);
		}
	}

	private enqueueBatchRequest<T>(lane: BatchLane<T>, key: string): Promise<T> {
		return new Promise<T>((resolve, reject) => {
			if (this.pendingBatchRequestCount >= this.MAX_PENDING_BATCH_REQUESTS) {
				reject(new ServiceUnavailableError());
				return;
			}
			const pendingRequest: PendingRequest<T> = {
				resolve,
				reject,
				settled: false,
				timeoutId: null,
			};
			pendingRequest.timeoutId = setTimeout(() => {
				this.rejectPendingRequests([pendingRequest], new GatewayTimeoutError());
				this.removePendingRequest(lane, key, pendingRequest);
			}, this.PENDING_REQUEST_TIMEOUT_MS);
			const pending = lane.pending.get(key) || [];
			pending.push(pendingRequest);
			lane.pending.set(key, pending);
			this.pendingBatchRequestCount += 1;
			Logger.debug(`[gateway-batch] Queued ${lane.method} request ${key}, total pending: ${pending.length}`);
			this.scheduleBatch(lane);
		});
	}

	private removePendingRequest<T>(lane: BatchLane<T>, key: string, request: PendingRequest<T>): void {
		const pending = lane.pending.get(key);
		const index = pending ? pending.indexOf(request) : -1;
		if (!pending || index < 0) {
			return;
		}
		pending.splice(index, 1);
		if (pending.length === 0) {
			lane.pending.delete(key);
		}
	}

	private rejectAllPendingBatchRequests(error: Error): void {
		for (const lane of this.lanes) {
			for (const pendingRequests of lane.pending.values()) {
				this.rejectPendingRequests(pendingRequests, error);
			}
			lane.pending.clear();
		}
		this.pendingBatchRequestCount = 0;
	}

	private async processGuildDataEntry([key, pending]: BatchEntry<GuildResponse>): Promise<void> {
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
			this.logBatchFailure('guild.get_data', key, transformedError);
		}
	}

	private async processGuildMemberEntry([key, pending]: BatchEntry<GuildMemberResult>): Promise<void> {
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
			this.logBatchFailure('guild.get_member', key, transformedError);
		}
	}

	private async processPermissionEntry([key, pending]: BatchEntry<boolean>): Promise<void> {
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
			this.logBatchFailure('guild.check_permission', key, transformedError);
		}
	}

	private async processAuthContextEntry([key, pending]: BatchEntry<GuildChannelAuthContext>): Promise<void> {
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
				this.logBatchFailure('guild.get_auth_context', key, transformedError);
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

	async dispatchGuildMany({guildId, events}: DispatchGuildManyParams): Promise<void> {
		if (events.length === 0) return;
		await this.call('guild.dispatch_many', {
			guild_id: guildId.toString(),
			events,
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
	}: GuildUserParams & {
		skipMembershipCheck?: boolean;
	}): Promise<GuildResponse> {
		const key = `${guildId.toString()}-${userId.toString()}-${skipMembershipCheck ? 'skip' : 'check'}`;
		return this.enqueueBatchRequest(this.guildDataLane, key);
	}

	async getGuildAuthContext({guildId, userId, channelId}: GuildAuthContextParams): Promise<GuildChannelAuthContext> {
		if (Date.now() < this.authContextUnsupportedUntil) {
			return await this.getGuildAuthContextFromGuildData({guildId, userId, channelId});
		}
		const key = `${guildId.toString()}-${userId.toString()}-${channelId ? channelId.toString() : '0'}`;
		return this.enqueueBatchRequest(this.authContextLane, key);
	}

	async getGuildMember({guildId, userId}: GuildUserParams): Promise<GuildMemberResult> {
		const key = `${guildId.toString()}-${userId.toString()}`;
		return this.enqueueBatchRequest(this.guildMemberLane, key);
	}

	async hasGuildMember({guildId, userId}: GuildUserParams): Promise<boolean> {
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
			...(after !== undefined && after > 0n && {after: after.toString()}),
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

	async getUserPermissions({guildId, userId, channelId}: GuildAuthContextParams): Promise<bigint> {
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
		const results = await Promise.all(
			chunkArray(guildIds, USER_PERMISSIONS_BATCH_SIZE).map((batch) =>
				this.call<{
					permissions: Array<{
						guild_id: string;
						permissions: string;
					}>;
				}>('guild.get_user_permissions_batch', {
					guild_ids: batch.map((id) => id.toString()),
					user_id: userId.toString(),
					channel_id: channelId ? channelId.toString() : '0',
				}),
			),
		);
		for (const result of results) {
			for (const item of result.permissions) {
				const guildId = BigInt(item.guild_id) as GuildID;
				permissionsMap.set(guildId, BigInt(item.permissions));
			}
		}
		return permissionsMap;
	}

	async checkPermission({guildId, userId, permission, channelId}: CheckPermissionParams): Promise<boolean> {
		const key = `${guildId.toString()}-${userId.toString()}-${permission.toString()}-${channelId?.toString() || '0'}`;
		return this.enqueueBatchRequest(this.permissionLane, key);
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

	async getViewableChannels({guildId, userId}: GuildUserParams): Promise<Array<ChannelID>> {
		const result = await this.call<{
			channel_ids: Array<string>;
		}>('guild.get_viewable_channels', {
			guild_id: guildId.toString(),
			user_id: userId.toString(),
		});
		return result.channel_ids.map((id: string) => createChannelID(BigInt(id)));
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

	async getFirstViewableTextChannel(guildId: GuildID): Promise<ChannelID | null> {
		const result = await this.call<{
			channel_id: string | null;
		}>('guild.get_first_viewable_text_channel', {
			guild_id: guildId.toString(),
		});
		return result.channel_id ? createChannelID(BigInt(result.channel_id)) : null;
	}

	async joinGuild({userId, guildId}: GuildUserParams): Promise<void> {
		await this.call('presence.join_guild', {
			user_id: userId.toString(),
			guild_id: guildId.toString(),
		});
	}

	async leaveGuild({userId, guildId}: GuildUserParams): Promise<void> {
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

	async startVisit({userId, guildId, operator}: {userId: UserID; guildId: GuildID; operator?: boolean}): Promise<void> {
		await this.call('presence.start_visit', {
			user_id: userId.toString(),
			guild_id: guildId.toString(),
			...(operator ? {operator: true} : {}),
		});
	}

	async endVisit({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<void> {
		await this.call('presence.end_visit', {
			user_id: userId.toString(),
			guild_id: guildId.toString(),
		});
	}

	async evictVisitors({guildId, ipKeys}: {guildId: GuildID; ipKeys: Array<string>}): Promise<Array<UserID>> {
		const result = await this.call<{user_ids: Array<string>}>('guild.evict_visitors', {
			guild_id: guildId.toString(),
			ip_keys: ipKeys,
		});
		return result.user_ids.map((userId) => createUserID(BigInt(userId)));
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

	async getCall(channelId: ChannelID): Promise<CallData | null> {
		return this.call<CallData | null>('call.get', {channel_id: channelId.toString()});
	}

	async createCall(
		channelId: ChannelID,
		messageId: string,
		region: string,
		ringing: Array<string>,
		recipients: Array<string>,
		caller?: CallCaller,
	): Promise<CallData> {
		return this.call<CallData>('call.create', {
			channel_id: channelId.toString(),
			message_id: messageId,
			region,
			ringing,
			recipients,
			...callCallerRpcParams(caller),
		});
	}

	async updateCallRegion(channelId: ChannelID, region: string | null): Promise<boolean> {
		return this.call<boolean>('call.update_region', {channel_id: channelId.toString(), region});
	}

	async ringCallRecipients(channelId: ChannelID, recipients: Array<string>, caller?: CallCaller): Promise<boolean> {
		return this.call<boolean>('call.ring', {
			channel_id: channelId.toString(),
			recipients,
			...callCallerRpcParams(caller),
		});
	}

	async stopRingingCallRecipients(channelId: ChannelID, recipients: Array<string>): Promise<boolean> {
		return this.call<boolean>('call.stop_ringing', {channel_id: channelId.toString(), recipients});
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
		for (const lane of this.lanes) {
			if (lane.timeout) {
				clearTimeout(lane.timeout);
				lane.timeout = null;
			}
		}
		this.rejectAllPendingBatchRequests(new ServiceUnavailableError());
	}
}
