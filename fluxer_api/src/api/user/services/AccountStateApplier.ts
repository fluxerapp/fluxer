// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {AdminRepository} from '@app/api/admin/AdminRepository';
import type {AdminArchiveService} from '@app/api/admin/services/AdminArchiveService';
import {type ChannelID, createUserID, type MessageID, type UserID} from '@app/api/BrandedTypes';
import {isIpBanExempt} from '@app/api/ban/IpBanExemptions';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import {dispatchChannelEvent} from '@app/api/channel/services/ChannelGatewayDispatch';
import {SYSTEM_USER_ID} from '@app/api/constants/Core';
import {IP_BAN_REFRESH_CHANNEL} from '@app/api/constants/IpBan';
import {withAccountChangeSource} from '@app/api/infrastructure/activity/ActivityMeta';
import type {
	ActionEnvelope,
	ActionOutcome,
	Observed,
	OutcomeStatus,
} from '@app/api/infrastructure/activity/Contract.generated';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {KVBulkMessageDeletionQueueService} from '@app/api/infrastructure/KVBulkMessageDeletionQueueService';
import type {User} from '@app/api/models/User';
import {isAccountLimitExempt} from '@app/api/user/AccountLimit';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {
	clearNewConversationLimit,
	isNewConversationLimitExempt,
	setNewConversationLimit,
} from '@app/api/user/NewConversationLimit';
import {
	type PartialUserChangePropagationDeps,
	propagatePartialUserChange,
} from '@app/api/user/services/PartialUserChangePropagation';
import {mapUserToPrivateResponse} from '@app/api/user/UserMappers';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {getSameIpDecisionKey, isPublicIpAddress, parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {ms} from 'itty-time';

export type ActionOf<T extends ActionEnvelope['type']> = Extract<ActionEnvelope, {type: T}>;

interface AccountUpdateDispatch {
	userUpdated(user: User): Promise<void>;
	profileChanged(user: User): Promise<void>;
	contentVisibilityChanged(user: User): Promise<void>;
	messagesRemoved(channelId: ChannelID, authorId: UserID, messageIds: Array<MessageID>): Promise<void>;
}

export interface AccountStateDeps {
	users: Pick<IUserRepository, 'findUnique' | 'compareAndSetFlags' | 'patchUpsert'>;
	dispatch: AccountUpdateDispatch;
	ipBans: Pick<AdminRepository, 'isIpBanned' | 'banIpTemp'>;
	cache: Pick<ICacheService, 'publish' | 'get' | 'set' | 'delete'>;
	archives: Pick<AdminArchiveService, 'triggerUserArchive' | 'listArchives'>;
	messageDeletionQueue: Pick<KVBulkMessageDeletionQueueService, 'scheduleDeletion' | 'removeFromQueue'>;
	messageDeletionDelayMs: number;
	authored: Pick<IChannelRepository, 'listMessagesByAuthor'>;
	now?: () => number;
}

const FLAGS_WRITE_ATTEMPTS = 3;
const AUTHORED_PAGE_SIZE = 200;
const MIN_TEMP_BAN_SECONDS = 60;
const RECENT_ARCHIVE_MS = ms('1 day');
const RECENT_ARCHIVE_SCAN = 20;

type ProfilePropagation = Omit<PartialUserChangePropagationDeps, 'gatewayService'>;

function gatewayDispatch(
	gateway: IGatewayService,
	profile: ProfilePropagation,
	channels: Pick<IChannelRepository, 'findUnique'>,
): AccountUpdateDispatch {
	return {
		async userUpdated(user) {
			await gateway.dispatchPresence({userId: user.id, event: 'USER_UPDATE', data: mapUserToPrivateResponse(user)});
		},
		async profileChanged(user) {
			await propagatePartialUserChange({...profile, gatewayService: gateway}, user);
		},
		async contentVisibilityChanged(user) {
			await profile.userCacheService.invalidateUserCache(user.id);
		},
		async messagesRemoved(channelId, authorId, messageIds) {
			const channel = await channels.findUnique(channelId);
			if (!channel) return;
			for (const messageId of messageIds) {
				await dispatchChannelEvent({
					gatewayService: gateway,
					channel,
					event: 'MESSAGE_DELETE',
					data: {channel_id: channelId.toString(), id: messageId.toString(), author_id: authorId.toString()},
				});
			}
		},
	};
}

export function accountStateDepsFromContext(
	ctx: ApiContext,
	ipBans: AccountStateDeps['ipBans'],
	profile: Omit<ProfilePropagation, 'userRepository'>,
	messageDeletion: Pick<AccountStateDeps, 'archives' | 'messageDeletionQueue' | 'messageDeletionDelayMs'>,
	channels: Pick<IChannelRepository, 'findUnique' | 'listMessagesByAuthor'>,
): AccountStateDeps {
	return {
		users: ctx.services.users,
		dispatch: gatewayDispatch(ctx.services.gateway, {...profile, userRepository: ctx.services.users}, channels),
		ipBans,
		cache: ctx.services.cache,
		...messageDeletion,
		authored: channels,
	};
}

function observedOf(user: User): Observed {
	return {
		flags: user.flags.toString(),
		deleted: (user.flags & UserFlags.DELETED) !== 0n,
	};
}

export function outcomeOf(
	env: Pick<ActionEnvelope, 'id'> & {type: string; user_id?: string},
	status: OutcomeStatus,
	user: User | null = null,
	detail: string | null = null,
): ActionOutcome {
	const outcome: ActionOutcome = {
		action_id: env.id,
		action_type: env.type,
		status,
		detail,
		observed: user ? observedOf(user) : null,
	};
	if (env.user_id) outcome.user_id = env.user_id;
	return outcome;
}

function isIneligible(user: User): boolean {
	return user.isBot || (user.flags & UserFlags.DELETED) !== 0n;
}

function isDeletionComplete(user: User): boolean {
	return (user.flags & UserFlags.DELETED) !== 0n && user.pendingDeletionAt === null;
}

export async function applySetAccountLimit(
	deps: AccountStateDeps,
	env: ActionOf<'set_account_limit'>,
): Promise<ActionOutcome> {
	return withAccountChangeSource('action', async () => {
		const userId = createUserID(BigInt(env.user_id));
		let user = await deps.users.findUnique(userId);
		for (let attempt = 0; attempt < FLAGS_WRITE_ATTEMPTS; attempt++) {
			if (!user) return outcomeOf(env, 'ineligible');
			if (isIneligible(user)) return outcomeOf(env, 'ineligible', user);
			if (env.on && isAccountLimitExempt(user)) return outcomeOf(env, 'exempt', user);
			const limited = (user.flags & UserFlags.ACCOUNT_LIMITED) !== 0n;
			if (limited === env.on) return outcomeOf(env, 'noop', user);
			const target = env.on ? user.flags | UserFlags.ACCOUNT_LIMITED : user.flags & ~UserFlags.ACCOUNT_LIMITED;
			const updated = await deps.users.compareAndSetFlags(user, target);
			if (updated) {
				await deps.dispatch.userUpdated(updated);
				return outcomeOf(env, 'applied', updated);
			}
			user = await deps.users.findUnique(userId);
		}
		throw new Error('User flags kept changing during apply');
	});
}

export async function applyHideProfile(deps: AccountStateDeps, env: ActionOf<'hide_profile'>): Promise<ActionOutcome> {
	return withAccountChangeSource('action', async () => {
		const userId = createUserID(BigInt(env.user_id));
		let user = await deps.users.findUnique(userId);
		for (let attempt = 0; attempt < FLAGS_WRITE_ATTEMPTS; attempt++) {
			if (!user) return outcomeOf(env, 'ineligible');
			if (isIneligible(user)) return outcomeOf(env, 'ineligible', user);
			if (env.on && isAccountLimitExempt(user)) return outcomeOf(env, 'exempt', user);
			const hidden = (user.flags & UserFlags.PROFILE_HIDDEN) !== 0n;
			if (hidden === env.on) return outcomeOf(env, 'noop', user);
			const target = env.on ? user.flags | UserFlags.PROFILE_HIDDEN : user.flags & ~UserFlags.PROFILE_HIDDEN;
			const updated = await deps.users.compareAndSetFlags(user, target);
			if (updated) {
				await deps.dispatch.userUpdated(updated);
				await deps.dispatch.profileChanged(updated);
				return outcomeOf(env, 'applied', updated);
			}
			user = await deps.users.findUnique(userId);
		}
		throw new Error('User flags kept changing during apply');
	});
}

export async function applyHideRecentMessages(
	deps: AccountStateDeps,
	env: ActionOf<'hide_recent_messages'>,
): Promise<ActionOutcome> {
	return withAccountChangeSource('action', async () => {
		const user = await deps.users.findUnique(createUserID(BigInt(env.user_id)));
		if (!user) return outcomeOf(env, 'ineligible');
		if (user.isBot || isDeletionComplete(user)) return outcomeOf(env, 'ineligible', user);
		const current = user.contentHiddenSince?.getTime() ?? null;
		if (!env.on) {
			if (current === null) return outcomeOf(env, 'noop', user);
			const shown = await deps.users.patchUpsert(user.id, {content_hidden_since: null}, user.toRow());
			await deps.dispatch.contentVisibilityChanged(shown);
			return outcomeOf(env, 'applied', shown);
		}
		if (isAccountLimitExempt(user)) return outcomeOf(env, 'exempt', user);
		const since = current === null ? env.since_ms : Math.min(current, env.since_ms);
		let hidden = user;
		if (since !== current) {
			hidden = await deps.users.patchUpsert(user.id, {content_hidden_since: new Date(since)}, user.toRow());
			await deps.dispatch.contentVisibilityChanged(hidden);
		}
		const removed = await removeAuthoredMessagesSince(deps, user.id, since);
		return outcomeOf(env, since === current ? 'noop' : 'applied', hidden, `messages=${removed}`);
	});
}

async function removeAuthoredMessagesSince(deps: AccountStateDeps, authorId: UserID, sinceMs: number): Promise<number> {
	let cursor: MessageID | undefined;
	let removed = 0;
	while (true) {
		const refs = await deps.authored.listMessagesByAuthor(authorId, AUTHORED_PAGE_SIZE, cursor);
		const inWindow = refs.filter(({messageId}) => snowflakeToDate(messageId).getTime() >= sinceMs);
		const byChannel = new Map<ChannelID, Array<MessageID>>();
		for (const {channelId, messageId} of inWindow) {
			const ids = byChannel.get(channelId);
			if (ids) ids.push(messageId);
			else byChannel.set(channelId, [messageId]);
		}
		for (const [channelId, messageIds] of byChannel) {
			await deps.dispatch.messagesRemoved(channelId, authorId, messageIds);
		}
		removed += inWindow.length;
		if (inWindow.length < refs.length || refs.length < AUTHORED_PAGE_SIZE) return removed;
		cursor = refs[refs.length - 1].messageId;
	}
}

export async function applyDeleteUserMessages(
	deps: AccountStateDeps,
	env: ActionOf<'delete_user_messages'>,
): Promise<ActionOutcome> {
	const user = await deps.users.findUnique(createUserID(BigInt(env.user_id)));
	if (!user) return outcomeOf(env, 'ineligible');
	if (!env.on) return cancelScheduledMessageDeletion(deps, env, user);
	if (user.isBot || isDeletionComplete(user)) return outcomeOf(env, 'ineligible', user);
	if (isAccountLimitExempt(user)) return outcomeOf(env, 'exempt', user);
	const now = deps.now?.() ?? Date.now();
	const archive = await archiveBeforeDeletion(deps, user.id, now);
	const target = now + deps.messageDeletionDelayMs;
	const current = user.pendingBulkMessageDeletionAt?.getTime() ?? null;
	const scheduledAt = current === null ? target : Math.min(current, target);
	await deps.messageDeletionQueue.scheduleDeletion(user.id, new Date(scheduledAt));
	const detail = `${archive} scheduled_at=${new Date(scheduledAt).toISOString()}`;
	if (scheduledAt === current) return outcomeOf(env, 'noop', user, detail);
	const scheduled = await deps.users.patchUpsert(
		user.id,
		{
			pending_bulk_message_deletion_at: new Date(scheduledAt),
			pending_bulk_message_deletion_channel_count: null,
			pending_bulk_message_deletion_message_count: null,
		},
		user.toRow(),
	);
	await deps.dispatch.userUpdated(scheduled);
	return outcomeOf(env, 'applied', scheduled, detail);
}

async function archiveBeforeDeletion(deps: AccountStateDeps, userId: UserID, now: number): Promise<string> {
	const archives = await deps.archives.listArchives({
		subjectType: 'user',
		subjectId: userId,
		limit: RECENT_ARCHIVE_SCAN,
	});
	const recent = archives.find(
		(archive) => archive.failed_at === null && now - Date.parse(archive.requested_at) < RECENT_ARCHIVE_MS,
	);
	if (recent) return `archive=${recent.archive_id} reused=true`;
	const created = await deps.archives.triggerUserArchive(userId, SYSTEM_USER_ID, true);
	return `archive=${created.archive_id} reused=false`;
}

async function cancelScheduledMessageDeletion(
	deps: AccountStateDeps,
	env: ActionOf<'delete_user_messages'>,
	user: User,
): Promise<ActionOutcome> {
	if (user.pendingBulkMessageDeletionAt === null) return outcomeOf(env, 'noop', user);
	const cancelled = await deps.users.patchUpsert(
		user.id,
		{
			pending_bulk_message_deletion_at: null,
			pending_bulk_message_deletion_channel_count: null,
			pending_bulk_message_deletion_message_count: null,
		},
		user.toRow(),
	);
	await deps.messageDeletionQueue.removeFromQueue(user.id);
	await deps.dispatch.userUpdated(cancelled);
	return outcomeOf(env, 'applied', cancelled);
}

function parseBanTarget(value: string): ReturnType<typeof parseIpAddress> {
	const direct = parseIpAddress(value);
	if (direct) return direct;
	const slash = value.lastIndexOf('/');
	if (slash <= 0) return null;
	const network = parseIpAddress(value.slice(0, slash));
	if (!network || getSameIpDecisionKey(network.normalized) !== value) return null;
	return network;
}

export async function applyTempBanIp(deps: AccountStateDeps, env: ActionOf<'temp_ban_ip'>): Promise<ActionOutcome> {
	const parsed = parseBanTarget(env.ip);
	if (!parsed || !isPublicIpAddress(parsed.normalized) || isIpBanExempt(parsed.normalized)) {
		return outcomeOf(env, 'exempt');
	}
	const now = deps.now?.() ?? Date.now();
	const ttlSeconds = Math.floor((env.until_ms - now) / 1000);
	if (ttlSeconds < MIN_TEMP_BAN_SECONDS) return outcomeOf(env, 'noop');
	if (await deps.ipBans.isIpBanned(parsed.normalized)) return outcomeOf(env, 'noop');
	await deps.ipBans.banIpTemp(getSameIpDecisionKey(parsed.normalized) ?? parsed.normalized, ttlSeconds);
	await deps.cache.publish(IP_BAN_REFRESH_CHANNEL, 'refresh');
	return outcomeOf(env, 'applied');
}

export async function applyLimitNewConversations(
	deps: AccountStateDeps,
	env: ActionOf<'limit_new_conversations'>,
): Promise<ActionOutcome> {
	const user = await deps.users.findUnique(createUserID(BigInt(env.user_id)));
	if (!user) return outcomeOf(env, 'ineligible');
	if (isIneligible(user)) return outcomeOf(env, 'ineligible', user);
	const store = {cache: deps.cache, now: deps.now};
	if (!env.on) {
		const lifted = await clearNewConversationLimit(user.id, store);
		return outcomeOf(env, lifted ? 'applied' : 'noop', user);
	}
	if (isNewConversationLimitExempt(user)) return outcomeOf(env, 'exempt', user);
	const applied = await setNewConversationLimit(user.id, env.until_ms, store);
	return outcomeOf(env, applied ? 'applied' : 'noop', user);
}
