// SPDX-License-Identifier: AGPL-3.0-or-later

import {createMessageID, type UserID} from '@app/api/BrandedTypes';
import type {IMessageRepository} from '@app/api/channel/repositories/IMessageRepository';
import {getCacheService, getChannelRepository} from '@app/api/middleware/ServiceSingletons';
import type {Channel} from '@app/api/models/Channel';
import type {User} from '@app/api/models/User';
import {isAccountLimitExempt} from '@app/api/user/AccountLimit';
import type {IUserRepository} from '@app/api/user/IUserRepository';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {NewConversationsLimitedError} from '@fluxer/errors/src/domains/user/NewConversationsLimitedError';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

export const NEW_CONVERSATION_LIMIT_MAX_MS = 7 * 24 * 60 * 60 * 1000;
const RECENT_PAGE = 100;
const OPENING_PAGE = 50;

export interface NewConversationLimit {
	until_ms: number;
	applied_at_ms: number;
}

type LimitCache = Pick<ICacheService, 'get' | 'set' | 'delete'>;

export interface NewConversationLimitDeps {
	cache?: LimitCache;
	now?: () => number;
}

function cacheOf(deps: NewConversationLimitDeps): LimitCache {
	return deps.cache ?? getCacheService();
}

function nowOf(deps: NewConversationLimitDeps): number {
	return deps.now?.() ?? Date.now();
}

function limitKey(userId: UserID | bigint | string): string {
	return `user:new_conversation_limit:${userId.toString()}`;
}

export function isNewConversationLimitExempt(user: Pick<User, 'isBot' | 'isSystem' | 'flags'>): boolean {
	return isAccountLimitExempt(user);
}

export async function getNewConversationLimit(
	userId: UserID,
	deps: NewConversationLimitDeps = {},
): Promise<NewConversationLimit | null> {
	const stored = await cacheOf(deps).get<NewConversationLimit>(limitKey(userId));
	if (!stored || typeof stored.until_ms !== 'number' || stored.until_ms <= nowOf(deps)) return null;
	return stored;
}

export async function setNewConversationLimit(
	userId: UserID,
	untilMs: number,
	deps: NewConversationLimitDeps = {},
): Promise<boolean> {
	const now = nowOf(deps);
	const until = Math.min(untilMs, now + NEW_CONVERSATION_LIMIT_MAX_MS);
	if (until <= now) return false;
	const current = await getNewConversationLimit(userId, deps);
	if (current && current.until_ms >= until) return false;
	const value: NewConversationLimit = {until_ms: until, applied_at_ms: now};
	await cacheOf(deps).set(limitKey(userId), value, Math.ceil((until - now) / 1000));
	return true;
}

export async function clearNewConversationLimit(userId: UserID, deps: NewConversationLimitDeps = {}): Promise<boolean> {
	const current = await getNewConversationLimit(userId, deps);
	await cacheOf(deps).delete(limitKey(userId));
	return current !== null;
}

export function oneToOneDmRecipient(channel: Pick<Channel, 'guildId' | 'type' | 'recipientIds'>, senderId: UserID) {
	if (channel.guildId || channel.type !== ChannelTypes.DM) return null;
	const others = Array.from(channel.recipientIds).filter((id) => id !== senderId);
	return others.length === 1 ? others[0]! : null;
}

type ConversationUsers = Pick<IUserRepository, 'getRelationship' | 'findExistingDmState' | 'findUnique'>;
type ConversationMessages = Pick<IMessageRepository, 'listMessages'>;

export interface NewConversationCheck {
	user: Pick<User, 'id' | 'isBot' | 'isSystem' | 'flags'>;
	targetId: UserID;
	users: ConversationUsers;
	messages?: ConversationMessages;
	channel?: Pick<Channel, 'id' | 'lastMessageId'> | null;
}

async function recipientHasWritten(
	messages: ConversationMessages,
	channel: Pick<Channel, 'id' | 'lastMessageId'>,
	recipientId: UserID,
): Promise<boolean> {
	if (channel.lastMessageId == null) return false;
	const recent = await messages.listMessages(channel.id, undefined, RECENT_PAGE);
	if (recent.some((message) => message.authorId === recipientId)) return true;
	if (recent.length < RECENT_PAGE) return false;
	const opening = await messages.listMessages(channel.id, undefined, OPENING_PAGE, createMessageID(BigInt(channel.id)));
	return opening.some((message) => message.authorId === recipientId);
}

export async function assertMayStartConversation(
	{user, targetId, users, messages, channel}: NewConversationCheck,
	deps: NewConversationLimitDeps = {},
): Promise<void> {
	if (isNewConversationLimitExempt(user)) return;
	const limit = await getNewConversationLimit(user.id, deps);
	if (!limit) return;
	const [friendship, target] = await Promise.all([
		users.getRelationship(user.id, targetId, RelationshipTypes.FRIEND),
		users.findUnique(targetId),
	]);
	if (friendship || !target || target.isBot || target.isSystem) return;
	const existing = channel === undefined ? await users.findExistingDmState(user.id, targetId) : channel;
	if (existing && (await recipientHasWritten(messages ?? getChannelRepository().messages, existing, targetId))) return;
	throw new NewConversationsLimitedError();
}
