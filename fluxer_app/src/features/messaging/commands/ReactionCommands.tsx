// SPDX-License-Identifier: AGPL-3.0-or-later

import {FeatureTemporarilyDisabledModal} from '@app/features/app/components/alerts/FeatureTemporarilyDisabledModal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import Authentication from '@app/features/auth/state/Authentication';
import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import {TooManyReactionsModal} from '@app/features/messaging/components/alerts/TooManyReactionsModal';
import MessageReactions from '@app/features/messaging/state/MessageReactions';
import Messages from '@app/features/messaging/state/MessagingMessages';
import type {ReactionEmoji} from '@app/features/messaging/utils/ReactionUtils';
import {resolveRetryAfterMs} from '@app/features/messaging/utils/RetryAfterUtils';
import {http} from '@app/features/platform/transport/RestTransport';
import {HttpError} from '@app/features/platform/types/EndpointError';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ME} from '@fluxer/constants/src/AppConstants';
import type {ReactionUsersPageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {UserPartial} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';

const YOU_CAN_T_ADD_NEW_REACTIONS_WHILE_YOU_DESCRIPTOR = msg({
	message: "You can't add new reactions while you're timed out.",
	comment: 'Error message in the messaging commands.',
});
const logger = new Logger('MessageReactions');
const MAX_RETRIES = 3;

interface ReactionFetchOptions {
	limit?: number;
	after?: string;
	totalCount?: number;
}

type ReactionOptimisticType =
	| 'MESSAGE_REACTION_ADD'
	| 'MESSAGE_REACTION_REMOVE'
	| 'MESSAGE_REACTION_REMOVE_ALL'
	| 'MESSAGE_REACTION_REMOVE_EMOJI';

const checkReactionResponse = (i18n: I18n, error: HttpError): boolean => {
	const errorCode = failureCode(error);
	if (error.status === 403) {
		if (errorCode === APIErrorCodes.FEATURE_TEMPORARILY_DISABLED) {
			logger.debug('Feature temporarily disabled, not retrying');
			ModalCommands.push(
				modal(() => (
					<FeatureTemporarilyDisabledModal data-flx="messaging.reaction-commands.check-reaction-response.feature-temporarily-disabled-modal" />
				)),
			);
			return true;
		}
		if (errorCode === APIErrorCodes.COMMUNICATION_DISABLED) {
			logger.debug('Communication disabled while timed out, not retrying');
			ToastCommands.createToast({
				type: 'info',
				children: i18n._(YOU_CAN_T_ADD_NEW_REACTIONS_WHILE_YOU_DESCRIPTOR),
			});
			return true;
		}
	}
	if (error.status === 400) {
		switch (errorCode) {
			case APIErrorCodes.MAX_REACTIONS:
				logger.debug(`Reaction limit reached: ${errorCode}`);
				ModalCommands.push(
					modal(() => (
						<TooManyReactionsModal data-flx="messaging.reaction-commands.check-reaction-response.too-many-reactions-modal" />
					)),
				);
				break;
		}
	}
	return true;
};
const optimisticUpdate = (
	type: ReactionOptimisticType,
	channelId: string,
	messageId: string,
	emoji: ReactionEmoji,
	userId?: string,
): void => {
	const actualUserId = userId ?? Authentication.currentUserId;
	if (!actualUserId) {
		logger.warn('Skipping optimistic reaction update because user ID is unavailable');
		return;
	}
	if (type === 'MESSAGE_REACTION_ADD') {
		MessageReactions.handleReactionAdd(messageId, actualUserId, emoji);
	} else if (type === 'MESSAGE_REACTION_REMOVE') {
		MessageReactions.handleReactionRemove(messageId, actualUserId, emoji);
	} else if (type === 'MESSAGE_REACTION_REMOVE_ALL') {
		MessageReactions.handleReactionRemoveAll(messageId);
	} else if (type === 'MESSAGE_REACTION_REMOVE_EMOJI') {
		MessageReactions.handleReactionRemoveEmoji(messageId, emoji);
	}
	if (type === 'MESSAGE_REACTION_ADD' || type === 'MESSAGE_REACTION_REMOVE') {
		Messages.handleReaction({
			type,
			channelId,
			messageId,
			userId: actualUserId,
			emoji,
			optimistic: true,
			skipReactionStore: true,
		});
	} else if (type === 'MESSAGE_REACTION_REMOVE_ALL') {
		Messages.handleRemoveAllReactions({channelId, messageId});
	} else if (type === 'MESSAGE_REACTION_REMOVE_EMOJI') {
		Messages.handleRemoveReactionEmoji({channelId, messageId, emoji});
	}
	logger.debug(
		`Optimistically applied ${type} for message ${messageId} ` +
			`with emoji ${emoji.name}${emoji.id ? `:${emoji.id}` : ''} by user ${actualUserId}`,
	);
};
const makeUrl = ({
	channelId,
	messageId,
	emoji,
	userId,
}: {
	channelId: string;
	messageId: string;
	emoji: ReactionEmoji;
	userId?: string;
}): string => {
	const emojiCode = encodeURIComponent(emoji.id ? `${emoji.name}:${emoji.id}` : emoji.name);
	return userId
		? Endpoints.CHANNEL_MESSAGE_REACTION_QUERY(channelId, messageId, emojiCode, userId)
		: Endpoints.CHANNEL_MESSAGE_REACTION(channelId, messageId, emojiCode);
};

function makeUsersUrl({
	channelId,
	messageId,
	emoji,
}: {
	channelId: string;
	messageId: string;
	emoji: ReactionEmoji;
}): string {
	const emojiCode = encodeURIComponent(emoji.id ? `${emoji.name}:${emoji.id}` : emoji.name);
	return Endpoints.CHANNEL_MESSAGE_REACTION_USERS(channelId, messageId, emojiCode);
}

function sessionQuery(): {session_id: string | null} {
	return {session_id: GatewayConnection.sessionId ?? null};
}

function reactionFetchQuery(options: ReactionFetchOptions): Record<string, number | string> | undefined {
	const query: Record<string, number | string> = {};
	if (options.limit !== undefined) query['limit'] = options.limit;
	if (options.after !== undefined) query['after'] = options.after;
	return Object.keys(query).length > 0 ? query : undefined;
}

function applyReactionFetchResult(
	messageId: string,
	emoji: ReactionEmoji,
	data: Array<UserPartial>,
	options: ReactionFetchOptions,
	responseHasMore?: boolean,
	requestId?: number,
	nextAfter?: string | null,
): void {
	const {limit, after, totalCount} = options;
	if (after !== undefined) {
		MessageReactions.handleFetchAppend(
			messageId,
			data,
			emoji,
			limit,
			responseHasMore,
			totalCount,
			requestId,
			nextAfter,
		);
		return;
	}
	MessageReactions.handleFetchSuccess(messageId, data, emoji, limit, responseHasMore, totalCount, requestId, nextAfter);
}

function addReactionRequest(channelId: string, messageId: string, emoji: ReactionEmoji): Promise<unknown> {
	return http.put(makeUrl({channelId, messageId, emoji, userId: ME}), {
		query: sessionQuery(),
	});
}

function removeReactionRequest(
	channelId: string,
	messageId: string,
	emoji: ReactionEmoji,
	userId?: string,
): Promise<unknown> {
	return http.delete(makeUrl({channelId, messageId, emoji, userId: userId || ME}), {
		query: sessionQuery(),
	});
}

function removeAllReactionsRequest(channelId: string, messageId: string): Promise<unknown> {
	return http.delete(Endpoints.CHANNEL_MESSAGE_REACTIONS(channelId, messageId));
}

function removeReactionEmojiRequest(channelId: string, messageId: string, emoji: ReactionEmoji): Promise<unknown> {
	return http.delete(makeUrl({channelId, messageId, emoji}));
}

async function retryWithExponentialBackoff<T>(func: () => Promise<T>, attempts = 0): Promise<T> {
	const delay = (ms: number) => new Promise((res) => setTimeout(res, ms));
	try {
		return await func();
	} catch (error) {
		if (!(error instanceof HttpError) || error.status !== 429) {
			throw error;
		}
		if (attempts < MAX_RETRIES) {
			const backoffTime = Math.max(2 ** attempts * 1000, resolveRetryAfterMs(error) ?? 0);
			logger.debug(`Rate limited, retrying in ${backoffTime}ms (attempt ${attempts + 1}/${MAX_RETRIES})`);
			await delay(backoffTime);
			return retryWithExponentialBackoff(func, attempts + 1);
		}
		logger.error(`Operation failed after ${MAX_RETRIES} attempts:`, error);
		throw error;
	}
}

const performReactionAction = (
	i18n: I18n,
	type: 'MESSAGE_REACTION_ADD' | 'MESSAGE_REACTION_REMOVE',
	apiFunc: () => Promise<unknown>,
	channelId: string,
	messageId: string,
	emoji: ReactionEmoji,
	userId?: string,
): void => {
	optimisticUpdate(type, channelId, messageId, emoji, userId);
	retryWithExponentialBackoff(apiFunc).catch((error) => {
		if (checkReactionResponse(i18n, error)) {
			logger.debug(`Reverting optimistic update for reaction in message ${messageId}`);
			optimisticUpdate(
				type === 'MESSAGE_REACTION_ADD' ? 'MESSAGE_REACTION_REMOVE' : 'MESSAGE_REACTION_ADD',
				channelId,
				messageId,
				emoji,
				userId,
			);
		}
	});
};

export async function getReactions(
	channelId: string,
	messageId: string,
	emoji: ReactionEmoji,
	options: ReactionFetchOptions = {},
): Promise<Array<UserPartial>> {
	const {limit, after, totalCount} = options;
	const requestId = MessageReactions.handleFetchPending(messageId, emoji);
	try {
		logger.debug(
			`Fetching reactions for message ${messageId} in channel ${channelId} with emoji ${emoji.name}${limit ? ` (limit: ${limit})` : ''}${after ? ` (after: ${after})` : ''}`,
		);
		const response = await http.get<ReactionUsersPageResponse>(makeUsersUrl({channelId, messageId, emoji}), {
			query: reactionFetchQuery(options),
		});
		const data = response.body?.items ?? [];
		const responseHasMore = response.body?.has_more;
		const nextAfter = response.body?.next_after;
		applyReactionFetchResult(messageId, emoji, data, {limit, after, totalCount}, responseHasMore, requestId, nextAfter);
		logger.debug(`Retrieved ${data.length} reactions for message ${messageId}`);
		return data;
	} catch (error) {
		logger.error(`Failed to get reactions for message ${messageId}:`, error);
		MessageReactions.handleFetchError(messageId, emoji, requestId);
		throw error;
	}
}

export async function loadMoreReactions(
	channelId: string,
	messageId: string,
	emoji: ReactionEmoji,
	options: {totalCount?: number} = {},
): Promise<void> {
	const fetchStatus = MessageReactions.getFetchStatus(messageId, emoji);
	if (fetchStatus === 'pending') return;
	if (!MessageReactions.getHasMore(messageId, emoji)) return;
	const after = MessageReactions.getLastUserId(messageId, emoji);
	if (!after) return;
	try {
		await getReactions(channelId, messageId, emoji, {limit: 100, after, totalCount: options.totalCount});
	} catch {}
}

export function addReaction(i18n: I18n, channelId: string, messageId: string, emoji: ReactionEmoji): void {
	logger.debug(`Adding reaction ${emoji.name} to message ${messageId}`);
	const apiFunc = () => addReactionRequest(channelId, messageId, emoji);
	performReactionAction(i18n, 'MESSAGE_REACTION_ADD', apiFunc, channelId, messageId, emoji);
}

export function removeReaction(
	i18n: I18n,
	channelId: string,
	messageId: string,
	emoji: ReactionEmoji,
	userId?: string,
): void {
	logger.debug(`Removing reaction ${emoji.name} from message ${messageId}`);
	const apiFunc = () => removeReactionRequest(channelId, messageId, emoji, userId);
	performReactionAction(i18n, 'MESSAGE_REACTION_REMOVE', apiFunc, channelId, messageId, emoji, userId);
}

export function removeAllReactions(i18n: I18n, channelId: string, messageId: string): void {
	logger.debug(`Removing all reactions from message ${messageId} in channel ${channelId}`);
	const apiFunc = () => removeAllReactionsRequest(channelId, messageId);
	retryWithExponentialBackoff(apiFunc).catch((error) => {
		checkReactionResponse(i18n, error);
	});
}

export function removeReactionEmoji(i18n: I18n, channelId: string, messageId: string, emoji: ReactionEmoji): void {
	logger.debug(`Removing all ${emoji.name} reactions from message ${messageId} in channel ${channelId}`);
	optimisticUpdate('MESSAGE_REACTION_REMOVE_EMOJI', channelId, messageId, emoji);
	const apiFunc = () => removeReactionEmojiRequest(channelId, messageId, emoji);
	retryWithExponentialBackoff(apiFunc).catch((error) => {
		checkReactionResponse(i18n, error);
	});
}
