// SPDX-License-Identifier: AGPL-3.0-or-later

import {FeatureTemporarilyDisabledModal} from '@app/features/app/components/alerts/FeatureTemporarilyDisabledModal';
import {MaxPollVoteCountReachedModal} from '@app/features/app/components/alerts/MaxPollVoteCountReachedModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {ERROR_DESCRIPTOR} from '@app/features/channel/components/channel_search_results/ChannelSearchResultsShared';
import {END_POLL_NOW_DESCRIPTOR, OKAY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Message as MessageModel} from '@app/features/messaging/models/MessagingMessage';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {UserPartial, UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import PollVotes from '../state/PollVotes';

const logger = new Logger('PollCommands');

const THIS_WILL_CLOSE_THE_POLL_DESCRIPTOR = msg({
	message: 'This will close the poll now and show the results.',
	comment: 'Description of the action of ending a poll in a confirmation modal.',
});

interface ShowEndPollConfirmationOptions {
	message: MessageModel;
	onEndPoll?: () => void;
}

interface VoteFetchOptions {
	limit?: number;
	after?: string;
	totalCount?: number;
}

interface FetchAnswerVotersResponse {
	users: Array<UserPartialResponse>;
	has_more?: boolean;
	next_after?: string | null;
}

export function showEndPollConfirmation(i18n: I18n, {message, onEndPoll}: ShowEndPollConfirmationOptions): void {
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(END_POLL_NOW_DESCRIPTOR)}
				description={i18n._(THIS_WILL_CLOSE_THE_POLL_DESCRIPTOR)}
				message={message}
				primaryText={i18n._(OKAY_DESCRIPTOR)}
				primaryVariant="primary"
				onPrimary={async () => {
					endPoll(i18n, message.channelId, message.id);
					onEndPoll?.();
				}}
				data-flx="messaging.message-commands.show-end-poll-confirmation.confirm-modal"
			/>
		)),
	);
}

function onHttpError(i18n: I18n, error: any) {
	const errorCode = failureCode(error);
	if (error.status === 403) {
		if (errorCode === APIErrorCodes.FEATURE_TEMPORARILY_DISABLED) {
			logger.debug('Feature temporarily disabled, not retrying');
			ModalCommands.push(
				modal(() => (
					<FeatureTemporarilyDisabledModal data-flx="messaging.poll-commands.check-poll-response.feature-temporarily-disabled-modal" />
				)),
			);
		}
		if (errorCode === APIErrorCodes.CANNOT_EDIT_OTHER_USER_MESSAGE) {
			logger.debug('Tried to end the poll of another user, somehow');
			ToastCommands.createToast({
				type: 'info',
				children: i18n._(ERROR_DESCRIPTOR),
			});
		}
	} else if (error.status === 400) {
		if (errorCode === APIErrorCodes.MAX_POLL_VOTES) {
			logger.debug('Reached max poll vote count');
			ModalCommands.push(
				modal(() => (
					<MaxPollVoteCountReachedModal data-flx="messaging.poll-commands.check-poll-response.max-poll-vote-count-reached-modal" />
				)),
			);
		}
	}
}

export function endPoll(i18n: I18n, channelId: string, messageId: string): Promise<unknown> {
	logger.debug(`Ending poll from message ${messageId} in channel ${channelId}`);
	return http.post(Endpoints.CHANNEL_POLL_EXPIRE(channelId, messageId)).catch((error) => onHttpError(i18n, error));
}

function applyPollVoteFetchResult(
	messageId: string,
	answerId: number,
	data: Array<UserPartial>,
	options: VoteFetchOptions,
	responseHasMore?: boolean,
	requestId?: number,
	nextAfter?: string | null,
): void {
	const {limit, after, totalCount} = options;
	if (after !== undefined) {
		PollVotes.handleFetchAppend(messageId, data, answerId, limit, responseHasMore, totalCount, requestId, nextAfter);
		return;
	}
	PollVotes.handleFetchSuccess(messageId, data, answerId, limit, responseHasMore, totalCount, requestId, nextAfter);
}

export async function getVotes(
	channelId: string,
	messageId: string,
	answerId: number,
	options: VoteFetchOptions = {},
): Promise<FetchAnswerVotersResponse> {
	const {limit, after} = options;
	const requestId = PollVotes.handleFetchPending(messageId, answerId);
	try {
		const response = await fetchAnswerVoters(null, channelId, messageId, answerId, limit, after);
		const users = response.users;
		const responseHasMore = response.has_more;
		const nextAfter = response.next_after;
		applyPollVoteFetchResult(messageId, answerId, users, {limit, after}, responseHasMore, requestId, nextAfter);
		logger.debug(`Retrieved ${response.users.length} reactions for message ${messageId}`);
		return response;
	} catch (error) {
		logger.error(`Failed to get reactions for message ${messageId}:`, error);
		PollVotes.handleFetchError(messageId, answerId, requestId);
		throw error;
	}
}

export async function loadMoreVotes(
	channelId: string,
	messageId: string,
	answerId: number,
	options: {totalCount?: number} = {},
): Promise<void> {
	const fetchStatus = PollVotes.getFetchStatus(messageId, answerId);
	if (fetchStatus === 'pending') return;
	if (!PollVotes.getHasMore(messageId, answerId)) return;
	const after = PollVotes.getLastUserId(messageId, answerId);
	if (!after) return;
	try {
		await getVotes(channelId, messageId, answerId, {limit: 100, after, totalCount: options.totalCount});
	} catch {}
}

export function addVote(i18n: I18n, channelId: string, messageId: string, answerIds: Array<number>): Promise<unknown> {
	logger.debug(`Adding vote ${answerIds} to poll from message ${messageId} in channel ${channelId}`);
	return http
		.put(Endpoints.CHANNEL_POLL_ANSWERS(channelId, messageId, '@me'), {
			body: {
				answerIds: answerIds.map((id) => String(id)),
			},
		})
		.catch((error) => onHttpError(i18n, error));
}

export function removeVote(i18n: I18n, channelId: string, messageId: string): Promise<unknown> {
	logger.debug(`Removing vote on poll from message ${messageId} in channel ${channelId}`);
	return http
		.put(Endpoints.CHANNEL_POLL_ANSWERS(channelId, messageId, '@me'), {
			body: {
				answerIds: [],
			},
		})
		.catch((error) => onHttpError(i18n, error));
}

export function fetchAnswerVoters(
	i18n: I18n | null,
	channelId: string,
	messageId: string,
	answerId: number,
	limit?: number,
	after?: string,
): Promise<FetchAnswerVotersResponse> {
	logger.debug(`Fetching voters for answer ${answerId} in poll from message ${messageId} in channel ${channelId}`);

	const query: Record<string, string> = {};
	if (limit) query.limit = `${limit}`;
	if (after) query.after = after;

	const response = http
		.get<FetchAnswerVotersResponse>(Endpoints.CHANNEL_POLL_ANSWER_VOTERS(channelId, messageId, answerId), {
			query,
		})
		.then((response) => response.body);

	if (i18n) {
		return response.catch((error) => {
			if (i18n) onHttpError(i18n, error);
			return {users: [], has_more: false, next_after: null};
		});
	}

	return response;
}
