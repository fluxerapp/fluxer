// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import * as PollCommands from '@app/features/messaging/commands/PollCommands';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import Messages from '@app/features/messaging/state/MessagingMessages';
import type {User} from '@app/features/user/models/User';
import type {MessagePoll, MessagePollAnswerCount} from '@fluxer/schema/src/domains/message/PollSchemas';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {getPollVoteKey} from '../state/PollVotes';
import {usePollVotes, usePollVoteUsers} from './useMessagePollAnswerVoterStore';

interface UseMessagePollAnswerVotersStateOptions {
	channelId: string;
	messageId: string;
	poll: MessagePoll;
	message?: Message | null;
	openToAnswerId?: number;
	isOpen?: boolean;
	onMissingMessage?: () => void;
}

interface MessagePollAnswerVotersState {
	message: Message | undefined;
	votes: ReadonlyArray<MessagePollAnswerCount>;
	selectedAnswerId: number;
	setSelectedAnswerId: (vote: number) => void;
	voters: ReadonlyArray<User>;
	fetchStatus: string;
	isLoading: boolean;
	hasMore: boolean;
	loadMore: () => void;
	guildId?: string;
	voterScrollerKey: string;
}

export function useMessagePollAnswerVotersState({
	channelId,
	messageId,
	poll,
	message: messageFallback,
	openToAnswerId,
	isOpen = true,
	onMissingMessage,
}: UseMessagePollAnswerVotersStateOptions): MessagePollAnswerVotersState {
	const [selectedAnswerId, setSelectedAnswerId] = useState<number>(openToAnswerId ?? 0);
	const messageFallbackMatches =
		messageFallback?.id === messageId && messageFallback.channelId === channelId ? messageFallback : undefined;
	const message = Messages.getMessage(channelId, messageId) ?? messageFallbackMatches;
	const votes = usePollVotes(messageId);
	const channel = Channels.getChannel(channelId);
	const guildId = channel?.guildId;

	useEffect(() => {
		if (openToAnswerId) {
			setSelectedAnswerId(openToAnswerId);
		}
	}, [openToAnswerId]);
	useEffect(() => {
		if (!isOpen) {
			return;
		}
		if (!message || !poll.answers || poll.answers.length === 0) {
			onMissingMessage?.();
			return;
		}
		if (!selectedAnswerId) setSelectedAnswerId(1);
	}, [isOpen, message, onMissingMessage, poll, selectedAnswerId]);
	const {voters, fetchStatus, hasMore, initialFetchLimit} = usePollVoteUsers(messageId, selectedAnswerId);
	const isLoading = fetchStatus === 'pending';
	const voterScrollerKey = useMemo(() => {
		if (!message || !selectedAnswerId) {
			return 'message-votes-voter-scroller';
		}
		return `message-votes-voter-scroller-${getPollVoteKey(message.id, selectedAnswerId)}`;
	}, [message?.id, selectedAnswerId]);
	useEffect(() => {
		if (!isOpen) {
			return;
		}
		if (!selectedAnswerId || !message) {
			return;
		}
		if (fetchStatus === 'pending') {
			return;
		}
		const voteOnMessage = votes.find((vote) => vote.id === selectedAnswerId);
		if (!voteOnMessage || !voteOnMessage.count || voteOnMessage.count === 0) {
			return;
		}
		const desiredInitialLimit = Math.min(100, voteOnMessage.count);
		if (
			fetchStatus !== 'idle' &&
			(fetchStatus !== 'success' || initialFetchLimit >= desiredInitialLimit || voters.length >= desiredInitialLimit)
		) {
			return;
		}
		PollCommands.getVotes(channelId, messageId, selectedAnswerId, {
			limit: 100,
			totalCount: voteOnMessage.count,
		}).catch(() => {});
	}, [channelId, fetchStatus, initialFetchLimit, isOpen, message, messageId, voters.length, selectedAnswerId]);
	const loadMore = useCallback(() => {
		if (!selectedAnswerId) return;
		PollCommands.loadMoreVotes(channelId, messageId, selectedAnswerId, {
			totalCount: poll.results?.answer_counts?.find((answerCount) => answerCount.id === selectedAnswerId)?.count ?? 0,
		});
	}, [channelId, messageId, selectedAnswerId, poll.results]);
	return {
		message,
		votes,
		selectedAnswerId,
		setSelectedAnswerId,
		voters,
		fetchStatus,
		isLoading,
		hasMore,
		loadMore,
		guildId,
		voterScrollerKey,
	};
}
