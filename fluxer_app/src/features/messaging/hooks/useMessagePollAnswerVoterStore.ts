// SPDX-License-Identifier: AGPL-3.0-or-later

import PollVotes, {type FetchStatus} from '@app/features/messaging/state/PollVotes';
import type {User} from '@app/features/user/models/User';
import type {MessagePollAnswerCount} from '@fluxer/schema/src/domains/message/PollSchemas';
import {useCallback, useSyncExternalStore} from 'react';

interface PollVotersSnapshot {
	voters: ReadonlyArray<User>;
	fetchStatus: FetchStatus;
	hasMore: boolean;
	initialFetchLimit: number;
}

export function usePollVotes(messageId: string): ReadonlyArray<MessagePollAnswerCount> {
	const subscribe = useCallback((listener: () => void) => PollVotes.subscribeMessage(messageId, listener), [messageId]);
	const getSnapshot = useCallback(() => PollVotes.getMessagePollVotes(messageId), [messageId]);
	return useSyncExternalStore(subscribe, getSnapshot, getSnapshot);
}

export function usePollVoteUsers(messageId: string, answerId?: number): PollVotersSnapshot {
	const subscribe = useCallback(
		(listener: () => void) => {
			if (!answerId) return () => {};
			return PollVotes.subscribePollVote(messageId, answerId, listener);
		},
		[answerId, messageId],
	);
	const getSnapshot = useCallback(
		() => (answerId ? PollVotes.getPollVoteVersion(messageId, answerId) : 0),
		[answerId, messageId],
	);
	useSyncExternalStore(subscribe, getSnapshot, getSnapshot);
	return {
		voters: answerId ? PollVotes.getPollVotes(messageId, answerId) : [],
		fetchStatus: answerId ? PollVotes.getFetchStatus(messageId, answerId) : 'idle',
		hasMore: answerId ? PollVotes.getHasMore(messageId, answerId) : false,
		initialFetchLimit: answerId ? PollVotes.getInitialFetchLimit(messageId, answerId) : 0,
	};
}
