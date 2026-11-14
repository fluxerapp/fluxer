// SPDX-License-Identifier: AGPL-3.0-or-later
import {
	buildSearchResultGroups,
	buildSearchResultGroupsByMessageId,
} from '@app/features/channel/components/SearchResultGrouping';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {getCollapsedMessageGroupKey} from '@app/features/messaging/utils/MessageGroupingUtils';
import LocalUserSpamOverride from '@app/features/moderation/state/LocalUserSpamOverride';
import {useCallback, useMemo, useState} from 'react';
export function useSearchResultCollapsedGroups(results: Array<Message>, channels: Array<Channel>) {
	const [revealedGroupKeys, setRevealedGroupKeys] = useState<Set<string>>(new Set());
	const searchChannelsById = useMemo(() => new Map(channels.map((channel) => [channel.id, channel])), [channels]);
	const resultGroups = useMemo(() => buildSearchResultGroups(results), [results]);
	const resultGroupsByMessageId = useMemo(() => buildSearchResultGroupsByMessageId(resultGroups), [resultGroups]);
	const spammerOverrideVersion = LocalUserSpamOverride.version;
	const collapsedMessageVisibility = useMemo(
		() => ({
			isMessageRevealed: (message: Message) => {
				const resultGroup = resultGroupsByMessageId.get(message.id);
				if (!resultGroup) return false;
				const channel = searchChannelsById.get(message.channelId) ?? Channels.getChannel(message.channelId);
				if (!channel) return false;
				const groupKey = getCollapsedMessageGroupKey({
					channel,
					messages: resultGroup.messages,
					messageId: message.id,
					treatSpam: true,
				});
				return groupKey != null && revealedGroupKeys.has(groupKey);
			},
		}),
		[resultGroupsByMessageId, revealedGroupKeys, searchChannelsById, spammerOverrideVersion],
	);
	const handleCollapsedGroupRevealChange = useCallback(
		(groupKey: string, revealed: boolean) =>
			setRevealedGroupKeys((current) => {
				const next = new Set(current);
				if (revealed) next.add(groupKey);
				else next.delete(groupKey);
				return next;
			}),
		[],
	);
	return {
		spammerOverrideVersion,
		searchChannelsById,
		resultGroups,
		collapsedMessageVisibility,
		revealedGroupKeys,
		setRevealedGroupKeys,
		handleCollapsedGroupRevealChange,
	};
}
