// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {useConfirmDeleteMyMessages} from '@app/features/guild/hooks/useConfirmDeleteMyMessages';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {THREAD_PARENT_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback} from 'react';

const DELETE_YOUR_MESSAGES_IN_THIS_CHANNEL_DESCRIPTOR = msg({
	message: 'Delete your messages in this channel?',
	comment: 'Confirmation modal title for the destructive "delete all my messages in this channel" action.',
});
const DELETE_YOUR_MESSAGES_IN_THIS_CONVERSATION_DESCRIPTOR = msg({
	message: 'Delete your messages in this conversation?',
	comment:
		'Confirmation modal title for the destructive "delete all my messages in this DM or group DM conversation" action.',
});
const DELETE_YOUR_MESSAGES_IN_THIS_THREAD_DESCRIPTOR = msg({
	message: 'Delete your messages in this thread?',
	comment:
		'Confirmation modal title for the destructive "delete all my messages in this thread" action. Also shown for forum and media posts.',
});
const THIS_WILL_PERMANENTLY_DELETE_EVERY_MESSAGE_YOU_HAVE_DESCRIPTOR = msg({
	message: 'This will permanently delete every message you have ever sent in this channel. This cannot be undone.',
	comment:
		'Confirmation modal description warning that every message the caller has sent in this channel will be deleted. Cannot be undone.',
});
const THIS_WILL_PERMANENTLY_DELETE_EVERY_MESSAGE_YOU_HAVE_IN_THIS_CHANNEL_AND_ITS_THREADS_DESCRIPTOR = msg({
	message:
		'This will permanently delete every message you have ever sent in this channel and its threads. This cannot be undone.',
	comment:
		'Confirmation modal description warning that every message the caller has sent in this channel and in all of its threads, active or archived, will be deleted. Also shown for forum and media channels, where the threads are their posts. Cannot be undone.',
});
const THIS_WILL_PERMANENTLY_DELETE_EVERY_MESSAGE_YOU_HAVE_IN_THIS_CONVERSATION_DESCRIPTOR = msg({
	message: 'This will permanently delete every message you have ever sent in this conversation. This cannot be undone.',
	comment:
		'Confirmation modal description warning that every message the caller has sent in a DM or group DM conversation will be deleted. Cannot be undone.',
});
const THIS_WILL_PERMANENTLY_DELETE_EVERY_MESSAGE_YOU_HAVE_IN_THIS_THREAD_DESCRIPTOR = msg({
	message: 'This will permanently delete every message you have ever sent in this thread. This cannot be undone.',
	comment:
		'Confirmation modal description warning that every message the caller has sent in this thread will be deleted. Also shown for forum and media posts. Cannot be undone.',
});
const DELETED_YOUR_MESSAGES_DESCRIPTOR = msg({
	message: 'Deleted your messages.',
	comment: 'Toast shown after the "delete my messages in channel" request succeeds.',
});
const COULDN_T_DELETE_YOUR_MESSAGES_DESCRIPTOR = msg({
	message: "Couldn't delete your messages",
	comment: 'Error modal title shown when the "delete my messages in channel" request failed.',
});
const WE_COULDN_T_DELETE_YOUR_MESSAGES_RIGHT_NOW_DESCRIPTOR = msg({
	message: "We couldn't delete your messages right now. Try again in a moment.",
	comment:
		'Error modal body shown when the "delete my messages in channel" request failed. Encourages the user to retry.',
});

function getConfirmDescriptors(channel: Channel | undefined): {
	title: MessageDescriptor;
	description: MessageDescriptor;
} {
	if (channel?.type === ChannelTypes.DM || channel?.type === ChannelTypes.GROUP_DM) {
		return {
			title: DELETE_YOUR_MESSAGES_IN_THIS_CONVERSATION_DESCRIPTOR,
			description: THIS_WILL_PERMANENTLY_DELETE_EVERY_MESSAGE_YOU_HAVE_IN_THIS_CONVERSATION_DESCRIPTOR,
		};
	}
	if (channel?.isThread()) {
		return {
			title: DELETE_YOUR_MESSAGES_IN_THIS_THREAD_DESCRIPTOR,
			description: THIS_WILL_PERMANENTLY_DELETE_EVERY_MESSAGE_YOU_HAVE_IN_THIS_THREAD_DESCRIPTOR,
		};
	}
	if (channel && ThreadGuilds.isActive(channel.guildId) && THREAD_PARENT_CHANNEL_TYPES.has(channel.type)) {
		return {
			title: DELETE_YOUR_MESSAGES_IN_THIS_CHANNEL_DESCRIPTOR,
			description: THIS_WILL_PERMANENTLY_DELETE_EVERY_MESSAGE_YOU_HAVE_IN_THIS_CHANNEL_AND_ITS_THREADS_DESCRIPTOR,
		};
	}
	return {
		title: DELETE_YOUR_MESSAGES_IN_THIS_CHANNEL_DESCRIPTOR,
		description: THIS_WILL_PERMANENTLY_DELETE_EVERY_MESSAGE_YOU_HAVE_DESCRIPTOR,
	};
}

export const useDeleteMyMessagesInChannel = () => {
	const {i18n} = useLingui();
	const confirmDelete = useConfirmDeleteMyMessages();
	return useCallback(
		(channelId: string) => {
			const {title, description} = getConfirmDescriptors(Channels.getChannel(channelId));
			confirmDelete({
				title: i18n._(title),
				description: i18n._(description),
				run: () => ChannelCommands.bulkDeleteMyMessages(channelId),
				toastText: i18n._(DELETED_YOUR_MESSAGES_DESCRIPTOR),
				errorTitle: i18n._(COULDN_T_DELETE_YOUR_MESSAGES_DESCRIPTOR),
				errorMessage: i18n._(WE_COULDN_T_DELETE_YOUR_MESSAGES_RIGHT_NOW_DESCRIPTOR),
				dataFlx: 'channel.use-delete-my-messages-in-channel.confirm-modal',
			});
		},
		[i18n, confirmDelete],
	);
};
