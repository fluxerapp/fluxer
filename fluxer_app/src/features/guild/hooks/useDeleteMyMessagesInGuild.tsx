// SPDX-License-Identifier: AGPL-3.0-or-later

import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {useConfirmDeleteMyMessages} from '@app/features/guild/hooks/useConfirmDeleteMyMessages';
import {TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback} from 'react';

const DELETE_YOUR_MESSAGES_IN_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Delete your messages in this community?',
	comment: 'Confirmation modal title for the destructive "delete all my messages in this community" action.',
});
const THIS_WILL_PERMANENTLY_DELETE_EVERY_MESSAGE_YOU_HAVE_DESCRIPTOR = msg({
	message: "Permanently delete every message you've sent here, across every channel. Can't be undone.",
	comment:
		'Confirmation modal description warning that every message the caller has sent across every channel in this community will be deleted. Cannot be undone.',
});
const DELETED_YOUR_MESSAGES_DESCRIPTOR = msg({
	message: 'Deleted your messages.',
	comment: 'Toast shown after the "delete my messages in community" request succeeds.',
});
const COULDN_T_DELETE_YOUR_MESSAGES_DESCRIPTOR = msg({
	message: "Couldn't delete your messages",
	comment: 'Error modal title shown when the "delete my messages in community" request failed.',
});

export const useDeleteMyMessagesInGuild = () => {
	const {i18n} = useLingui();
	const confirmDelete = useConfirmDeleteMyMessages();
	return useCallback(
		(guildId: string) => {
			confirmDelete({
				title: i18n._(DELETE_YOUR_MESSAGES_IN_THIS_COMMUNITY_DESCRIPTOR),
				description: i18n._(THIS_WILL_PERMANENTLY_DELETE_EVERY_MESSAGE_YOU_HAVE_DESCRIPTOR),
				run: () => GuildCommands.bulkDeleteMyMessages(guildId),
				toastText: i18n._(DELETED_YOUR_MESSAGES_DESCRIPTOR),
				errorTitle: i18n._(COULDN_T_DELETE_YOUR_MESSAGES_DESCRIPTOR),
				errorMessage: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx: 'guild.use-delete-my-messages-in-guild.confirm-modal',
			});
		},
		[i18n, confirmDelete],
	);
};
