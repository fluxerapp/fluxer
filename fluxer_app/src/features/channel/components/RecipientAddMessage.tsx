// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/GuildJoinMessage.module.css';
import {SystemMessage} from '@app/features/channel/components/SystemMessage';
import {SystemMessageUsername} from '@app/features/channel/components/SystemMessageUsername';
import {useSystemMessageData} from '@app/features/messaging/hooks/useSystemMessageData';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import Users from '@app/features/user/state/Users';
import {Trans} from '@lingui/react/macro';
import {UserPlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

interface RecipientAddMessageProps {
	message: Message;
}

export const RecipientAddMessage = observer(({message}: RecipientAddMessageProps) => {
	const {author, channel, guild} = useSystemMessageData(message);
	const addedUserId = message.mentions.length > 0 ? message.mentions[0].id : null;
	const addedUser = Users.getUser(addedUserId ?? '');
	if (!channel) {
		return null;
	}
	const messageContent = addedUser ? (
		<Trans>
			<SystemMessageUsername
				key={author.id}
				author={author}
				guild={guild}
				message={message}
				data-flx="channel.recipient-add-message.system-message-username"
			/>{' '}
			added{' '}
			<SystemMessageUsername
				key={addedUser.id}
				author={addedUser}
				guild={guild}
				message={message}
				data-flx="channel.recipient-add-message.system-message-username--2"
			/>{' '}
			to the group.
		</Trans>
	) : (
		<Trans>
			<SystemMessageUsername
				key={author.id}
				author={author}
				guild={guild}
				message={message}
				data-flx="channel.recipient-add-message.system-message-username--3"
			/>{' '}
			added someone to the group.
		</Trans>
	);
	return (
		<SystemMessage
			icon={UserPlusIcon}
			iconWeight="bold"
			iconClassname={styles.icon}
			message={message}
			messageContent={messageContent}
			data-flx="channel.recipient-add-message.system-message"
		/>
	);
});
