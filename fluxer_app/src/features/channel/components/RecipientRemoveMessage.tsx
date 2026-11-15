// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/RecipientRemoveMessage.module.css';
import {SystemMessage} from '@app/features/channel/components/SystemMessage';
import {SystemMessageUsername} from '@app/features/channel/components/SystemMessageUsername';
import {useSystemMessageData} from '@app/features/messaging/hooks/useSystemMessageData';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import Users from '@app/features/user/state/Users';
import {Trans} from '@lingui/react/macro';
import {UserMinusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface RecipientRemoveMessageProps {
	message: Message;
}

export const RecipientRemoveMessage = observer(({message}: RecipientRemoveMessageProps) => {
	const {author, channel, guild} = useSystemMessageData(message);
	const removedUserId = message.mentions.length > 0 ? message.mentions[0].id : null;
	const removedUser = Users.getUser(removedUserId ?? '');
	if (!channel) {
		return null;
	}
	const isSelfRemove = removedUserId === author.id;
	let messageContent: React.ReactNode;
	if (isSelfRemove) {
		messageContent = (
			<Trans>
				<SystemMessageUsername
					key={author.id}
					author={author}
					guild={guild}
					message={message}
					data-flx="channel.recipient-remove-message.system-message-username"
				/>{' '}
				has left the group.
			</Trans>
		);
	} else if (removedUser) {
		messageContent = (
			<Trans>
				<SystemMessageUsername
					key={author.id}
					author={author}
					guild={guild}
					message={message}
					data-flx="channel.recipient-remove-message.system-message-username--2"
				/>{' '}
				removed{' '}
				<SystemMessageUsername
					key={removedUser.id}
					author={removedUser}
					guild={guild}
					message={message}
					data-flx="channel.recipient-remove-message.system-message-username--3"
				/>{' '}
				from the group.
			</Trans>
		);
	} else {
		messageContent = (
			<Trans>
				<SystemMessageUsername
					key={author.id}
					author={author}
					guild={guild}
					message={message}
					data-flx="channel.recipient-remove-message.system-message-username--4"
				/>{' '}
				removed someone from the group.
			</Trans>
		);
	}
	return (
		<SystemMessage
			icon={UserMinusIcon}
			iconWeight="bold"
			iconClassname={styles.icon}
			message={message}
			messageContent={messageContent}
			data-flx="channel.recipient-remove-message.system-message"
		/>
	);
});
