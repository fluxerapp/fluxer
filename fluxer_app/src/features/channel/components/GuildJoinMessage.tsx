// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/GuildJoinMessage.module.css';
import {SystemMessage} from '@app/features/channel/components/SystemMessage';
import {SystemMessageUsername} from '@app/features/channel/components/SystemMessageUsername';
import {useSystemMessageData} from '@app/features/messaging/hooks/useSystemMessageData';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {SystemMessageUtils} from '@app/features/messaging/utils/SystemMessageUtils';
import {ArrowRightIcon} from '@app/features/ui/icons/DirectionalIcons';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

interface GuildJoinMessageProps {
	message: Message;
}

export const GuildJoinMessage = observer(({message}: GuildJoinMessageProps) => {
	const {i18n} = useLingui();
	const {author, channel, guild} = useSystemMessageData(message);
	if (!channel) {
		return null;
	}
	const messageContent = SystemMessageUtils.getGuildJoinMessage(
		message.id,
		<SystemMessageUsername
			author={author}
			guild={guild}
			message={message}
			key={author.id}
			data-flx="channel.guild-join-message.system-message-username"
		/>,
		i18n,
	);
	return (
		<SystemMessage
			icon={ArrowRightIcon}
			iconWeight="bold"
			iconClassname={styles.icon}
			message={message}
			messageContent={messageContent}
			data-flx="channel.guild-join-message.system-message"
		/>
	);
});
