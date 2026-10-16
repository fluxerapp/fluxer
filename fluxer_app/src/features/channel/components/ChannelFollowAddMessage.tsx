// SPDX-License-Identifier: AGPL-3.0-or-later

import {CrosspostCommunityPopout} from '@app/features/channel/components/CrosspostCommunityPopout';
import joinStyles from '@app/features/channel/components/GuildJoinMessage.module.css';
import {SystemMessage} from '@app/features/channel/components/SystemMessage';
import {SystemMessageUsername} from '@app/features/channel/components/SystemMessageUsername';
import {useSystemMessageData} from '@app/features/messaging/hooks/useSystemMessageData';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import styles from '@app/features/theme/styles/Message.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Trans} from '@lingui/react/macro';
import {ArrowRightIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';

interface ChannelFollowAddMessageProps {
	message: Message;
}

export const ChannelFollowAddMessage = observer(({message}: ChannelFollowAddMessageProps) => {
	const {author, guild} = useSystemMessageData(message);
	const sourceComponent = (
		<CrosspostCommunityPopout
			key="source"
			message={message}
			data-flx="channel.channel-follow-add-message.crosspost-community-popout"
		>
			<FocusRing data-flx="channel.channel-follow-add-message.focus-ring">
				<button
					type="button"
					className={styles.systemMessageLink}
					data-flx="channel.channel-follow-add-message.system-message-link"
				>
					{message.content}
				</button>
			</FocusRing>
		</CrosspostCommunityPopout>
	);
	const messageContent = (
		<Trans comment="System message shown in a channel after someone makes it follow an announcement channel. The source is the follower webhook name, usually the source community and channel, for example 'Fluxer News #announcements'.">
			<SystemMessageUsername
				key={author.id}
				author={author}
				guild={guild}
				message={message}
				data-flx="channel.channel-follow-add-message.system-message-username"
			/>{' '}
			followed {sourceComponent} into this channel. Messages published there will appear here.
		</Trans>
	);
	return (
		<SystemMessage
			icon={ArrowRightIcon}
			iconWeight="bold"
			iconClassname={joinStyles.icon}
			message={message}
			messageContent={messageContent}
			data-flx="channel.channel-follow-add-message.system-message"
		/>
	);
});
