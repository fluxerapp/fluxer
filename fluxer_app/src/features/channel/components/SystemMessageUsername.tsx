// SPDX-License-Identifier: AGPL-3.0-or-later

import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {PreloadableUserPopout} from '@app/features/channel/components/PreloadableUserPopout';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildMembers from '@app/features/member/state/GuildMembers';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import styles from '@app/features/theme/styles/Message.module.css';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useRef} from 'react';

export const SystemMessageUsername = observer(
	React.forwardRef<
		HTMLElement,
		{
			author: User;
			guild?: Guild;
			message: Message;
		}
	>(({author, guild, message}, ref) => {
		const usernameRef = useRef<HTMLSpanElement | null>(null);
		const contextMenuOpen = useContextMenuHoverState(usernameRef);
		const member = GuildMembers.getMember(guild?.id ?? '', author.id);
		return (
			<PreloadableUserPopout
				ref={ref}
				user={author}
				isWebhook={false}
				guildId={guild?.id}
				channelId={message.channelId}
				message={message}
				enableLongPressActions={true}
				longPressWrapperElement="span"
				data-flx="channel.system-message-username.preloadable-user-popout"
			>
				<span
					className={clsx(styles.systemMessageLink, contextMenuOpen && styles.contextMenuUnderline)}
					role="button"
					tabIndex={-1}
					style={{color: member?.getColorString()}}
					data-user-id={author.id}
					data-guild-id={guild?.id}
					ref={usernameRef}
					data-flx="channel.system-message-username.system-message-link"
				>
					{NicknameUtils.getNickname(author, guild?.id)}
				</span>
			</PreloadableUserPopout>
		);
	}),
);
