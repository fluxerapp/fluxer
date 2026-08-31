// SPDX-License-Identifier: AGPL-3.0-or-later

import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import styles from '@app/features/theme/styles/Message.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {clsx} from 'clsx';
import React from 'react';

export const SystemMessageMessageLink = React.forwardRef<
	HTMLElement,
	{
		linkText: string;
		guildId?: string;
		linkedMessage?: Message | null;
	}
>(({linkText, guildId, linkedMessage}, ref) => {
	const activate = () => {
		if (linkedMessage)
			NavigationCommands.navigateToMessage(guildId, linkedMessage.channelId, linkedMessage.id, 'push');
	};

	return (
		<FocusRing ref={ref} data-flx="channel.system-message-message-link.focus-ring--2">
			<span
				role="button"
				tabIndex={0}
				// aria-roledescription={i18n._(MESSAGE_LINK_DESCRIPTOR)}
				onClick={(e) => {
					e.stopPropagation();
					activate();
				}}
				onKeyDown={(e) => {
					if (!isKeyboardActivationKey(e.key)) return;
					e.preventDefault();
					e.stopPropagation();
					activate();
				}}
				className={clsx(styles.systemMessageLink)}
				data-poll-message-id={linkedMessage?.id}
				data-guild-id={guildId}
				data-flx="channel.system-message-message-link.system-message-link"
			>
				{linkText}
			</span>
		</FocusRing>
	);
});
