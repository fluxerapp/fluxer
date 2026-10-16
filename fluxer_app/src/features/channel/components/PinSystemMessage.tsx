// SPDX-License-Identifier: AGPL-3.0-or-later

import {SystemMessage} from '@app/features/channel/components/SystemMessage';
import {SystemMessageUsername} from '@app/features/channel/components/SystemMessageUsername';
import {useSystemMessageData} from '@app/features/messaging/hooks/useSystemMessageData';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import {ComponentBus} from '@app/features/platform/utils/ComponentBus';
import styles from '@app/features/theme/styles/Message.module.css';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Trans} from '@lingui/react/macro';
import {PushPinIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

interface PinSystemMessageProps {
	message: Message;
}

export const PinSystemMessage = observer(({message}: PinSystemMessageProps) => {
	const {author, channel, guild} = useSystemMessageData(message);
	const mobileLayout = MobileLayout;
	const jumpToMessage = useCallback(() => {
		if (message.messageReference?.message_id) {
			goToMessage(message.channelId, message.messageReference.message_id, {
				returnToMessageId: message.id,
				returnChannelId: message.channelId,
			});
		}
	}, [message.channelId, message.id, message.messageReference?.message_id]);
	const openPins = useCallback(() => {
		if (mobileLayout.enabled) {
			ComponentBus.dispatch('CHANNEL_DETAILS_OPEN', {
				initialTab: 'pins',
			});
		} else {
			ComponentBus.dispatch('CHANNEL_PINS_OPEN');
		}
	}, [mobileLayout.enabled]);
	if (!channel) {
		return null;
	}
	const messageContent = (
		<Trans>
			<SystemMessageUsername
				key={author.id}
				author={author}
				guild={guild}
				message={message}
				data-flx="channel.pin-system-message.system-message-username"
			/>{' '}
			pinned{' '}
			<button
				key={`pin-${message.id}`}
				type="button"
				className={styles.systemMessageLink}
				onClick={jumpToMessage}
				data-flx="channel.pin-system-message.system-message-link.jump-to-message.button"
			>
				a message
			</button>{' '}
			to this channel. See{' '}
			<button
				key={`pin-all-${message.id}`}
				type="button"
				className={styles.systemMessageLink}
				onClick={openPins}
				data-flx="channel.pin-system-message.system-message-link.open-pins.button"
			>
				all pinned messages
			</button>
			.
		</Trans>
	);
	return (
		<SystemMessage
			icon={PushPinIcon}
			iconWeight="fill"
			message={message}
			messageContent={messageContent}
			data-flx="channel.pin-system-message.system-message"
		/>
	);
});
