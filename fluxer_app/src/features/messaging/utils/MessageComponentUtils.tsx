// SPDX-License-Identifier: AGPL-3.0-or-later

import {CallMessage} from '@app/features/channel/components/CallMessage';
import {ChannelIconChangeMessage} from '@app/features/channel/components/ChannelIconChangeMessage';
import {ChannelNameChangeMessage} from '@app/features/channel/components/ChannelNameChangeMessage';
import {GuildJoinMessage} from '@app/features/channel/components/GuildJoinMessage';
import {PinSystemMessage} from '@app/features/channel/components/PinSystemMessage';
import {PollClosedMessage} from '@app/features/channel/components/PollClosedMessage';
import {RecipientAddMessage} from '@app/features/channel/components/RecipientAddMessage';
import {RecipientRemoveMessage} from '@app/features/channel/components/RecipientRemoveMessage';
import {UnknownMessage} from '@app/features/channel/components/UnknownMessage';
import {UserMessage} from '@app/features/channel/components/UserMessage';
import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import Users from '@app/features/user/state/Users';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type React from 'react';

export function getMessageComponent(
	message: Message,
	_channel: Channel,
	forceUnknownMessageType = false,
): React.ReactElement {
	const currentUser = Users.getCurrentUser();
	if (forceUnknownMessageType && currentUser && message.author.id === currentUser.id) {
		return <UnknownMessage data-flx="messaging.message-component-utils.get-message-component.unknown-message" />;
	}
	switch (message.type) {
		case MessageTypes.USER_JOIN:
			return (
				<GuildJoinMessage
					message={message}
					data-flx="messaging.message-component-utils.get-message-component.guild-join-message"
				/>
			);
		case MessageTypes.CHANNEL_PINNED_MESSAGE:
			return (
				<PinSystemMessage
					message={message}
					data-flx="messaging.message-component-utils.get-message-component.pin-system-message"
				/>
			);
		case MessageTypes.RECIPIENT_ADD:
			return (
				<RecipientAddMessage
					message={message}
					data-flx="messaging.message-component-utils.get-message-component.recipient-add-message"
				/>
			);
		case MessageTypes.RECIPIENT_REMOVE:
			return (
				<RecipientRemoveMessage
					message={message}
					data-flx="messaging.message-component-utils.get-message-component.recipient-remove-message"
				/>
			);
		case MessageTypes.CALL:
			return (
				<CallMessage
					message={message}
					data-flx="messaging.message-component-utils.get-message-component.call-message"
				/>
			);
		case MessageTypes.CHANNEL_NAME_CHANGE:
			return (
				<ChannelNameChangeMessage
					message={message}
					data-flx="messaging.message-component-utils.get-message-component.channel-name-change-message"
				/>
			);
		case MessageTypes.CHANNEL_ICON_CHANGE:
			return (
				<ChannelIconChangeMessage
					message={message}
					data-flx="messaging.message-component-utils.get-message-component.channel-icon-change-message"
				/>
			);
		case MessageTypes.POLL_RESULT:
			return (
				<PollClosedMessage
					message={message}
					data-flx="messaging.message-component-utils.get-message-component.poll-closed-message"
				/>
			);
		case MessageTypes.DEFAULT:
		case MessageTypes.REPLY:
		case MessageTypes.CLIENT_SYSTEM:
			return <UserMessage data-flx="messaging.message-component-utils.get-message-component.user-message" />;
		default:
			return <UnknownMessage data-flx="messaging.message-component-utils.get-message-component.unknown-message--2" />;
	}
}
