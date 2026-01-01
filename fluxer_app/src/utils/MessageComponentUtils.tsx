/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {MessageTypes} from '~/Constants';
import {CallMessage} from '~/components/channel/CallMessage';
import {ChannelIconChangeMessage} from '~/components/channel/ChannelIconChangeMessage';
import {ChannelNameChangeMessage} from '~/components/channel/ChannelNameChangeMessage';
import {GuildJoinMessage} from '~/components/channel/GuildJoinMessage';
import {PinSystemMessage} from '~/components/channel/PinSystemMessage';
import {RecipientAddMessage} from '~/components/channel/RecipientAddMessage';
import {RecipientRemoveMessage} from '~/components/channel/RecipientRemoveMessage';
import {UnknownMessage} from '~/components/channel/UnknownMessage';
import {UserMessage} from '~/components/channel/UserMessage';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {MessageRecord} from '~/records/MessageRecord';
import UserStore from '~/stores/UserStore';

export const getMessageComponent = (
	_channel: ChannelRecord,
	message: MessageRecord,
	forceUnknownMessageType: boolean,
) => {
	const currentUser = UserStore.getCurrentUser();
	if (forceUnknownMessageType && currentUser && message.author.id === currentUser.id) {
		return <UnknownMessage />;
	}

	switch (message.type) {
		case MessageTypes.USER_JOIN:
			return <GuildJoinMessage message={message} />;
		case MessageTypes.CHANNEL_PINNED_MESSAGE:
			return <PinSystemMessage message={message} />;
		case MessageTypes.RECIPIENT_ADD:
			return <RecipientAddMessage message={message} />;
		case MessageTypes.RECIPIENT_REMOVE:
			return <RecipientRemoveMessage message={message} />;
		case MessageTypes.CALL:
			return <CallMessage message={message} />;
		case MessageTypes.CHANNEL_NAME_CHANGE:
			return <ChannelNameChangeMessage message={message} />;
		case MessageTypes.CHANNEL_ICON_CHANGE:
			return <ChannelIconChangeMessage message={message} />;
		case MessageTypes.DEFAULT:
		case MessageTypes.REPLY:
		case MessageTypes.CLIENT_SYSTEM:
			return <UserMessage />;
		default:
			return <UnknownMessage />;
	}
};
