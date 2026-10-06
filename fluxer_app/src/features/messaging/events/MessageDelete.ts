// SPDX-License-Identifier: AGPL-3.0-or-later

import ChannelPins from '@app/features/channel/state/ChannelPins';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import MessageReferences from '@app/features/messaging/state/MessageReferences';
import MessageReply from '@app/features/messaging/state/MessageReply';
import Messages from '@app/features/messaging/state/MessagingMessages';
import SavedMessages from '@app/features/messaging/state/SavedMessages';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import ReadStates from '@app/features/read_state/state/ReadStates';
import Notification from '@app/features/ui/state/Notification';
import TtsUtils from '@app/features/voice/utils/VoiceTtsUtils';

interface MessageDeletePayload {
	id: string;
	channel_id: string;
}

export function handleMessageDelete(data: MessageDeletePayload, _context: GatewayHandlerContext): void {
	SavedMessages.handleMessageDelete(data.id);
	ChannelPins.handleMessageDelete(data.channel_id, data.id);
	Messages.handleMessageDelete({channelId: data.channel_id, id: data.id});
	MessageReferences.handleMessageDelete(data.channel_id, data.id);
	MessageReply.handleMessageDelete(data.channel_id, data.id);
	ReadStates.handleMessageDelete({channelId: data.channel_id});
	MentionFeed.handleMessageDelete(data.id);
	Notification.handleMessageDelete({channelId: data.channel_id});
	TtsUtils.handleMessageDelete(data.channel_id, data.id);
}
