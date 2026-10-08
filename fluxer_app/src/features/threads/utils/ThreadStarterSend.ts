// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import * as MessageSubmitUtils from '@app/features/messaging/utils/MessageSubmitUtils';
import Users from '@app/features/user/state/Users';
import type {MessageAttachment, MessageStickerItem} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';

export interface ThreadStarterPayload {
	content: string;
	nonce: string;
	attachments: Array<MessageAttachment>;
	hasAttachments: boolean;
	stickers: Array<MessageStickerItem>;
	favoriteMemeId?: string;
}

export function sendThreadStarter(thread: Channel, payload: ThreadStarterPayload): void {
	const currentUser = Users.getCurrentUser();
	if (!currentUser || !MessageCommands.reserveSend(thread.id, payload.nonce)) return;
	const message = MessageSubmitUtils.createOptimisticMessage(
		{
			content: payload.content,
			channelId: thread.id,
			nonce: payload.nonce,
			currentUser,
			stickers: payload.stickers,
			favoriteMemeId: payload.favoriteMemeId,
		},
		payload.attachments,
	);
	MessageCommands.createOptimistic(thread.id, message.toJSON());
	void MessageCommands.send(thread.id, {
		content: message.content,
		nonce: payload.nonce,
		hasAttachments: payload.attachments.length > 0 || payload.hasAttachments,
		allowedMentions: {replied_user: true},
		flags: message.flags,
		stickers: payload.stickers,
		favoriteMemeId: payload.favoriteMemeId,
	});
}
