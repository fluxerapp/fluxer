// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HandlerHost} from '@app/features/app/keybindings/keybind_manager/handlers/types';
import {
	requestAdjacentMessageReply,
	requestCopyMessageId,
	requestCopyMessageLink,
	requestCopyMessageText,
	requestDeleteMessage,
	requestMarkMessageUnread,
	requestMessageForward,
	requestMessagePin,
	requestMessageReply,
	requestSpeakMessage,
	requestToggleBookmark,
	requestToggleSuppressEmbeds,
	startAdjacentMessageEdit,
	startMessageEdit,
	triggerAddReaction,
} from '@app/features/channel/components/MessageActionUtils';
import MessageFocus from '@app/features/messaging/state/MessageFocus';
import type {I18n} from '@lingui/core';

export function registerMessageHandlers(host: HandlerHost, i18n: I18n): void {
	host.register('message_edit', ({type, context}) => {
		if (type !== 'press') return;
		const message = context?.focusedMessage ?? MessageFocus.getFocusedMessage();
		if (!message) return;
		startMessageEdit(message);
	});
	host.register('message_delete', ({type, context}) => {
		if (type !== 'press') return;
		const message = context?.focusedMessage ?? MessageFocus.getFocusedMessage();
		if (!message) return;
		requestDeleteMessage(message, i18n);
	});
	host.register('message_pin', ({type, context}) => {
		if (type !== 'press') return;
		const message = context?.focusedMessage ?? MessageFocus.getFocusedMessage();
		if (!message) return;
		requestMessagePin(message, i18n);
	});
	host.register('message_react', ({type, context}) => {
		if (type !== 'press') return;
		const message = context?.focusedMessage ?? MessageFocus.getFocusedMessage();
		if (!message) return;
		triggerAddReaction(message);
	});
	host.register('message_reply', ({type, context}) => {
		if (type !== 'press') return;
		const message = context?.focusedMessage ?? MessageFocus.getFocusedMessage();
		if (!message) return;
		requestMessageReply(message, {sourceChannel: context?.focusedChannel});
	});
	host.register('message_reply_prev', ({type}) => {
		if (type !== 'press' || !host.currentChannelId) return;
		requestAdjacentMessageReply(host.currentChannelId, -1);
	});
	host.register('message_reply_next', ({type}) => {
		if (type !== 'press' || !host.currentChannelId) return;
		requestAdjacentMessageReply(host.currentChannelId, 1);
	});
	host.register('message_edit_prev', ({type}) => {
		if (type !== 'press' || !host.currentChannelId) return;
		startAdjacentMessageEdit(host.currentChannelId, -1);
	});
	host.register('message_edit_next', ({type}) => {
		if (type !== 'press' || !host.currentChannelId) return;
		startAdjacentMessageEdit(host.currentChannelId, 1);
	});
	host.register('message_forward', ({type, context}) => {
		if (type !== 'press') return;
		const message = context?.focusedMessage ?? MessageFocus.getFocusedMessage();
		if (!message) return;
		requestMessageForward(message, context?.focusedChannel);
	});
	host.register('message_copy_text', ({type, context}) => {
		if (type !== 'press') return;
		const message = context?.focusedMessage ?? MessageFocus.getFocusedMessage();
		if (!message) return;
		requestCopyMessageText(message, i18n);
	});
	host.register('message_speak', ({type, context}) => {
		if (type !== 'press') return;
		const message = context?.focusedMessage ?? MessageFocus.getFocusedMessage();
		if (!message) return;
		requestSpeakMessage(message);
	});
	host.register('message_mark_unread', ({type, context}) => {
		if (type !== 'press') return;
		const message = context?.focusedMessage ?? MessageFocus.getFocusedMessage();
		if (!message) return;
		requestMarkMessageUnread(message);
	});
	host.register('message_bookmark', ({type, context}) => {
		if (type !== 'press') return;
		const message = context?.focusedMessage ?? MessageFocus.getFocusedMessage();
		if (!message) return;
		requestToggleBookmark(message, i18n);
	});
	host.register('message_toggle_embeds', ({type, context}) => {
		if (type !== 'press') return;
		const message = context?.focusedMessage ?? MessageFocus.getFocusedMessage();
		if (!message) return;
		requestToggleSuppressEmbeds(message, i18n);
	});
	host.register('message_copy_link', ({type, context}) => {
		if (type !== 'press') return;
		const message = context?.focusedMessage ?? MessageFocus.getFocusedMessage();
		if (!message) return;
		requestCopyMessageLink(message, i18n, context?.focusedChannel);
	});
	host.register('message_copy_id', ({type, context}) => {
		if (type !== 'press') return;
		const message = context?.focusedMessage ?? MessageFocus.getFocusedMessage();
		if (!message) return;
		requestCopyMessageId(message, i18n);
	});
}
