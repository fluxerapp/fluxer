// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {buildUserMessageCopyText} from '@app/features/messaging/utils/MessageCopyTextUtils';
import {SystemMessageUtils} from '@app/features/messaging/utils/SystemMessageUtils';
import UserSettings from '@app/features/user/state/UserSettings';
import type {I18n} from '@lingui/core';

export function buildMessagePlaintextCopyText(message: Message, i18n: I18n): string | null {
	if (message.isSystemMessage()) {
		return SystemMessageUtils.stringify(message, i18n);
	}
	if (!message.isUserMessage()) {
		return null;
	}
	return buildUserMessageCopyText(message, {
		channelId: message.channelId,
		i18n,
		includeEmbeds: UserSettings.renderEmbeds && !message.suppressEmbeds,
	});
}
