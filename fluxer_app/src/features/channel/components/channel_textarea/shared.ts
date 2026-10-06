// SPDX-License-Identifier: AGPL-3.0-or-later

import {CANNOT_SEND_MESSAGES_IN_CHANNEL_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {msg} from '@lingui/core/macro';

export const MESSAGE_CHANNEL_DESCRIPTOR = msg({
	message: 'Message #{channelName}',
	comment:
		'Placeholder text in the message textarea of a community channel. The # is literal; channelName is the channel name, or the word "channel" when the channel has no name.',
});
export const YOU_DO_NOT_HAVE_PERMISSION_TO_SEND_MESSAGES_DESCRIPTOR = CANNOT_SEND_MESSAGES_IN_CHANNEL_DESCRIPTOR;
export const CHANNEL_DESCRIPTOR = msg({
	message: 'channel',
	comment:
		'Fallback channel-name placeholder used inside the textarea message hint when no name is available. Lowercase.',
});
export const MESSAGE_USER_DESCRIPTOR = msg({
	message: 'Message @{userName}',
	comment:
		'Placeholder text in the message textarea of a one-to-one DM. The @ is literal; userName is the display name of the other person.',
});
export const MESSAGE_GROUP_DESCRIPTOR = msg({
	message: 'Message {groupName}',
	comment: 'Placeholder text in the message textarea of a group DM; groupName is the name of the group.',
});
export const OPEN_MENU_DESCRIPTOR = msg({
	message: 'Open menu',
	comment: 'Accessible label for the plus button that opens the textarea attachment and customization menu.',
});
