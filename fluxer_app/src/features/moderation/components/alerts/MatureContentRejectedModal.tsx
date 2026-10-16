// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import Channels from '@app/features/channel/state/Channels';
import {formatUserSettingsPath} from '@app/features/user/components/settings_utils/SettingsConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const MATURE_CONTENT_NOT_ALLOWED_DESCRIPTOR = msg({
	message: 'Mature content not allowed',
	comment: 'Error message in the mature content rejected modal.',
});
const THIS_CHANNEL_IS_NOT_MARKED_FOR_MATURE_CONTENT_DESCRIPTOR = msg({
	message:
		'This channel is not marked for mature content. Mature content can only be sent in channels marked for mature content. Ask a moderator to update this channel if appropriate.',
	comment: 'Label in the mature content rejected modal.',
});
const SOMEONE_IN_THIS_CONVERSATION_BLOCKS_SENSITIVE_MEDIA_DESCRIPTOR = msg({
	message:
		'You or the person you are messaging has chosen to block sensitive media in direct messages. Each of you can change this in {sensitiveContentSettingsPath}.',
	comment:
		'Label in the mature content rejected modal for one-to-one direct messages. Preserve {sensitiveContentSettingsPath}; it is a user settings path inserted by code.',
});
const THIS_GROUP_IS_NOT_MARKED_FOR_MATURE_CONTENT_DESCRIPTOR = msg({
	message:
		'This group is not marked for mature content. The group owner can turn on Mature content in the group settings if everyone in the group is 18 or older.',
	comment: 'Label in the mature content rejected modal for group direct messages.',
});

interface MatureContentRejectedModalProps {
	channelId?: string;
}

export const MatureContentRejectedModal = observer(({channelId}: MatureContentRejectedModalProps) => {
	const {i18n} = useLingui();
	const channelType = channelId ? Channels.getChannel(channelId)?.type : undefined;
	let message = i18n._(THIS_CHANNEL_IS_NOT_MARKED_FOR_MATURE_CONTENT_DESCRIPTOR);
	if (channelType === ChannelTypes.GROUP_DM) {
		message = i18n._(THIS_GROUP_IS_NOT_MARKED_FOR_MATURE_CONTENT_DESCRIPTOR);
	} else if (channelType === ChannelTypes.DM) {
		message = i18n._(SOMEONE_IN_THIS_CONVERSATION_BLOCKS_SENSITIVE_MEDIA_DESCRIPTOR, {
			sensitiveContentSettingsPath: formatUserSettingsPath(i18n, 'privacy_safety', 'sensitive-content'),
		});
	}
	return (
		<GenericErrorModal
			title={i18n._(MATURE_CONTENT_NOT_ALLOWED_DESCRIPTOR)}
			message={message}
			data-flx="moderation.mature-content-rejected-modal.confirm-modal"
		/>
	);
});
