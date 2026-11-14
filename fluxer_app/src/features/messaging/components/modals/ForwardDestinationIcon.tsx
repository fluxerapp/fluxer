// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ForwardDestinationOption} from '@app/features/app/components/dialogs/shared/UseForwardDestinations';
import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import {ChannelBadgedIcon, ChannelOriginIcon} from '@app/features/channel/components/ChannelOriginIcon';
import type {Channel} from '@app/features/channel/models/Channel';
import {Avatar} from '@app/features/ui/components/Avatar';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import Users from '@app/features/user/state/Users';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {observer} from 'mobx-react-lite';

const DESTINATION_ICON_SIZE = 32;

function renderPersonalNotesIcon(channel: Channel) {
	const currentUser = Users.currentUser;
	if (currentUser == null) {
		return (
			<ChannelOriginIcon
				channel={channel}
				size={DESTINATION_ICON_SIZE}
				data-flx="messaging.forward-destination-icon.render-personal-notes-icon.channel-origin-icon"
			/>
		);
	}
	return (
		<ChannelBadgedIcon
			channel={channel}
			size={DESTINATION_ICON_SIZE}
			data-flx="messaging.forward-destination-icon.render-personal-notes-icon.channel-badged-icon"
		>
			<Avatar
				user={currentUser}
				size={DESTINATION_ICON_SIZE}
				status={null}
				data-flx="messaging.forward-destination-icon.render-personal-notes-icon.avatar"
			/>
		</ChannelBadgedIcon>
	);
}

export const ForwardDestinationIcon = observer(function ForwardDestinationIcon({
	option,
}: {
	option: ForwardDestinationOption;
}) {
	if (option.user != null) {
		return (
			<StatusAwareAvatar
				user={option.user}
				size={DESTINATION_ICON_SIZE}
				data-flx="messaging.forward-destination-icon.status-aware-avatar"
			/>
		);
	}
	const channel = option.channel;
	if (channel == null) return null;
	switch (channel.type) {
		case ChannelTypes.GROUP_DM:
			return (
				<GroupDMAvatar
					channel={channel}
					size={DESTINATION_ICON_SIZE}
					data-flx="messaging.forward-destination-icon.group-dm-avatar"
				/>
			);
		case ChannelTypes.DM_PERSONAL_NOTES:
			return renderPersonalNotesIcon(channel);
		default:
			return (
				<ChannelOriginIcon
					channel={channel}
					size={DESTINATION_ICON_SIZE}
					data-flx="messaging.forward-destination-icon.channel-origin-icon"
				/>
			);
	}
});
