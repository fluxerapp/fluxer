// SPDX-License-Identifier: AGPL-3.0-or-later

import {openChannelFollowModal} from '@app/features/channel/components/modals/ChannelFollowModal';
import type {Channel} from '@app/features/channel/models/Channel';
import {
	canFollowAnnouncementChannel,
	FOLLOW_CHANNEL_DESCRIPTOR,
	FOLLOW_DESCRIPTOR,
} from '@app/features/channel/utils/ChannelFollowUtils';
import {Button} from '@app/features/ui/button/Button';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

export const ChannelFollowButton = observer(({channel, className}: {channel: Channel; className?: string}) => {
	const {i18n} = useLingui();
	if (!canFollowAnnouncementChannel(channel)) return null;
	return (
		<Button
			variant="secondary"
			superCompact
			fitContent
			className={className}
			aria-label={i18n._(FOLLOW_CHANNEL_DESCRIPTOR)}
			onClick={() => openChannelFollowModal(channel.id)}
			data-flx="channel.channel-header-components.channel-follow-button.button.open-follow-modal"
		>
			{i18n._(FOLLOW_DESCRIPTOR)}
		</Button>
	);
});
