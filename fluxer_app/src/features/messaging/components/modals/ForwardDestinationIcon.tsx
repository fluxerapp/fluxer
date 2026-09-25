// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ForwardDestinationOption} from '@app/features/app/components/dialogs/shared/UseForwardDestinations';
import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import styles from '@app/features/messaging/components/modals/ForwardDestinationIcon.module.css';
import {Avatar} from '@app/features/ui/components/Avatar';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import Users from '@app/features/user/state/Users';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';

const DESTINATION_ICON_SIZE = 32;

function BadgedIcon({badge, children}: {badge: ReactNode; children: ReactNode}) {
	return (
		<div className={styles.frame} data-flx="messaging.forward-destination-icon.frame">
			<div className={styles.cutout} data-flx="messaging.forward-destination-icon.cutout">
				{children}
			</div>
			<div className={styles.badge} data-flx="messaging.forward-destination-icon.badge">
				{badge}
			</div>
		</div>
	);
}

function renderChannelBadge(channel: Channel) {
	return ChannelUtils.getIcon(channel, {className: styles.badgeIcon, weight: 'bold'});
}

function renderFallbackIcon(channel: Channel) {
	return ChannelUtils.getIcon(channel, {className: styles.fallbackIcon, weight: 'bold'});
}

function renderGuildChannelIcon(channel: Channel) {
	const guild = channel.guildId == null ? undefined : Guilds.getGuild(channel.guildId);
	if (guild == null) {
		return renderFallbackIcon(channel);
	}
	return (
		<BadgedIcon
			badge={renderChannelBadge(channel)}
			data-flx="messaging.forward-destination-icon.render-guild-channel-icon.badged-icon"
		>
			<GuildIcon
				id={guild.id}
				name={guild.name}
				icon={guild.icon}
				sizePx={DESTINATION_ICON_SIZE}
				data-flx="messaging.forward-destination-icon.guild-icon"
			/>
		</BadgedIcon>
	);
}

function renderPersonalNotesIcon(channel: Channel) {
	const currentUser = Users.currentUser;
	if (currentUser == null) {
		return renderFallbackIcon(channel);
	}
	return (
		<BadgedIcon
			badge={renderChannelBadge(channel)}
			data-flx="messaging.forward-destination-icon.render-personal-notes-icon.badged-icon"
		>
			<Avatar
				user={currentUser}
				size={DESTINATION_ICON_SIZE}
				status={null}
				data-flx="messaging.forward-destination-icon.avatar"
			/>
		</BadgedIcon>
	);
}

export const ForwardDestinationIcon = observer(function ForwardDestinationIcon({
	option,
}: {
	option: ForwardDestinationOption;
}) {
	if (option.user != null) {
		return (
			<div className={styles.frame} data-flx="messaging.forward-destination-icon.frame--4">
				<StatusAwareAvatar
					user={option.user}
					size={DESTINATION_ICON_SIZE}
					data-flx="messaging.forward-destination-icon.status-aware-avatar"
				/>
			</div>
		);
	}
	const channel = option.channel;
	if (channel == null) return null;
	switch (channel.type) {
		case ChannelTypes.GROUP_DM:
			return (
				<div className={styles.frame} data-flx="messaging.forward-destination-icon.frame--5">
					<GroupDMAvatar
						channel={channel}
						size={DESTINATION_ICON_SIZE}
						data-flx="messaging.forward-destination-icon.group-dm-avatar"
					/>
				</div>
			);
		case ChannelTypes.DM_PERSONAL_NOTES:
			return renderPersonalNotesIcon(channel);
		default:
			return renderGuildChannelIcon(channel);
	}
});
