// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ChannelOriginIcon.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type {CSSProperties, ReactNode} from 'react';

type OriginIconStyle = CSSProperties & {'--origin-icon-size': string};

interface ChannelBadgedIconProps {
	channel: Channel;
	children: ReactNode;
	highlighted?: boolean;
	size: number;
}

interface ChannelOriginIconProps {
	channel: Channel;
	highlighted?: boolean;
	size: number;
}

function originIconStyle(size: number): OriginIconStyle {
	return {'--origin-icon-size': remFromPx(size)};
}

export function ChannelBadgedIcon({channel, children, highlighted = false, size}: ChannelBadgedIconProps) {
	return (
		<div
			className={styles.frame}
			style={originIconStyle(size)}
			data-flx="channel.channel-origin-icon.channel-badged-icon.frame"
		>
			<div className={styles.cutout} data-flx="channel.channel-origin-icon.channel-badged-icon.cutout">
				{children}
			</div>
			<div
				className={clsx(styles.badge, highlighted && styles.badgeHighlighted)}
				data-flx="channel.channel-origin-icon.channel-badged-icon.badge"
			>
				{ChannelUtils.getIcon(channel, {className: styles.badgeIcon, weight: 'bold'})}
			</div>
		</div>
	);
}

export const ChannelOriginIcon = observer(function ChannelOriginIcon({
	channel,
	highlighted = false,
	size,
}: ChannelOriginIconProps) {
	const guild = channel.guildId == null ? undefined : Guilds.getGuild(channel.guildId);
	if (guild == null) {
		return (
			<span className={styles.frame} style={originIconStyle(size)} data-flx="channel.channel-origin-icon.frame">
				{ChannelUtils.getIcon(channel, {
					className: clsx(styles.fallbackIcon, highlighted && styles.fallbackIconHighlighted),
					weight: 'bold',
				})}
			</span>
		);
	}
	return (
		<ChannelBadgedIcon
			channel={channel}
			highlighted={highlighted}
			size={size}
			data-flx="channel.channel-origin-icon.channel-badged-icon"
		>
			<GuildIcon
				id={guild.id}
				name={guild.name}
				icon={guild.icon}
				sizePx={size}
				data-flx="channel.channel-origin-icon.guild-icon"
			/>
		</ChannelBadgedIcon>
	);
});
