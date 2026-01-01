/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {CaretRightIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {GroupDMAvatar} from '~/components/common/GroupDMAvatar';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import {Avatar} from '~/components/uikit/Avatar';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import type {ChannelRecord} from '~/records/ChannelRecord';
import GuildStore from '~/stores/GuildStore';
import UserStore from '~/stores/UserStore';
import * as ChannelUtils from '~/utils/ChannelUtils';
import styles from './MessageContextPrefix.module.css';

const getChannelDisplayName = (channel: ChannelRecord): string => {
	if (channel.isPrivate()) {
		return ChannelUtils.getDMDisplayName(channel);
	}
	return channel.name?.trim() || ChannelUtils.getName(channel);
};

const renderChannelIcon = (channel: ChannelRecord): React.ReactNode => {
	if (channel.isPersonalNotes()) {
		return ChannelUtils.getIcon(channel, {className: styles.channelIcon});
	}

	if (channel.isDM()) {
		const recipientId = channel.recipientIds[0];
		const recipient = recipientId ? UserStore.getUser(recipientId) : null;

		if (recipient) {
			return (
				<div className={styles.channelIconAvatar}>
					<Avatar user={recipient} size={20} status={null} className={styles.channelIconAvatarImage} />
				</div>
			);
		}

		return ChannelUtils.getIcon(channel, {className: styles.channelIcon});
	}

	if (channel.isGroupDM()) {
		return (
			<div className={styles.channelIconAvatar}>
				<GroupDMAvatar channel={channel} size={20} disableStatusIndicator />
			</div>
		);
	}

	return ChannelUtils.getIcon(channel, {className: styles.channelIcon});
};

export interface MessageContextPrefixProps {
	channel: ChannelRecord;
	showGuildMeta?: boolean;
	compact?: boolean;
	onClick?: () => void;
}

export const MessageContextPrefix = observer(
	({channel, showGuildMeta = false, compact = false, onClick}: MessageContextPrefixProps) => {
		const guild = channel.guildId ? (GuildStore.getGuild(channel.guildId) ?? null) : null;
		const effectiveShowGuildMeta = Boolean(showGuildMeta && guild);

		const channelDisplayName = getChannelDisplayName(channel);

		return (
			<div className={[styles.channelHeader, compact && styles.channelHeaderCompact].filter(Boolean).join(' ')}>
				{!effectiveShowGuildMeta && renderChannelIcon(channel)}

				<FocusRing offset={-2} ringClassName={styles.focusRingTight}>
					<button type="button" className={styles.channelNameButton} onClick={onClick}>
						{effectiveShowGuildMeta ? (
							<span className={styles.channelScopeRow}>
								<GuildIcon
									id={guild!.id}
									name={guild!.name}
									icon={guild!.icon}
									className={styles.channelScopeGuildIcon}
									sizePx={12}
								/>
								<span className={styles.channelScopeGuildName}>{guild!.name}</span>
								<CaretRightIcon className={styles.channelScopeChevron} size={12} weight="bold" />
								<span className={styles.channelScopeChannelInfo}>
									{ChannelUtils.getIcon(channel, {className: styles.channelScopeChannelIcon})}
									<span className={styles.channelScopeChannelName}>{channelDisplayName}</span>
								</span>
							</span>
						) : (
							<span className={styles.channelNameText}>
								<span className={styles.channelNamePrimary}>{channelDisplayName}</span>
							</span>
						)}
					</button>
				</FocusRing>
			</div>
		);
	},
);
