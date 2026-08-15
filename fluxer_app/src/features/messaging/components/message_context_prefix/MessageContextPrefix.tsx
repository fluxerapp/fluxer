// SPDX-License-Identifier: AGPL-3.0-or-later

import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import styles from '@app/features/messaging/components/message_context_prefix/MessageContextPrefix.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Avatar} from '@app/features/ui/components/Avatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import Users from '@app/features/user/state/Users';
import {useLingui} from '@lingui/react';
import {CaretRightIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const getChannelDisplayName = (channel: Channel): string => {
	if (channel.isPrivate()) {
		return ChannelUtils.getDMDisplayName(channel);
	}
	return channel.name?.trim() || ChannelUtils.getName(channel);
};
const renderChannelIcon = (channel: Channel): React.ReactNode => {
	if (channel.isPersonalNotes()) {
		return ChannelUtils.getIcon(channel, {className: styles.channelIcon});
	}
	if (channel.isDM()) {
		const recipientId = channel.recipientIds[0];
		const recipient = recipientId ? Users.getUser(recipientId) : null;
		if (recipient) {
			return (
				<div
					className={styles.channelIconAvatar}
					data-flx="messaging.message-context-prefix.message-context-prefix.render-channel-icon.channel-icon-avatar"
				>
					<Avatar
						user={recipient}
						size={20}
						status={null}
						className={styles.channelIconAvatarImage}
						data-flx="messaging.message-context-prefix.message-context-prefix.render-channel-icon.channel-icon-avatar-image"
					/>
				</div>
			);
		}
		return ChannelUtils.getIcon(channel, {className: styles.channelIcon});
	}
	if (channel.isGroupDM()) {
		return (
			<div
				className={styles.channelIconAvatar}
				data-flx="messaging.message-context-prefix.message-context-prefix.render-channel-icon.channel-icon-avatar--2"
			>
				<GroupDMAvatar
					channel={channel}
					size={20}
					disableStatusIndicator
					data-flx="messaging.message-context-prefix.message-context-prefix.render-channel-icon.group-dm-avatar"
				/>
			</div>
		);
	}
	return ChannelUtils.getIcon(channel, {className: styles.channelIcon});
};

export interface MessageContextPrefixProps {
	channel: Channel;
	showGuildMeta?: boolean;
	compact?: boolean;
	onClick?: () => void;
}

export const MessageContextPrefix = observer(
	({channel, showGuildMeta = false, compact = false, onClick}: MessageContextPrefixProps) => {
		useLingui();
		const guild = channel.guildId ? (Guilds.getGuild(channel.guildId) ?? null) : null;
		const effectiveShowGuildMeta = Boolean(showGuildMeta && guild);
		const channelDisplayName = getChannelDisplayName(channel);
		return (
			<div
				className={[styles.channelHeader, compact && styles.channelHeaderCompact].filter(Boolean).join(' ')}
				data-flx="messaging.message-context-prefix.message-context-prefix.div"
			>
				{!effectiveShowGuildMeta && renderChannelIcon(channel)}
				<FocusRing
					offset={-2}
					ringClassName={styles.focusRingTight}
					data-flx="messaging.message-context-prefix.message-context-prefix.focus-ring"
				>
					<button
						type="button"
						className={styles.channelNameButton}
						onClick={onClick}
						data-flx="messaging.message-context-prefix.message-context-prefix.channel-name-button.click"
					>
						{effectiveShowGuildMeta ? (
							<span
								className={styles.channelScopeRow}
								data-flx="messaging.message-context-prefix.message-context-prefix.channel-scope-row"
							>
								<GuildIcon
									id={guild!.id}
									name={guild!.name}
									icon={guild!.icon}
									className={styles.channelScopeGuildIcon}
									sizePx={12}
									data-flx="messaging.message-context-prefix.message-context-prefix.channel-scope-guild-icon"
								/>
								<span
									className={styles.channelScopeGuildName}
									data-flx="messaging.message-context-prefix.message-context-prefix.channel-scope-guild-name"
								>
									{guild!.name}
								</span>
								<CaretRightIcon
									className={styles.channelScopeChevron}
									size={remFromPx(12)}
									weight="bold"
									data-flx="messaging.message-context-prefix.message-context-prefix.channel-scope-chevron"
								/>
								<span
									className={styles.channelScopeChannelInfo}
									data-flx="messaging.message-context-prefix.message-context-prefix.channel-scope-channel-info"
								>
									{ChannelUtils.getIcon(channel, {className: styles.channelScopeChannelIcon})}
									<span
										className={styles.channelScopeChannelName}
										data-flx="messaging.message-context-prefix.message-context-prefix.channel-scope-channel-name"
									>
										{channelDisplayName}
									</span>
								</span>
							</span>
						) : (
							<span
								className={styles.channelNameText}
								data-flx="messaging.message-context-prefix.message-context-prefix.channel-name-text"
							>
								<span
									className={styles.channelNamePrimary}
									data-flx="messaging.message-context-prefix.message-context-prefix.channel-name-primary"
								>
									{channelDisplayName}
								</span>
							</span>
						)}
					</button>
				</FocusRing>
			</div>
		);
	},
);
