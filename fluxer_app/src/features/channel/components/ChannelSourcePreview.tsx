// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import styles from '@app/features/channel/components/ChannelSourcePreview.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {getGroupDMAccentColor} from '@app/features/channel/utils/GroupDMColorUtils';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import {DIRECT_MESSAGE_DESCRIPTOR, PERSONAL_NOTES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Link} from '@app/features/platform/components/router/RouterReact';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {MentionBadge} from '@app/features/ui/components/MentionBadge';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import Users from '@app/features/user/state/Users';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {MEDIA_PROXY_ICON_SIZE_DEFAULT} from '@fluxer/constants/src/MediaProxyAssetSizes';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CaretRightIcon, HashIcon, NotePencilIcon, UserIcon, UsersIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

const CHANNEL_DESCRIPTOR = msg({
	message: 'Channel',
	comment: 'Short label in the channel source preview. Keep it concise.',
});
const GROUP_DESCRIPTOR = msg({
	message: 'Group',
	comment: 'Short label in the channel source preview. Keep it concise.',
});
const JUMP_TO_DESCRIPTOR = msg({
	message: 'Jump to {displayName}',
	comment:
		'Short label in the channel source preview. Keep it concise. Preserve {displayName}; it is inserted by code.',
});

type SubtextTone = 'muted' | 'primary';

interface ChannelAvatarFallbackLabels {
	channel: string;
	directMessage: string;
	group: string;
	personalNotes: string;
}

interface ChannelSourcePreviewProps {
	channel: Channel;
	onClick?: () => void;
	linkTo?: string;
	onContextMenu?: (event: React.MouseEvent<HTMLElement>) => void;
	mentionCount?: number;
	avatarSize?: MediaProxyImageSize;
	variant?: 'default' | 'inline';
	showChannelIcon?: boolean;
	className?: string;
}

function getChannelDisplayName(channel: Channel): string {
	if (channel.isPrivate()) {
		return ChannelUtils.getDMDisplayName(channel);
	}
	return channel.name?.trim() || ChannelUtils.getName(channel);
}

function getGroupDmMemberCount(channel: Channel): number {
	const currentUserId = Authentication.currentUserId;
	if (!currentUserId) return channel.recipientIds.length;
	const memberIds = new Set(channel.recipientIds);
	memberIds.add(currentUserId);
	return memberIds.size;
}

function renderAvatarImage(props: {url: string; label: string; size: number}): React.ReactNode {
	const {url, label, size} = props;
	return (
		<div
			className={styles.avatar}
			style={{width: remFromPx(size), height: remFromPx(size), backgroundImage: `url(${url})`}}
			role="img"
			aria-label={label}
			data-flx="channel.channel-source-preview.render-avatar-image.avatar"
		/>
	);
}

function renderAvatarFallback(props: {
	label: string;
	size: number;
	background?: string;
	content: React.ReactNode;
}): React.ReactNode {
	const {label, size, background, content} = props;
	return (
		<div
			className={styles.avatar}
			style={{width: remFromPx(size), height: remFromPx(size), backgroundColor: background}}
			role="img"
			aria-label={label}
			data-flx="channel.channel-source-preview.render-avatar-fallback.avatar"
		>
			{content}
		</div>
	);
}

function renderChannelAvatar(
	channel: Channel,
	size: MediaProxyImageSize,
	fallbackLabels: ChannelAvatarFallbackLabels,
): React.ReactNode {
	if (channel.guildId) {
		const guild = Guilds.getGuild(channel.guildId);
		if (!guild) {
			return renderAvatarFallback({
				label: fallbackLabels.channel,
				size,
				content: (
					<HashIcon
						className={styles.fallbackIcon}
						weight="bold"
						data-flx="channel.channel-source-preview.render-channel-avatar.fallback-icon"
					/>
				),
			});
		}
		return (
			<GuildIcon
				id={guild.id}
				name={guild.name}
				icon={guild.icon}
				sizePx={size}
				containerProps={{
					role: 'img',
					'aria-label': guild.name,
					'data-flx': 'channel.channel-source-preview.render-channel-avatar.guild-icon',
				}}
				data-flx="channel.channel-source-preview.render-channel-avatar.guild-icon"
			/>
		);
	}
	if (channel.isDM()) {
		const recipientId = channel.getRecipientId();
		const recipient = recipientId ? Users.getUser(recipientId) : null;
		if (recipient) {
			const url = AvatarUtils.getUserAvatarURL({id: recipient.id, avatar: recipient.avatar}, false);
			return renderAvatarImage({url, label: recipient.tag, size});
		}
		return renderAvatarFallback({
			label: fallbackLabels.directMessage,
			size,
			content: (
				<UserIcon
					className={styles.fallbackIcon}
					weight="bold"
					data-flx="channel.channel-source-preview.render-channel-avatar.fallback-icon--2"
				/>
			),
		});
	}
	if (channel.isGroupDM()) {
		const iconUrl = AvatarUtils.getChannelIconURL(
			{id: channel.id, icon: channel.icon},
			MEDIA_PROXY_ICON_SIZE_DEFAULT,
			true,
		);
		if (iconUrl) {
			return renderAvatarImage({url: iconUrl, label: fallbackLabels.group, size});
		}
		const accentColor = getGroupDMAccentColor(channel.id);
		return renderAvatarFallback({
			label: fallbackLabels.group,
			size,
			background: accentColor,
			content: (
				<UsersIcon
					className={styles.fallbackIcon}
					weight="bold"
					data-flx="channel.channel-source-preview.render-channel-avatar.fallback-icon--3"
				/>
			),
		});
	}
	if (channel.isPersonalNotes()) {
		return renderAvatarFallback({
			label: fallbackLabels.personalNotes,
			size,
			content: (
				<NotePencilIcon
					className={styles.fallbackIcon}
					weight="bold"
					data-flx="channel.channel-source-preview.render-channel-avatar.fallback-icon--4"
				/>
			),
		});
	}
	return renderAvatarFallback({
		label: fallbackLabels.channel,
		size,
		content: (
			<HashIcon
				className={styles.fallbackIcon}
				weight="bold"
				data-flx="channel.channel-source-preview.render-channel-avatar.fallback-icon--5"
			/>
		),
	});
}

export const ChannelSourcePreview = observer(function ChannelSourcePreview({
	channel,
	onClick,
	linkTo,
	onContextMenu,
	mentionCount,
	avatarSize,
	variant = 'default',
	showChannelIcon = true,
	className,
}: ChannelSourcePreviewProps) {
	const {i18n} = useLingui();
	const guild = channel.guildId ? (Guilds.getGuild(channel.guildId) ?? null) : null;
	const category = channel.parentId ? (Channels.getChannel(channel.parentId) ?? null) : null;
	const isGroupDm = channel.isGroupDM();
	const isUserDm = channel.isDM();
	const channelDisplayName = getChannelDisplayName(channel);
	const displayName = isUserDm ? `@${channelDisplayName}` : channelDisplayName;
	const resolvedAvatarSize: MediaProxyImageSize = avatarSize ?? (variant === 'inline' ? 24 : 32);
	const avatarFallbackLabels = useMemo(
		() => ({
			channel: i18n._(CHANNEL_DESCRIPTOR),
			directMessage: i18n._(DIRECT_MESSAGE_DESCRIPTOR),
			group: i18n._(GROUP_DESCRIPTOR),
			personalNotes: i18n._(PERSONAL_NOTES_DESCRIPTOR),
		}),
		[i18n.locale],
	);
	const {subtext, subtextTone} = useMemo(() => {
		if (isGroupDm) {
			const memberCount = getGroupDmMemberCount(channel);
			const label = plural(
				{count: memberCount},
				{
					one: '# member',
					other: '# members',
				},
			);
			return {subtext: label, subtextTone: 'primary' as SubtextTone};
		}
		if (guild) {
			if (category?.name) {
				return {
					subtext: (
						<span className={styles.subtextBreadcrumb} data-flx="channel.channel-source-preview.subtext-breadcrumb">
							<span data-flx="channel.channel-source-preview.span">{guild.name}</span>
							<CaretRightIcon
								className={styles.subtextChevron}
								weight="bold"
								data-flx="channel.channel-source-preview.subtext-chevron"
							/>
							<span data-flx="channel.channel-source-preview.span--2">{category.name}</span>
						</span>
					),
					subtextTone: 'muted' as SubtextTone,
				};
			}
			return {subtext: guild.name, subtextTone: 'muted' as SubtextTone};
		}
		return {subtext: null, subtextTone: 'muted' as SubtextTone};
	}, [category?.name, channel, guild, isGroupDm, i18n]);
	const nameContent = (
		<span className={styles.nameText} data-flx="channel.channel-source-preview.name-text">
			{displayName}
		</span>
	);
	const nameAriaLabel = i18n._(JUMP_TO_DESCRIPTOR, {displayName});
	const renderNameAction = () => {
		if (linkTo) {
			return (
				<FocusRing
					offset={-2}
					ringClassName={styles.focusRingTight}
					data-flx="channel.channel-source-preview.render-name-action.focus-ring"
				>
					<Link
						to={linkTo}
						className={styles.nameLink}
						aria-label={nameAriaLabel}
						data-flx="channel.channel-source-preview.render-name-action.name-link"
					>
						{nameContent}
					</Link>
				</FocusRing>
			);
		}
		if (onClick) {
			return (
				<FocusRing
					offset={-2}
					ringClassName={styles.focusRingTight}
					data-flx="channel.channel-source-preview.render-name-action.focus-ring--2"
				>
					<button
						type="button"
						className={styles.nameButton}
						onClick={onClick}
						aria-label={nameAriaLabel}
						data-flx="channel.channel-source-preview.render-name-action.name-button.click"
					>
						{nameContent}
					</button>
				</FocusRing>
			);
		}
		return nameContent;
	};
	const containerInteractionProps = onContextMenu ? {onContextMenu} : {};
	return (
		<div
			className={clsx(styles.container, variant === 'inline' && styles.inline, className)}
			data-flx="channel.channel-source-preview.container"
			{...containerInteractionProps}
		>
			{renderChannelAvatar(channel, resolvedAvatarSize, avatarFallbackLabels)}
			<div className={styles.textContainer} data-flx="channel.channel-source-preview.text-container">
				<div className={styles.nameRow} data-flx="channel.channel-source-preview.name-row">
					{showChannelIcon && channel.guildId
						? ChannelUtils.getIcon(channel, {className: styles.channelIcon, weight: 'bold'})
						: null}
					{renderNameAction()}
					{mentionCount != null && mentionCount > 0 ? (
						<MentionBadge
							mentionCount={mentionCount}
							size="small"
							data-flx="channel.channel-source-preview.mention-badge"
						/>
					) : null}
				</div>
				{subtext ? (
					<div
						className={clsx(styles.subtext, subtextTone === 'primary' ? styles.subtextPrimary : styles.subtextMuted)}
						data-flx="channel.channel-source-preview.subtext"
					>
						{subtext}
					</div>
				) : null}
			</div>
		</div>
	);
});
