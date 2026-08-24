// SPDX-License-Identifier: AGPL-3.0-or-later

import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {EXPIRES_DESCRIPTOR, NEVER_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey, stopPropagationOnEnterSpace} from '@app/features/input/utils/KeyboardUtils';
import styles from '@app/features/invite/components/InviteListItem.module.css';
import {useInviteCountdown} from '@app/features/invite/hooks/useInviteCountdown';
import {isGuildInvite} from '@app/features/invite/types/InviteTypes';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import Users from '@app/features/user/state/Users';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {formatShortRelativeTime} from '@fluxer/date_utils/src/DateDuration';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ClipboardIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';

const EXPIRED_DESCRIPTOR = msg({
	message: 'Expired',
	comment: 'Error message in the invite list item.',
});
const UNKNOWN_DESCRIPTOR = msg({
	message: 'Unknown',
	comment: 'Short label in the invite list item. Keep it concise.',
});
const NO_CATEGORY_DESCRIPTOR = msg({
	message: 'No category',
	comment: 'Empty-state text in the invite list item.',
});
const COPY_INVITE_LINK_DESCRIPTOR = msg({
	message: 'Copy invite link',
	comment: 'Button or menu action label in the invite list item. Keep it concise.',
});
const INVITE_MASKED_WHILE_SHARING_DESCRIPTOR = msg({
	message: 'Invite hidden while sharing',
	comment: 'Replacement text for an invite code while streaming privacy is active.',
});
const COPY_PAUSED_WHILE_SHARING_DESCRIPTOR = msg({
	message: 'Copy paused while sharing',
	comment: 'Tooltip for disabled invite copy buttons while streaming privacy is active.',
});
const REVOKE_INVITE_DESCRIPTOR = msg({
	message: 'Revoke invite',
	comment: 'Short label in the invite list item. Keep it concise.',
});
export const InviteListHeader = observer(
	({showChannel = false, showCreatedDate = false}: {showChannel?: boolean; showCreatedDate?: boolean}) => {
		const {i18n} = useLingui();
		return (
			<div
				className={showChannel ? styles.header : styles.headerWithoutChannel}
				data-flx="invite.invite-list-item.invite-list-header.header"
			>
				<div className={styles.headerColumn} data-flx="invite.invite-list-item.invite-list-header.header-column">
					<Trans>Inviter</Trans>
				</div>
				{showChannel && (
					<div className={styles.headerColumn} data-flx="invite.invite-list-item.invite-list-header.header-column--2">
						<Trans>Channel</Trans>
					</div>
				)}
				<div className={styles.headerColumn} data-flx="invite.invite-list-item.invite-list-header.header-column--3">
					<Trans>Code</Trans>
				</div>
				<div className={styles.headerColumn} data-flx="invite.invite-list-item.invite-list-header.header-column--4">
					<Trans>Uses</Trans>
				</div>
				<div className={styles.headerColumn} data-flx="invite.invite-list-item.invite-list-header.header-column--5">
					{showCreatedDate ? <Trans>Created</Trans> : i18n._(EXPIRES_DESCRIPTOR)}
				</div>
			</div>
		);
	},
);
export const InviteListItem: React.FC<{
	invite: Invite;
	onRevoke: (code: string) => void;
	showChannel?: boolean;
	showCreatedDate?: boolean;
	onMobilePress?: (invite: Invite) => void;
}> = observer(({invite, onRevoke, showChannel = false, showCreatedDate = false, onMobilePress}) => {
	const {i18n} = useLingui();
	const {countdown, isMonospace} = useInviteCountdown(invite.expires_at, i18n._(EXPIRED_DESCRIPTOR));
	const hideInviteLinks = StreamerMode.shouldHideInviteLinks;
	const inviter = Users.getUser(invite.inviter?.id || '');
	const avatarUrl = inviter ? AvatarUtils.getUserAvatarURL(inviter, false) : null;
	const {enabled: isMobile} = MobileLayout;
	const guildInvite = isGuildInvite(invite) ? invite : null;
	const inviterDisplayName = inviter ? NicknameUtils.getNickname(inviter, guildInvite?.guild.id) : null;
	const channelFromState = guildInvite ? Channels.getChannel(guildInvite.channel.id) : null;
	const categoryFromState = channelFromState ? Channels.getChannel(channelFromState.parentId || '') : null;
	const channel = showChannel ? channelFromState : null;
	const category = showChannel && channelFromState?.parentId ? categoryFromState : null;
	const usesText = useMemo(() => {
		if (!guildInvite) {
			return '0';
		}
		const currentUses = guildInvite.uses ?? 0;
		const maxUses = guildInvite.max_uses ?? 0;
		if (maxUses > 0) {
			return `${currentUses} / ${maxUses}`;
		}
		return String(currentUses);
	}, [guildInvite]);
	const dateDisplay = useMemo(() => {
		if (showCreatedDate) {
			if (!guildInvite?.created_at) {
				return '';
			}
			const createdDate = new Date(guildInvite.created_at);
			return formatShortRelativeTime(createdDate);
		}
		return countdown || i18n._(NEVER_DESCRIPTOR);
	}, [showCreatedDate, guildInvite, countdown]);
	const dateTooltip = useMemo(() => {
		if (showCreatedDate) {
			if (!guildInvite?.created_at) {
				return null;
			}
			const createdDate = new Date(guildInvite.created_at);
			return DateUtils.getFormattedDateTimeWithSeconds(createdDate);
		}
		return null;
	}, [showCreatedDate, guildInvite]);
	const dateIsMonospace = !showCreatedDate && isMonospace;
	const inviteUrl = `${RuntimeConfig.inviteEndpoint}/${invite.code}`;
	const handleCopy = (e: React.MouseEvent) => {
		e.stopPropagation();
		if (hideInviteLinks) return;
		TextCopyCommands.copy(i18n, inviteUrl);
	};
	const handleRowClick = () => {
		if (hideInviteLinks) return;
		if (isMobile) {
			if (onMobilePress) {
				onMobilePress(invite);
				return;
			}
			TextCopyCommands.copy(i18n, inviteUrl);
		}
	};
	const handleKeyDown = (e: React.KeyboardEvent) => {
		if (isMobile && isKeyboardActivationKey(e.key)) {
			e.preventDefault();
			handleRowClick();
		}
	};
	const itemClass = isMobile ? styles.mobileItem : showChannel ? styles.itemWithChannel : styles.itemWithoutChannel;
	const itemContent = (
		<>
			<div className={styles.inviter} data-flx="invite.invite-list-item.inviter">
				<span className={styles.label} data-flx="invite.invite-list-item.label">
					<Trans>Inviter:</Trans>
				</span>
				{inviter && avatarUrl ? (
					<>
						<img src={avatarUrl} alt="" className={styles.avatar} data-flx="invite.invite-list-item.avatar" />
						<span className={styles.username} data-flx="invite.invite-list-item.username">
							{inviterDisplayName}
						</span>
					</>
				) : (
					<span className={styles.usernameUnknown} data-flx="invite.invite-list-item.username-unknown">
						{i18n._(UNKNOWN_DESCRIPTOR)}
					</span>
				)}
			</div>
			{showChannel && channel && (
				<div className={styles.channel} data-flx="invite.invite-list-item.channel">
					<span className={styles.label} data-flx="invite.invite-list-item.label--2">
						<Trans>Channel:</Trans>
					</span>
					{ChannelUtils.getIcon(channel, {size: 20, className: styles.channelIcon})}
					<div className={styles.channelInfo} data-flx="invite.invite-list-item.channel-info">
						<span className={styles.channelName} data-flx="invite.invite-list-item.channel-name">
							{channel.name}
						</span>
						{channel.type !== ChannelTypes.GUILD_CATEGORY && (
							<span className={styles.categoryName} data-flx="invite.invite-list-item.category-name">
								{category ? category.name : i18n._(NO_CATEGORY_DESCRIPTOR)}
							</span>
						)}
					</div>
				</div>
			)}
			<div className={styles.code} data-flx="invite.invite-list-item.code">
				<span className={styles.label} data-flx="invite.invite-list-item.label--3">
					<Trans>Code:</Trans>
				</span>
				<span className={styles.inviteCode} data-flx="invite.invite-list-item.invite-code">
					{hideInviteLinks ? i18n._(INVITE_MASKED_WHILE_SHARING_DESCRIPTOR) : invite.code}
				</span>
				<Tooltip
					text={i18n._(hideInviteLinks ? COPY_PAUSED_WHILE_SHARING_DESCRIPTOR : COPY_INVITE_LINK_DESCRIPTOR)}
					data-flx="invite.invite-list-item.tooltip"
				>
					<FocusRing offset={-2} data-flx="invite.invite-list-item.focus-ring">
						<button
							type="button"
							onClick={handleCopy}
							className={styles.copyButtonHidden}
							aria-label={i18n._(COPY_INVITE_LINK_DESCRIPTOR)}
							disabled={hideInviteLinks}
							onKeyDown={stopPropagationOnEnterSpace}
							data-flx="invite.invite-list-item.copy-button-hidden"
						>
							<ClipboardIcon className={styles.copyIcon} data-flx="invite.invite-list-item.copy-icon" />
						</button>
					</FocusRing>
				</Tooltip>
			</div>
			<div className={styles.uses} data-flx="invite.invite-list-item.uses">
				<span className={styles.label} data-flx="invite.invite-list-item.label--4">
					<Trans>Uses:</Trans>
				</span>
				<span className={styles.usesText} data-flx="invite.invite-list-item.uses-text">
					{usesText}
				</span>
			</div>
			<div className={styles.date} data-flx="invite.invite-list-item.date">
				<span className={styles.label} data-flx="invite.invite-list-item.label--5">
					{showCreatedDate ? <Trans>Created:</Trans> : <Trans>Expires:</Trans>}
				</span>
				{dateTooltip ? (
					<Tooltip text={dateTooltip} data-flx="invite.invite-list-item.tooltip--2">
						<span
							className={dateIsMonospace ? styles.dateTextMonospace : styles.dateText}
							data-flx="invite.invite-list-item.date-text"
						>
							{dateDisplay}
						</span>
					</Tooltip>
				) : (
					<span
						className={dateIsMonospace ? styles.dateTextMonospace : styles.dateText}
						data-flx="invite.invite-list-item.date-text--2"
					>
						{dateDisplay}
					</span>
				)}
			</div>
			<Tooltip text={i18n._(REVOKE_INVITE_DESCRIPTOR)} data-flx="invite.invite-list-item.tooltip--3">
				<FocusRing offset={-2} data-flx="invite.invite-list-item.focus-ring--2">
					<button
						type="button"
						onClick={(e) => {
							e.stopPropagation();
							onRevoke(invite.code);
						}}
						className={styles.revokeButtonHidden}
						aria-label={i18n._(REVOKE_INVITE_DESCRIPTOR)}
						onKeyDown={stopPropagationOnEnterSpace}
						data-flx="invite.invite-list-item.revoke-button-hidden.stop-propagation"
					>
						<XIcon className={styles.revokeIcon} weight="bold" data-flx="invite.invite-list-item.revoke-icon" />
					</button>
				</FocusRing>
			</Tooltip>
		</>
	);
	if (isMobile) {
		return (
			<div
				role="button"
				onClick={handleRowClick}
				onKeyDown={handleKeyDown}
				tabIndex={0}
				className={itemClass}
				data-flx="invite.invite-list-item.button.row-click"
			>
				{itemContent}
			</div>
		);
	}
	return (
		<div className={itemClass} data-flx="invite.invite-list-item.div">
			{itemContent}
		</div>
	);
});
