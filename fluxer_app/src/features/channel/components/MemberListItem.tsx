// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import {UserTag} from '@app/features/channel/components/ChannelUserTag';
import {CompactMemberCustomStatus} from '@app/features/channel/components/CompactMemberCustomStatus';
import styles from '@app/features/channel/components/MemberListItem.module.css';
import {PreloadableUserPopout} from '@app/features/channel/components/PreloadableUserPopout';
import Guilds from '@app/features/guild/state/Guilds';
import {useMemberListCustomStatus} from '@app/features/member/hooks/useMemberListCustomStatus';
import {useMemberListPresence} from '@app/features/member/hooks/useMemberListPresence';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import TypingIndicator from '@app/features/typing/state/TypingIndicator';
import {GroupDMMemberContextMenu} from '@app/features/ui/action_menu/GroupDMContextMenu';
import {GuildMemberContextMenu} from '@app/features/ui/action_menu/GuildMemberContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {FocusRingWrapper} from '@app/features/ui/components/FocusRingWrapper';
import {ListStatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import {useTextOverflow} from '@app/features/ui/hooks/useTextOverflow';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {User} from '@app/features/user/models/User';
import type {CustomStatus} from '@app/features/user/state/CustomStatus';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {isOfflineStatus} from '@fluxer/constants/src/StatusConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CrownIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useRef, useState} from 'react';

const COMMUNITY_OWNER_DESCRIPTOR = msg({
	message: 'Community owner',
	comment: 'Short label in the channel and chat member list item. Keep it concise.',
});
const GROUP_OWNER_DESCRIPTOR = msg({
	message: 'Group owner',
	comment: 'Short label in the direct message member list item. Keep it concise.',
});

type MemberRoleColorStyle = React.CSSProperties & {'--member-role-color': string};

interface MemberListItemProps {
	user: User;
	channelId: string;
	guildId?: string;
	guildMember?: GuildMember;
	status?: StatusType;
	customStatus?: CustomStatus | null;
	isOwner?: boolean;
	roleColor?: string;
	displayName?: string;
	disableBackdrop?: boolean;
	avatarMediaSize?: MediaProxyImageSize;
}

export const MemberListItem: React.FC<MemberListItemProps> = observer((props) => {
	const {i18n} = useLingui();
	const {
		user,
		channelId,
		guildId,
		guildMember,
		status: providedStatus,
		customStatus: providedCustomStatus,
		isOwner = false,
		roleColor,
		displayName,
		disableBackdrop = false,
		avatarMediaSize,
	} = props;
	const itemRef = useRef<HTMLButtonElement>(null);
	const hookStatus = useMemberListPresence({
		guildId: guildId ?? '',
		channelId,
		userId: user.id,
		enabled: providedStatus === undefined,
	});
	const status = providedStatus !== undefined ? providedStatus : hookStatus;
	const hookCustomStatus = useMemberListCustomStatus({
		guildId: guildId ?? '',
		channelId,
		userId: user.id,
		enabled: guildId !== undefined && providedCustomStatus === undefined,
	});
	const memberListCustomStatus = providedCustomStatus !== undefined ? providedCustomStatus : hookCustomStatus;
	const [contextMenuOpen, setContextMenuOpen] = useState(false);
	const contextMenuTicketRef = useRef(0);
	const isCurrentUser = user.id === Authentication.currentUserId;
	const isTyping = TypingIndicator.isMemberListTyping(channelId, user.id, Authentication.currentUserId);
	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			const ticket = contextMenuTicketRef.current + 1;
			contextMenuTicketRef.current = ticket;
			setContextMenuOpen(true);
			ContextMenuCommands.openFromEvent(
				event,
				({onClose}) => (
					<>
						{guildId ? (
							<GuildMemberContextMenu
								user={user}
								onClose={onClose}
								guildId={guildId}
								channelId={channelId}
								member={guildMember}
								data-flx="channel.member-list-item.handle-context-menu.guild-member-context-menu"
							/>
						) : (
							<GroupDMMemberContextMenu
								userId={user.id}
								channelId={channelId}
								onClose={onClose}
								data-flx="channel.member-list-item.handle-context-menu.group-dm-member-context-menu"
							/>
						)}
					</>
				),
				{
					onClose: () => {
						if (contextMenuTicketRef.current === ticket) {
							setContextMenuOpen(false);
						}
					},
				},
			);
		},
		[user, guildId, channelId, guildMember],
	);
	const showOwnerCrown =
		isOwner &&
		!(guildId !== undefined && (Guilds.getGuild(guildId)?.features.has(GuildFeatures.HIDE_OWNER_CROWN) ?? false));
	const nickname = displayName || NicknameUtils.getNickname(user, guildId, channelId);
	const nameRef = useRef<HTMLSpanElement>(null);
	const isNameOverflowing = useTextOverflow(nameRef, {content: nickname, measureTextRange: true});
	const nameStyle = useMemo<MemberRoleColorStyle | undefined>(
		() => (roleColor ? {'--member-role-color': roleColor} : undefined),
		[roleColor],
	);
	const memberAvatarUrl = useMemo(() => {
		if (!guildId || !guildMember) {
			return undefined;
		}
		return AvatarUtils.getGuildMemberDisplayAvatarURL({
			guildId,
			user,
			memberAvatar: guildMember.avatar,
			avatarUnset: guildMember.isAvatarUnset(),
			animated: false,
			size: avatarMediaSize,
		});
	}, [avatarMediaSize, guildId, guildMember, user]);
	const memberHoverAvatarUrl = useMemo(() => {
		if (!guildId || !guildMember) {
			return undefined;
		}
		return AvatarUtils.getGuildMemberDisplayAvatarURL({
			guildId,
			user,
			memberAvatar: guildMember.avatar,
			avatarUnset: guildMember.isAvatarUnset(),
			animated: true,
			size: avatarMediaSize,
		});
	}, [avatarMediaSize, guildId, guildMember, user]);
	const nameContent = (
		<span
			ref={nameRef}
			className={clsx(styles.name, roleColor && styles.nameRoleColored)}
			style={nameStyle}
			data-flx="channel.member-list-item.name"
		>
			{nickname}
		</span>
	);
	const content = (
		<FocusRingWrapper
			focusRingClassName={styles.memberFocusRing}
			data-flx="channel.member-list-item.focus-ring-wrapper"
		>
			<button
				type="button"
				className={clsx(
					styles.button,
					!isCurrentUser && isOfflineStatus(status) && !contextMenuOpen && styles.buttonOffline,
					contextMenuOpen && styles.buttonContextMenuOpen,
				)}
				onContextMenu={handleContextMenu}
				data-member-list-focus-item="true"
				data-flx="channel.member-list-item.button.context-menu"
			>
				<div className={styles.grid} data-flx="channel.member-list-item.grid">
					<span className={styles.content} data-flx="channel.member-list-item.content">
						<div className={styles.avatarContainer} data-flx="channel.member-list-item.avatar-container">
							<ListStatusAwareAvatar
								user={user}
								size={32}
								isTyping={isTyping}
								showOffline={isCurrentUser || isTyping}
								guildId={guildId}
								status={status}
								avatarUrl={memberAvatarUrl}
								hoverAvatarUrl={memberHoverAvatarUrl}
								mediaSize={avatarMediaSize}
								data-flx="channel.member-list-item.status-aware-avatar"
							/>
						</div>
						<div className={styles.userInfoContainer} data-flx="channel.member-list-item.user-info-container">
							<div className={styles.nameContainer} data-flx="channel.member-list-item.name-container">
								{isNameOverflowing ? (
									<Tooltip text={nickname} data-flx="channel.member-list-item.name-tooltip">
										{nameContent}
									</Tooltip>
								) : (
									nameContent
								)}
								{showOwnerCrown && (
									<div className={styles.ownerIcon} data-flx="channel.member-list-item.owner-icon">
										<Tooltip
											text={i18n._(guildId ? COMMUNITY_OWNER_DESCRIPTOR : GROUP_OWNER_DESCRIPTOR)}
											data-flx="channel.member-list-item.tooltip"
										>
											<CrownIcon className={styles.crownIcon} data-flx="channel.member-list-item.crown-icon" />
										</Tooltip>
									</div>
								)}
								{user.bot && (
									<UserTag
										className={styles.userTag}
										system={user.system}
										data-flx="channel.member-list-item.user-tag"
									/>
								)}
							</div>
							<CompactMemberCustomStatus
								customStatus={memberListCustomStatus}
								userId={user.id}
								className={styles.memberCustomStatus}
								data-flx="channel.member-list-item.member-custom-status"
							/>
						</div>
					</span>
				</div>
			</button>
		</FocusRingWrapper>
	);
	return (
		<PreloadableUserPopout
			ref={itemRef}
			user={user}
			isWebhook={false}
			guildId={guildId}
			guildMember={guildMember}
			channelId={channelId}
			disableContextMenu
			disableBackdrop={disableBackdrop}
			profilePopoutAnimationType="profile-slide-inverted"
			data-flx="channel.member-list-item.preloadable-user-popout"
		>
			{content}
		</PreloadableUserPopout>
	);
});
