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

import {Trans, useLingui} from '@lingui/react/macro';

import {
	BellIcon,
	BugIcon,
	CaretDownIcon,
	CaretRightIcon,
	CaretUpIcon,
	ChatCircleIcon,
	CheckIcon,
	CrownIcon,
	DotsThreeVerticalIcon,
	GearIcon,
	MagnifyingGlassIcon,
	PencilIcon,
	PushPinIcon,
	SignOutIcon,
	StarIcon,
	TicketIcon,
	UserPlusIcon,
	UsersIcon,
	XIcon,
} from '@phosphor-icons/react';

import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';

import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as ReadStateActionCreators from '~/actions/ReadStateActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';

import {ChannelTypes, isOfflineStatus, ME, MessageNotifications, Permissions} from '~/Constants';

import {DMCloseFailedModal} from '~/components/alerts/DMCloseFailedModal';
import {ChannelSearchBottomSheet} from '~/components/bottomsheets/ChannelSearchBottomSheet';
import {createMuteConfig, getMuteDurationOptions} from '~/components/channel/muteOptions';
import {PreloadableUserPopout} from '~/components/channel/PreloadableUserPopout';
import {UserTag} from '~/components/channel/UserTag';
import {CustomStatusDisplay} from '~/components/common/CustomStatusDisplay/CustomStatusDisplay';
import {GroupDMAvatar} from '~/components/common/GroupDMAvatar';
import {ChannelDebugModal} from '~/components/debug/ChannelDebugModal';
import {UserDebugModal} from '~/components/debug/UserDebugModal';
import {LongPressable} from '~/components/LongPressable';
import {AddFriendsToGroupModal} from '~/components/modals/AddFriendsToGroupModal';
import {ChannelSettingsModal} from '~/components/modals/ChannelSettingsModal';
import {ChannelTopicModal} from '~/components/modals/ChannelTopicModal';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {CreateDMModal} from '~/components/modals/CreateDMModal';
import {EditGroupModal} from '~/components/modals/EditGroupModal';
import {GroupInvitesModal} from '~/components/modals/GroupInvitesModal';
import {GuildNotificationSettingsModal} from '~/components/modals/GuildNotificationSettingsModal';
import {GuildMemberActionsSheet} from '~/components/modals/guildTabs/GuildMemberActionsSheet';
import {InviteModal} from '~/components/modals/InviteModal';
import {ChannelPinsContent} from '~/components/shared/ChannelPinsContent';
import {
	CopyIdIcon,
	CopyLinkIcon,
	DeleteIcon,
	EditIcon,
	InviteIcon,
	MarkAsReadIcon,
} from '~/components/uikit/ContextMenu/ContextMenuIcons';
import {
	MenuBottomSheet,
	type MenuGroupType,
	type MenuItemType,
	type MenuRadioType,
} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {Scroller} from '~/components/uikit/Scroller';
import * as Sheet from '~/components/uikit/Sheet/Sheet';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';

import {useLeaveGroup} from '~/hooks/useLeaveGroup';
import {useMemberListSubscription} from '~/hooks/useMemberListSubscription';
import {usePressable} from '~/hooks/usePressable';

import {SafeMarkdown} from '~/lib/markdown';
import {MarkdownContext} from '~/lib/markdown/renderers';

import {Routes} from '~/Routes';

import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import type {UserRecord} from '~/records/UserRecord';

import AccessibilityStore from '~/stores/AccessibilityStore';
import AuthenticationStore from '~/stores/AuthenticationStore';
import FavoritesStore from '~/stores/FavoritesStore';
import GuildStore from '~/stores/GuildStore';
import MemberSidebarStore from '~/stores/MemberSidebarStore';
import PermissionStore from '~/stores/PermissionStore';
import PresenceStore from '~/stores/PresenceStore';
import ReadStateStore from '~/stores/ReadStateStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import TypingStore from '~/stores/TypingStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import UserStore from '~/stores/UserStore';

import markupStyles from '~/styles/Markup.module.css';

import * as ChannelUtils from '~/utils/ChannelUtils';
import {getMutedText, getNotificationSettingsLabel} from '~/utils/ContextMenuUtils';
import {MAX_GROUP_DM_RECIPIENTS} from '~/utils/groupDmUtils';
import * as InviteUtils from '~/utils/InviteUtils';
import {buildChannelLink} from '~/utils/messageLinkUtils';
import * as NicknameUtils from '~/utils/NicknameUtils';
import * as RouterUtils from '~/utils/RouterUtils';

import styles from './ChannelDetailsBottomSheet.module.css';

const MEMBER_ITEM_HEIGHT = 56;
const INITIAL_MEMBER_RANGE: [number, number] = [0, 99];
const SCROLL_BUFFER = 50;

const SkeletonMemberItem = () => (
	<div className={styles.skeletonItem}>
		<div className={clsx(styles.skeletonAvatar, styles.skeleton)} />
		<div className={styles.skeletonInfo}>
			<div className={clsx(styles.skeletonName, styles.skeleton)} />
			<div className={clsx(styles.skeletonStatus, styles.skeleton)} />
		</div>
	</div>
);

type ChannelDetailsTab = 'members' | 'pins';

interface ChannelDetailsBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channel: ChannelRecord;
	initialTab?: ChannelDetailsTab;
	openSearchImmediately?: boolean;
}

interface QuickActionButtonProps {
	icon: React.ReactNode;
	label: string;
	onClick: () => void;
	isActive?: boolean;
	danger?: boolean;
	disabled?: boolean;
}

const QuickActionButton: React.FC<QuickActionButtonProps> = ({icon, label, onClick, isActive, danger, disabled}) => {
	const {isPressed, pressableProps} = usePressable(disabled);

	return (
		<button
			type="button"
			onClick={onClick}
			disabled={disabled}
			className={clsx(
				styles.quickActionButton,
				isPressed && styles.quickActionButtonPressed,
				isActive && styles.quickActionButtonActive,
				danger && styles.quickActionButtonDanger,
				disabled && styles.quickActionButtonDisabled,
			)}
			{...pressableProps}
		>
			<div className={styles.quickActionIcon}>{icon}</div>
			<span className={styles.quickActionLabel}>{label}</span>
		</button>
	);
};

const MobileMemberListItem = observer(
	({
		guild,
		channelId,
		member,
		onLongPress,
	}: {
		guild: GuildRecord;
		channelId: string;
		member: GuildMemberRecord;
		onLongPress?: (member: GuildMemberRecord) => void;
	}) => {
		const {t} = useLingui();
		const isTyping = TypingStore.isTyping(channelId, member.user.id);
		const status = PresenceStore.getStatus(member.user.id);

		const handleLongPress = React.useCallback(() => {
			onLongPress?.(member);
		}, [member, onLongPress]);

		const content = (
			<PreloadableUserPopout user={member.user} isWebhook={false} guildId={guild.id} position="left-start">
				<div
					className={`${styles.memberListItem} ${
						!member.isCurrentUser() && isOfflineStatus(status) ? styles.memberListItemOffline : ''
					}`}
				>
					<StatusAwareAvatar
						user={member.user}
						size={40}
						isTyping={isTyping}
						showOffline={member.user.id === AuthenticationStore.currentUserId || isTyping}
						guildId={guild.id}
					/>
					<div className={styles.memberContent}>
						<div className={styles.memberNameRow}>
							<span className={styles.memberName} style={{color: member.getColorString()}}>
								{NicknameUtils.getNickname(member.user, guild.id)}
							</span>
							{guild.isOwner(member.user.id) && (
								<div className={styles.crownContainer}>
									<Tooltip text={t`Community Owner`}>
										<CrownIcon className={styles.crownIcon} />
									</Tooltip>
								</div>
							)}
							{member.user.bot && <UserTag className={styles.memberTag} system={member.user.system} />}
						</div>
						{!member.user.bot && (
							<CustomStatusDisplay
								userId={member.user.id}
								className={styles.memberCustomStatus}
								showText={true}
								showTooltip={false}
								animateOnParentHover
							/>
						)}
					</div>
				</div>
			</PreloadableUserPopout>
		);

		if (onLongPress) {
			return (
				<LongPressable onLongPress={handleLongPress} delay={500}>
					{content}
				</LongPressable>
			);
		}

		return content;
	},
);

interface LazyMemberListGroupProps {
	guild: GuildRecord;
	group: {id: string; count: number};
	channelId: string;
	members: Array<GuildMemberRecord>;
	onMemberLongPress?: (member: GuildMemberRecord) => void;
}

const LazyMemberListGroup = observer(
	({guild, group, channelId, members, onMemberLongPress}: LazyMemberListGroupProps) => {
		const {t} = useLingui();
		const groupName = group.id === 'online' ? t`Online` : group.id === 'offline' ? t`Offline` : group.id;

		return (
			<div className={styles.memberGroupContainer}>
				<div className={styles.memberGroupHeader}>
					{groupName} — {group.count}
				</div>
				<div className={styles.memberGroupList}>
					{members.map((member, index) => (
						<React.Fragment key={member.user.id}>
							<MobileMemberListItem
								guild={guild}
								channelId={channelId}
								member={member}
								onLongPress={onMemberLongPress}
							/>
							{index < members.length - 1 && <div className={styles.memberDivider} />}
						</React.Fragment>
					))}
				</div>
			</div>
		);
	},
);

const LazyGuildMemberList = observer(
	({
		guild,
		channel,
		onMemberLongPress,
		enabled = true,
	}: {
		guild: GuildRecord;
		channel: ChannelRecord;
		onMemberLongPress?: (member: GuildMemberRecord) => void;
		enabled?: boolean;
	}) => {
		const [subscribedRange, setSubscribedRange] = React.useState<[number, number]>(INITIAL_MEMBER_RANGE);

		const {subscribe} = useMemberListSubscription({
			guildId: guild.id,
			channelId: channel.id,
			enabled,
		});

		const memberListState = MemberSidebarStore.getList(guild.id, channel.id);
		const isLoading = !memberListState || memberListState.items.size === 0;

		const handleScroll = React.useCallback(
			(event: React.UIEvent<HTMLDivElement>) => {
				const target = event.currentTarget;
				const scrollTop = target.scrollTop;
				const clientHeight = target.clientHeight;

				const startIndex = Math.max(0, Math.floor(scrollTop / MEMBER_ITEM_HEIGHT) - SCROLL_BUFFER);
				const endIndex = Math.ceil((scrollTop + clientHeight) / MEMBER_ITEM_HEIGHT) + SCROLL_BUFFER;

				if (startIndex !== subscribedRange[0] || endIndex !== subscribedRange[1]) {
					const newRange: [number, number] = [startIndex, endIndex];
					setSubscribedRange(newRange);
					subscribe([newRange]);
				}
			},
			[subscribedRange, subscribe],
		);

		if (isLoading) {
			return (
				<div className={styles.memberListContent}>
					<div className={styles.memberGroupContainer}>
						<div className={clsx(styles.memberGroupHeader, styles.skeletonHeader, styles.skeleton)} />
						<div className={styles.memberGroupList}>
							{Array.from({length: 10}).map((_, i) => (
								<React.Fragment key={i}>
									<SkeletonMemberItem />
									{i < 9 && <div className={styles.memberDivider} />}
								</React.Fragment>
							))}
						</div>
					</div>
				</div>
			);
		}

		const groupedItems: Map<string, Array<GuildMemberRecord>> = new Map();
		const groups = memberListState.groups;

		for (const group of groups) {
			groupedItems.set(group.id, []);
		}

		for (const [, item] of memberListState.items) {
			if (item.type === 'member') {
				const member = item.data as GuildMemberRecord;
				for (let i = groups.length - 1; i >= 0; i--) {
					const group = groups[i];
					const members = groupedItems.get(group.id);
					if (members) {
						members.push(member);
						break;
					}
				}
			}
		}

		return (
			<div className={styles.memberListContent} onScroll={handleScroll}>
				{groups.map((group) => {
					const members = groupedItems.get(group.id) ?? [];
					if (members.length === 0) {
						return null;
					}
					return (
						<LazyMemberListGroup
							key={group.id}
							guild={guild}
							group={group}
							channelId={channel.id}
							members={members}
							onMemberLongPress={onMemberLongPress}
						/>
					);
				})}
			</div>
		);
	},
);

const GuildMemberList = observer(
	({
		guild,
		channel,
		onMemberLongPress,
		enabled = true,
	}: {
		guild: GuildRecord;
		channel: ChannelRecord;
		onMemberLongPress?: (member: GuildMemberRecord) => void;
		enabled?: boolean;
	}) => {
		return (
			<LazyGuildMemberList guild={guild} channel={channel} onMemberLongPress={onMemberLongPress} enabled={enabled} />
		);
	},
);

export const ChannelDetailsBottomSheet: React.FC<ChannelDetailsBottomSheetProps> = observer(
	({isOpen, onClose, channel, initialTab = 'members', openSearchImmediately = false}) => {
		const {t, i18n} = useLingui();
		const [activeTab, setActiveTab] = React.useState<ChannelDetailsTab>(initialTab);
		const [muteSheetOpen, setMuteSheetOpen] = React.useState(false);
		const [searchSheetOpen, setSearchSheetOpen] = React.useState(false);
		const [isTopicExpanded, setIsTopicExpanded] = React.useState(false);
		const [moreOptionsSheetOpen, setMoreOptionsSheetOpen] = React.useState(false);
		const [notificationSheetOpen, setNotificationSheetOpen] = React.useState(false);
		const [activeMemberSheet, setActiveMemberSheet] = React.useState<{
			member: GuildMemberRecord;
			user: UserRecord;
		} | null>(null);

		const leaveGroup = useLeaveGroup();

		React.useEffect(() => {
			setActiveTab(initialTab);
		}, [initialTab]);

		React.useEffect(() => {
			if (openSearchImmediately && isOpen) {
				setSearchSheetOpen(true);
			}
		}, [openSearchImmediately, isOpen]);

		const isDM = channel.type === ChannelTypes.DM;
		const isPersonalNotes = channel.type === ChannelTypes.DM_PERSONAL_NOTES;
		const isGuildChannel = channel.guildId != null;
		const guild = isGuildChannel ? GuildStore.getGuild(channel.guildId) : null;
		const recipient = isDM && channel.recipientIds.length > 0 ? UserStore.getUser(channel.recipientIds[0]) : null;
		const currentUser = UserStore.currentUser;
		const currentUserId = AuthenticationStore.currentUserId;

		const guildId = channel.guildId ?? null;
		const settingsGuildId = isGuildChannel ? channel.guildId : null;
		const channelOverride = UserGuildSettingsStore.getChannelOverride(settingsGuildId, channel.id);
		const isMuted = channelOverride?.muted ?? false;
		const muteConfig = channelOverride?.mute_config;
		const mutedText = getMutedText(isMuted, muteConfig);

		const isGroupDMOwner = channel.type === ChannelTypes.GROUP_DM && channel.ownerId === currentUserId;

		const channelTypeLabel = React.useMemo(() => {
			switch (channel.type) {
				case ChannelTypes.GUILD_TEXT:
					return t`Text Channel`;
				case ChannelTypes.GUILD_VOICE:
					return t`Voice Channel`;
				case ChannelTypes.DM:
					return t`Direct Message`;
				case ChannelTypes.DM_PERSONAL_NOTES:
					return t`Personal Notes`;
				case ChannelTypes.GROUP_DM:
					return t`Group Direct Message`;
				default:
					return t`Channel`;
			}
		}, [channel.type]);

		const isFavorited = !!FavoritesStore.getChannel(channel.id);
		const showFavorites = AccessibilityStore.showFavorites;

		const isGroupDM = channel.type === ChannelTypes.GROUP_DM;

		const developerMode = UserSettingsStore.developerMode;

		const handleSearchClick = () => {
			setSearchSheetOpen(true);
		};

		const handleBellClick = () => {
			setMuteSheetOpen(true);
		};

		const handleCogClick = () => {
			setMoreOptionsSheetOpen(true);
		};

		const handleMoreOptionsClose = () => {
			setMoreOptionsSheetOpen(false);
		};

		const handleNotificationClose = () => {
			setNotificationSheetOpen(false);
		};

		const handleMarkAsRead = React.useCallback(() => {
			ReadStateActionCreators.ack(channel.id, true, true);
			ToastActionCreators.createToast({type: 'success', children: t`Marked as read`});
		}, [channel.id]);

		const handleInvite = React.useCallback(() => {
			ModalActionCreators.push(modal(() => <InviteModal channelId={channel.id} />));
			onClose();
		}, [channel.id, onClose]);

		const handleCopyLink = React.useCallback(() => {
			const channelLink = buildChannelLink({
				guildId: channel.guildId,
				channelId: channel.id,
			});
			TextCopyActionCreators.copy(i18n, channelLink);
			ToastActionCreators.createToast({type: 'success', children: t`Link copied to clipboard`});
		}, [channel.id, channel.guildId, i18n]);

		const handleCopyId = React.useCallback(() => {
			TextCopyActionCreators.copy(i18n, channel.id);
			ToastActionCreators.createToast({type: 'success', children: t`Channel ID copied to clipboard`});
		}, [channel.id, i18n]);

		const handleToggleFavorite = React.useCallback(() => {
			if (isFavorited) {
				FavoritesStore.removeChannel(channel.id);
				ToastActionCreators.createToast({type: 'success', children: t`Removed from favorites`});
			} else {
				FavoritesStore.addChannel(channel.id, channel.guildId ?? ME, null);
				ToastActionCreators.createToast({type: 'success', children: t`Added to favorites`});
			}
		}, [channel.id, channel.guildId, isFavorited]);

		const handleDebugChannel = React.useCallback(() => {
			const channelName = channel.name ?? t`Channel`;
			ModalActionCreators.push(modal(() => <ChannelDebugModal title={channelName} channel={channel} />));
			onClose();
		}, [channel, onClose]);

		const handleDebugUser = React.useCallback(() => {
			if (!recipient) return;
			ModalActionCreators.push(modal(() => <UserDebugModal title={recipient.username} user={recipient} />));
			onClose();
		}, [recipient, onClose]);

		const handlePinDM = React.useCallback(async () => {
			handleMoreOptionsClose();
			try {
				await PrivateChannelActionCreators.pinDmChannel(channel.id);
				ToastActionCreators.createToast({
					type: 'success',
					children: isGroupDM ? t`Pinned group` : t`Pinned DM`,
				});
			} catch (error) {
				console.error('Failed to pin:', error);
				ToastActionCreators.createToast({
					type: 'error',
					children: isGroupDM ? t`Failed to pin group` : t`Failed to pin DM`,
				});
			}
		}, [channel.id, isGroupDM]);

		const handleUnpinDM = React.useCallback(async () => {
			handleMoreOptionsClose();
			try {
				await PrivateChannelActionCreators.unpinDmChannel(channel.id);
				ToastActionCreators.createToast({
					type: 'success',
					children: isGroupDM ? t`Unpinned group` : t`Unpinned DM`,
				});
			} catch (error) {
				console.error('Failed to unpin:', error);
				ToastActionCreators.createToast({
					type: 'error',
					children: isGroupDM ? t`Failed to unpin group` : t`Failed to unpin DM`,
				});
			}
		}, [channel.id, isGroupDM]);

		const handleCloseDM = React.useCallback(() => {
			handleMoreOptionsClose();
			onClose();
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Close DM`}
						description={t`Are you sure you want to close your DM with ${recipient?.username ?? ''}? You can always reopen it later.`}
						primaryText={t`Close DM`}
						primaryVariant="danger-primary"
						onPrimary={async () => {
							try {
								await ChannelActionCreators.remove(channel.id);
								const selectedChannel = SelectedChannelStore.selectedChannelIds.get(ME);
								if (selectedChannel === channel.id) {
									RouterUtils.transitionTo(Routes.ME);
								}
								ToastActionCreators.createToast({
									type: 'success',
									children: t`DM closed`,
								});
							} catch (error) {
								console.error('Failed to close DM:', error);
								ModalActionCreators.push(modal(() => <DMCloseFailedModal />));
							}
						}}
					/>
				)),
			);
		}, [channel.id, recipient, onClose]);

		const handleLeaveGroup = React.useCallback(() => {
			handleMoreOptionsClose();
			onClose();
			leaveGroup(channel.id);
		}, [channel.id, onClose, leaveGroup]);

		const handleEditGroup = React.useCallback(() => {
			handleMoreOptionsClose();
			onClose();
			ModalActionCreators.push(modal(() => <EditGroupModal channelId={channel.id} />));
		}, [channel.id, onClose]);

		const handleShowInvites = React.useCallback(() => {
			handleMoreOptionsClose();
			onClose();
			ModalActionCreators.push(modal(() => <GroupInvitesModal channelId={channel.id} />));
		}, [channel.id, onClose]);

		const handleOpenAddFriendsToGroup = React.useCallback(() => {
			handleMoreOptionsClose();
			onClose();
			ModalActionCreators.push(modal(() => <AddFriendsToGroupModal channelId={channel.id} />));
		}, [channel.id, onClose]);

		const handleCopyUserId = React.useCallback(() => {
			if (!recipient) return;
			TextCopyActionCreators.copy(i18n, recipient.id);
			ToastActionCreators.createToast({type: 'success', children: t`User ID copied to clipboard`});
		}, [recipient, i18n]);

		const handleEditChannel = React.useCallback(() => {
			ModalActionCreators.push(modal(() => <ChannelSettingsModal channelId={channel.id} />));
			onClose();
		}, [channel.id, onClose]);

		const handleDeleteChannel = React.useCallback(() => {
			onClose();
			const channelType = channel.type === ChannelTypes.GUILD_VOICE ? t`Voice Channel` : t`Text Channel`;
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Delete ${channelType}`}
						description={t`Are you sure you want to delete #${channel.name ?? 'this channel'}? This cannot be undone.`}
						primaryText={t`Delete Channel`}
						primaryVariant="danger-primary"
						onPrimary={async () => {
							try {
								await ChannelActionCreators.remove(channel.id);
								ToastActionCreators.createToast({
									type: 'success',
									children: t`Channel deleted`,
								});
							} catch (error) {
								console.error('Failed to delete channel:', error);
								ToastActionCreators.createToast({
									type: 'error',
									children: t`Failed to delete channel`,
								});
							}
						}}
					/>
				)),
			);
		}, [channel.id, channel.name, channel.type, onClose]);

		const handleOpenGuildNotificationSettings = React.useCallback(() => {
			if (!guildId) return;
			ModalActionCreators.push(modal(() => <GuildNotificationSettingsModal guildId={guildId} />));
		}, [guildId]);

		const handleOpenCreateGroupModal = React.useCallback(() => {
			const duplicateExcludeChannelId = channel.type === ChannelTypes.GROUP_DM ? channel.id : undefined;
			ModalActionCreators.push(
				modal(() => (
					<CreateDMModal
						initialSelectedUserIds={Array.from(channel.recipientIds)}
						duplicateExcludeChannelId={duplicateExcludeChannelId}
					/>
				)),
			);
		}, [channel.id, channel.recipientIds, channel.type]);

		const handleNotificationLevelChange = React.useCallback(
			(level: number) => {
				if (!guildId) return;
				if (level === MessageNotifications.INHERIT) {
					UserGuildSettingsActionCreators.updateChannelOverride(
						guildId,
						channel.id,
						{
							message_notifications: MessageNotifications.INHERIT,
						},
						{persistImmediately: true},
					);
				} else {
					UserGuildSettingsActionCreators.updateMessageNotifications(guildId, level, channel.id, {
						persistImmediately: true,
					});
				}
			},
			[guildId, channel.id],
		);

		const handleMemberLongPress = React.useCallback((member: GuildMemberRecord) => {
			setActiveMemberSheet({member, user: member.user});
		}, []);

		const handleCloseMemberSheet = React.useCallback(() => {
			setActiveMemberSheet(null);
		}, []);

		const isMemberTabVisible = isOpen && activeTab === 'members';

		return (
			<>
				<Sheet.Root isOpen={isOpen} onClose={onClose} snapPoints={[0, 1]} initialSnap={1}>
					<Sheet.Handle />
					<Sheet.Content padding="none">
						<Scroller className={styles.mainScroller}>
							<div className={styles.channelInfoSection}>
								<Sheet.CloseButton onClick={onClose} className={styles.closeButton} />
								<div className={styles.channelInfoContainer}>
									{isDM && recipient ? (
										<StatusAwareAvatar user={recipient} size={48} />
									) : isGroupDM ? (
										<GroupDMAvatar channel={channel} size={48} />
									) : isPersonalNotes && currentUser ? (
										<StatusAwareAvatar user={currentUser} size={48} />
									) : (
										<div className={styles.channelAvatar}>
											{ChannelUtils.getIcon(channel, {className: styles.iconLarge})}
										</div>
									)}

									<div className={styles.channelInfoContent}>
										{isDM && recipient ? (
											<>
												<div className={styles.channelInfoUserContainer}>
													<span className={styles.channelInfoUsername}>{recipient.username}</span>
													<span className={styles.channelInfoDiscriminator}>#{recipient.discriminator}</span>
												</div>
												{recipient.bot && <UserTag className={styles.channelInfoTag} system={recipient.system} />}
											</>
										) : isGroupDM ? (
											<>
												<h2 className={styles.channelInfoTitle}>{ChannelUtils.getDMDisplayName(channel)}</h2>
												<p className={styles.channelInfoSubtitle}>
													{t`Group DM · ${channel.recipientIds.length + 1} members`}
												</p>
											</>
										) : isPersonalNotes ? (
											<>
												<h2 className={styles.channelInfoTitle}>
													<Trans>Personal Notes</Trans>
												</h2>
												<p className={styles.channelInfoSubtitle}>
													<Trans>Your private space</Trans>
												</p>
											</>
										) : (
											<>
												<h2 className={styles.channelInfoTitle}>
													<span className={styles.channelNameWithIcon}>
														{ChannelUtils.getIcon(channel, {className: styles.channelNameIcon})}
														{channel.name}
													</span>
												</h2>
												<p className={styles.channelInfoSubtitle}>{channelTypeLabel}</p>
											</>
										)}
									</div>
								</div>

								{channel.topic && !isDM && !isPersonalNotes && (
									<div className={styles.topicSectionContainer}>
										<div className={styles.topicWrapper}>
											<div
												role="button"
												className={`${markupStyles.markup} ${styles.topicMarkup} ${!isTopicExpanded ? styles.topicMarkupCollapsed : ''}`}
												style={
													isTopicExpanded
														? {
																wordWrap: 'break-word',
																overflowWrap: 'break-word',
																whiteSpace: 'break-spaces',
															}
														: undefined
												}
												onClick={() =>
													ModalActionCreators.push(modal(() => <ChannelTopicModal channelId={channel.id} />))
												}
												onKeyDown={(e) =>
													e.key === 'Enter' &&
													ModalActionCreators.push(modal(() => <ChannelTopicModal channelId={channel.id} />))
												}
												tabIndex={0}
											>
												<SafeMarkdown
													content={channel.topic}
													options={{
														context: MarkdownContext.RESTRICTED_INLINE_REPLY,
														channelId: channel.id,
													}}
												/>
											</div>
											<button
												type="button"
												onClick={() => setIsTopicExpanded(!isTopicExpanded)}
												className={styles.topicExpandButton}
											>
												{isTopicExpanded ? (
													<CaretUpIcon className={styles.iconSmall} weight="bold" />
												) : (
													<CaretDownIcon className={styles.iconSmall} weight="bold" />
												)}
											</button>
										</div>
									</div>
								)}
							</div>

							<div className={styles.quickActionsRow}>
								<div className={styles.quickActionsScroll}>
									<QuickActionButton
										icon={<BellIcon weight={isMuted ? 'regular' : 'fill'} size={20} />}
										label={isMuted ? t`Unmute` : t`Mute`}
										onClick={handleBellClick}
										isActive={isMuted}
									/>

									<QuickActionButton
										icon={<MagnifyingGlassIcon weight="bold" size={20} />}
										label={t`Search`}
										onClick={handleSearchClick}
									/>

									<QuickActionButton
										icon={<DotsThreeVerticalIcon weight="bold" size={20} />}
										label={t`More`}
										onClick={handleCogClick}
									/>
								</div>
							</div>

							<div className={styles.tabBarContainer}>
								<button
									type="button"
									onClick={() => setActiveTab('members')}
									className={`${styles.tabButton} ${activeTab === 'members' ? styles.tabButtonActive : styles.tabButtonInactive}`}
									style={activeTab === 'members' ? {borderBottomColor: 'var(--brand-primary-light)'} : undefined}
								>
									<UsersIcon className={styles.tabIcon} />
									<Trans>Members</Trans>
								</button>
								<button
									type="button"
									onClick={() => setActiveTab('pins')}
									className={`${styles.tabButton} ${activeTab === 'pins' ? styles.tabButtonActive : styles.tabButtonInactive}`}
									style={activeTab === 'pins' ? {borderBottomColor: 'var(--brand-primary-light)'} : undefined}
								>
									<PushPinIcon className={styles.tabIcon} />
									<Trans>Pins</Trans>
								</button>
							</div>

							<div className={styles.contentArea}>
								{activeTab === 'members' && (
									<div className={styles.membersTabContent}>
										{(isDM || isGroupDM || isPersonalNotes) && (
											<div className={styles.dmMembersContainer}>
												{isDM && recipient && (
													<button type="button" className={styles.newGroupButton} onClick={handleOpenCreateGroupModal}>
														<div className={styles.newGroupIconContainer}>
															<ChatCircleIcon className={`${styles.iconMedium} ${styles.newGroupIconWhite}`} />
														</div>
														<div className={styles.newGroupContent}>
															<p className={styles.newGroupTitle}>
																<Trans>New Group</Trans>
															</p>
															<p className={styles.newGroupSubtitle}>
																<Trans>Create a new group with {recipient.username}</Trans>
															</p>
														</div>
														<CaretRightIcon className={styles.iconMedium} weight="bold" />
													</button>
												)}

												<div className={styles.membersHeader}>
													<Trans>Members</Trans> —{' '}
													{isPersonalNotes ? 1 : isGroupDM ? channel.recipientIds.length + 1 : 2}
												</div>
												<div className={styles.membersListContainer}>
													{(() => {
														let memberIds: Array<string> = [];
														if (isPersonalNotes) {
															memberIds = currentUser ? [currentUser.id] : [];
														} else if (isGroupDM) {
															memberIds = [...channel.recipientIds];
															if (currentUser && !memberIds.includes(currentUser.id)) {
																memberIds.push(currentUser.id);
															}
														} else if (isDM) {
															memberIds = [...channel.recipientIds];
															if (currentUser && !memberIds.includes(currentUser.id)) {
																memberIds.push(currentUser.id);
															}
														}

														return memberIds.map((userId, index, arr) => {
															const user = UserStore.getUser(userId);
															if (!user) return null;

															const isCurrentUser = user.id === currentUser?.id;
															const isOwner = isGroupDM && channel.ownerId === user.id;

															const handleUserClick = () => {
																UserProfileActionCreators.openUserProfile(user.id);
															};

															return (
																<React.Fragment key={user.id}>
																	<button type="button" onClick={handleUserClick} className={styles.memberItemButton}>
																		<StatusAwareAvatar user={user} size={40} />
																		<div className={styles.memberItemContent}>
																			<span className={styles.memberItemName}>
																				{user.username}
																				{isCurrentUser && (
																					<span className={styles.memberItemYou}>
																						{' '}
																						<Trans>(you)</Trans>
																					</span>
																				)}
																			</span>
																			{(user.bot || isOwner) && (
																				<div className={styles.memberItemTags}>
																					{user.bot && <UserTag system={user.system} />}
																					{isOwner && (
																						<Tooltip text={t`Group Owner`}>
																							<CrownIcon className={styles.ownerCrown} weight="fill" />
																						</Tooltip>
																					)}
																				</div>
																			)}
																		</div>
																	</button>
																	{index < arr.length - 1 && <div className={styles.memberItemDivider} />}
																</React.Fragment>
															);
														});
													})()}
												</div>
											</div>
										)}
										{isGuildChannel && guild && (
											<GuildMemberList
												guild={guild}
												channel={channel}
												onMemberLongPress={handleMemberLongPress}
												enabled={isMemberTabVisible}
											/>
										)}
									</div>
								)}
								{activeTab === 'pins' && (
									<div className={styles.pinsTabContent}>
										<ChannelPinsContent channel={channel} onJump={onClose} />
									</div>
								)}
							</div>
						</Scroller>
					</Sheet.Content>
				</Sheet.Root>

				<Sheet.Root isOpen={muteSheetOpen} onClose={() => setMuteSheetOpen(false)} snapPoints={[0, 1]} initialSnap={1}>
					<Sheet.Handle />
					<Sheet.Header trailing={<Sheet.CloseButton onClick={() => setMuteSheetOpen(false)} />}>
						<Sheet.Title>
							{(() => {
								if (isMuted) {
									if (isGuildChannel) {
										return t`Unmute Channel`;
									}
									return t`Unmute Conversation`;
								}
								if (isGuildChannel) {
									return t`Mute Channel`;
								}
								return t`Mute Conversation`;
							})()}
						</Sheet.Title>
					</Sheet.Header>
					<Sheet.Content padding="none">
						<div className={styles.muteSheetContainer}>
							<div className={styles.muteSheetContent}>
								{isMuted && mutedText ? (
									<>
										<div className={styles.muteStatusBanner}>
											<p className={styles.muteStatusText}>
												<Trans>Currently: {mutedText}</Trans>
											</p>
										</div>
										<div className={styles.muteOptionsContainer}>
											<button
												type="button"
												onClick={() => {
													UserGuildSettingsActionCreators.updateChannelOverride(
														settingsGuildId,
														channel.id,
														{
															muted: false,
															mute_config: null,
														},
														{persistImmediately: true},
													);
													setMuteSheetOpen(false);
												}}
												className={styles.muteOptionButton}
											>
												<span className={styles.muteOptionLabel}>
													<Trans>Unmute</Trans>
												</span>
											</button>
										</div>
									</>
								) : (
									<div className={styles.muteOptionsContainer}>
										{getMuteDurationOptions(t).map((option, index, array) => {
											const isSelected =
												isMuted &&
												((option.value === null && !muteConfig?.end_time) ||
													(option.value !== null && muteConfig?.selected_time_window === option.value));

											return (
												<React.Fragment key={option.label}>
													<button
														type="button"
														onClick={() => {
															UserGuildSettingsActionCreators.updateChannelOverride(
																settingsGuildId,
																channel.id,
																{
																	muted: true,
																	mute_config: createMuteConfig(option.value),
																},
																{persistImmediately: true},
															);
															setMuteSheetOpen(false);
														}}
														className={styles.muteOptionButton}
													>
														<span className={styles.muteOptionLabel}>{option.label}</span>
														{isSelected && <CheckIcon className={styles.iconMedium} weight="bold" />}
													</button>
													{index < array.length - 1 && <div className={styles.muteOptionDivider} />}
												</React.Fragment>
											);
										})}
									</div>
								)}
							</div>
						</div>
					</Sheet.Content>
				</Sheet.Root>

				<MenuBottomSheet
					isOpen={moreOptionsSheetOpen}
					onClose={handleMoreOptionsClose}
					title={isGroupDM ? t`Group Settings` : isDM ? t`DM Settings` : t`Channel Settings`}
					groups={React.useMemo(() => {
						const groups: Array<MenuGroupType> = [];
						const hasUnread = ReadStateStore.hasUnread(channel.id);

						const commonItems: Array<MenuItemType> = [];

						if (showFavorites && !isPersonalNotes) {
							commonItems.push({
								id: 'favorite',
								icon: <StarIcon weight={isFavorited ? 'fill' : 'regular'} size={20} />,
								label: isFavorited ? t`Remove from Favorites` : t`Add to Favorites`,
								onClick: () => {
									handleToggleFavorite();
									handleMoreOptionsClose();
								},
							});
						}

						if (hasUnread) {
							commonItems.push({
								id: 'mark-as-read',
								icon: <MarkAsReadIcon size={20} />,
								label: t`Mark as Read`,
								onClick: handleMarkAsRead,
							});
						}

						if (isDM || isGroupDM) {
							commonItems.push(
								channel.isPinned
									? {
											id: 'unpin',
											icon: <PushPinIcon weight="fill" size={20} />,
											label: isGroupDM ? t`Unpin Group DM` : t`Unpin DM`,
											onClick: handleUnpinDM,
										}
									: {
											id: 'pin',
											icon: <PushPinIcon weight="fill" size={20} />,
											label: isGroupDM ? t`Pin Group DM` : t`Pin DM`,
											onClick: handlePinDM,
										},
							);
						}

						const canInvite = isGuildChannel ? InviteUtils.canInviteToChannel(channel.id, channel.guildId) : false;

						if (canInvite) {
							commonItems.push({
								id: 'invite',
								icon: <InviteIcon size={20} />,
								label: t`Invite People`,
								onClick: handleInvite,
							});
						}

						if (isGuildChannel) {
							commonItems.push(
								{
									id: 'copy-link',
									icon: <CopyLinkIcon size={20} />,
									label: t`Copy Link`,
									onClick: handleCopyLink,
								},
								{
									id: 'notification-settings',
									icon: <BellIcon weight="fill" size={20} />,
									label: t`Notification Settings`,
									onClick: () => {
										handleMoreOptionsClose();
										setNotificationSheetOpen(true);
									},
								},
							);
						}

						if (commonItems.length > 0) {
							groups.push({items: commonItems});
						}

						if (isGroupDM) {
							const groupItems: Array<MenuItemType> = [
								{
									id: 'edit-group',
									icon: <PencilIcon weight="fill" size={20} />,
									label: t`Edit Group`,
									onClick: handleEditGroup,
								},
							];

							if (channel.recipientIds.length + 1 < MAX_GROUP_DM_RECIPIENTS) {
								groupItems.push({
									id: 'add-friends',
									icon: <UserPlusIcon weight="fill" size={20} />,
									label: t`Add Friends to Group`,
									onClick: () => {
										handleMoreOptionsClose();
										ModalActionCreators.push(modal(() => <AddFriendsToGroupModal channelId={channel.id} />));
									},
								});
							}

							if (isGroupDMOwner) {
								groupItems.push({
									id: 'invites',
									icon: <TicketIcon weight="fill" size={20} />,
									label: t`Invites`,
									onClick: handleShowInvites,
								});
							}

							groups.push({items: groupItems});
						}

						if (isGuildChannel) {
							const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {
								channelId: channel.id,
								guildId: channel.guildId,
							});

							if (canManageChannels) {
								const managerItems: Array<MenuItemType> = [
									{
										id: 'edit-channel',
										icon: <EditIcon size={20} />,
										label: t`Edit Channel`,
										onClick: handleEditChannel,
									},
									{
										id: 'delete-channel',
										icon: <DeleteIcon size={20} />,
										label: t`Delete Channel`,
										onClick: handleDeleteChannel,
										danger: true,
									},
								];
								groups.push({items: managerItems});
							}
						}

						if (isDM) {
							groups.push({
								items: [
									{
										id: 'close-dm',
										icon: <XIcon weight="bold" size={20} />,
										label: t`Close DM`,
										onClick: handleCloseDM,
										danger: true,
									},
								],
							});
						}

						if (isGroupDM) {
							groups.push({
								items: [
									{
										id: 'leave-group',
										icon: <SignOutIcon weight="fill" size={20} />,
										label: t`Leave Group`,
										onClick: handleLeaveGroup,
										danger: true,
									},
								],
							});
						}

						const miscItems: Array<MenuItemType> = [];

						if (developerMode) {
							miscItems.push({
								id: 'debug-channel',
								icon: <BugIcon weight="fill" size={20} />,
								label: t`Debug Channel`,
								onClick: handleDebugChannel,
							});

							if (isDM && recipient) {
								miscItems.push({
									id: 'debug-user',
									icon: <BugIcon weight="fill" size={20} />,
									label: t`Debug User`,
									onClick: handleDebugUser,
								});
							}
						}

						if (isDM && recipient) {
							miscItems.push({
								id: 'copy-user-id',
								icon: <CopyIdIcon size={20} />,
								label: t`Copy User ID`,
								onClick: handleCopyUserId,
							});
						}

						miscItems.push({
							id: 'copy-channel-id',
							icon: <CopyIdIcon size={20} />,
							label: t`Copy Channel ID`,
							onClick: handleCopyId,
						});

						if (miscItems.length > 0) {
							groups.push({items: miscItems});
						}

						return groups;
					}, [
						channel.id,
						channel.guildId,
						channel.name,
						channel.type,
						channel.isPinned,
						channel.recipientIds,
						isDM,
						isGroupDM,
						isGuildChannel,
						isGroupDMOwner,
						isPersonalNotes,
						showFavorites,
						isFavorited,
						recipient,
						developerMode,
						handleMarkAsRead,
						handleInvite,
						handleCopyLink,
						handlePinDM,
						handleUnpinDM,
						handleEditChannel,
						handleDeleteChannel,
						handleEditGroup,
						handleShowInvites,
						handleOpenAddFriendsToGroup,
						handleCloseDM,
						handleLeaveGroup,
						handleToggleFavorite,
						handleDebugChannel,
						handleDebugUser,
						handleCopyUserId,
						handleCopyId,
					])}
				/>

				<MenuBottomSheet
					isOpen={notificationSheetOpen}
					onClose={handleNotificationClose}
					title={t`Notification Settings`}
					groups={React.useMemo((): Array<MenuGroupType> => {
						const categoryId = channel.parentId;
						const hasCategory = categoryId != null;

						const channelNotifications = UserGuildSettingsStore.getChannelOverride(
							guildId,
							channel.id,
						)?.message_notifications;
						const currentNotificationLevel = channelNotifications ?? MessageNotifications.INHERIT;

						const guildNotificationLevel = UserGuildSettingsStore.getGuildMessageNotifications(guildId);

						const categoryOverride = UserGuildSettingsStore.getChannelOverride(guildId, categoryId ?? '');
						const categoryNotifications = categoryId ? categoryOverride?.message_notifications : undefined;

						const resolveEffectiveLevel = (level: number | undefined, fallback: number): number => {
							if (level === undefined || level === MessageNotifications.INHERIT) {
								return fallback;
							}
							return level;
						};

						const categoryDefaultLevel = resolveEffectiveLevel(categoryNotifications, guildNotificationLevel);
						const defaultSubtext = getNotificationSettingsLabel(categoryDefaultLevel) ?? undefined;

						return [
							{
								items: [
									{
										label: hasCategory ? t`Category Default` : t`Community Default`,
										subtext: defaultSubtext,
										selected: currentNotificationLevel === MessageNotifications.INHERIT,
										onSelect: () => handleNotificationLevelChange(MessageNotifications.INHERIT),
									},
									{
										label: t`All Messages`,
										selected: currentNotificationLevel === MessageNotifications.ALL_MESSAGES,
										onSelect: () => handleNotificationLevelChange(MessageNotifications.ALL_MESSAGES),
									},
									{
										label: t`Only @mentions`,
										selected: currentNotificationLevel === MessageNotifications.ONLY_MENTIONS,
										onSelect: () => handleNotificationLevelChange(MessageNotifications.ONLY_MENTIONS),
									},
									{
										label: t`Nothing`,
										selected: currentNotificationLevel === MessageNotifications.NO_MESSAGES,
										onSelect: () => handleNotificationLevelChange(MessageNotifications.NO_MESSAGES),
									},
								] as Array<MenuRadioType>,
							},
							{
								items: [
									{
										id: 'open-guild-settings',
										icon: <GearIcon weight="bold" size={20} />,
										label: t`Open Community Notification Settings`,
										onClick: handleOpenGuildNotificationSettings,
									},
								] as Array<MenuItemType>,
							},
						];
					}, [
						guildId,
						channel.id,
						channel.parentId,
						handleNotificationLevelChange,
						handleOpenGuildNotificationSettings,
					])}
				/>

				<ChannelSearchBottomSheet
					isOpen={searchSheetOpen}
					onClose={() => setSearchSheetOpen(false)}
					channel={channel}
				/>

				{activeMemberSheet && guildId && (
					<GuildMemberActionsSheet
						isOpen={true}
						onClose={handleCloseMemberSheet}
						user={activeMemberSheet.user}
						member={activeMemberSheet.member}
						guildId={guildId}
					/>
				)}
			</>
		);
	},
);
