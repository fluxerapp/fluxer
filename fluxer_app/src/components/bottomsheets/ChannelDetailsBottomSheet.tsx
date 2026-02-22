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

import * as ChannelActionCreators from '@app/actions/ChannelActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as PrivateChannelActionCreators from '@app/actions/PrivateChannelActionCreators';
import * as ReadStateActionCreators from '@app/actions/ReadStateActionCreators';
import * as TextCopyActionCreators from '@app/actions/TextCopyActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import * as UserGuildSettingsActionCreators from '@app/actions/UserGuildSettingsActionCreators';
import * as UserProfileActionCreators from '@app/actions/UserProfileActionCreators';
import {DMCloseFailedModal} from '@app/components/alerts/DMCloseFailedModal';
import styles from '@app/components/bottomsheets/ChannelDetailsBottomSheet.module.css';
import {ChannelSearchBottomSheet} from '@app/components/bottomsheets/ChannelSearchBottomSheet';
import {MuteDurationSheet} from '@app/components/bottomsheets/MuteDurationSheet';
import {createMuteConfig} from '@app/components/channel/MuteOptions';
import {PreloadableUserPopout} from '@app/components/channel/PreloadableUserPopout';
import {MemberListUnavailableFallback} from '@app/components/channel/shared/MemberListUnavailableFallback';
import {UserTag} from '@app/components/channel/UserTag';
import {CustomStatusDisplay} from '@app/components/common/custom_status_display/CustomStatusDisplay';
import {GroupDMAvatar} from '@app/components/common/GroupDMAvatar';
import {ChannelDebugModal} from '@app/components/debug/ChannelDebugModal';
import {UserDebugModal} from '@app/components/debug/UserDebugModal';
import {LongPressable} from '@app/components/LongPressable';
import {AddFriendsToGroupModal} from '@app/components/modals/AddFriendsToGroupModal';
import {ChannelSettingsModal} from '@app/components/modals/ChannelSettingsModal';
import {ChannelTopicModal} from '@app/components/modals/ChannelTopicModal';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import {CreateDMModal} from '@app/components/modals/CreateDMModal';
import {EditGroupModal} from '@app/components/modals/EditGroupModal';
import {GroupInvitesModal} from '@app/components/modals/GroupInvitesModal';
import {GuildNotificationSettingsModal} from '@app/components/modals/GuildNotificationSettingsModal';
import {GuildMemberActionsSheet} from '@app/components/modals/guild_tabs/GuildMemberActionsSheet';
import {InviteModal} from '@app/components/modals/InviteModal';
import {ChannelPinsContent} from '@app/components/shared/ChannelPinsContent';
import {
	ChevronRightIcon,
	CloseDMIcon,
	CollapseChevronIcon,
	CopyIdIcon,
	CopyLinkIcon,
	DebugMessageIcon,
	DeleteIcon,
	EditIcon,
	ExpandChevronIcon,
	FavoriteIcon,
	InviteIcon,
	InvitesIcon,
	LeaveIcon,
	MarkAsReadIcon,
	MembersIcon,
	MoreOptionsVerticalIcon,
	MuteIcon,
	NewGroupIcon,
	NotificationSettingsIcon,
	OwnerCrownIcon,
	PinIcon,
	SearchIcon,
	SettingsIcon,
} from '@app/components/uikit/context_menu/ContextMenuIcons';
import type {MenuGroupType, MenuItemType, MenuRadioType} from '@app/components/uikit/menu_bottom_sheet/MenuBottomSheet';
import {MenuBottomSheet} from '@app/components/uikit/menu_bottom_sheet/MenuBottomSheet';
import {Scroller} from '@app/components/uikit/Scroller';
import {StatusAwareAvatar} from '@app/components/uikit/StatusAwareAvatar';
import * as Sheet from '@app/components/uikit/sheet/Sheet';
import {Tooltip} from '@app/components/uikit/tooltip/Tooltip';
import {useLeaveGroup} from '@app/hooks/useLeaveGroup';
import {useMemberListCustomStatus} from '@app/hooks/useMemberListCustomStatus';
import {useMemberListPresence} from '@app/hooks/useMemberListPresence';
import {useMemberListSubscription} from '@app/hooks/useMemberListSubscription';
import {usePressable} from '@app/hooks/usePressable';
import {Logger} from '@app/lib/Logger';
import {SafeMarkdown} from '@app/lib/markdown';
import {MarkdownContext} from '@app/lib/markdown/renderers/RendererTypes';
import {Routes} from '@app/Routes';
import type {ChannelRecord} from '@app/records/ChannelRecord';
import type {GuildMemberRecord} from '@app/records/GuildMemberRecord';
import type {GuildRecord} from '@app/records/GuildRecord';
import type {UserRecord} from '@app/records/UserRecord';
import AccessibilityStore from '@app/stores/AccessibilityStore';
import AuthenticationStore from '@app/stores/AuthenticationStore';
import FavoritesStore from '@app/stores/FavoritesStore';
import GuildStore from '@app/stores/GuildStore';
import MemberSidebarStore from '@app/stores/MemberSidebarStore';
import PermissionStore from '@app/stores/PermissionStore';
import ReadStateStore from '@app/stores/ReadStateStore';
import SelectedChannelStore from '@app/stores/SelectedChannelStore';
import TypingStore from '@app/stores/TypingStore';
import UserGuildSettingsStore from '@app/stores/UserGuildSettingsStore';
import UserSettingsStore from '@app/stores/UserSettingsStore';
import UserStore from '@app/stores/UserStore';
import markupStyles from '@app/styles/Markup.module.css';
import * as ChannelUtils from '@app/utils/ChannelUtils';
import {getMutedText, getNotificationSettingsLabel} from '@app/utils/ContextMenuUtils';
import {isGroupDmFull} from '@app/utils/GroupDmUtils';
import * as InviteUtils from '@app/utils/InviteUtils';
import {
	buildMemberListLayout,
	getRowIndexRangeForMemberIndexRange,
	getTotalMemberCount,
} from '@app/utils/MemberListLayout';
import * as MemberListUtils from '@app/utils/MemberListUtils';
import {buildChannelLink} from '@app/utils/MessageLinkUtils';
import * as NicknameUtils from '@app/utils/NicknameUtils';
import * as RouterUtils from '@app/utils/RouterUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {isOfflineStatus} from '@fluxer/constants/src/StatusConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const logger = new Logger('ChannelDetailsBottomSheet');

const MEMBER_ITEM_HEIGHT = 56;
const INITIAL_MEMBER_RANGE: [number, number] = [0, 99];
const SCROLL_BUFFER = 50;

function isScrollableOverflow(value: string): boolean {
	return value === 'auto' || value === 'scroll' || value === 'overlay';
}

function findScrollableParent(node: HTMLElement | null): HTMLElement | null {
	let currentNode = node?.parentElement ?? null;
	while (currentNode) {
		const computedStyle = window.getComputedStyle(currentNode);
		if (isScrollableOverflow(computedStyle.overflowY) || isScrollableOverflow(computedStyle.overflow)) {
			return currentNode;
		}
		currentNode = currentNode.parentElement;
	}
	return null;
}

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
		const status = useMemberListPresence({
			guildId: guild.id,
			channelId,
			userId: member.user.id,
			enabled: true,
		});
		const memberListCustomStatus = useMemberListCustomStatus({
			guildId: guild.id,
			channelId,
			userId: member.user.id,
			enabled: true,
		});

		const handleLongPress = useCallback(() => {
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
						status={status}
					/>
					<div className={styles.memberContent}>
						<div className={styles.memberNameRow}>
							<span className={styles.memberName} style={{color: member.getColorString()}}>
								{NicknameUtils.getNickname(member.user, guild.id)}
							</span>
							{guild.isOwner(member.user.id) && (
								<div className={styles.crownContainer}>
									<Tooltip text={t`Community Owner`}>
										<OwnerCrownIcon className={styles.crownIcon} />
									</Tooltip>
								</div>
							)}
							{member.user.bot && <UserTag className={styles.memberTag} system={member.user.system} />}
						</div>
						{!member.user.bot && (
							<CustomStatusDisplay
								customStatus={memberListCustomStatus}
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
		const groupName = (() => {
			switch (group.id) {
				case 'online':
					return t`Online`;
				case 'offline':
					return t`Offline`;
				default: {
					const role = guild.getRole(group.id);
					return role?.name ?? group.id;
				}
			}
		})();

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
		const subscribedRangeRef = useRef<[number, number]>(INITIAL_MEMBER_RANGE);
		const listContainerRef = useRef<HTMLDivElement | null>(null);
		const scrollAnimationFrameRef = useRef<number | null>(null);
		const memberListUpdatesDisabled = (guild.disabledOperations & GuildOperations.MEMBER_LIST_UPDATES) !== 0;

		const {subscribe} = useMemberListSubscription({
			guildId: guild.id,
			channelId: channel.id,
			enabled: enabled && !memberListUpdatesDisabled,
			allowInitialUnfocusedLoad: true,
		});

		const memberListState = MemberSidebarStore.getList(guild.id, channel.id);
		const isLoading = !memberListState || memberListState.items.size === 0;
		const memberCount = memberListState?.memberCount ?? 0;
		const groups = memberListState?.groups ?? [];
		const layouts = useMemo(() => buildMemberListLayout(groups), [groups]);
		const totalMembers = useMemo(() => Math.max(memberCount, getTotalMemberCount(groups)), [groups, memberCount]);

		const updateSubscribedRange = useCallback(
			(scrollTop: number, clientHeight: number) => {
				const startMemberIndex = Math.max(0, Math.floor(scrollTop / MEMBER_ITEM_HEIGHT) - SCROLL_BUFFER);
				const endMemberIndex = Math.max(
					INITIAL_MEMBER_RANGE[1],
					Math.ceil((scrollTop + clientHeight) / MEMBER_ITEM_HEIGHT) + SCROLL_BUFFER,
				);

				let nextRange: [number, number] = [startMemberIndex, endMemberIndex];
				if (totalMembers > 0) {
					const maxMemberIndex = totalMembers - 1;
					const clampedStart = Math.min(startMemberIndex, maxMemberIndex);
					const clampedEnd = Math.min(Math.max(clampedStart, endMemberIndex), maxMemberIndex);
					const rowRange = getRowIndexRangeForMemberIndexRange(layouts, clampedStart, clampedEnd);
					nextRange = rowRange ?? [clampedStart, clampedEnd];
				}

				const [previousStart, previousEnd] = subscribedRangeRef.current;
				if (nextRange[0] === previousStart && nextRange[1] === previousEnd) {
					return;
				}

				subscribedRangeRef.current = nextRange;
				subscribe([nextRange]);
			},
			[layouts, subscribe, totalMembers],
		);

		useEffect(() => {
			if (!enabled || memberListUpdatesDisabled) {
				return;
			}

			const listContainer = listContainerRef.current;
			if (!listContainer) {
				return;
			}

			const scrollParent = findScrollableParent(listContainer);
			if (!scrollParent) {
				return;
			}

			const processScroll = () => {
				scrollAnimationFrameRef.current = null;
				updateSubscribedRange(scrollParent.scrollTop, scrollParent.clientHeight);
			};

			const handleScroll = () => {
				if (scrollAnimationFrameRef.current !== null) {
					return;
				}
				scrollAnimationFrameRef.current = window.requestAnimationFrame(processScroll);
			};

			handleScroll();
			scrollParent.addEventListener('scroll', handleScroll, {passive: true});
			window.addEventListener('resize', handleScroll);

			return () => {
				scrollParent.removeEventListener('scroll', handleScroll);
				window.removeEventListener('resize', handleScroll);
				if (scrollAnimationFrameRef.current !== null) {
					window.cancelAnimationFrame(scrollAnimationFrameRef.current);
					scrollAnimationFrameRef.current = null;
				}
			};
		}, [enabled, memberListUpdatesDisabled, updateSubscribedRange]);

		if (memberListUpdatesDisabled) {
			return (
				<div className={styles.memberListFallbackContainer}>
					<MemberListUnavailableFallback className={styles.memberListFallback} />
				</div>
			);
		}

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
		const seenMemberIds = new Set<string>();
		const groupById = new Map(groups.map((group) => [group.id, group]));

		for (const layout of layouts) {
			const members: Array<GuildMemberRecord> = [];
			for (let index = layout.memberStartIndex; index <= layout.memberEndIndex; index++) {
				const item = memberListState.items.get(index);
				if (!item) {
					continue;
				}
				const member = item.data as GuildMemberRecord;
				if (!seenMemberIds.has(member.user.id)) {
					seenMemberIds.add(member.user.id);
					members.push(member);
				}
			}
			const group = groupById.get(layout.id);
			if (group) {
				groupedItems.set(group.id, members);
			}
		}

		return (
			<div className={styles.memberListContent} ref={listContainerRef}>
				{layouts.map((layout) => {
					const group = groupById.get(layout.id);
					if (!group) {
						return null;
					}
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
		const [activeTab, setActiveTab] = useState<ChannelDetailsTab>(initialTab);
		const [muteSheetOpen, setMuteSheetOpen] = useState(false);
		const [searchSheetOpen, setSearchSheetOpen] = useState(false);
		const [isTopicExpanded, setIsTopicExpanded] = useState(false);
		const [moreOptionsSheetOpen, setMoreOptionsSheetOpen] = useState(false);
		const [notificationSheetOpen, setNotificationSheetOpen] = useState(false);
		const [activeMemberSheet, setActiveMemberSheet] = useState<{
			member: GuildMemberRecord;
			user: UserRecord;
		} | null>(null);

		const leaveGroup = useLeaveGroup();

		useEffect(() => {
			setActiveTab(initialTab);
		}, [initialTab]);

		useEffect(() => {
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

		const channelTypeLabel = useMemo(() => {
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

		const moreOptionsTitle = (() => {
			if (isGroupDM) return t`Group Settings`;
			if (isDM) return t`DM Settings`;
			return t`Channel Settings`;
		})();

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

		const handleMarkAsRead = useCallback(() => {
			ReadStateActionCreators.ack(channel.id, true, true);
			ToastActionCreators.createToast({type: 'success', children: t`Marked as read`});
		}, [channel.id]);

		const handleInvite = useCallback(() => {
			ModalActionCreators.push(modal(() => <InviteModal channelId={channel.id} />));
			onClose();
		}, [channel.id, onClose]);

		const handleCopyLink = useCallback(() => {
			const channelLink = buildChannelLink({
				guildId: channel.guildId,
				channelId: channel.id,
			});
			TextCopyActionCreators.copy(i18n, channelLink);
			ToastActionCreators.createToast({type: 'success', children: t`Link copied to clipboard`});
		}, [channel.id, channel.guildId, i18n]);

		const handleCopyId = useCallback(() => {
			TextCopyActionCreators.copy(i18n, channel.id);
			ToastActionCreators.createToast({type: 'success', children: t`Channel ID copied to clipboard`});
		}, [channel.id, i18n]);

		const handleToggleFavorite = useCallback(() => {
			if (isFavorited) {
				FavoritesStore.removeChannel(channel.id);
				ToastActionCreators.createToast({type: 'success', children: t`Removed from favorites`});
			} else {
				FavoritesStore.addChannel(channel.id, channel.guildId ?? ME, null);
				ToastActionCreators.createToast({type: 'success', children: t`Added to favorites`});
			}
		}, [channel.id, channel.guildId, isFavorited]);

		const handleDebugChannel = useCallback(() => {
			const channelName = channel.name ?? t`Channel`;
			ModalActionCreators.push(modal(() => <ChannelDebugModal title={channelName} channel={channel} />));
			onClose();
		}, [channel, onClose]);

		const handleDebugUser = useCallback(() => {
			if (!recipient) return;
			ModalActionCreators.push(modal(() => <UserDebugModal title={recipient.username} user={recipient} />));
			onClose();
		}, [recipient, onClose]);

		const handlePinDM = useCallback(async () => {
			handleMoreOptionsClose();
			try {
				await PrivateChannelActionCreators.pinDmChannel(channel.id);
				ToastActionCreators.createToast({
					type: 'success',
					children: isGroupDM ? t`Pinned group` : t`Pinned DM`,
				});
			} catch (error) {
				logger.error('Failed to pin:', error);
				ToastActionCreators.createToast({
					type: 'error',
					children: isGroupDM ? t`Failed to pin group` : t`Failed to pin DM`,
				});
			}
		}, [channel.id, isGroupDM]);

		const handleUnpinDM = useCallback(async () => {
			handleMoreOptionsClose();
			try {
				await PrivateChannelActionCreators.unpinDmChannel(channel.id);
				ToastActionCreators.createToast({
					type: 'success',
					children: isGroupDM ? t`Unpinned group` : t`Unpinned DM`,
				});
			} catch (error) {
				logger.error('Failed to unpin:', error);
				ToastActionCreators.createToast({
					type: 'error',
					children: isGroupDM ? t`Failed to unpin group` : t`Failed to unpin DM`,
				});
			}
		}, [channel.id, isGroupDM]);

		const handleCloseDM = useCallback(() => {
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
								logger.error('Failed to close DM:', error);
								window.setTimeout(() => {
									ModalActionCreators.push(modal(() => <DMCloseFailedModal />));
								}, 0);
							}
						}}
					/>
				)),
			);
		}, [channel.id, recipient, onClose]);

		const handleLeaveGroup = useCallback(() => {
			handleMoreOptionsClose();
			onClose();
			leaveGroup(channel.id);
		}, [channel.id, onClose, leaveGroup]);

		const handleEditGroup = useCallback(() => {
			handleMoreOptionsClose();
			onClose();
			ModalActionCreators.push(modal(() => <EditGroupModal channelId={channel.id} />));
		}, [channel.id, onClose]);

		const handleShowInvites = useCallback(() => {
			handleMoreOptionsClose();
			onClose();
			ModalActionCreators.push(modal(() => <GroupInvitesModal channelId={channel.id} />));
		}, [channel.id, onClose]);

		const handleOpenAddFriendsToGroup = useCallback(() => {
			handleMoreOptionsClose();
			onClose();
			ModalActionCreators.push(modal(() => <AddFriendsToGroupModal channelId={channel.id} />));
		}, [channel.id, onClose]);

		const handleCopyUserId = useCallback(() => {
			if (!recipient) return;
			TextCopyActionCreators.copy(i18n, recipient.id);
			ToastActionCreators.createToast({type: 'success', children: t`User ID copied to clipboard`});
		}, [recipient, i18n]);

		const handleEditChannel = useCallback(() => {
			ModalActionCreators.push(modal(() => <ChannelSettingsModal channelId={channel.id} />));
			onClose();
		}, [channel.id, onClose]);

		const handleDeleteChannel = useCallback(() => {
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
								logger.error('Failed to delete channel:', error);
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

		const handleOpenGuildNotificationSettings = useCallback(() => {
			if (!guildId) return;
			ModalActionCreators.push(modal(() => <GuildNotificationSettingsModal guildId={guildId} />));
		}, [guildId]);

		const handleOpenCreateGroupModal = useCallback(() => {
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

		const handleNotificationLevelChange = useCallback(
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

		const handleMute = useCallback(
			(duration: number | null) => {
				UserGuildSettingsActionCreators.updateChannelOverride(
					settingsGuildId,
					channel.id,
					{
						muted: true,
						mute_config: createMuteConfig(duration),
					},
					{persistImmediately: true},
				);
				setMuteSheetOpen(false);
			},
			[settingsGuildId, channel.id],
		);

		const handleUnmute = useCallback(() => {
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
		}, [settingsGuildId, channel.id]);

		const handleMemberLongPress = useCallback((member: GuildMemberRecord) => {
			setActiveMemberSheet({member, user: member.user});
		}, []);

		const handleCloseMemberSheet = useCallback(() => {
			setActiveMemberSheet(null);
		}, []);

		const isMemberTabVisible = isOpen && activeTab === 'members';
		const dmMemberGroups = (() => {
			if (!(isDM || isGroupDM || isPersonalNotes)) return [];

			const currentUserId = AuthenticationStore.currentUserId;
			let memberIds: Array<string> = [];

			if (isPersonalNotes) {
				memberIds = currentUser ? [currentUser.id] : [];
			} else {
				memberIds = [...channel.recipientIds];
				if (currentUserId && !memberIds.includes(currentUserId)) {
					memberIds.push(currentUserId);
				}
			}

			const users = memberIds.map((id) => UserStore.getUser(id)).filter((u): u is UserRecord => u != null);
			return MemberListUtils.getGroupDMMemberGroups(users);
		})();

		return (
			<>
				<Sheet.Root isOpen={isOpen} onClose={onClose} snapPoints={[0, 1]} initialSnap={1}>
					<Sheet.Handle />
					<Sheet.Content padding="none">
						<Scroller key="channel-details-scroller" className={styles.mainScroller}>
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
													{channel.recipientIds.length + 1 === 1
														? t`Group DM · 1 member`
														: t`Group DM · ${channel.recipientIds.length + 1} members`}
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
													<CollapseChevronIcon className={styles.iconSmall} />
												) : (
													<ExpandChevronIcon className={styles.iconSmall} />
												)}
											</button>
										</div>
									</div>
								)}
							</div>

							<div className={styles.quickActionsRow}>
								<div className={styles.quickActionsScroll}>
									<QuickActionButton
										icon={<MuteIcon size={20} />}
										label={isMuted ? t`Unmute` : t`Mute`}
										onClick={handleBellClick}
										isActive={isMuted}
									/>

									<QuickActionButton icon={<SearchIcon size={20} />} label={t`Search`} onClick={handleSearchClick} />

									<QuickActionButton
										icon={<MoreOptionsVerticalIcon size={20} />}
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
									<MembersIcon className={styles.tabIcon} />
									<Trans>Members</Trans>
								</button>
								<button
									type="button"
									onClick={() => setActiveTab('pins')}
									className={`${styles.tabButton} ${activeTab === 'pins' ? styles.tabButtonActive : styles.tabButtonInactive}`}
									style={activeTab === 'pins' ? {borderBottomColor: 'var(--brand-primary-light)'} : undefined}
								>
									<PinIcon className={styles.tabIcon} />
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
															<NewGroupIcon className={`${styles.iconMedium} ${styles.newGroupIconWhite}`} />
														</div>
														<div className={styles.newGroupContent}>
															<p className={styles.newGroupTitle}>
																<Trans>New Group</Trans>
															</p>
															<p className={styles.newGroupSubtitle}>
																<Trans>Create a new group with {recipient.username}</Trans>
															</p>
														</div>
														<ChevronRightIcon className={styles.iconMedium} />
													</button>
												)}

												{dmMemberGroups.map((group) => (
													<div key={group.id} className={styles.memberGroupContainer}>
														<div className={styles.memberGroupHeader}>
															{group.displayName} — {group.count}
														</div>
														<div className={styles.memberGroupList}>
															{group.users.map((user, index) => {
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
																								<OwnerCrownIcon className={styles.ownerCrown} />
																							</Tooltip>
																						)}
																					</div>
																				)}
																			</div>
																		</button>
																		{index < group.users.length - 1 && <div className={styles.memberItemDivider} />}
																	</React.Fragment>
																);
															})}
														</div>
													</div>
												))}
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

				<MuteDurationSheet
					isOpen={muteSheetOpen}
					onClose={() => setMuteSheetOpen(false)}
					isMuted={isMuted}
					mutedText={mutedText}
					muteConfig={muteConfig}
					muteTitle={isGuildChannel ? t`Mute Channel` : t`Mute Conversation`}
					unmuteTitle={isGuildChannel ? t`Unmute Channel` : t`Unmute Conversation`}
					onMute={handleMute}
					onUnmute={handleUnmute}
				/>

				<MenuBottomSheet
					isOpen={moreOptionsSheetOpen}
					onClose={handleMoreOptionsClose}
					title={moreOptionsTitle}
					groups={useMemo(() => {
						const groups: Array<MenuGroupType> = [];
						const hasUnread = ReadStateStore.hasUnread(channel.id);

						const commonItems: Array<MenuItemType> = [];

						if (showFavorites && !isPersonalNotes) {
							commonItems.push({
								id: 'favorite',
								icon: <FavoriteIcon filled={isFavorited} size={20} />,
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
											icon: <PinIcon size={20} />,
											label: isGroupDM ? t`Unpin Group DM` : t`Unpin DM`,
											onClick: handleUnpinDM,
										}
									: {
											id: 'pin',
											icon: <PinIcon size={20} />,
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
									icon: <NotificationSettingsIcon size={20} />,
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
									icon: <EditIcon size={20} />,
									label: t`Edit Group`,
									onClick: handleEditGroup,
								},
							];

							if (!isGroupDmFull(channel)) {
								groupItems.push({
									id: 'add-friends',
									icon: <InviteIcon size={20} />,
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
									icon: <InvitesIcon size={20} />,
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
										icon: <CloseDMIcon size={20} />,
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
										icon: <LeaveIcon size={20} />,
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
								icon: <DebugMessageIcon size={20} />,
								label: t`Debug Channel`,
								onClick: handleDebugChannel,
							});

							if (isDM && recipient) {
								miscItems.push({
									id: 'debug-user',
									icon: <DebugMessageIcon size={20} />,
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
					groups={useMemo((): Array<MenuGroupType> => {
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
										icon: <SettingsIcon size={20} />,
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
