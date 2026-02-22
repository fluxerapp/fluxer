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
import * as ContextMenuActionCreators from '@app/actions/ContextMenuActionCreators';
import * as InviteActionCreators from '@app/actions/InviteActionCreators';
import * as LayoutActionCreators from '@app/actions/LayoutActionCreators';
import * as MessageActionCreators from '@app/actions/MessageActionCreators';
import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import {modal} from '@app/actions/ModalActionCreators';
import * as NavigationActionCreators from '@app/actions/NavigationActionCreators';
import * as PremiumModalActionCreators from '@app/actions/PremiumModalActionCreators';
import * as PrivateChannelActionCreators from '@app/actions/PrivateChannelActionCreators';
import * as ReadStateActionCreators from '@app/actions/ReadStateActionCreators';
import * as TextCopyActionCreators from '@app/actions/TextCopyActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import * as UserActionCreators from '@app/actions/UserActionCreators';
import * as UserGuildSettingsActionCreators from '@app/actions/UserGuildSettingsActionCreators';
import * as UserProfileActionCreators from '@app/actions/UserProfileActionCreators';
import {DMCloseFailedModal} from '@app/components/alerts/DMCloseFailedModal';
import {CreateDMBottomSheet} from '@app/components/bottomsheets/CreateDMBottomSheet';
import styles from '@app/components/channel/direct_message/DMList.module.css';
import {createMuteConfig, getMuteDurationOptions} from '@app/components/channel/MuteOptions';
import {UserTag} from '@app/components/channel/UserTag';
import {CustomStatusDisplay} from '@app/components/common/custom_status_display/CustomStatusDisplay';
import {GroupDMAvatar} from '@app/components/common/GroupDMAvatar';
import {ChannelDebugModal} from '@app/components/debug/ChannelDebugModal';
import {UserDebugModal} from '@app/components/debug/UserDebugModal';
import {LongPressable} from '@app/components/LongPressable';
import {AddFriendSheet} from '@app/components/modals/AddFriendSheet';
import {ChangeFriendNicknameModal} from '@app/components/modals/ChangeFriendNicknameModal';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import {CreateDMModal} from '@app/components/modals/CreateDMModal';
import {EditGroupBottomSheet} from '@app/components/modals/EditGroupBottomSheet';
import {EditGroupModal} from '@app/components/modals/EditGroupModal';
import {GroupInvitesBottomSheet} from '@app/components/modals/GroupInvitesBottomSheet';
import {GroupInvitesModal} from '@app/components/modals/GroupInvitesModal';
import {
	AcceptFriendRequestIcon,
	AddNoteIcon,
	BlockUserIcon,
	CancelFriendRequestIcon,
	CloseDMIcon,
	CopyIdIcon,
	DebugIcon,
	EditGroupIcon,
	EditIcon,
	FavoriteIcon,
	GroupInvitesIcon,
	IgnoreFriendRequestIcon,
	InviteToCommunityIcon,
	LeaveIcon,
	MarkAsReadIcon,
	MuteIcon,
	PinIcon,
	RemoveFriendIcon,
	SendFriendRequestIcon,
	SendInviteToCommunityIcon,
	ViewProfileIcon,
	VoiceCallIcon,
} from '@app/components/uikit/context_menu/ContextMenuIcons';
import {DMContextMenu} from '@app/components/uikit/context_menu/DMContextMenu';
import {GroupDMContextMenu} from '@app/components/uikit/context_menu/GroupDMContextMenu';
import FocusRing from '@app/components/uikit/focus_ring/FocusRing';
import {KeybindHint, TooltipWithKeybind} from '@app/components/uikit/keybind_hint/KeybindHint';
import {MentionBadge} from '@app/components/uikit/MentionBadge';
import type {MenuGroupType, MenuItemType} from '@app/components/uikit/menu_bottom_sheet/MenuBottomSheet';
import {MenuBottomSheet} from '@app/components/uikit/menu_bottom_sheet/MenuBottomSheet';
import {Scroller} from '@app/components/uikit/Scroller';
import {StatusAwareAvatar} from '@app/components/uikit/StatusAwareAvatar';
import {Tooltip} from '@app/components/uikit/tooltip/Tooltip';
import {useContextMenuHoverState} from '@app/hooks/useContextMenuHoverState';
import {useLeaveGroup} from '@app/hooks/useLeaveGroup';
import {getCustomStatusText, normalizeCustomStatus} from '@app/lib/CustomStatus';
import {Logger} from '@app/lib/Logger';
import {SafeMarkdown} from '@app/lib/markdown';
import {MarkdownContext} from '@app/lib/markdown/renderers/RendererTypes';
import {useLocation} from '@app/lib/router/React';
import {Routes} from '@app/Routes';
import type {ChannelRecord} from '@app/records/ChannelRecord';
import type {GuildRecord} from '@app/records/GuildRecord';
import AccessibilityStore, {DMMessagePreviewMode} from '@app/stores/AccessibilityStore';
import AuthenticationStore from '@app/stores/AuthenticationStore';
import ChannelStore from '@app/stores/ChannelStore';
import FavoritesStore from '@app/stores/FavoritesStore';
import GuildMemberStore from '@app/stores/GuildMemberStore';
import GuildStore from '@app/stores/GuildStore';
import KeyboardModeStore from '@app/stores/KeyboardModeStore';
import MessageStore from '@app/stores/MessageStore';
import MobileLayoutStore from '@app/stores/MobileLayoutStore';
import PresenceStore from '@app/stores/PresenceStore';
import QuickSwitcherStore from '@app/stores/QuickSwitcherStore';
import ReadStateStore from '@app/stores/ReadStateStore';
import RelationshipStore from '@app/stores/RelationshipStore';
import RuntimeConfigStore from '@app/stores/RuntimeConfigStore';
import SelectedChannelStore from '@app/stores/SelectedChannelStore';
import TypingStore from '@app/stores/TypingStore';
import UserGuildSettingsStore from '@app/stores/UserGuildSettingsStore';
import UserSettingsStore from '@app/stores/UserSettingsStore';
import UserStore from '@app/stores/UserStore';
import {checkAndStartCall} from '@app/utils/CallUtils';
import * as ChannelUtils from '@app/utils/ChannelUtils';
import {getMutedText} from '@app/utils/ContextMenuUtils';
import {getSortedDmChannels} from '@app/utils/DmChannelUtils';
import * as InviteUtils from '@app/utils/InviteUtils';
import * as NicknameUtils from '@app/utils/NicknameUtils';
import {shouldShowPremiumFeatures} from '@app/utils/PremiumUtils';
import * as RelationshipActionUtils from '@app/utils/RelationshipActionUtils';
import {SystemMessageUtils} from '@app/utils/SystemMessageUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {PublicUserFlags, RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {formatShortRelativeTime} from '@fluxer/date_utils/src/DateDuration';
import {extractTimestamp, fromTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	CrownIcon,
	MagnifyingGlassIcon,
	NotePencilIcon,
	PaperPlaneIcon,
	PlusIcon,
	PushPinIcon,
	UserPlusIcon,
	UsersThreeIcon,
	XIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

interface InviteCandidate {
	guild: GuildRecord;
	channelId: string;
}

const logger = new Logger('DMList');

const DMListItem = observer(({channel, isSelected}: {channel: ChannelRecord; isSelected: boolean}) => {
	const {t, i18n} = useLingui();
	const readState = ReadStateStore.get(channel.id);

	const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
	const recipient = !isGroupDM ? UserStore.getUser(channel.recipientIds[0]) : null;
	const recipientId = recipient?.id || '';
	const isBotDM = Boolean(recipient?.bot || recipient?.system);
	const isTyping = TypingStore.isTyping(channel.id, recipientId);
	const hasUnreadMessages = () => readState.hasUnread();
	const isMobile = MobileLayoutStore.isMobileLayout();
	const isMuted = UserGuildSettingsStore.isChannelMuted(null, channel.id);
	const [menuOpen, setMenuOpen] = useState(false);
	const [nestedSheet, setNestedSheet] = useState<{
		title: string;
		groups: Array<MenuGroupType>;
	} | null>(null);
	const [editGroupSheetOpen, setEditGroupSheetOpen] = useState(false);
	const [invitesSheetOpen, setInvitesSheetOpen] = useState(false);
	const currentUser = UserStore.getCurrentUser();
	const lastMessage = MessageStore.getCachedMessages(channel.id)?.last();
	const leaveGroup = useLeaveGroup();
	const {keyboardModeEnabled} = KeyboardModeStore;
	const [isFocused, setIsFocused] = useState(false);
	const scrollTargetRef = useRef<HTMLElement | null>(null);
	const setDesktopRef = useCallback((node: HTMLButtonElement | null) => {
		scrollTargetRef.current = node;
	}, []);
	const setMobileRef = useCallback((node: HTMLDivElement | null) => {
		scrollTargetRef.current = node;
	}, []);
	const contextMenuOpen = useContextMenuHoverState(scrollTargetRef);
	const closeAllSheets = useCallback(() => {
		setMenuOpen(false);
		setNestedSheet(null);
	}, []);
	const openNestedSheet = useCallback((title: string, groups: Array<MenuGroupType>) => {
		setMenuOpen(false);
		setNestedSheet({title, groups});
	}, []);
	const closeNestedSheet = useCallback(() => {
		setNestedSheet(null);
	}, []);
	const developerMode = UserSettingsStore.developerMode;
	const restrictRecipientActions = recipient?.system ?? false;
	const relationshipType = recipient ? RelationshipStore.getRelationship(recipient.id)?.type : undefined;
	const favoritesCategories = FavoritesStore.sortedCategories;
	const favoriteEntry = FavoritesStore.getChannel(channel.id);
	const favoriteLabel = (() => {
		if (channel.isDM()) {
			return t`Favorite DM`;
		}
		if (channel.isGroupDM()) {
			return t`Favorite Group DM`;
		}
		return t`Favorite Channel`;
	})();
	const unfavoriteLabel = (() => {
		if (channel.isDM()) {
			return t`Unfavorite DM`;
		}
		if (channel.isGroupDM()) {
			return t`Unfavorite Group DM`;
		}
		return t`Unfavorite Channel`;
	})();
	const channelOverride = UserGuildSettingsStore.getChannelOverride(null, channel.id);
	const bottomSheetIsMuted = channelOverride?.muted ?? false;
	const bottomSheetMutedHint = getMutedText(bottomSheetIsMuted, channelOverride?.mute_config);
	const muteDurationOptions = getMuteDurationOptions(i18n);
	const inviteCandidates = useMemo<Array<InviteCandidate>>(() => {
		if (!recipient || restrictRecipientActions || recipient.bot) return [];
		return GuildStore.getGuilds()
			.filter((guild) => !GuildMemberStore.getMember(guild.id, recipient.id))
			.map((guild): InviteCandidate | null => {
				const channelId = getDefaultInviteChannelId(guild.id);
				return channelId ? {guild, channelId} : null;
			})
			.filter((candidate): candidate is InviteCandidate => Boolean(candidate))
			.sort((a, b) => a.guild.name.localeCompare(b.guild.name));
	}, [recipient?.id, restrictRecipientActions]);

	const buildFavoriteGroups = () => [
		{
			items: [
				{
					icon: <FavoriteIcon size={20} filled={true} />,
					label: t`Uncategorized`,
					onClick: () => handleAddToFavorites(null),
				},
				...favoritesCategories.map((category) => ({
					icon: <FavoriteIcon size={20} filled={true} />,
					label: category.name,
					onClick: () => handleAddToFavorites(category.id),
				})),
			],
		},
	];

	const buildInviteGroups = () => [
		{
			items: inviteCandidates.map((candidate) => ({
				icon: <SendInviteToCommunityIcon size={20} />,
				label: candidate.guild.name,
				onClick: () => handleSendInvite(candidate),
			})),
		},
	];

	const buildMuteGroups = () => [
		{
			items: muteDurationOptions.map((duration) => ({
				label: duration.label,
				onClick: () => handleMute(duration.value),
			})),
		},
	];

	useEffect(() => {
		if (isSelected) {
			scrollTargetRef.current?.scrollIntoView({block: 'nearest'});
		}
	}, [isSelected]);

	if (!isGroupDM && !recipient) return null;

	const displayName = ChannelUtils.getDMDisplayName(channel);

	const navigateTo = () => {
		NavigationActionCreators.selectChannel(ME, channel.id);
		if (MobileLayoutStore.isMobileLayout()) {
			LayoutActionCreators.updateMobileLayoutState(false, true);
		}
	};

	const handleRemoveChannel = (e?: React.MouseEvent | React.KeyboardEvent) => {
		if (e) {
			e.preventDefault();
			e.stopPropagation();
		}
		setMenuOpen(false);

		if (isGroupDM) {
			leaveGroup(channel.id);
			return;
		}

		ChannelActionCreators.remove(channel.id);
		const selectedChannel = SelectedChannelStore.selectedChannelIds.get(ME);
		if (selectedChannel === channel.id) {
			NavigationActionCreators.deselectGuild();
		}
	};

	const handleCopyChannelId = async () => {
		await TextCopyActionCreators.copy(i18n, channel.id);
		closeAllSheets();
	};

	const handleCopyUserId = async () => {
		if (!recipient) return;
		await TextCopyActionCreators.copy(i18n, recipient.id);
		closeAllSheets();
	};

	const handleMarkAsRead = () => {
		ReadStateActionCreators.ack(channel.id, true, true);
		closeAllSheets();
	};

	const handleCloseDm = () => {
		closeAllSheets();

		const username = recipient?.username ?? '';
		const description = isGroupDM
			? t(i18n)`Are you sure you want to close this group DM? You can always reopen it later.`
			: t(i18n)`Are you sure you want to close your DM with ${username}? You can always reopen it later.`;

		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t(i18n)`Close DM`}
					description={description}
					primaryText={t(i18n)`Close DM`}
					primaryVariant="danger-primary"
					onPrimary={async () => {
						try {
							await ChannelActionCreators.remove(channel.id);

							const selectedChannel = SelectedChannelStore.selectedChannelIds.get(ME);
							if (selectedChannel === channel.id) {
								NavigationActionCreators.deselectGuild();
							}

							ToastActionCreators.createToast({
								type: 'success',
								children: t(i18n)`DM closed`,
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
	};

	const handleViewProfile = () => {
		if (!recipient) return;
		closeAllSheets();
		UserProfileActionCreators.openUserProfile(recipient.id);
	};

	const handleStartVoiceCall = async () => {
		if (!recipient || recipient.bot) return;
		closeAllSheets();
		try {
			const dmChannelId = await PrivateChannelActionCreators.ensureDMChannel(recipient.id);
			await checkAndStartCall(dmChannelId);
		} catch (error) {
			logger.error('Failed to start voice call:', error);
		}
	};

	const handleAddNote = () => {
		if (!recipient || recipient.bot || restrictRecipientActions) return;
		closeAllSheets();
		UserProfileActionCreators.openUserProfile(recipient.id, undefined, true);
	};

	const handleChangeFriendNickname = () => {
		if (!recipient || relationshipType !== RelationshipTypes.FRIEND) return;
		closeAllSheets();
		ModalActionCreators.push(modal(() => <ChangeFriendNicknameModal user={recipient} />));
	};

	const handleMute = (duration: number | null) => {
		closeAllSheets();
		UserGuildSettingsActionCreators.updateChannelOverride(
			null,
			channel.id,
			{
				muted: true,
				mute_config: duration ? createMuteConfig(duration) : null,
			},
			{persistImmediately: true},
		);
	};

	const handleUnmute = () => {
		closeAllSheets();
		UserGuildSettingsActionCreators.updateChannelOverride(
			null,
			channel.id,
			{
				muted: false,
				mute_config: null,
			},
			{persistImmediately: true},
		);
	};

	const handleAddToFavorites = (categoryId: string | null) => {
		closeAllSheets();
		const guildId = channel.guildId ?? ME;
		FavoritesStore.addChannel(channel.id, guildId, categoryId);
		ToastActionCreators.createToast({type: 'success', children: t`Channel added to favorites`});
	};

	const handleRemoveFromFavorites = () => {
		closeAllSheets();
		FavoritesStore.removeChannel(channel.id);
		ToastActionCreators.createToast({type: 'success', children: t`Channel removed from favorites`});
	};

	const handleSendFriendRequest = async () => {
		if (!recipient) return;
		closeAllSheets();
		await RelationshipActionUtils.sendFriendRequest(i18n, recipient.id);
	};

	const handleAcceptFriendRequest = () => {
		if (!recipient) return;
		closeAllSheets();
		RelationshipActionUtils.acceptFriendRequest(i18n, recipient.id);
	};

	const handleIgnoreFriendRequest = () => {
		if (!recipient) return;
		closeAllSheets();
		RelationshipActionUtils.ignoreFriendRequest(i18n, recipient.id);
	};

	const handleRemoveFriend = () => {
		if (!recipient) return;
		closeAllSheets();
		RelationshipActionUtils.showRemoveFriendConfirmation(i18n, recipient);
	};

	const handleBlockUser = () => {
		if (!recipient) return;
		closeAllSheets();
		RelationshipActionUtils.showBlockUserConfirmation(i18n, recipient);
	};

	const handleUnblockUser = () => {
		if (!recipient) return;
		closeAllSheets();
		RelationshipActionUtils.unblockUser(i18n, recipient.id);
	};

	const handleSendInvite = async (candidate: InviteCandidate) => {
		closeAllSheets();
		if (!recipient) return;
		try {
			const invite = await InviteActionCreators.create(candidate.channelId);
			const inviteUrl = `${RuntimeConfigStore.inviteEndpoint}/${invite.code}`;
			const dmChannelId = await PrivateChannelActionCreators.ensureDMChannel(recipient.id);
			await MessageActionCreators.send(dmChannelId, {
				content: inviteUrl,
				nonce: fromTimestamp(Date.now()),
			});
			ToastActionCreators.createToast({
				type: 'success',
				children: t(i18n)`Invite sent for ${candidate.guild.name}`,
			});
		} catch (error) {
			logger.error('Failed to send invite via DM sheet:', error);
			ToastActionCreators.createToast({type: 'error', children: t`Failed to send invite. Please try again.`});
		}
	};

	const handleEditGroup = () => {
		setMenuOpen(false);
		if (isMobile) {
			setEditGroupSheetOpen(true);
		} else {
			ModalActionCreators.push(modal(() => <EditGroupModal channelId={channel.id} />));
		}
	};

	const handleShowInvites = () => {
		setMenuOpen(false);
		if (isMobile) {
			setInvitesSheetOpen(true);
		} else {
			ModalActionCreators.push(modal(() => <GroupInvitesModal channelId={channel.id} />));
		}
	};

	const handleLeaveGroup = () => {
		setMenuOpen(false);
		leaveGroup(channel.id);
	};

	const handlePinChannel = async () => {
		closeAllSheets();
		try {
			await PrivateChannelActionCreators.pinDmChannel(channel.id);
			ToastActionCreators.createToast({type: 'success', children: t`Pinned DM`});
		} catch {
			ToastActionCreators.createToast({type: 'error', children: t`Failed to pin DM`});
		}
	};

	const handleUnpinChannel = async () => {
		closeAllSheets();
		try {
			await PrivateChannelActionCreators.unpinDmChannel(channel.id);
			ToastActionCreators.createToast({type: 'success', children: t`Unpinned DM`});
		} catch {
			ToastActionCreators.createToast({type: 'error', children: t`Failed to unpin DM`});
		}
	};

	const menuGroups: Array<MenuGroupType> = [];

	if (isGroupDM) {
		menuGroups.push({
			items: [
				{
					icon: <EditGroupIcon size={20} />,
					label: t`Edit Group`,
					onClick: handleEditGroup,
				},
				channel.isPinned
					? {
							icon: <PinIcon size={20} />,
							label: t`Unpin Group DM`,
							onClick: handleUnpinChannel,
						}
					: {
							icon: <PinIcon size={20} />,
							label: t`Pin Group DM`,
							onClick: handlePinChannel,
						},
			],
		});
		const isOwner = channel.ownerId === AuthenticationStore.currentUserId;
		if (isOwner) {
			menuGroups[0].items.push({
				icon: <GroupInvitesIcon size={20} />,
				label: t`Invites`,
				onClick: handleShowInvites,
			});
		}
		menuGroups.push({
			items: [
				{
					icon: <LeaveIcon size={20} />,
					label: t`Leave Group`,
					onClick: handleLeaveGroup,
					danger: true,
				},
				{
					icon: <CopyIdIcon size={20} />,
					label: t`Copy Channel ID`,
					onClick: handleCopyChannelId,
				},
			],
		});
	} else {
		menuGroups.push({
			items: [
				channel.isPinned
					? {
							icon: <PinIcon size={20} />,
							label: t`Unpin DM`,
							onClick: handleUnpinChannel,
						}
					: {
							icon: <PinIcon size={20} />,
							label: t`Pin DM`,
							onClick: handlePinChannel,
						},
				{
					icon: <CloseDMIcon size={20} />,
					label: t`Close DM`,
					onClick: () => handleRemoveChannel(),
					danger: true,
				},
				{
					icon: <CopyIdIcon size={20} />,
					label: t`Copy Channel ID`,
					onClick: handleCopyChannelId,
				},
			],
		});
	}

	const mobileMenuGroups: Array<MenuGroupType> = [];

	mobileMenuGroups.push({
		items: [
			{
				icon: <MarkAsReadIcon size={20} />,
				label: t`Mark as Read`,
				onClick: handleMarkAsRead,
				disabled: !hasUnreadMessages(),
			},
			{
				icon: <PinIcon size={20} />,
				label: channel.isPinned ? t`Unpin DM` : t`Pin DM`,
				onClick: channel.isPinned ? handleUnpinChannel : handlePinChannel,
			},
		],
	});

	if (AccessibilityStore.showFavorites) {
		const favoriteItems: Array<MenuItemType> = [];
		if (favoriteEntry) {
			favoriteItems.push({
				icon: <FavoriteIcon size={20} filled={true} />,
				label: unfavoriteLabel,
				onClick: handleRemoveFromFavorites,
			});
		} else if (favoritesCategories.length === 0) {
			favoriteItems.push({
				icon: <FavoriteIcon size={20} filled={true} />,
				label: favoriteLabel,
				onClick: () => handleAddToFavorites(null),
			});
		} else {
			favoriteItems.push({
				icon: <FavoriteIcon size={20} filled={true} />,
				label: favoriteLabel,
				onClick: () => openNestedSheet(favoriteLabel, buildFavoriteGroups()),
			});
		}
		if (favoriteItems.length > 0) {
			mobileMenuGroups.push({items: favoriteItems});
		}
	}

	if (recipient && !recipient.bot) {
		const profileItems: Array<MenuItemType> = [
			{
				icon: <ViewProfileIcon size={20} />,
				label: t`View Profile`,
				onClick: handleViewProfile,
			},
			{
				icon: <VoiceCallIcon size={20} />,
				label: t`Start Voice Call`,
				onClick: handleStartVoiceCall,
			},
		];
		if (!restrictRecipientActions) {
			profileItems.push(
				{
					icon: <AddNoteIcon size={20} />,
					label: t`Add Note`,
					onClick: handleAddNote,
				},
				relationshipType === RelationshipTypes.FRIEND
					? {
							icon: <EditIcon size={20} />,
							label: t`Change Friend Nickname`,
							onClick: handleChangeFriendNickname,
						}
					: {
							icon: <EditIcon size={20} />,
							label: t`Change Friend Nickname`,
							onClick: handleChangeFriendNickname,
							disabled: true,
						},
			);
		}
		profileItems.push({
			icon: <CloseDMIcon size={20} />,
			label: t`Close DM`,
			onClick: handleCloseDm,
			danger: true,
		});
		mobileMenuGroups.push({items: profileItems});
	}

	const relationshipItems: Array<MenuItemType> = [];

	if (recipient && !restrictRecipientActions) {
		if (inviteCandidates.length > 0) {
			relationshipItems.push({
				icon: <InviteToCommunityIcon size={20} />,
				label: t`Invite to Community`,
				onClick: () => openNestedSheet(t`Invite to Community`, buildInviteGroups()),
			});
		}

		if (recipient.bot && !(recipient.flags & PublicUserFlags.FRIENDLY_BOT)) {
			if (relationshipType === RelationshipTypes.FRIEND) {
				relationshipItems.push({
					icon: <RemoveFriendIcon size={20} />,
					label: t`Remove Friend`,
					onClick: handleRemoveFriend,
				});
			} else if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
				relationshipItems.push(
					{
						icon: <AcceptFriendRequestIcon size={20} />,
						label: t`Accept Friend Request`,
						onClick: handleAcceptFriendRequest,
					},
					{
						icon: <IgnoreFriendRequestIcon size={20} />,
						label: t`Ignore Friend Request`,
						onClick: handleIgnoreFriendRequest,
					},
				);
			} else if (relationshipType === RelationshipTypes.OUTGOING_REQUEST) {
				relationshipItems.push({
					icon: <CancelFriendRequestIcon size={20} />,
					label: t`Friend Request Sent`,
					onClick: () => undefined,
					disabled: true,
				});
			} else {
				relationshipItems.push({
					icon: <SendFriendRequestIcon size={20} />,
					label: t`Add Friend`,
					onClick: handleSendFriendRequest,
				});
			}
		} else {
			switch (relationshipType) {
				case RelationshipTypes.FRIEND:
					relationshipItems.push({
						icon: <RemoveFriendIcon size={20} />,
						label: t`Remove Friend`,
						onClick: handleRemoveFriend,
					});
					break;
				case RelationshipTypes.INCOMING_REQUEST:
					relationshipItems.push(
						{
							icon: <AcceptFriendRequestIcon size={20} />,
							label: t`Accept Friend Request`,
							onClick: handleAcceptFriendRequest,
						},
						{
							icon: <IgnoreFriendRequestIcon size={20} />,
							label: t`Ignore Friend Request`,
							onClick: handleIgnoreFriendRequest,
						},
					);
					break;
				case RelationshipTypes.OUTGOING_REQUEST:
					relationshipItems.push({
						icon: <CancelFriendRequestIcon size={20} />,
						label: t`Friend Request Sent`,
						onClick: () => undefined,
						disabled: true,
					});
					break;
				default:
					relationshipItems.push({
						icon: <SendFriendRequestIcon size={20} />,
						label: t`Add Friend`,
						onClick: handleSendFriendRequest,
					});
			}
		}

		if (relationshipType === RelationshipTypes.BLOCKED) {
			relationshipItems.push({
				icon: <BlockUserIcon size={20} />,
				label: t`Unblock`,
				onClick: handleUnblockUser,
			});
		} else {
			relationshipItems.push({
				icon: <BlockUserIcon size={20} />,
				label: t`Block`,
				onClick: handleBlockUser,
			});
		}
	}

	if (relationshipItems.length > 0) {
		mobileMenuGroups.push({items: relationshipItems});
	}

	if (bottomSheetIsMuted) {
		mobileMenuGroups.push({
			items: [
				{
					icon: <MuteIcon size={20} />,
					label: t`Unmute DM`,
					onClick: handleUnmute,
					hint: bottomSheetMutedHint ?? undefined,
				},
			],
		});
	} else {
		mobileMenuGroups.push({
			items: [
				{
					icon: <MuteIcon size={20} />,
					label: t`Mute DM`,
					onClick: () => openNestedSheet(t`Mute DM`, buildMuteGroups()),
					hint: bottomSheetMutedHint ?? undefined,
				},
			],
		});
	}

	if (developerMode) {
		const developerItems: Array<MenuItemType> = [];
		if (recipient) {
			developerItems.push({
				icon: <DebugIcon size={20} />,
				label: t`Debug User`,
				onClick: () => {
					closeAllSheets();
					ModalActionCreators.push(modal(() => <UserDebugModal title={t`User Debug`} user={recipient} />));
				},
			});
		}
		developerItems.push({
			icon: <DebugIcon size={20} />,
			label: t`Debug Channel`,
			onClick: () => {
				closeAllSheets();
				ModalActionCreators.push(modal(() => <ChannelDebugModal title={t`Channel Debug`} channel={channel} />));
			},
		});
		mobileMenuGroups.push({items: developerItems});
	}

	const copyItems: Array<MenuItemType> = [];
	if (recipient) {
		copyItems.push({
			icon: <CopyIdIcon size={20} />,
			label: t`Copy User ID`,
			onClick: handleCopyUserId,
		});
	}
	copyItems.push({
		icon: <CopyIdIcon size={20} />,
		label: t`Copy Channel ID`,
		onClick: handleCopyChannelId,
	});
	mobileMenuGroups.push({items: copyItems});

	const animationSettings = {opacity: 1, height: 8};
	const motionSettings = {
		animate: animationSettings,
		exit: {opacity: 0},
		initial: animationSettings,
		transition: {duration: 0},
	};

	const relativeTime = channel.lastMessageId
		? formatShortRelativeTime(extractTimestamp(channel.lastMessageId), '1m')
		: null;

	const shouldShowMessagePreviewSetting = (() => {
		if (AccessibilityStore.dmMessagePreviewMode === DMMessagePreviewMode.ALL) {
			return true;
		}
		if (AccessibilityStore.dmMessagePreviewMode === DMMessagePreviewMode.UNREAD_ONLY) {
			return hasUnreadMessages();
		}
		return false;
	})();

	const getMessagePreview = (): React.ReactNode | null => {
		if (!lastMessage) return null;

		const isCurrentUser = lastMessage.author.id === currentUser?.id;
		const authorPrefix = isCurrentUser ? `${t`You`}: ` : `${NicknameUtils.getNickname(lastMessage.author)}: `;

		if (lastMessage.type !== MessageTypes.DEFAULT && lastMessage.type !== MessageTypes.REPLY) {
			const systemText = SystemMessageUtils.stringify(lastMessage, i18n);
			if (systemText) {
				return systemText.replace(/\.$/, '');
			}
			return null;
		}

		if (lastMessage.content) {
			return (
				<>
					{authorPrefix}
					<span className={styles.dmItemPreviewMarkdown}>
						<SafeMarkdown
							content={lastMessage.content}
							options={{
								context: MarkdownContext.RESTRICTED_INLINE_REPLY,
								channelId: channel.id,
								messageId: lastMessage.id,
								disableAnimatedEmoji: true,
							}}
						/>
					</span>
				</>
			);
		}

		if (lastMessage.attachments?.length > 0) {
			return (
				<>
					{authorPrefix}
					<span className={styles.dmItemSubtextItalic}>{t`Sent an attachment`}</span>
				</>
			);
		}

		return null;
	};

	const messagePreview = shouldShowMessagePreviewSetting ? getMessagePreview() : null;

	const handleContextMenu = (event: React.MouseEvent) => {
		event.preventDefault();
		event.stopPropagation();
		if (isGroupDM) {
			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<GroupDMContextMenu channel={channel} onClose={onClose} />
			));
		} else if (recipient) {
			ContextMenuActionCreators.openFromEvent(event, ({onClose}) => (
				<DMContextMenu channel={channel} recipient={recipient} onClose={onClose} />
			));
		}
	};

	if (isMobile) {
		return (
			<>
				<FocusRing offset={-2}>
					<LongPressable
						ref={setMobileRef}
						onLongPress={() => setMenuOpen(true)}
						className={clsx(
							isSelected
								? styles.dmItemMobileSelected
								: hasUnreadMessages()
									? styles.dmItemMobileUnread
									: styles.dmItemMobile,
							isMuted && styles.dmItemMobileMuted,
							contextMenuOpen && styles.contextMenuActive,
						)}
						pressedClassName={styles.dmItemMobilePressed}
						role="button"
						tabIndex={0}
						onClick={navigateTo}
						onContextMenu={handleContextMenu}
					>
						<AnimatePresence>
							{hasUnreadMessages() && (
								<div className={styles.dmItemUnreadIndicatorContainerMobile}>
									<motion.span {...motionSettings} className={styles.dmItemUnreadIndicator} />
								</div>
							)}
						</AnimatePresence>
						<div className={styles.dmItemContent}>
							<div className={styles.dmItemAvatarWrapper}>
								{isGroupDM ? (
									<GroupDMAvatar channel={channel} size={40} />
								) : (
									<StatusAwareAvatar user={recipient!} size={40} isTyping={isTyping} showOffline={true} />
								)}
							</div>
							<div className={styles.dmItemInfo}>
								<span className={styles.dmItemName}>
									{channel.isPinned && <PushPinIcon weight="fill" className={styles.dmItemPinIcon} />}
									<span className={styles.dmItemNameText}>{displayName}</span>
									{!isGroupDM && isBotDM && <UserTag className={styles.dmItemUserTag} system={recipient?.system} />}
								</span>
								{!isGroupDM && recipient && !messagePreview && (
									<Tooltip
										position="bottom"
										text={() => getCustomStatusText(normalizeCustomStatus(PresenceStore.getCustomStatus(recipient.id)))}
									>
										<CustomStatusDisplay
											userId={recipient.id}
											className={styles.dmItemCustomStatus}
											showText={true}
											showTooltip={false}
											animateOnParentHover
										/>
									</Tooltip>
								)}
								{isGroupDM && (
									<span className={clsx(styles.dmItemSubtext, styles.dmItemMembersSubtext)}>
										{channel.recipientIds.length + 1 === 1
											? t`1 Member`
											: t`${channel.recipientIds.length + 1} Members`}
									</span>
								)}
								{!isGroupDM && messagePreview && <span className={styles.dmItemSubtext}>{messagePreview}</span>}
							</div>
							{relativeTime && <span className={styles.dmItemTimestamp}>{relativeTime}</span>}
						</div>
					</LongPressable>
				</FocusRing>
				<MenuBottomSheet isOpen={menuOpen} onClose={closeAllSheets} groups={mobileMenuGroups} />
				{nestedSheet && (
					<MenuBottomSheet
						isOpen={Boolean(nestedSheet)}
						onClose={closeNestedSheet}
						groups={nestedSheet.groups}
						title={nestedSheet.title}
						showCloseButton={true}
					/>
				)}
				{isGroupDM && (
					<>
						<EditGroupBottomSheet
							isOpen={editGroupSheetOpen}
							onClose={() => setEditGroupSheetOpen(false)}
							channelId={channel.id}
						/>
						<GroupInvitesBottomSheet
							isOpen={invitesSheetOpen}
							onClose={() => setInvitesSheetOpen(false)}
							channelId={channel.id}
						/>
					</>
				)}
			</>
		);
	}

	return (
		<>
			<FocusRing offset={-2}>
				<button
					ref={setDesktopRef}
					type="button"
					className={clsx(
						isSelected ? styles.dmItemSelected : hasUnreadMessages() ? styles.dmItemUnread : styles.dmItem,
						isMuted && styles.dmItemMuted,
						contextMenuOpen && styles.contextMenuActive,
					)}
					onClick={navigateTo}
					onContextMenu={handleContextMenu}
					onFocus={() => setIsFocused(true)}
					onBlur={() => setIsFocused(false)}
				>
					<AnimatePresence>
						{hasUnreadMessages() && (
							<div className={styles.dmItemUnreadIndicatorContainerDesktop}>
								<motion.span {...motionSettings} className={styles.dmItemUnreadIndicator} />
							</div>
						)}
					</AnimatePresence>
					<div className={styles.dmItemContent}>
						<div className={styles.dmItemAvatarWrapper}>
							{isGroupDM ? (
								<GroupDMAvatar channel={channel} size={32} />
							) : (
								<StatusAwareAvatar user={recipient!} size={32} isTyping={isTyping} showOffline={true} />
							)}
						</div>
						<div className={styles.dmItemInfo}>
							<span className={styles.dmItemName}>
								{channel.isPinned && <PushPinIcon weight="fill" className={styles.dmItemPinIcon} />}
								<span className={styles.dmItemNameText}>{displayName}</span>
								{!isGroupDM && isBotDM && <UserTag className={styles.dmItemUserTag} system={recipient?.system} />}
							</span>
							{!isGroupDM && recipient && !messagePreview && (
								<Tooltip
									position="bottom"
									text={() => getCustomStatusText(normalizeCustomStatus(PresenceStore.getCustomStatus(recipient.id)))}
								>
									<CustomStatusDisplay
										userId={recipient.id}
										className={styles.dmItemCustomStatus}
										showText={true}
										showTooltip={false}
										animateOnParentHover
									/>
								</Tooltip>
							)}
							{isGroupDM && (
								<span className={clsx(styles.dmItemSubtext, styles.dmItemMembersSubtext)}>
									{channel.recipientIds.length + 1 === 1 ? t`1 Member` : t`${channel.recipientIds.length + 1} Members`}
								</span>
							)}
							{!isGroupDM && messagePreview && <span className={styles.dmItemSubtext}>{messagePreview}</span>}
						</div>
						<FocusRing offset={-2}>
							<div
								role="button"
								tabIndex={0}
								className={styles.dmItemCloseButton}
								style={{opacity: isFocused && keyboardModeEnabled ? 1 : undefined}}
								onClick={handleRemoveChannel}
								onKeyDown={(e) => {
									if (e.key === 'Enter' || e.key === ' ') {
										handleRemoveChannel(e);
									}
								}}
							>
								<XIcon weight="bold" className={styles.iconSize4} />
							</div>
						</FocusRing>
					</div>
				</button>
			</FocusRing>
			{isGroupDM && (
				<>
					<EditGroupBottomSheet
						isOpen={editGroupSheetOpen}
						onClose={() => setEditGroupSheetOpen(false)}
						channelId={channel.id}
					/>
					<GroupInvitesBottomSheet
						isOpen={invitesSheetOpen}
						onClose={() => setInvitesSheetOpen(false)}
						channelId={channel.id}
					/>
				</>
			)}
		</>
	);
});

const ClickableItem = observer(
	({isSelected, onClick, children}: {isSelected?: boolean; onClick: () => void; children: React.ReactNode}) => (
		<FocusRing offset={-2}>
			<button
				type="button"
				className={isSelected ? styles.clickableItemSelected : styles.clickableItem}
				onClick={onClick}
			>
				{children}
			</button>
		</FocusRing>
	),
);

const getDmRouteChannelId = (pathname: string): string | null => {
	if (!pathname.startsWith(`${Routes.ME}/`)) {
		return null;
	}

	const [, , , channelId] = pathname.split('/');
	return channelId ?? null;
};

const canInviteInChannel = (channel?: ChannelRecord | null): channel is ChannelRecord => {
	if (!channel || !channel.guildId) {
		return false;
	}
	return InviteUtils.canInviteToChannel(channel.id, channel.guildId);
};

const getDefaultInviteChannelId = (guildId: string): string | null => {
	const selectedChannelId = SelectedChannelStore.selectedChannelIds.get(guildId);
	if (selectedChannelId) {
		const selectedChannel = ChannelStore.getChannel(selectedChannelId);
		if (canInviteInChannel(selectedChannel)) {
			return selectedChannel.id;
		}
	}

	const guildChannels = ChannelStore.getGuildChannels(guildId);
	for (const guildChannel of guildChannels) {
		if (canInviteInChannel(guildChannel)) {
			return guildChannel.id;
		}
	}

	return null;
};

export const DMList = observer(() => {
	const {t} = useLingui();
	const dmChannels = ChannelStore.dmChannels;
	const location = useLocation();
	const isFriendsTab = location.pathname === Routes.ME;
	const currentUser = UserStore.currentUser;
	const isMobile = MobileLayoutStore.isMobileLayout();
	const relationships = RelationshipStore.getRelationships();
	const pendingCount = relationships.filter((relation) => relation.type === RelationshipTypes.INCOMING_REQUEST).length;
	const [addFriendSheetOpen, setAddFriendSheetOpen] = useState(false);
	const [newMessageSheetOpen, setNewMessageSheetOpen] = useState(false);
	const selectedChannelId = SelectedChannelStore.selectedChannelIds.get(ME);
	const recentDmVisit = SelectedChannelStore.recentChannelVisits.find((visit) => visit.guildId === ME);
	const lastSelectedDmChannelId = selectedChannelId ?? recentDmVisit?.channelId ?? null;

	const handleOpenCreateDMModal = useCallback(() => {
		ModalActionCreators.push(modal(() => <CreateDMModal />));
	}, []);

	const [hasPreloaded, setHasPreloaded] = useState(false);
	useEffect(() => {
		if (!hasPreloaded && isMobile && dmChannels.length > 0 && dmChannels.length <= 100) {
			const channelIds = dmChannels.map((channel) => channel.id);
			UserActionCreators.preloadDMMessages(channelIds).catch(() => {});
			setHasPreloaded(true);
		}
	}, [hasPreloaded, isMobile, dmChannels.length]);

	const currentUserId = currentUser?.id;
	const personalNotesPath = currentUserId ? Routes.dmChannel(currentUserId) : '';

	const filteredDmChannels = useMemo(() => getSortedDmChannels(dmChannels, currentUserId), [dmChannels, currentUserId]);

	const routeDmChannelId = getDmRouteChannelId(location.pathname);
	const isMobileDmIndexRoute = isMobile && location.pathname === Routes.ME;
	const highlightedChannelId = (() => {
		if (routeDmChannelId && routeDmChannelId !== currentUserId) {
			return routeDmChannelId;
		}
		if (isMobileDmIndexRoute && lastSelectedDmChannelId !== currentUserId) {
			return filteredDmChannels[0]?.id ?? null;
		}
		return null;
	})();
	const shouldHighlightPersonalNotes =
		currentUserId != null &&
		(routeDmChannelId === currentUserId || (isMobileDmIndexRoute && lastSelectedDmChannelId === currentUserId));
	const showPremiumFeatures = shouldShowPremiumFeatures();
	const showMobilePlutoniumButton = showPremiumFeatures && !isMobile;

	const navigateTo = (path: string) => () => {
		if (Routes.isDMRoute(path)) {
			if (path === Routes.ME) {
				NavigationActionCreators.selectChannel(ME, null);
			} else {
				const channelId = path.split('/').pop();
				if (channelId && channelId !== ME) {
					NavigationActionCreators.selectChannel(ME, channelId);
				} else {
					NavigationActionCreators.selectChannel(ME, null);
				}
			}
		} else {
			NavigationActionCreators.selectChannel(ME, null);
		}
		if (MobileLayoutStore.isMobileLayout()) {
			LayoutActionCreators.updateMobileLayoutState(false, true);
		}
	};

	if (isMobile) {
		return (
			<div className={styles.mobileContainer}>
				<div className={styles.mobileHeader}>
					<h1 className={styles.mobileHeaderTitle}>
						<Trans>Messages</Trans>
					</h1>
					<div className={styles.mobileHeaderActions}>
						<FocusRing offset={-2}>
							<button type="button" onClick={() => QuickSwitcherStore.show()} className={styles.mobileHeaderButton}>
								<MagnifyingGlassIcon weight="bold" className={styles.iconSize5} />
							</button>
						</FocusRing>
						<FocusRing offset={-2}>
							<button
								type="button"
								onClick={() => setAddFriendSheetOpen(true)}
								className={styles.mobileAddFriendButton}
							>
								<UserPlusIcon weight="fill" className={styles.iconSize4} />
								<span>
									<Trans>Add Friends</Trans>
								</span>
								{pendingCount > 0 && <div className={styles.mobileAddFriendBadge}>{pendingCount}</div>}
							</button>
						</FocusRing>
					</div>
				</div>

				<Scroller className={styles.mobileScroller} key="dm-list-mobile-scroller">
					<div className={styles.mobileScrollerContent}>
						{currentUserId && (
							<FocusRing offset={-2}>
								<button
									type="button"
									onClick={navigateTo(personalNotesPath)}
									className={
										shouldHighlightPersonalNotes
											? styles.mobilePersonalNotesButtonSelected
											: styles.mobilePersonalNotesButton
									}
								>
									<div className={styles.mobileSpecialButtonContent}>
										<div className={styles.mobileSpecialButtonIcon}>
											<NotePencilIcon weight="fill" className={styles.iconSize5} />
										</div>
										<div className={styles.mobileSpecialButtonText}>
											<span className={styles.mobileSpecialButtonLabel}>
												<Trans>Personal Notes</Trans>
											</span>
										</div>
									</div>
								</button>
							</FocusRing>
						)}

						{showMobilePlutoniumButton && (
							<FocusRing offset={-2}>
								<button
									type="button"
									onClick={() => PremiumModalActionCreators.open()}
									className={styles.mobilePlutoniumButton}
								>
									<div className={styles.mobileSpecialButtonContent}>
										<div className={styles.mobileSpecialButtonIcon}>
											<CrownIcon weight="fill" className={styles.iconSize5} />
										</div>
										<div className={styles.mobileSpecialButtonText}>
											<span className={styles.mobileSpecialButtonLabel}>
												<Trans>Plutonium</Trans>
											</span>
										</div>
									</div>
								</button>
							</FocusRing>
						)}

						{filteredDmChannels.map((channel) => {
							const isSelected = highlightedChannelId === channel.id;
							return <DMListItem key={channel.id} channel={channel} isSelected={isSelected} />;
						})}
						<div style={{height: 'var(--spacing-2)'}} />
					</div>
				</Scroller>

				<FocusRing offset={-2}>
					<button type="button" onClick={() => setNewMessageSheetOpen(true)} className={styles.mobileFab}>
						<PaperPlaneIcon weight="fill" className={styles.sendIcon} />
					</button>
				</FocusRing>

				<AddFriendSheet isOpen={addFriendSheetOpen} onClose={() => setAddFriendSheetOpen(false)} />
				<CreateDMBottomSheet isOpen={newMessageSheetOpen} onClose={() => setNewMessageSheetOpen(false)} />
			</div>
		);
	}

	return (
		<div className={styles.dmListContainer}>
			<FocusRing offset={-2}>
				<button type="button" className={styles.dmListHeader} onClick={() => QuickSwitcherStore.show()}>
					<div className={styles.dmListHeaderButton}>
						<span className={styles.dmListHeaderText}>
							<Trans>Quick Switcher</Trans>
						</span>
						<div className={styles.dmListHeaderShortcut}>
							<KeybindHint action="quick_switcher" />
						</div>
					</div>
				</button>
			</FocusRing>
			<Scroller className={styles.desktopScroller} key="dm-list-desktop-scroller">
				<div className={styles.scrollerContent}>
					<div style={{height: 'var(--spacing-2)'}} />
					<ClickableItem isSelected={isFriendsTab} onClick={navigateTo(Routes.ME)}>
						<div className={styles.clickableItemInner}>
							<div className={styles.clickableItemContent}>
								<div className={styles.clickableItemIcon}>
									<UsersThreeIcon weight="fill" className={styles.iconSize5} />
								</div>
								<span className={styles.clickableItemText}>
									<Trans>Friends</Trans>
								</span>
							</div>
							<MentionBadge mentionCount={pendingCount} />
						</div>
					</ClickableItem>

					{currentUserId && (
						<ClickableItem isSelected={shouldHighlightPersonalNotes} onClick={navigateTo(personalNotesPath)}>
							<div className={styles.clickableItemContent}>
								<div className={styles.clickableItemIcon}>
									<NotePencilIcon weight="fill" className={styles.iconSize5} />
								</div>
								<span className={styles.clickableItemText}>
									<Trans>Personal Notes</Trans>
								</span>
							</div>
						</ClickableItem>
					)}

					{showPremiumFeatures && (
						<ClickableItem onClick={() => PremiumModalActionCreators.open()}>
							<div className={styles.clickableItemContent}>
								<div className={styles.clickableItemIcon}>
									<CrownIcon weight="fill" className={styles.iconSize5} />
								</div>
								<span className={styles.clickableItemText}>
									<Trans>Plutonium</Trans>
								</span>
							</div>
						</ClickableItem>
					)}

					<div className={styles.dmSectionSeparator} />

					<div className={styles.dmSectionHeader}>
						<div className={styles.dmSectionHeaderText}>
							<span className={styles.dmSectionHeaderLabel}>
								<Trans>Direct Messages</Trans>
							</span>
						</div>
						<Tooltip
							text={() => <TooltipWithKeybind label={t`Create DM`} action="create_private_group" />}
							position="top"
						>
							<FocusRing offset={-2}>
								<button type="button" className={styles.dmCreateButton} onClick={handleOpenCreateDMModal}>
									<PlusIcon weight="bold" className={styles.iconSize4} />
								</button>
							</FocusRing>
						</Tooltip>
					</div>

					<div className={styles.dmChannelList}>
						{filteredDmChannels.map((channel) => {
							const isSelected = highlightedChannelId === channel.id;
							return <DMListItem key={channel.id} channel={channel} isSelected={isSelected} />;
						})}
					</div>
					<div style={{height: 'var(--spacing-2)'}} />
				</div>
			</Scroller>
		</div>
	);
});
