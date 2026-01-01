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

import {Plural, Trans, useLingui} from '@lingui/react/macro';

import {
	CopyIcon,
	CrownIcon,
	MagnifyingGlassIcon,
	NotePencilIcon,
	PaperPlaneIcon,
	PlusIcon,
	PushPinIcon,
	SignOutIcon,
	UserPlusIcon,
	UsersThreeIcon,
	XIcon,
} from '@phosphor-icons/react';

import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as LayoutActionCreators from '~/actions/LayoutActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as NavigationActionCreators from '~/actions/NavigationActionCreators';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UserActionCreators from '~/actions/UserActionCreators';

import {ChannelTypes, ME, MessageTypes, RelationshipTypes} from '~/Constants';

import {CreateDMBottomSheet} from '~/components/bottomsheets/CreateDMBottomSheet';
import {UserTag} from '~/components/channel/UserTag';
import {CustomStatusDisplay} from '~/components/common/CustomStatusDisplay/CustomStatusDisplay';
import {GroupDMAvatar} from '~/components/common/GroupDMAvatar';
import {LongPressable} from '~/components/LongPressable';
import {AddFriendSheet} from '~/components/modals/AddFriendSheet';
import {CreateDMModal} from '~/components/modals/CreateDMModal';
import {EditGroupBottomSheet} from '~/components/modals/EditGroupBottomSheet';
import {EditGroupModal} from '~/components/modals/EditGroupModal';
import {GroupInvitesBottomSheet} from '~/components/modals/GroupInvitesBottomSheet';
import {GroupInvitesModal} from '~/components/modals/GroupInvitesModal';
import {DMContextMenu} from '~/components/uikit/ContextMenu/DMContextMenu';
import {GroupDMContextMenu} from '~/components/uikit/ContextMenu/GroupDMContextMenu';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {KeybindHint, TooltipWithKeybind} from '~/components/uikit/KeybindHint/KeybindHint';
import {MentionBadge} from '~/components/uikit/MentionBadge';
import type {MenuGroupType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {MenuBottomSheet} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {Scroller} from '~/components/uikit/Scroller';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';

import {useLeaveGroup} from '~/hooks/useLeaveGroup';

import {getCustomStatusText, normalizeCustomStatus} from '~/lib/customStatus';
import {SafeMarkdown} from '~/lib/markdown';
import {MarkdownContext} from '~/lib/markdown/renderers';
import {useLocation} from '~/lib/router';

import {Routes} from '~/Routes';

import type {ChannelRecord} from '~/records/ChannelRecord';

import AccessibilityStore, {DMMessagePreviewMode} from '~/stores/AccessibilityStore';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import KeyboardModeStore from '~/stores/KeyboardModeStore';
import MessageStore from '~/stores/MessageStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PresenceStore from '~/stores/PresenceStore';
import QuickSwitcherStore from '~/stores/QuickSwitcherStore';
import ReadStateStore from '~/stores/ReadStateStore';
import RelationshipStore from '~/stores/RelationshipStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import TypingStore from '~/stores/TypingStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import UserStore from '~/stores/UserStore';

import * as ChannelUtils from '~/utils/ChannelUtils';
import {getSortedDmChannels} from '~/utils/dmChannelUtils';
import * as NicknameUtils from '~/utils/NicknameUtils';
import {shouldShowPremiumFeatures} from '~/utils/PremiumUtils';
import * as RouterUtils from '~/utils/RouterUtils';
import SnowflakeUtil from '~/utils/SnowflakeUtil';
import {SystemMessageUtils} from '~/utils/SystemMessageUtils';
import * as TimeUtils from '~/utils/TimeUtils';

import styles from './DMList.module.css';

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
	const [editGroupSheetOpen, setEditGroupSheetOpen] = useState(false);
	const [invitesSheetOpen, setInvitesSheetOpen] = useState(false);
	const currentUser = UserStore.getCurrentUser();
	const lastMessage = MessageStore.getMessages(channel.id).last();
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

	useEffect(() => {
		if (isSelected) {
			scrollTargetRef.current?.scrollIntoView({block: 'nearest'});
		}
	}, [isSelected]);

	if (!isGroupDM && !recipient) return null;

	const displayName = ChannelUtils.getDMDisplayName(channel);

	const dmPath = Routes.dmChannel(channel.id);

	const navigateTo = () => {
		RouterUtils.transitionTo(dmPath);
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
			RouterUtils.transitionTo(Routes.ME);
		}
	};

	const handleCopyChannelId = async () => {
		await TextCopyActionCreators.copy(i18n, channel.id);
		setMenuOpen(false);
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
		setMenuOpen(false);
		try {
			await PrivateChannelActionCreators.pinDmChannel(channel.id);
			ToastActionCreators.createToast({type: 'success', children: t`Pinned DM`});
		} catch {
			ToastActionCreators.createToast({type: 'error', children: t`Failed to pin DM`});
		}
	};

	const handleUnpinChannel = async () => {
		setMenuOpen(false);
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
					icon: <NotePencilIcon weight="fill" className={styles.iconSize5} />,
					label: t`Edit Group`,
					onClick: handleEditGroup,
				},
				channel.isPinned
					? {
							icon: <PushPinIcon weight="fill" className={styles.iconSize5} />,
							label: t`Unpin Group DM`,
							onClick: handleUnpinChannel,
						}
					: {
							icon: <PushPinIcon weight="fill" className={styles.iconSize5} />,
							label: t`Pin Group DM`,
							onClick: handlePinChannel,
						},
			],
		});
		const isOwner = channel.ownerId === AuthenticationStore.currentUserId;
		if (isOwner) {
			menuGroups[0].items.push({
				icon: <PaperPlaneIcon weight="fill" className={styles.iconSize5} />,
				label: t`Invites`,
				onClick: handleShowInvites,
			});
		}
		menuGroups.push({
			items: [
				{
					icon: <SignOutIcon weight="fill" className={styles.iconSize5} />,
					label: t`Leave Group`,
					onClick: handleLeaveGroup,
					danger: true,
				},
				{
					icon: <CopyIcon weight="fill" className={styles.iconSize5} />,
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
							icon: <PushPinIcon weight="fill" className={styles.iconSize5} />,
							label: t`Unpin DM`,
							onClick: handleUnpinChannel,
						}
					: {
							icon: <PushPinIcon weight="fill" className={styles.iconSize5} />,
							label: t`Pin DM`,
							onClick: handlePinChannel,
						},
				{
					icon: <XIcon weight="bold" className={styles.iconSize5} />,
					label: t`Close DM`,
					onClick: () => handleRemoveChannel(),
					danger: true,
				},
				{
					icon: <CopyIcon weight="fill" className={styles.iconSize5} />,
					label: t`Copy Channel ID`,
					onClick: handleCopyChannelId,
				},
			],
		});
	}

	const animationSettings = {opacity: 1, height: 8};
	const motionSettings = {
		animate: animationSettings,
		exit: {opacity: 0},
		initial: animationSettings,
		transition: {duration: 0},
	};

	const relativeTime = channel.lastMessageId
		? TimeUtils.formatShortRelativeTime(SnowflakeUtil.extractTimestamp(channel.lastMessageId))
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
										<Plural value={channel.recipientIds.length + 1} one="# Member" other="# Members" />
									</span>
								)}
								{!isGroupDM && messagePreview && <span className={styles.dmItemSubtext}>{messagePreview}</span>}
							</div>
							{relativeTime && <span className={styles.dmItemTimestamp}>{relativeTime}</span>}
						</div>
					</LongPressable>
				</FocusRing>
				<MenuBottomSheet isOpen={menuOpen} onClose={() => setMenuOpen(false)} groups={menuGroups} />
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
									<Plural value={channel.recipientIds.length + 1} one="# Member" other="# Members" />
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
				}
			}
		} else {
			NavigationActionCreators.selectChannel(ME, null);
		}
		RouterUtils.transitionTo(path);
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
					<button type="button" onClick={() => setNewMessageSheetOpen(true)} className={styles.mobileFAB}>
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
