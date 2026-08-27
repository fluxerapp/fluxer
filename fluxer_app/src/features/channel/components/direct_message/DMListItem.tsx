// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility, {DMMessagePreviewMode} from '@app/features/accessibility/state/Accessibility';
import {LongPressable} from '@app/features/app/components/LongPressable';
import {getChannelUnreadState} from '@app/features/app/components/layout/utils/ChannelUnreadState';
import {CustomStatusDisplay} from '@app/features/app/components/shared/custom_status_display/CustomStatusDisplay';
import {GroupDMAvatar} from '@app/features/app/components/shared/GroupDMAvatar';
import {useChannelHoverPreload} from '@app/features/app/hooks/useChannelHoverPreload';
import {useContextMenuHoverState} from '@app/features/app/hooks/useContextMenuHoverState';
import {UserTag} from '@app/features/channel/components/ChannelUserTag';
import styles from '@app/features/channel/components/direct_message/DirectMessageList.module.css';
import type {InviteCandidate} from '@app/features/channel/components/direct_message/DMListHelpers';
import {getDefaultInviteChannelId} from '@app/features/channel/components/direct_message/DMListHelpers';
import {buildMobileMenuGroups} from '@app/features/channel/components/direct_message/DMListItemMenuGroups';
import {useDMListItemHandlers} from '@app/features/channel/components/direct_message/useDMListItemHandlers';
import {EditGroupBottomSheet} from '@app/features/channel/components/modals/EditGroupBottomSheet';
import {GroupInvitesBottomSheet} from '@app/features/channel/components/modals/GroupInvitesBottomSheet';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {useLeaveGroup} from '@app/features/guild/hooks/useLeaveGroup';
import Guilds from '@app/features/guild/state/Guilds';
import {MENTION_COUNT_ARIA_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import Messages from '@app/features/messaging/state/MessagingMessages';
import {SystemMessageUtils} from '@app/features/messaging/utils/SystemMessageUtils';
import ReadStates from '@app/features/read_state/state/ReadStates';
import Relationships from '@app/features/relationship/state/Relationships';
import TypingIndicator from '@app/features/typing/state/TypingIndicator';
import {DMContextMenu} from '@app/features/ui/action_menu/DMContextMenu';
import {GroupDMContextMenu} from '@app/features/ui/action_menu/GroupDMContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {ListStatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {useTextOverflow} from '@app/features/ui/hooks/useTextOverflow';
import type {MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import type {User} from '@app/features/user/models/User';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {formatShortRelativeTime} from '@fluxer/date_utils/src/DateDuration';
import {extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PushPinIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const YOU_DESCRIPTOR = msg({
	message: 'You',
	comment: 'First-person prefix used in DM list row preview when the current user is the message author.',
});
const SENT_AN_ATTACHMENT_DESCRIPTOR = msg({
	message: 'Sent an attachment',
	comment: 'DM list row preview text when the most recent message has only attachments.',
});
const SELECTED_DESCRIPTOR = msg({
	message: 'selected',
	comment: 'Screen-reader suffix announcing that the DM list row is the active selection. Lowercase fragment.',
});
const UNREAD_DESCRIPTOR = msg({
	message: 'unread',
	comment: 'Screen-reader fragment announcing that the DM list row has unread messages. Lowercase fragment.',
});
const MUTED_DESCRIPTOR = msg({
	message: 'muted',
	comment: 'Screen-reader fragment announcing that the DM list row is muted. Lowercase fragment.',
});
const PINNED_DESCRIPTOR = msg({
	message: 'pinned',
	comment: 'Screen-reader fragment announcing that the DM list row is pinned. Lowercase fragment.',
});
const CLOSE_DIRECT_MESSAGE_WITH_DESCRIPTOR = msg({
	message: 'Close direct message with {displayName}',
	comment: 'Accessible label and tooltip for the close-DM button on a DM list row.',
});

interface DMListItemProps {
	channel: Channel;
	isSelected: boolean;
}

interface ResolvedDMListItemProps extends DMListItemProps {
	isGroupDM: boolean;
	recipient: User | null;
}

interface DMListItemNameTextProps {
	displayName: string;
	dataFlx: string;
}

function DMListItemNameText({displayName, dataFlx}: DMListItemNameTextProps) {
	const nameRef = useRef<HTMLSpanElement>(null);
	const isNameOverflowing = useTextOverflow(nameRef, {content: displayName, measureTextRange: true});
	const content = (
		<span ref={nameRef} className={styles.dmItemNameText} data-flx={dataFlx}>
			{displayName}
		</span>
	);
	if (!isNameOverflowing) {
		return content;
	}
	return (
		<Tooltip text={displayName} data-flx="channel.direct-message.dm-list-item.name-tooltip">
			{content}
		</Tooltip>
	);
}

const ResolvedDMListItem = observer(function ResolvedDMListItem({
	channel,
	isSelected,
	isGroupDM,
	recipient,
}: ResolvedDMListItemProps) {
	const {i18n} = useLingui();
	const recipientId = recipient?.id || '';
	const isBotDM = Boolean(recipient?.bot || recipient?.system);
	const isTyping = isGroupDM
		? channel.recipientIds.some((groupRecipientId) => TypingIndicator.isTyping(channel.id, groupRecipientId))
		: TypingIndicator.isTyping(channel.id, recipientId);
	const hasUnreadMessages = () => ReadStates.hasUnread(channel.id);
	const isMobile = MobileLayout.isMobileLayout();
	const isMuted = UserGuildSettings.isChannelDirectlyMuted(null, channel.id);
	const mentionCount = ReadStates.getMentionCount(channel.id);
	const unreadState = getChannelUnreadState({
		hasUnread: ReadStates.hasUnread(channel.id),
		unreadCount: ReadStates.getUnreadCount(channel.id),
		mentionCount,
		isMuted,
		showFadedUnreadOnMutedChannels: Accessibility.showFadedUnreadOnMutedChannels,
	});
	const [menuOpen, setMenuOpen] = useState(false);
	const [nestedSheet, setNestedSheet] = useState<{
		title: string;
		groups: Array<MenuGroupType>;
	} | null>(null);
	const [editGroupSheetOpen, setEditGroupSheetOpen] = useState(false);
	const [invitesSheetOpen, setInvitesSheetOpen] = useState(false);
	const currentUser = Users.getCurrentUser();
	const lastMessage = Messages.getCachedMessages(channel.id)?.last();
	const leaveGroup = useLeaveGroup();
	const {keyboardModeEnabled} = KeyboardMode;
	const [isFocused, setIsFocused] = useState(false);
	const scrollTargetRef = useRef<HTMLElement | null>(null);
	const setDesktopRef = useCallback((node: HTMLDivElement | null) => {
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
	const restrictRecipientActions = recipient?.system ?? false;
	const relationshipType = recipient ? Relationships.getRelationship(recipient.id)?.type : undefined;
	const inviteCandidates = useMemo<Array<InviteCandidate>>(() => {
		if (!recipient || restrictRecipientActions || recipient.bot) return [];
		return Guilds.getGuilds()
			.filter((guild) => !GuildMembers.getMember(guild.id, recipient.id))
			.map((guild): InviteCandidate | null => {
				const channelId = getDefaultInviteChannelId(guild.id);
				return channelId ? {guild, channelId} : null;
			})
			.filter((candidate): candidate is InviteCandidate => Boolean(candidate))
			.sort((a, b) => a.guild.name.localeCompare(b.guild.name));
	}, [recipient?.id, restrictRecipientActions]);
	const handlers = useDMListItemHandlers({
		channel,
		recipient,
		isGroupDM,
		isMobile,
		relationshipType,
		restrictRecipientActions,
		closeAllSheets,
		setMenuOpen,
		setEditGroupSheetOpen,
		setInvitesSheetOpen,
		leaveGroup,
		i18n,
	});
	const {scheduleChannelPreload, cancelChannelPreload, preloadChannelNow} = useChannelHoverPreload({
		channel,
		guild: null,
		preloadMemberList: false,
	});
	const handleNavigate = useCallback(() => {
		preloadChannelNow();
		handlers.navigateTo();
	}, [handlers.navigateTo, preloadChannelNow]);
	const mobileMenuGroups = buildMobileMenuGroups({
		channel,
		recipient,
		relationshipType,
		restrictRecipientActions,
		inviteCandidates,
		hasUnreadMessages: hasUnreadMessages(),
		handlers,
		openNestedSheet,
		closeAllSheets,
		i18n,
	});
	useEffect(() => {
		if (isSelected) {
			scrollTargetRef.current?.scrollIntoView({block: 'nearest'});
		}
	}, [isSelected]);
	const displayName = ChannelUtils.getDMDisplayName(channel);
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
		if (Accessibility.dmMessagePreviewMode === DMMessagePreviewMode.ALL) {
			return true;
		}
		if (Accessibility.dmMessagePreviewMode === DMMessagePreviewMode.UNREAD_ONLY) {
			return hasUnreadMessages();
		}
		return false;
	})();
	const getMessagePreview = (): React.ReactNode | null => {
		if (!lastMessage) return null;
		const isCurrentUser = lastMessage.author.id === currentUser?.id;
		const authorPrefix = isCurrentUser
			? `${i18n._(YOU_DESCRIPTOR)}: `
			: `${NicknameUtils.getNickname(lastMessage.author, null, channel.id)}: `;
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
					<span
						className={styles.dmItemPreviewMarkdown}
						data-flx="channel.direct-message.dm-list-item.get-message-preview.dm-item-preview-markdown"
					>
						<SafeMarkdown
							content={lastMessage.content}
							options={{
								context: MarkdownContext.RESTRICTED_INLINE_REPLY,
								channelId: channel.id,
								messageId: lastMessage.id,
								disableAnimatedEmoji: true,
								mentionChannels: lastMessage.mentionChannels,
							}}
							data-flx="channel.direct-message.dm-list-item.get-message-preview.safe-markdown"
						/>
					</span>
				</>
			);
		}
		if (lastMessage.attachments?.length > 0) {
			return (
				<>
					{authorPrefix}
					<span
						className={styles.dmItemSubtextItalic}
						data-flx="channel.direct-message.dm-list-item.get-message-preview.dm-item-subtext-italic"
					>
						{i18n._(SENT_AN_ATTACHMENT_DESCRIPTOR)}
					</span>
				</>
			);
		}
		return null;
	};
	const messagePreview = shouldShowMessagePreviewSetting ? getMessagePreview() : null;
	const dmAriaLabel = useMemo(() => {
		const parts = [displayName];
		if (isSelected) parts.push(i18n._(SELECTED_DESCRIPTOR));
		if (mentionCount > 0) parts.push(i18n._(MENTION_COUNT_ARIA_DESCRIPTOR, {mentionCount}));
		else if (unreadState.hasUnreadMessages) parts.push(i18n._(UNREAD_DESCRIPTOR));
		if (isMuted) parts.push(i18n._(MUTED_DESCRIPTOR));
		if (channel.isPinned) parts.push(i18n._(PINNED_DESCRIPTOR));
		return parts.join(', ');
	}, [channel.isPinned, displayName, isMuted, isSelected, mentionCount, unreadState.hasUnreadMessages, i18n.locale]);
	const handleContextMenu = (event: React.MouseEvent) => {
		event.preventDefault();
		event.stopPropagation();
		if (isGroupDM) {
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<GroupDMContextMenu
					channel={channel}
					onClose={onClose}
					data-flx="channel.direct-message.dm-list-item.handle-context-menu.group-dm-context-menu"
				/>
			));
		} else if (recipient) {
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<DMContextMenu
					channel={channel}
					recipient={recipient}
					onClose={onClose}
					data-flx="channel.direct-message.dm-list-item.handle-context-menu.dm-context-menu"
				/>
			));
		}
	};
	if (isMobile) {
		return (
			<>
				<FocusRing offset={-2} data-flx="channel.direct-message.dm-list-item.focus-ring">
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
						data-dm-list-focus-item="true"
						aria-label={dmAriaLabel}
						aria-current={isSelected ? 'page' : undefined}
						onClick={handleNavigate}
						onContextMenu={handleContextMenu}
						onKeyDown={(event) => {
							if (!isKeyboardActivationKey(event.key)) return;
							event.preventDefault();
							handleNavigate();
						}}
						data-flx="channel.direct-message.dm-list-item.dm-item-mobile-selected.navigate-to"
					>
						<AnimatePresence data-flx="channel.direct-message.dm-list-item.animate-presence">
							{unreadState.shouldShowUnreadIndicator && (
								<div
									className={styles.dmItemUnreadIndicatorContainerMobile}
									data-flx="channel.direct-message.dm-list-item.dm-item-unread-indicator-container-mobile"
								>
									<motion.span
										data-flx="channel.direct-message.dm-list-item.dm-item-unread-indicator"
										{...motionSettings}
										className={styles.dmItemUnreadIndicator}
									/>
								</div>
							)}
						</AnimatePresence>
						<div className={styles.dmItemContent} data-flx="channel.direct-message.dm-list-item.dm-item-content">
							<div
								className={styles.dmItemAvatarWrapper}
								data-flx="channel.direct-message.dm-list-item.dm-item-avatar-wrapper"
							>
								{isGroupDM ? (
									<GroupDMAvatar
										channel={channel}
										size={40}
										isTyping={isTyping}
										data-flx="channel.direct-message.dm-list-item.group-dm-avatar"
									/>
								) : (
									<ListStatusAwareAvatar
										user={recipient!}
										size={40}
										isTyping={isTyping}
										showOffline={true}
										data-flx="channel.direct-message.dm-list-item.status-aware-avatar"
									/>
								)}
							</div>
							<div className={styles.dmItemInfo} data-flx="channel.direct-message.dm-list-item.dm-item-info">
								<span className={styles.dmItemName} data-flx="channel.direct-message.dm-list-item.dm-item-name">
									{channel.isPinned && (
										<PushPinIcon
											weight="fill"
											className={styles.dmItemPinIcon}
											data-flx="channel.direct-message.dm-list-item.dm-item-pin-icon"
										/>
									)}
									<DMListItemNameText
										displayName={displayName}
										dataFlx="channel.direct-message.dm-list-item.dm-item-name-text"
										data-flx="channel.direct-message.dm-list-item.resolved-dm-list-item.dm-list-item-name-text"
									/>
									{!isGroupDM && isBotDM && (
										<UserTag
											className={styles.dmItemUserTag}
											system={recipient?.system}
											data-flx="channel.direct-message.dm-list-item.dm-item-user-tag"
										/>
									)}
								</span>
								{!isGroupDM && recipient && !messagePreview && (
									<CustomStatusDisplay
										userId={recipient.id}
										className={styles.dmItemCustomStatus}
										showText={true}
										showTooltip
										tooltipPosition="bottom"
										animateOnParentHover
										data-flx="channel.direct-message.dm-list-item.dm-item-custom-status"
									/>
								)}
								{isGroupDM && (
									<span
										className={clsx(styles.dmItemSubtext, styles.dmItemMembersSubtext)}
										data-flx="channel.direct-message.dm-list-item.dm-item"
									>
										{plural(
											{count: channel.recipientIds.length + 1},
											{
												one: '# member',
												other: '# members',
											},
										)}
									</span>
								)}
								{!isGroupDM && messagePreview && (
									<span className={styles.dmItemSubtext} data-flx="channel.direct-message.dm-list-item.dm-item-subtext">
										{messagePreview}
									</span>
								)}
							</div>
							{relativeTime && (
								<span
									className={styles.dmItemTimestamp}
									data-flx="channel.direct-message.dm-list-item.dm-item-timestamp"
								>
									{relativeTime}
								</span>
							)}
						</div>
					</LongPressable>
				</FocusRing>
				<MenuBottomSheet
					isOpen={menuOpen}
					onClose={closeAllSheets}
					groups={mobileMenuGroups}
					data-flx="channel.direct-message.dm-list-item.menu-bottom-sheet"
				/>
				{nestedSheet && (
					<MenuBottomSheet
						isOpen={Boolean(nestedSheet)}
						onClose={closeNestedSheet}
						groups={nestedSheet.groups}
						title={nestedSheet.title}
						showCloseButton={true}
						data-flx="channel.direct-message.dm-list-item.menu-bottom-sheet--2"
					/>
				)}
				{isGroupDM && (
					<>
						<EditGroupBottomSheet
							isOpen={editGroupSheetOpen}
							onClose={() => setEditGroupSheetOpen(false)}
							channelId={channel.id}
							data-flx="channel.direct-message.dm-list-item.edit-group-bottom-sheet"
						/>
						<GroupInvitesBottomSheet
							isOpen={invitesSheetOpen}
							onClose={() => setInvitesSheetOpen(false)}
							channelId={channel.id}
							data-flx="channel.direct-message.dm-list-item.group-invites-bottom-sheet"
						/>
					</>
				)}
			</>
		);
	}
	return (
		<>
			<FocusRing offset={-2} data-flx="channel.direct-message.dm-list-item.focus-ring--2">
				<div
					ref={setDesktopRef}
					className={clsx(
						isSelected ? styles.dmItemSelected : hasUnreadMessages() ? styles.dmItemUnread : styles.dmItem,
						isMuted && styles.dmItemMuted,
						contextMenuOpen && styles.contextMenuActive,
					)}
					onClick={handleNavigate}
					onContextMenu={handleContextMenu}
					onMouseEnter={scheduleChannelPreload}
					onMouseLeave={cancelChannelPreload}
					onFocus={() => setIsFocused(true)}
					onBlur={() => setIsFocused(false)}
					data-dm-list-focus-item="true"
					role="link"
					tabIndex={0}
					aria-label={dmAriaLabel}
					aria-current={isSelected ? 'page' : undefined}
					onKeyDown={(event) => {
						if (event.target !== event.currentTarget) return;
						if (!isKeyboardActivationKey(event.key)) return;
						event.preventDefault();
						handleNavigate();
					}}
					data-flx="channel.direct-message.dm-list-item.dm-item-selected.navigate-to"
				>
					<AnimatePresence data-flx="channel.direct-message.dm-list-item.animate-presence--2">
						{unreadState.shouldShowUnreadIndicator && (
							<div
								className={styles.dmItemUnreadIndicatorContainerDesktop}
								data-flx="channel.direct-message.dm-list-item.dm-item-unread-indicator-container-desktop"
							>
								<motion.span
									data-flx="channel.direct-message.dm-list-item.dm-item-unread-indicator--2"
									{...motionSettings}
									className={styles.dmItemUnreadIndicator}
								/>
							</div>
						)}
					</AnimatePresence>
					<div className={styles.dmItemContent} data-flx="channel.direct-message.dm-list-item.dm-item-content--2">
						<div
							className={styles.dmItemAvatarWrapper}
							data-flx="channel.direct-message.dm-list-item.dm-item-avatar-wrapper--2"
						>
							{isGroupDM ? (
								<GroupDMAvatar
									channel={channel}
									size={32}
									isTyping={isTyping}
									data-flx="channel.direct-message.dm-list-item.group-dm-avatar--2"
								/>
							) : (
								<ListStatusAwareAvatar
									user={recipient!}
									size={32}
									isTyping={isTyping}
									showOffline={true}
									data-flx="channel.direct-message.dm-list-item.status-aware-avatar--2"
								/>
							)}
						</div>
						<div className={styles.dmItemInfo} data-flx="channel.direct-message.dm-list-item.dm-item-info--2">
							<span className={styles.dmItemName} data-flx="channel.direct-message.dm-list-item.dm-item-name--2">
								{channel.isPinned && (
									<PushPinIcon
										weight="fill"
										className={styles.dmItemPinIcon}
										data-flx="channel.direct-message.dm-list-item.dm-item-pin-icon--2"
									/>
								)}
								<DMListItemNameText
									displayName={displayName}
									dataFlx="channel.direct-message.dm-list-item.dm-item-name-text--2"
									data-flx="channel.direct-message.dm-list-item.resolved-dm-list-item.dm-list-item-name-text--2"
								/>
								{!isGroupDM && isBotDM && (
									<UserTag
										className={styles.dmItemUserTag}
										system={recipient?.system}
										data-flx="channel.direct-message.dm-list-item.dm-item-user-tag--2"
									/>
								)}
							</span>
							{!isGroupDM && recipient && !messagePreview && (
								<CustomStatusDisplay
									userId={recipient.id}
									className={styles.dmItemCustomStatus}
									showText={true}
									showTooltip
									tooltipPosition="bottom"
									animateOnParentHover
									data-flx="channel.direct-message.dm-list-item.dm-item-custom-status--2"
								/>
							)}
							{isGroupDM && (
								<span
									className={clsx(styles.dmItemSubtext, styles.dmItemMembersSubtext)}
									data-flx="channel.direct-message.dm-list-item.dm-item--2"
								>
									{plural(
										{count: channel.recipientIds.length + 1},
										{
											one: '# member',
											other: '# members',
										},
									)}
								</span>
							)}
							{!isGroupDM && messagePreview && (
								<span
									className={styles.dmItemSubtext}
									data-flx="channel.direct-message.dm-list-item.dm-item-subtext--2"
								>
									{messagePreview}
								</span>
							)}
						</div>
						<FocusRing offset={-2} data-flx="channel.direct-message.dm-list-item.focus-ring--3">
							<button
								type="button"
								tabIndex={-1}
								className={styles.dmItemCloseButton}
								style={{opacity: isFocused && keyboardModeEnabled ? 1 : undefined}}
								onClick={handlers.handleRemoveChannel}
								aria-label={i18n._(CLOSE_DIRECT_MESSAGE_WITH_DESCRIPTOR, {displayName})}
								data-flx="channel.direct-message.dm-list-item.dm-item-close-button.remove-channel"
							>
								<XIcon
									weight="bold"
									className={styles.iconSize4}
									data-flx="channel.direct-message.dm-list-item.icon-size4"
								/>
							</button>
						</FocusRing>
					</div>
				</div>
			</FocusRing>
			{isGroupDM && (
				<>
					<EditGroupBottomSheet
						isOpen={editGroupSheetOpen}
						onClose={() => setEditGroupSheetOpen(false)}
						channelId={channel.id}
						data-flx="channel.direct-message.dm-list-item.edit-group-bottom-sheet--2"
					/>
					<GroupInvitesBottomSheet
						isOpen={invitesSheetOpen}
						onClose={() => setInvitesSheetOpen(false)}
						channelId={channel.id}
						data-flx="channel.direct-message.dm-list-item.group-invites-bottom-sheet--2"
					/>
				</>
			)}
		</>
	);
});
export const DMListItem = observer((props: DMListItemProps) => {
	const {channel} = props;
	const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
	const recipient = !isGroupDM ? (Users.getUser(channel.recipientIds[0]) ?? null) : null;
	if (!isGroupDM && !recipient) return null;
	return (
		<ResolvedDMListItem
			data-flx="channel.direct-message.dm-list-item.resolved-dm-list-item"
			{...props}
			isGroupDM={isGroupDM}
			recipient={recipient}
		/>
	);
});
