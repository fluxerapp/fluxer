// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {LongPressable} from '@app/features/app/components/LongPressable';
import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {useRovingFocusList} from '@app/features/app/hooks/useRovingFocusList';
import {PersonalNotesPurgeFailedModal} from '@app/features/channel/components/alerts/PersonalNotesPurgeFailedModal';
import {CreateDMBottomSheet} from '@app/features/channel/components/bottomsheets/CreateDMBottomSheet';
import styles from '@app/features/channel/components/direct_message/DirectMessageList.module.css';
import {getDmRouteChannelId} from '@app/features/channel/components/direct_message/DMListHelpers';
import {DMListItem} from '@app/features/channel/components/direct_message/DMListItem';
import {CreateDMModal} from '@app/features/channel/components/modals/CreateDMModal';
import Channels from '@app/features/channel/state/Channels';
import {getCreateDMRestrictionMessage, getCreateDmRestriction} from '@app/features/channel/utils/CreateDMModalUtils';
import {PERSONAL_NOTES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {getSortedDmChannels} from '@app/features/messaging/utils/DmChannelUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {AddFriendSheet} from '@app/features/relationship/components/modals/AddFriendSheet';
import Relationships from '@app/features/relationship/state/Relationships';
import QuickSwitcher from '@app/features/search/state/QuickSwitcher';
import {DeleteIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {PersonalNotesContextMenu} from '@app/features/ui/action_menu/PersonalNotesContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as LayoutCommands from '@app/features/ui/commands/LayoutCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {MentionBadge} from '@app/features/ui/components/MentionBadge';
import {Scroller} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {KeybindHint, TooltipWithKeybind} from '@app/features/ui/keybind_hint/KeybindHint';
import type {MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import Users from '@app/features/user/state/Users';
import {ME} from '@fluxer/constants/src/AppConstants';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	CrownIcon,
	MagnifyingGlassIcon,
	NotePencilIcon,
	PaperPlaneIcon,
	PlusIcon,
	UserPlusIcon,
	UsersThreeIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const PURGE_PERSONAL_NOTES_DESCRIPTOR = msg({
	message: 'Purge personal notes',
	comment: 'Title of the destructive confirmation alert when wiping the personal notes self-DM. Keep tone serious.',
});
const ARE_YOU_SURE_YOU_WANT_TO_DELETE_EVERY_DESCRIPTOR = msg({
	message:
		'Are you sure you want to delete every message in your personal notes? This will permanently remove all messages and their attachments. This action cannot be undone.',
	comment:
		'Body of the destructive purge-personal-notes confirmation alert. Keep tone serious and explicit about irreversibility.',
});
const PURGE_DESCRIPTOR = msg({
	message: 'Purge',
	comment: 'Confirm button label on the purge-personal-notes destructive alert.',
});
const PURGED_MESSAGES_FROM_PERSONAL_NOTES_DESCRIPTOR = msg({
	message: 'Purged {deletedCount} messages from personal notes',
	comment: 'Toast confirmation after wiping personal notes. deletedCount is the number of messages removed.',
});
const PERSONAL_NOTES_WERE_ALREADY_EMPTY_DESCRIPTOR = msg({
	message: 'Personal notes were already empty',
	comment: 'Toast shown when triggering the purge personal notes flow on an already empty self-DM.',
});
const OPEN_QUICK_SWITCHER_DESCRIPTOR = msg({
	message: 'Open quick switcher',
	comment: 'Accessible label and tooltip for the search field at the top of the DM list. Opens the quick switcher.',
});
const NEW_MESSAGE_DESCRIPTOR = msg({
	message: 'New message',
	comment: 'Tooltip on the new message button at the top of the DM list.',
});
const CREATE_DM_DESCRIPTOR = msg({
	message: 'Create DM',
	comment: 'Accessible label for the create-DM trigger in the DM list header.',
});
const logger = new Logger('DMList');
const ClickableItem = observer(
	({
		isSelected,
		onClick,
		onContextMenu,
		children,
	}: {
		isSelected?: boolean;
		onClick: () => void;
		onContextMenu?: (event: React.MouseEvent) => void;
		children: React.ReactNode;
	}) => (
		<FocusRing offset={-2} data-flx="channel.direct-message.dm-list.clickable-item.focus-ring">
			<button
				type="button"
				className={isSelected ? styles.clickableItemSelected : styles.clickableItem}
				onClick={onClick}
				onContextMenu={onContextMenu}
				aria-current={isSelected ? 'page' : undefined}
				data-flx="channel.direct-message.dm-list.clickable-item.clickable-item.button"
			>
				{children}
			</button>
		</FocusRing>
	),
);
export const DMList = observer(() => {
	const {i18n} = useLingui();
	const dmChannels = Channels.dmChannels;
	const location = useLocation();
	const isFriendsTab = location.pathname === Routes.ME;
	const currentUser = Users.currentUser;
	const createDmRestriction = getCreateDmRestriction(currentUser);
	const createDmTooltipText = createDmRestriction ? getCreateDMRestrictionMessage(i18n, createDmRestriction) : null;
	const isMobile = MobileLayout.isMobileLayout();
	const relationships = Relationships.getRelationships();
	const pendingCount = relationships.filter((relation) => relation.type === RelationshipTypes.INCOMING_REQUEST).length;
	const [addFriendSheetOpen, setAddFriendSheetOpen] = useState(false);
	const [newMessageSheetOpen, setNewMessageSheetOpen] = useState(false);
	const [personalNotesSheetOpen, setPersonalNotesSheetOpen] = useState(false);
	const selectedChannelId = SelectedChannel.selectedChannelIds.get(ME);
	const recentDmVisit = SelectedChannel.recentChannelVisits.find((visit) => visit.guildId === ME);
	const lastSelectedDmChannelId = selectedChannelId ?? recentDmVisit?.channelId ?? null;
	const handleOpenCreateDMModal = useCallback(() => {
		if (createDmRestriction) return;
		ModalCommands.push(
			modal(() => (
				<CreateDMModal data-flx="channel.direct-message.dm-list.handle-open-create-dm-modal.create-dm-modal" />
			)),
		);
	}, [createDmRestriction]);
	const [hasPreloaded, setHasPreloaded] = useState(false);
	useEffect(() => {
		if (!hasPreloaded && isMobile && dmChannels.length > 0 && dmChannels.length <= 100) {
			const channelIds = dmChannels.map((channel) => channel.id);
			UserCommands.preloadDMMessages(channelIds).catch(() => {});
			setHasPreloaded(true);
		}
	}, [hasPreloaded, isMobile, dmChannels.length]);
	const currentUserId = currentUser?.id;
	const personalNotesPath = currentUserId ? Routes.dmChannel(currentUserId) : '';
	const requestPurgePersonalNotes = useCallback(() => {
		if (!currentUserId) return;
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(PURGE_PERSONAL_NOTES_DESCRIPTOR)}
					description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_EVERY_DESCRIPTOR)}
					primaryText={i18n._(PURGE_DESCRIPTOR)}
					primaryVariant="danger"
					onPrimary={async () => {
						try {
							const {deletedCount} = await MessageCommands.purgePersonalNotes(currentUserId);
							ToastCommands.createToast({
								type: 'success',
								children:
									deletedCount > 0
										? i18n._(PURGED_MESSAGES_FROM_PERSONAL_NOTES_DESCRIPTOR, {deletedCount})
										: i18n._(PERSONAL_NOTES_WERE_ALREADY_EMPTY_DESCRIPTOR),
							});
						} catch (error) {
							logger.error('Failed to purge personal notes:', error);
							ModalCommands.push(
								modal(() => (
									<PersonalNotesPurgeFailedModal data-flx="channel.direct-message.dm-list.request-purge-personal-notes.personal-notes-purge-failed-modal" />
								)),
							);
						}
					}}
					data-flx="channel.direct-message.dm-list.request-purge-personal-notes.confirm-modal"
				/>
			)),
		);
	}, [currentUserId, i18n]);
	const personalNotesMenuGroups = useMemo<Array<MenuGroupType>>(
		() => [
			{
				items: [
					{
						icon: (
							<DeleteIcon size={20} data-flx="channel.direct-message.dm-list.personal-notes-menu-groups.delete-icon" />
						),
						label: i18n._(PURGE_PERSONAL_NOTES_DESCRIPTOR),
						danger: true,
						onClick: () => {
							setPersonalNotesSheetOpen(false);
							requestPurgePersonalNotes();
						},
					},
				],
			},
		],
		[requestPurgePersonalNotes, i18n.locale],
	);
	const handlePersonalNotesContextMenu = useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<PersonalNotesContextMenu
					onPurge={requestPurgePersonalNotes}
					onClose={onClose}
					data-flx="channel.direct-message.dm-list.handle-personal-notes-context-menu.personal-notes-context-menu"
				/>
			));
		},
		[requestPurgePersonalNotes],
	);
	const filteredDmChannels = useMemo(() => getSortedDmChannels(dmChannels, currentUserId), [dmChannels, currentUserId]);
	const dmListNavigationRef = useRovingFocusList<HTMLDivElement>({
		focusableSelector: '[data-dm-list-focus-item="true"]',
		orientation: 'vertical',
		loop: true,
		enabled: KeyboardMode.keyboardModeEnabled,
		restoreFocusOnWindowFocus: false,
		manageTabIndex: true,
	});
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
				NavigationCommands.selectChannel(ME, null);
			} else {
				const channelId = path.split('/').pop();
				if (channelId && channelId !== ME) {
					NavigationCommands.selectChannel(ME, channelId);
				} else {
					NavigationCommands.selectChannel(ME, null);
				}
			}
		} else {
			NavigationCommands.selectChannel(ME, null);
		}
		if (MobileLayout.isMobileLayout()) {
			LayoutCommands.updateMobileLayoutState(false, true);
		}
	};
	if (isMobile) {
		return (
			<div className={styles.mobileContainer} data-flx="channel.direct-message.dm-list.mobile-container">
				<div className={styles.mobileHeader} data-flx="channel.direct-message.dm-list.mobile-header">
					<h1 className={styles.mobileHeaderTitle} data-flx="channel.direct-message.dm-list.mobile-header-title">
						<Trans>Messages</Trans>
					</h1>
					<div className={styles.mobileHeaderActions} data-flx="channel.direct-message.dm-list.mobile-header-actions">
						<FocusRing offset={-2} data-flx="channel.direct-message.dm-list.focus-ring">
							<button
								type="button"
								onClick={() => QuickSwitcher.show()}
								className={styles.mobileHeaderButton}
								aria-label={i18n._(OPEN_QUICK_SWITCHER_DESCRIPTOR)}
								data-flx="channel.direct-message.dm-list.mobile-header-button.show"
							>
								<MagnifyingGlassIcon
									weight="bold"
									className={styles.iconSize5}
									data-flx="channel.direct-message.dm-list.icon-size5"
								/>
							</button>
						</FocusRing>
						<FocusRing offset={-2} data-flx="channel.direct-message.dm-list.focus-ring--2">
							<button
								type="button"
								onClick={() => setAddFriendSheetOpen(true)}
								className={styles.mobileAddFriendButton}
								data-flx="channel.direct-message.dm-list.mobile-add-friend-button.set-add-friend-sheet-open"
							>
								<UserPlusIcon
									weight="fill"
									className={styles.iconSize4}
									data-flx="channel.direct-message.dm-list.icon-size4"
								/>
								<span data-flx="channel.direct-message.dm-list.span">
									<Trans>Add friends</Trans>
								</span>
								{pendingCount > 0 && (
									<div
										className={styles.mobileAddFriendBadge}
										data-flx="channel.direct-message.dm-list.mobile-add-friend-badge"
									>
										{pendingCount}
									</div>
								)}
							</button>
						</FocusRing>
					</div>
				</div>
				<Scroller
					className={styles.mobileScroller}
					key="dm-list-mobile-scroller"
					data-flx="channel.direct-message.dm-list.mobile-scroller"
				>
					<div
						className={styles.mobileScrollerContent}
						data-flx="channel.direct-message.dm-list.mobile-scroller-content"
					>
						{currentUserId && (
							<FocusRing offset={-2} data-flx="channel.direct-message.dm-list.focus-ring--3">
								<LongPressable
									onLongPress={() => setPersonalNotesSheetOpen(true)}
									onClick={navigateTo(personalNotesPath)}
									onKeyDown={(event) => {
										if (!isKeyboardActivationKey(event.key)) return;
										event.preventDefault();
										navigateTo(personalNotesPath)();
									}}
									onContextMenu={(event) => {
										event.preventDefault();
										setPersonalNotesSheetOpen(true);
									}}
									className={
										shouldHighlightPersonalNotes
											? styles.mobilePersonalNotesButtonSelected
											: styles.mobilePersonalNotesButton
									}
									role="button"
									tabIndex={0}
									aria-current={shouldHighlightPersonalNotes ? 'page' : undefined}
									data-flx="channel.direct-message.dm-list.mobile-personal-notes-button.navigate-to"
								>
									<div
										className={styles.mobileSpecialButtonContent}
										data-flx="channel.direct-message.dm-list.mobile-special-button-content"
									>
										<div
											className={styles.mobileSpecialButtonIcon}
											data-flx="channel.direct-message.dm-list.mobile-special-button-icon"
										>
											<NotePencilIcon
												weight="fill"
												className={styles.iconSize5}
												data-flx="channel.direct-message.dm-list.icon-size5--2"
											/>
										</div>
										<div
											className={styles.mobileSpecialButtonText}
											data-flx="channel.direct-message.dm-list.mobile-special-button-text"
										>
											<span
												className={styles.mobileSpecialButtonLabel}
												data-flx="channel.direct-message.dm-list.mobile-special-button-label"
											>
												{i18n._(PERSONAL_NOTES_DESCRIPTOR)}
											</span>
										</div>
									</div>
								</LongPressable>
							</FocusRing>
						)}
						{showMobilePlutoniumButton && (
							<FocusRing offset={-2} data-flx="channel.direct-message.dm-list.focus-ring--4">
								<button
									type="button"
									onClick={() => PremiumModalCommands.open()}
									className={styles.mobilePlutoniumButton}
									data-flx="channel.direct-message.dm-list.mobile-plutonium-button.open"
								>
									<div
										className={styles.mobileSpecialButtonContent}
										data-flx="channel.direct-message.dm-list.mobile-special-button-content--2"
									>
										<div
											className={styles.mobileSpecialButtonIcon}
											data-flx="channel.direct-message.dm-list.mobile-special-button-icon--2"
										>
											<CrownIcon
												weight="fill"
												className={styles.iconSize5}
												data-flx="channel.direct-message.dm-list.icon-size5--3"
											/>
										</div>
										<div
											className={styles.mobileSpecialButtonText}
											data-flx="channel.direct-message.dm-list.mobile-special-button-text--2"
										>
											<span
												className={styles.mobileSpecialButtonLabel}
												data-flx="channel.direct-message.dm-list.mobile-special-button-label--2"
											>
												{PREMIUM_PRODUCT_NAME}
											</span>
										</div>
									</div>
								</button>
							</FocusRing>
						)}
						{filteredDmChannels.map((channel) => {
							const isSelected = highlightedChannelId === channel.id;
							return (
								<DMListItem
									key={channel.id}
									channel={channel}
									isSelected={isSelected}
									data-flx="channel.direct-message.dm-list.dm-list-item"
								/>
							);
						})}
						<div style={{height: 'var(--spacing-2)'}} data-flx="channel.direct-message.dm-list.div" />
					</div>
				</Scroller>
				<FocusRing offset={-2} data-flx="channel.direct-message.dm-list.focus-ring--5">
					<button
						type="button"
						onClick={() => setNewMessageSheetOpen(true)}
						className={styles.mobileFab}
						aria-label={i18n._(NEW_MESSAGE_DESCRIPTOR)}
						data-flx="channel.direct-message.dm-list.mobile-fab.set-new-message-sheet-open.button"
					>
						<PaperPlaneIcon
							weight="fill"
							className={styles.sendIcon}
							data-flx="channel.direct-message.dm-list.send-icon"
						/>
					</button>
				</FocusRing>
				<AddFriendSheet
					isOpen={addFriendSheetOpen}
					onClose={() => setAddFriendSheetOpen(false)}
					data-flx="channel.direct-message.dm-list.add-friend-sheet"
				/>
				<CreateDMBottomSheet
					isOpen={newMessageSheetOpen}
					onClose={() => setNewMessageSheetOpen(false)}
					data-flx="channel.direct-message.dm-list.create-dm-bottom-sheet"
				/>
				<MenuBottomSheet
					isOpen={personalNotesSheetOpen}
					onClose={() => setPersonalNotesSheetOpen(false)}
					groups={personalNotesMenuGroups}
					data-flx="channel.direct-message.dm-list.menu-bottom-sheet"
				/>
			</div>
		);
	}
	return (
		<div className={styles.dmListContainer} data-flx="channel.direct-message.dm-list.dm-list-container">
			<FocusRing offset={-2} data-flx="channel.direct-message.dm-list.focus-ring--6">
				<button
					type="button"
					className={styles.dmListHeader}
					onClick={() => QuickSwitcher.show()}
					data-flx="channel.direct-message.dm-list.dm-list-header.show.button"
				>
					<div className={styles.dmListHeaderButton} data-flx="channel.direct-message.dm-list.dm-list-header-button">
						<span className={styles.dmListHeaderText} data-flx="channel.direct-message.dm-list.dm-list-header-text">
							<Trans>Quick switcher</Trans>
						</span>
						<div
							className={styles.dmListHeaderShortcut}
							data-flx="channel.direct-message.dm-list.dm-list-header-shortcut"
						>
							<KeybindHint action="nav_quick_switcher" data-flx="channel.direct-message.dm-list.keybind-hint" />
						</div>
					</div>
				</button>
			</FocusRing>
			<Scroller
				className={styles.desktopScroller}
				key="dm-list-desktop-scroller"
				data-flx="channel.direct-message.dm-list.desktop-scroller"
			>
				<div className={styles.scrollerContent} data-flx="channel.direct-message.dm-list.scroller-content">
					<div style={{height: 'var(--spacing-2)'}} data-flx="channel.direct-message.dm-list.div--2" />
					<ClickableItem
						isSelected={isFriendsTab}
						onClick={navigateTo(Routes.ME)}
						data-flx="channel.direct-message.dm-list.clickable-item.navigate-to"
					>
						<div className={styles.clickableItemInner} data-flx="channel.direct-message.dm-list.clickable-item-inner">
							<div
								className={styles.clickableItemContent}
								data-flx="channel.direct-message.dm-list.clickable-item-content"
							>
								<div className={styles.clickableItemIcon} data-flx="channel.direct-message.dm-list.clickable-item-icon">
									<UsersThreeIcon
										weight="fill"
										className={styles.iconSize5}
										data-flx="channel.direct-message.dm-list.icon-size5--4"
									/>
								</div>
								<span
									className={styles.clickableItemText}
									data-flx="channel.direct-message.dm-list.clickable-item-text"
								>
									<Trans>Friends</Trans>
								</span>
							</div>
							<MentionBadge mentionCount={pendingCount} data-flx="channel.direct-message.dm-list.mention-badge" />
						</div>
					</ClickableItem>
					{currentUserId && (
						<ClickableItem
							isSelected={shouldHighlightPersonalNotes}
							onClick={navigateTo(personalNotesPath)}
							onContextMenu={handlePersonalNotesContextMenu}
							data-flx="channel.direct-message.dm-list.clickable-item.navigate-to--2"
						>
							<div
								className={styles.clickableItemContent}
								data-flx="channel.direct-message.dm-list.clickable-item-content--2"
							>
								<div
									className={styles.clickableItemIcon}
									data-flx="channel.direct-message.dm-list.clickable-item-icon--2"
								>
									<NotePencilIcon
										weight="fill"
										className={styles.iconSize5}
										data-flx="channel.direct-message.dm-list.icon-size5--5"
									/>
								</div>
								<span
									className={styles.clickableItemText}
									data-flx="channel.direct-message.dm-list.clickable-item-text--2"
								>
									{i18n._(PERSONAL_NOTES_DESCRIPTOR)}
								</span>
							</div>
						</ClickableItem>
					)}
					{showPremiumFeatures && (
						<ClickableItem
							onClick={() => PremiumModalCommands.open()}
							data-flx="channel.direct-message.dm-list.clickable-item.open"
						>
							<div
								className={styles.clickableItemContent}
								data-flx="channel.direct-message.dm-list.clickable-item-content--3"
							>
								<div
									className={styles.clickableItemIcon}
									data-flx="channel.direct-message.dm-list.clickable-item-icon--3"
								>
									<CrownIcon
										weight="fill"
										className={styles.iconSize5}
										data-flx="channel.direct-message.dm-list.icon-size5--6"
									/>
								</div>
								<span
									className={styles.clickableItemText}
									data-flx="channel.direct-message.dm-list.clickable-item-text--3"
								>
									{PREMIUM_PRODUCT_NAME}
								</span>
							</div>
						</ClickableItem>
					)}
					<div className={styles.dmSectionSeparator} data-flx="channel.direct-message.dm-list.dm-section-separator" />
					<div className={styles.dmSectionHeader} data-flx="channel.direct-message.dm-list.dm-section-header">
						<div
							className={styles.dmSectionHeaderText}
							data-flx="channel.direct-message.dm-list.dm-section-header-text"
						>
							<span
								className={styles.dmSectionHeaderLabel}
								data-flx="channel.direct-message.dm-list.dm-section-header-label"
							>
								<Trans>Direct messages</Trans>
							</span>
						</div>
						<Tooltip
							text={
								createDmTooltipText ??
								(() => (
									<TooltipWithKeybind
										label={i18n._(CREATE_DM_DESCRIPTOR)}
										action="chat_new_dm"
										data-flx="channel.direct-message.dm-list.tooltip-with-keybind"
									/>
								))
							}
							position="top"
							data-flx="channel.direct-message.dm-list.tooltip"
						>
							<span
								className={styles.dmCreateButtonTooltipTarget}
								data-flx="channel.direct-message.direct-message-list.dm-list.dm-create-button-tooltip-target"
							>
								<FocusRing offset={-2} data-flx="channel.direct-message.dm-list.focus-ring--7">
									<button
										type="button"
										className={styles.dmCreateButton}
										onClick={handleOpenCreateDMModal}
										disabled={Boolean(createDmRestriction)}
										aria-label={i18n._(CREATE_DM_DESCRIPTOR)}
										data-flx="channel.direct-message.dm-list.dm-create-button.open-create-dm-modal"
									>
										<PlusIcon
											weight="bold"
											className={styles.iconSize4}
											data-flx="channel.direct-message.dm-list.icon-size4--2"
										/>
									</button>
								</FocusRing>
							</span>
						</Tooltip>
					</div>
					<div
						className={styles.dmChannelList}
						ref={dmListNavigationRef}
						role="list"
						data-flx="channel.direct-message.dm-list.dm-channel-list"
					>
						{filteredDmChannels.map((channel) => {
							const isSelected = highlightedChannelId === channel.id;
							return (
								<DMListItem
									key={channel.id}
									channel={channel}
									isSelected={isSelected}
									data-flx="channel.direct-message.dm-list.dm-list-item--2"
								/>
							);
						})}
					</div>
					<div className={styles.desktopBottomSpacer} data-flx="channel.direct-message.dm-list.bottom-spacer" />
				</div>
			</Scroller>
		</div>
	);
});
