// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Accessibility from '@app/features/accessibility/state/Accessibility';
import {MuteDurationSheet} from '@app/features/app/components/bottomsheets/MuteDurationSheet';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {ChannelPinsContent} from '@app/features/app/components/shared/ChannelPinsContent';
import Authentication from '@app/features/auth/state/Authentication';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {
	showChannelErrorModal,
	showChannelErrorModalAfterCurrentModal,
} from '@app/features/channel/components/alerts/ChannelErrorModalUtils';
import {DMCloseFailedModal} from '@app/features/channel/components/alerts/DMCloseFailedModal';
import styles from '@app/features/channel/components/bottomsheets/ChannelDetailsBottomSheet.module.css';
import {GuildMemberList} from '@app/features/channel/components/bottomsheets/ChannelDetailsBottomSheetMemberList';
import type {
	ChannelDetailsBottomSheetProps,
	ChannelDetailsTab,
} from '@app/features/channel/components/bottomsheets/ChannelDetailsBottomSheetTypes';
import {ChannelSearchBottomSheet} from '@app/features/channel/components/bottomsheets/ChannelSearchBottomSheet';
import {
	ARE_YOU_SURE_YOU_WANT_TO_CLOSE_YOUR_DESCRIPTOR,
	ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR,
	CHANNEL_DESCRIPTOR,
	CHANNEL_DETAILS_SECTIONS_DESCRIPTOR,
	CHANNEL_ID_COPIED_TO_CLIPBOARD_DESCRIPTOR,
	CHANNEL_SETTINGS_DESCRIPTOR,
	DELETE_DESCRIPTOR,
	DIRECT_MESSAGE_DESCRIPTOR,
	DM_CLOSED_DESCRIPTOR,
	DM_SETTINGS_DESCRIPTOR,
	FAILED_TO_DELETE_CHANNEL_DESCRIPTOR,
	FAILED_TO_PIN_DM_DESCRIPTOR,
	FAILED_TO_PIN_GROUP_DESCRIPTOR,
	FAILED_TO_UNPIN_DM_DESCRIPTOR,
	FAILED_TO_UNPIN_GROUP_DESCRIPTOR,
	GROUP_DIRECT_MESSAGE_DESCRIPTOR,
	GROUP_SETTINGS_DESCRIPTOR,
	logger,
	MARKED_AS_READ_DESCRIPTOR,
	MORE_DESCRIPTOR,
	MUTE_DESCRIPTOR,
	PINNED_GROUP_DESCRIPTOR,
	SEARCH_DESCRIPTOR,
	THIS_CHANNEL_DESCRIPTOR,
	UNMUTE_DESCRIPTOR,
	UNPINNED_GROUP_DESCRIPTOR,
	USER_ID_COPIED_TO_CLIPBOARD_DESCRIPTOR,
} from '@app/features/channel/components/bottomsheets/channel_details_bottom_sheet/ChannelDetailsBottomSheetShared';
import {ChannelInfoHeader} from '@app/features/channel/components/bottomsheets/channel_details_bottom_sheet/ChannelInfoHeader';
import {DMMembersList} from '@app/features/channel/components/bottomsheets/channel_details_bottom_sheet/DMMembersList';
import {MoreOptionsSheet} from '@app/features/channel/components/bottomsheets/channel_details_bottom_sheet/MoreOptionsSheet';
import {NotificationSettingsSheet} from '@app/features/channel/components/bottomsheets/channel_details_bottom_sheet/NotificationSettingsSheet';
import {QuickActionButton} from '@app/features/channel/components/bottomsheets/channel_details_bottom_sheet/QuickActionButton';
import {createMuteConfig} from '@app/features/channel/components/MuteOptions';
import {ChannelSettingsModal} from '@app/features/channel/components/modals/ChannelSettingsModal';
import {CreateDMModal} from '@app/features/channel/components/modals/CreateDMModal';
import {EditGroupModal} from '@app/features/channel/components/modals/EditGroupModal';
import {GroupInvitesModal} from '@app/features/channel/components/modals/GroupInvitesModal';
import {useDeleteMyMessagesInChannel} from '@app/features/channel/hooks/useDeleteMyMessagesInChannel';
import {
	CLOSE_DM_DESCRIPTOR,
	DELETE_CHANNEL_DESCRIPTOR,
	MUTE_CHANNEL_DESCRIPTOR,
	MUTE_CONVERSATION_DESCRIPTOR,
	UNMUTE_CHANNEL_DESCRIPTOR,
	UNMUTE_CONVERSATION_DESCRIPTOR,
} from '@app/features/channel/utils/ChannelMessageDescriptors';
import {ChannelDebugModal} from '@app/features/devtools/components/debug/ChannelDebugModal';
import {UserDebugModal} from '@app/features/devtools/components/debug/UserDebugModal';
import {GuildNotificationSettingsModal} from '@app/features/guild/components/modals/GuildNotificationSettingsModal';
import {GuildMemberActionsSheet} from '@app/features/guild/components/modals/guild_tabs/GuildMemberActionsSheet';
import {useLeaveGroup} from '@app/features/guild/hooks/useLeaveGroup';
import Guilds from '@app/features/guild/state/Guilds';
import {
	ADDED_TO_FAVORITES_TOAST_DESCRIPTOR,
	CHANNEL_DELETED_DESCRIPTOR,
	LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR,
	PERSONAL_NOTES_DESCRIPTOR,
	PINNED_DM_DESCRIPTOR,
	REMOVED_FROM_FAVORITES_TOAST_DESCRIPTOR,
	TEXT_CHANNEL_DESCRIPTOR,
	TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR,
	UNPINNED_DM_DESCRIPTOR,
	VOICE_CHANNEL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {InviteModal} from '@app/features/invite/components/modals/InviteModal';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import * as MemberListUtils from '@app/features/member/utils/MemberListUtils';
import Favorites from '@app/features/messaging/state/Favorites';
import {buildChannelLink} from '@app/features/messaging/utils/MessageLinkUtils';
import SelectedChannel from '@app/features/navigation/state/SelectedChannel';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import {
	MembersIcon,
	MoreOptionsVerticalIcon,
	MuteIcon,
	PinIcon,
	SearchIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Scroller} from '@app/features/ui/components/Scroller';
import * as Sheet from '@app/features/ui/sheet/Sheet';
import {getNextTabIndex, getTabNavigationDirection} from '@app/features/ui/tabs/TabKeyboardNavigation';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import type {User} from '@app/features/user/models/User';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {getMutedText} from '@app/lib/overlay/OverlayContextMenu';
import {ME} from '@fluxer/constants/src/AppConstants';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {MessageNotifications} from '@fluxer/constants/src/NotificationConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const CHANNEL_DETAILS_TAB_ORDER: ReadonlyArray<ChannelDetailsTab> = ['members', 'pins'];
export const ChannelDetailsBottomSheet: React.FC<ChannelDetailsBottomSheetProps> = observer(
	({isOpen, onClose, channel, initialTab = 'members'}) => {
		const {i18n} = useLingui();
		const [activeTab, setActiveTab] = useState<ChannelDetailsTab>(initialTab);
		const [muteSheetOpen, setMuteSheetOpen] = useState(false);
		const [searchSheetOpen, setSearchSheetOpen] = useState(false);
		const [moreOptionsSheetOpen, setMoreOptionsSheetOpen] = useState(false);
		const [notificationSheetOpen, setNotificationSheetOpen] = useState(false);
		const [activeMemberSheet, setActiveMemberSheet] = useState<{member: GuildMember; user: User} | null>(null);
		const leaveGroup = useLeaveGroup();
		const deleteMyMessagesInChannel = useDeleteMyMessagesInChannel();
		useEffect(() => {
			setActiveTab(initialTab);
		}, [initialTab]);
		const isDM = channel.type === ChannelTypes.DM;
		const isPersonalNotes = channel.type === ChannelTypes.DM_PERSONAL_NOTES;
		const isGuildChannel = channel.guildId != null;
		const guild = isGuildChannel ? Guilds.getGuild(channel.guildId) : null;
		const recipient = isDM && channel.recipientIds.length > 0 ? Users.getUser(channel.recipientIds[0]) : null;
		const currentUser = Users.currentUser;
		const currentUserId = Authentication.currentUserId;
		const guildId = channel.guildId ?? null;
		const settingsGuildId = isGuildChannel ? channel.guildId : null;
		const channelOverride = UserGuildSettings.getChannelOverride(settingsGuildId, channel.id);
		const isMuted = channelOverride?.muted ?? false;
		const muteConfig = channelOverride?.mute_config;
		const mutedText = getMutedText(isMuted, muteConfig);
		const isGroupDMOwner = channel.type === ChannelTypes.GROUP_DM && channel.ownerId === currentUserId;
		const channelTypeLabel = useMemo(() => {
			switch (channel.type) {
				case ChannelTypes.GUILD_TEXT:
					return i18n._(TEXT_CHANNEL_DESCRIPTOR);
				case ChannelTypes.GUILD_VOICE:
					return i18n._(VOICE_CHANNEL_DESCRIPTOR);
				case ChannelTypes.DM:
					return i18n._(DIRECT_MESSAGE_DESCRIPTOR);
				case ChannelTypes.DM_PERSONAL_NOTES:
					return i18n._(PERSONAL_NOTES_DESCRIPTOR);
				case ChannelTypes.GROUP_DM:
					return i18n._(GROUP_DIRECT_MESSAGE_DESCRIPTOR);
				default:
					return i18n._(CHANNEL_DESCRIPTOR);
			}
		}, [channel.type, i18n.locale]);
		const isFavorited = !!Favorites.getChannel(channel.id);
		const showFavorites = Accessibility.showFavorites;
		const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
		const membersTabId = `channel-details-${channel.id}-members-tab`;
		const pinsTabId = `channel-details-${channel.id}-pins-tab`;
		const membersPanelId = `channel-details-${channel.id}-members-panel`;
		const pinsPanelId = `channel-details-${channel.id}-pins-panel`;
		const developerMode = UserSettings.developerMode;
		const moreOptionsTitle = (() => {
			if (isGroupDM) return i18n._(GROUP_SETTINGS_DESCRIPTOR);
			if (isDM) return i18n._(DM_SETTINGS_DESCRIPTOR);
			return i18n._(CHANNEL_SETTINGS_DESCRIPTOR);
		})();
		const closeMoreOptions = useCallback(() => setMoreOptionsSheetOpen(false), []);
		const closeNotificationSheet = useCallback(() => setNotificationSheetOpen(false), []);
		const handleTabKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLButtonElement>) => {
				const direction = getTabNavigationDirection(event.key, 'horizontal');
				if (!direction) return;
				const nextIndex = getNextTabIndex(
					CHANNEL_DETAILS_TAB_ORDER.indexOf(activeTab),
					CHANNEL_DETAILS_TAB_ORDER.length,
					direction,
				);
				const nextTab = nextIndex == null ? null : (CHANNEL_DETAILS_TAB_ORDER[nextIndex] ?? null);
				if (!nextTab) return;
				event.preventDefault();
				event.stopPropagation();
				setActiveTab(nextTab);
				const targetId = nextTab === 'members' ? membersTabId : pinsTabId;
				requestAnimationFrame(() => document.getElementById(targetId)?.focus());
			},
			[activeTab, membersTabId, pinsTabId],
		);
		const handleMarkAsRead = useCallback(() => {
			ReadStateCommands.ack(channel.id, true, true);
			ToastCommands.createToast({type: 'success', children: i18n._(MARKED_AS_READ_DESCRIPTOR)});
		}, [channel.id, i18n]);
		const handleInvite = useCallback(() => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<InviteModal
						channelId={channel.id}
						data-flx="channel.channel-details-bottom-sheet.handle-invite.invite-modal"
					/>
				)),
			);
		}, [channel.id, onClose]);
		const handleCopyLink = useCallback(() => {
			const channelLink = buildChannelLink({guildId: channel.guildId, channelId: channel.id});
			TextCopyCommands.copy(i18n, channelLink);
			ToastCommands.createToast({type: 'success', children: i18n._(LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR)});
		}, [channel.id, channel.guildId, i18n]);
		const handleCopyId = useCallback(() => {
			TextCopyCommands.copy(i18n, channel.id);
			ToastCommands.createToast({type: 'success', children: i18n._(CHANNEL_ID_COPIED_TO_CLIPBOARD_DESCRIPTOR)});
		}, [channel.id, i18n]);
		const handleToggleFavorite = useCallback(() => {
			if (isFavorited) {
				Favorites.removeChannel(channel.id);
				ToastCommands.createToast({type: 'success', children: i18n._(REMOVED_FROM_FAVORITES_TOAST_DESCRIPTOR)});
			} else {
				Favorites.addChannel(channel.id, channel.guildId ?? ME, null);
				ToastCommands.createToast({type: 'success', children: i18n._(ADDED_TO_FAVORITES_TOAST_DESCRIPTOR)});
			}
		}, [channel.id, channel.guildId, isFavorited, i18n]);
		const handleDebugChannel = useCallback(() => {
			const channelName = channel.name ?? i18n._(CHANNEL_DESCRIPTOR);
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<ChannelDebugModal
						title={channelName}
						channel={channel}
						data-flx="channel.channel-details-bottom-sheet.handle-debug-channel.channel-debug-modal"
					/>
				)),
			);
		}, [channel, i18n, onClose]);
		const handleDebugUser = useCallback(() => {
			if (!recipient) return;
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<UserDebugModal
						title={NicknameUtils.getDisplayName(recipient)}
						user={recipient}
						data-flx="channel.channel-details-bottom-sheet.handle-debug-user.user-debug-modal"
					/>
				)),
			);
		}, [recipient, onClose]);
		const handlePinDM = useCallback(async () => {
			closeMoreOptions();
			try {
				await PrivateChannelCommands.pinDmChannel(channel.id);
				ToastCommands.createToast({
					type: 'success',
					children: isGroupDM ? i18n._(PINNED_GROUP_DESCRIPTOR) : i18n._(PINNED_DM_DESCRIPTOR),
				});
			} catch (error) {
				logger.error('Failed to pin:', error);
				showChannelErrorModal({
					title: isGroupDM ? i18n._(FAILED_TO_PIN_GROUP_DESCRIPTOR) : i18n._(FAILED_TO_PIN_DM_DESCRIPTOR),
					message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
					dataFlx: 'channel.channel-details-bottom-sheet.pin-dm-failed.generic-error-modal',
				});
			}
		}, [channel.id, isGroupDM, closeMoreOptions, i18n]);
		const handleUnpinDM = useCallback(async () => {
			closeMoreOptions();
			try {
				await PrivateChannelCommands.unpinDmChannel(channel.id);
				ToastCommands.createToast({
					type: 'success',
					children: isGroupDM ? i18n._(UNPINNED_GROUP_DESCRIPTOR) : i18n._(UNPINNED_DM_DESCRIPTOR),
				});
			} catch (error) {
				logger.error('Failed to unpin:', error);
				showChannelErrorModal({
					title: isGroupDM ? i18n._(FAILED_TO_UNPIN_GROUP_DESCRIPTOR) : i18n._(FAILED_TO_UNPIN_DM_DESCRIPTOR),
					message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
					dataFlx: 'channel.channel-details-bottom-sheet.unpin-dm-failed.generic-error-modal',
				});
			}
		}, [channel.id, isGroupDM, closeMoreOptions, i18n]);
		const handleCloseDM = useCallback(() => {
			ModalCommands.pushAfterBottomSheetClose(
				() => {
					closeMoreOptions();
					onClose();
				},
				modal(() => (
					<ConfirmModal
						title={i18n._(CLOSE_DM_DESCRIPTOR)}
						description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_CLOSE_YOUR_DESCRIPTOR, {
							recipientUsername: recipient ? NicknameUtils.getNickname(recipient, null) : '',
						})}
						primaryText={i18n._(CLOSE_DM_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={async () => {
							try {
								await ChannelCommands.remove(channel.id);
								const selectedChannel = SelectedChannel.selectedChannelIds.get(ME);
								if (selectedChannel === channel.id) {
									RouterUtils.transitionTo(Routes.ME);
								}
								ToastCommands.createToast({type: 'success', children: i18n._(DM_CLOSED_DESCRIPTOR)});
							} catch (error) {
								logger.error('Failed to close DM:', error);
								window.setTimeout(() => {
									ModalCommands.push(
										modal(() => (
											<DMCloseFailedModal data-flx="channel.channel-details-bottom-sheet.handle-close-dm.dm-close-failed-modal" />
										)),
									);
								}, 0);
							}
						}}
						data-flx="channel.channel-details-bottom-sheet.handle-close-dm.confirm-modal"
					/>
				)),
			);
		}, [channel.id, i18n, recipient, onClose, closeMoreOptions]);
		const handleLeaveGroup = useCallback(() => {
			ModalCommands.runAfterBottomSheetClose(
				() => {
					closeMoreOptions();
					onClose();
				},
				() => leaveGroup(channel.id),
			);
		}, [channel.id, onClose, leaveGroup, closeMoreOptions]);
		const handleDeleteMyMessagesInChannel = useCallback(() => {
			ModalCommands.runAfterBottomSheetClose(
				() => {
					closeMoreOptions();
					onClose();
				},
				() => deleteMyMessagesInChannel(channel.id),
			);
		}, [channel.id, deleteMyMessagesInChannel, onClose, closeMoreOptions]);
		const handleEditGroup = useCallback(() => {
			ModalCommands.pushAfterBottomSheetClose(
				() => {
					closeMoreOptions();
					onClose();
				},
				modal(() => (
					<EditGroupModal
						channelId={channel.id}
						data-flx="channel.channel-details-bottom-sheet.handle-edit-group.edit-group-modal"
					/>
				)),
			);
		}, [channel.id, onClose, closeMoreOptions]);
		const handleShowInvites = useCallback(() => {
			ModalCommands.pushAfterBottomSheetClose(
				() => {
					closeMoreOptions();
					onClose();
				},
				modal(() => (
					<GroupInvitesModal
						channelId={channel.id}
						data-flx="channel.channel-details-bottom-sheet.handle-show-invites.group-invites-modal"
					/>
				)),
			);
		}, [channel.id, onClose, closeMoreOptions]);
		const handleCopyUserId = useCallback(() => {
			if (!recipient) return;
			TextCopyCommands.copy(i18n, recipient.id);
			ToastCommands.createToast({type: 'success', children: i18n._(USER_ID_COPIED_TO_CLIPBOARD_DESCRIPTOR)});
		}, [recipient, i18n]);
		const handleEditChannel = useCallback(() => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<ChannelSettingsModal
						channelId={channel.id}
						data-flx="channel.channel-details-bottom-sheet.handle-edit-channel.channel-settings-modal"
					/>
				)),
			);
		}, [channel.id, onClose]);
		const handleDeleteChannel = useCallback(() => {
			const channelType =
				channel.type === ChannelTypes.GUILD_VOICE ? i18n._(VOICE_CHANNEL_DESCRIPTOR) : i18n._(TEXT_CHANNEL_DESCRIPTOR);
			const channelLabel = channel.name ? `#${channel.name}` : i18n._(THIS_CHANNEL_DESCRIPTOR);
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<ConfirmModal
						title={i18n._(DELETE_DESCRIPTOR, {channelType})}
						description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR, {channelLabel})}
						primaryText={i18n._(DELETE_CHANNEL_DESCRIPTOR)}
						primaryVariant="danger"
						onPrimary={async () => {
							try {
								await ChannelCommands.remove(channel.id);
								ToastCommands.createToast({type: 'success', children: i18n._(CHANNEL_DELETED_DESCRIPTOR)});
							} catch (error) {
								logger.error('Failed to delete channel:', error);
								showChannelErrorModalAfterCurrentModal({
									title: i18n._(FAILED_TO_DELETE_CHANNEL_DESCRIPTOR),
									message: i18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
									dataFlx: 'channel.channel-details-bottom-sheet.delete-channel-failed.generic-error-modal',
								});
							}
						}}
						data-flx="channel.channel-details-bottom-sheet.handle-delete-channel.confirm-modal"
					/>
				)),
			);
		}, [channel.id, channel.name, channel.type, i18n, onClose]);
		const handleOpenGuildNotificationSettings = useCallback(() => {
			if (!guildId) return;
			ModalCommands.push(
				modal(() => (
					<GuildNotificationSettingsModal
						guildId={guildId}
						data-flx="channel.channel-details-bottom-sheet.handle-open-guild-notification-settings.guild-notification-settings-modal"
					/>
				)),
			);
		}, [guildId]);
		const handleOpenCreateGroupModal = useCallback(() => {
			const duplicateExcludeChannelId = channel.type === ChannelTypes.GROUP_DM ? channel.id : undefined;
			ModalCommands.push(
				modal(() => (
					<CreateDMModal
						initialSelectedUserIds={Array.from(channel.recipientIds)}
						duplicateExcludeChannelId={duplicateExcludeChannelId}
						data-flx="channel.channel-details-bottom-sheet.handle-open-create-group-modal.create-dm-modal"
					/>
				)),
			);
		}, [channel.id, channel.recipientIds, channel.type]);
		const handleNotificationLevelChange = useCallback(
			(level: number) => {
				if (!guildId) return;
				if (level === MessageNotifications.INHERIT) {
					UserGuildSettingsCommands.updateChannelOverride(
						guildId,
						channel.id,
						{message_notifications: MessageNotifications.INHERIT},
						{persistImmediately: true},
					);
				} else {
					UserGuildSettingsCommands.updateMessageNotifications(guildId, level, channel.id, {
						persistImmediately: true,
					});
				}
			},
			[guildId, channel.id],
		);
		const handleMute = useCallback(
			(duration: number | null) => {
				UserGuildSettingsCommands.updateChannelOverride(
					settingsGuildId,
					channel.id,
					{muted: true, mute_config: createMuteConfig(duration)},
					{persistImmediately: true},
				);
				setMuteSheetOpen(false);
			},
			[settingsGuildId, channel.id],
		);
		const handleUnmute = useCallback(() => {
			UserGuildSettingsCommands.updateChannelOverride(
				settingsGuildId,
				channel.id,
				{muted: false, mute_config: null},
				{persistImmediately: true},
			);
			setMuteSheetOpen(false);
		}, [settingsGuildId, channel.id]);
		const handleMemberLongPress = useCallback((member: GuildMember) => {
			setActiveMemberSheet({member, user: member.user});
		}, []);
		const handleCloseMemberSheet = useCallback(() => {
			setActiveMemberSheet(null);
		}, []);
		const handleOpenNotificationSheet = useCallback(() => setNotificationSheetOpen(true), []);
		const isMemberTabVisible = isOpen && activeTab === 'members';
		const dmMemberGroups = (() => {
			if (!(isDM || isGroupDM || isPersonalNotes)) return [];
			let memberIds: Array<string> = [];
			if (isPersonalNotes) {
				memberIds = currentUser ? [currentUser.id] : [];
			} else {
				memberIds = [...channel.recipientIds];
				if (currentUserId && !memberIds.includes(currentUserId)) {
					memberIds.push(currentUserId);
				}
			}
			const users = memberIds.map((id) => Users.getUser(id)).filter((u): u is User => u != null);
			return MemberListUtils.getGroupDMMemberGroups(users);
		})();
		return (
			<>
				<Sheet.Root
					isOpen={isOpen}
					onClose={onClose}
					snapPoints={[0, 1]}
					initialSnap={1}
					data-flx="channel.channel-details-bottom-sheet.sheet-root"
				>
					<Sheet.Handle data-flx="channel.channel-details-bottom-sheet.sheet-handle" />
					<Sheet.Content padding="none" data-flx="channel.channel-details-bottom-sheet.sheet-content">
						<Scroller
							key="channel-details-scroller"
							className={styles.mainScroller}
							data-flx="channel.channel-details-bottom-sheet.main-scroller"
						>
							<ChannelInfoHeader
								channel={channel}
								currentUser={currentUser}
								recipient={recipient}
								channelTypeLabel={channelTypeLabel}
								onClose={onClose}
								data-flx="channel.channel-details-bottom-sheet.channel-info-header"
							/>
							<div className={styles.quickActionsRow} data-flx="channel.channel-details-bottom-sheet.quick-actions-row">
								<div
									className={styles.quickActionsScroll}
									data-flx="channel.channel-details-bottom-sheet.quick-actions-scroll"
								>
									<QuickActionButton
										icon={<MuteIcon size={20} data-flx="channel.channel-details-bottom-sheet.mute-icon" />}
										label={isMuted ? i18n._(UNMUTE_DESCRIPTOR) : i18n._(MUTE_DESCRIPTOR)}
										onClick={() => setMuteSheetOpen(true)}
										isActive={isMuted}
										data-flx="channel.channel-details-bottom-sheet.quick-action-button.bell-click"
									/>
									<QuickActionButton
										icon={<SearchIcon size={20} data-flx="channel.channel-details-bottom-sheet.search-icon" />}
										label={i18n._(SEARCH_DESCRIPTOR)}
										onClick={() => setSearchSheetOpen(true)}
										data-flx="channel.channel-details-bottom-sheet.quick-action-button.search-click"
									/>
									<QuickActionButton
										icon={
											<MoreOptionsVerticalIcon
												size={20}
												data-flx="channel.channel-details-bottom-sheet.more-options-vertical-icon"
											/>
										}
										label={i18n._(MORE_DESCRIPTOR)}
										onClick={() => setMoreOptionsSheetOpen(true)}
										data-flx="channel.channel-details-bottom-sheet.quick-action-button.cog-click"
									/>
								</div>
							</div>
							<div
								className={styles.tabBarContainer}
								role="tablist"
								aria-label={i18n._(CHANNEL_DETAILS_SECTIONS_DESCRIPTOR)}
								data-flx="channel.channel-details-bottom-sheet.tab-bar-container"
							>
								<button
									id={membersTabId}
									type="button"
									role="tab"
									aria-selected={activeTab === 'members'}
									aria-controls={membersPanelId}
									tabIndex={activeTab === 'members' ? 0 : -1}
									onClick={() => setActiveTab('members')}
									onKeyDown={handleTabKeyDown}
									className={`${styles.tabButton} ${activeTab === 'members' ? styles.tabButtonActive : styles.tabButtonInactive}`}
									style={activeTab === 'members' ? {borderBottomColor: 'var(--brand-primary-light)'} : undefined}
									data-flx="channel.channel-details-bottom-sheet.tab-button.set-active-tab"
								>
									<MembersIcon
										className={styles.tabIcon}
										aria-hidden="true"
										data-flx="channel.channel-details-bottom-sheet.tab-icon"
									/>
									<Trans>Members</Trans>
								</button>
								<button
									id={pinsTabId}
									type="button"
									role="tab"
									aria-selected={activeTab === 'pins'}
									aria-controls={pinsPanelId}
									tabIndex={activeTab === 'pins' ? 0 : -1}
									onClick={() => setActiveTab('pins')}
									onKeyDown={handleTabKeyDown}
									className={`${styles.tabButton} ${activeTab === 'pins' ? styles.tabButtonActive : styles.tabButtonInactive}`}
									style={activeTab === 'pins' ? {borderBottomColor: 'var(--brand-primary-light)'} : undefined}
									data-flx="channel.channel-details-bottom-sheet.tab-button.set-active-tab--2"
								>
									<PinIcon
										className={styles.tabIcon}
										aria-hidden="true"
										data-flx="channel.channel-details-bottom-sheet.tab-icon--2"
									/>
									<Trans>Pins</Trans>
								</button>
							</div>
							<div className={styles.contentArea} data-flx="channel.channel-details-bottom-sheet.content-area">
								{activeTab === 'members' && (
									<div
										id={membersPanelId}
										className={styles.membersTabContent}
										role="tabpanel"
										aria-labelledby={membersTabId}
										data-flx="channel.channel-details-bottom-sheet.members-tab-content"
									>
										{(isDM || isGroupDM || isPersonalNotes) && (
											<DMMembersList
												channel={channel}
												currentUser={currentUser}
												recipient={recipient}
												members={dmMemberGroups}
												onOpenCreateGroupModal={handleOpenCreateGroupModal}
												data-flx="channel.channel-details-bottom-sheet.dm-members-list"
											/>
										)}
										{isGuildChannel && guild && (
											<GuildMemberList
												guild={guild}
												channel={channel}
												onMemberLongPress={handleMemberLongPress}
												enabled={isMemberTabVisible}
												data-flx="channel.channel-details-bottom-sheet.guild-member-list"
											/>
										)}
									</div>
								)}
								{activeTab === 'pins' && (
									<div
										id={pinsPanelId}
										className={styles.pinsTabContent}
										role="tabpanel"
										aria-labelledby={pinsTabId}
										data-flx="channel.channel-details-bottom-sheet.pins-tab-content"
									>
										<ChannelPinsContent
											channel={channel}
											onJump={onClose}
											data-flx="channel.channel-details-bottom-sheet.channel-pins-content"
										/>
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
					muteTitle={i18n._(isGuildChannel ? MUTE_CHANNEL_DESCRIPTOR : MUTE_CONVERSATION_DESCRIPTOR)}
					unmuteTitle={i18n._(isGuildChannel ? UNMUTE_CHANNEL_DESCRIPTOR : UNMUTE_CONVERSATION_DESCRIPTOR)}
					onMute={handleMute}
					onUnmute={handleUnmute}
					data-flx="channel.channel-details-bottom-sheet.mute-duration-sheet"
				/>
				<MoreOptionsSheet
					isOpen={moreOptionsSheetOpen}
					onClose={closeMoreOptions}
					title={moreOptionsTitle}
					channel={channel}
					recipient={recipient}
					showFavorites={showFavorites}
					isFavorited={isFavorited}
					isPersonalNotes={isPersonalNotes}
					isDM={isDM}
					isGroupDM={isGroupDM}
					isGuildChannel={isGuildChannel}
					isGroupDMOwner={isGroupDMOwner}
					developerMode={developerMode}
					onToggleFavorite={handleToggleFavorite}
					onMarkAsRead={handleMarkAsRead}
					onPinDM={handlePinDM}
					onUnpinDM={handleUnpinDM}
					onInvite={handleInvite}
					onCopyLink={handleCopyLink}
					onOpenNotificationSheet={handleOpenNotificationSheet}
					onEditGroup={handleEditGroup}
					onShowInvites={handleShowInvites}
					onEditChannel={handleEditChannel}
					onDeleteChannel={handleDeleteChannel}
					onDeleteMyMessages={handleDeleteMyMessagesInChannel}
					onCloseDM={handleCloseDM}
					onLeaveGroup={handleLeaveGroup}
					onDebugChannel={handleDebugChannel}
					onDebugUser={handleDebugUser}
					onCopyUserId={handleCopyUserId}
					onCopyChannelId={handleCopyId}
					data-flx="channel.channel-details-bottom-sheet.more-options-sheet"
				/>
				<NotificationSettingsSheet
					isOpen={notificationSheetOpen}
					onClose={closeNotificationSheet}
					channel={channel}
					guildId={guildId}
					onChangeLevel={handleNotificationLevelChange}
					onOpenGuildSettings={handleOpenGuildNotificationSettings}
					data-flx="channel.channel-details-bottom-sheet.notification-settings-sheet"
				/>
				<ChannelSearchBottomSheet
					isOpen={searchSheetOpen}
					onClose={() => setSearchSheetOpen(false)}
					channel={channel}
					data-flx="channel.channel-details-bottom-sheet.channel-search-bottom-sheet"
				/>
				{activeMemberSheet && guildId && (
					<GuildMemberActionsSheet
						isOpen={true}
						onClose={handleCloseMemberSheet}
						user={activeMemberSheet.user}
						member={activeMemberSheet.member}
						guildId={guildId}
						data-flx="channel.channel-details-bottom-sheet.guild-member-actions-sheet"
					/>
				)}
			</>
		);
	},
);
