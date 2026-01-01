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
	BellSlashIcon,
	BookOpenIcon,
	CheckIcon,
	CopyIcon,
	IdentificationCardIcon,
	NoteIcon,
	PencilIcon,
	PhoneIcon,
	ProhibitIcon,
	PushPinIcon,
	SignOutIcon,
	StarIcon,
	TicketIcon,
	UserCircleIcon,
	UserMinusIcon,
	UserPlusIcon,
	UsersIcon,
	XIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import * as InviteActionCreators from '~/actions/InviteActionCreators';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as ReadStateActionCreators from '~/actions/ReadStateActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {ChannelTypes, ME, RelationshipTypes} from '~/Constants';
import {DMCloseFailedModal} from '~/components/alerts/DMCloseFailedModal';
import {GroupLeaveFailedModal} from '~/components/alerts/GroupLeaveFailedModal';
import {ChangeFriendNicknameModal} from '~/components/modals/ChangeFriendNicknameModal';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {EditGroupModal} from '~/components/modals/EditGroupModal';
import {GroupInvitesModal} from '~/components/modals/GroupInvitesModal';
import type {MenuGroupType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {MenuBottomSheet} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import * as Sheet from '~/components/uikit/Sheet/Sheet';
import {Routes} from '~/Routes';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import type {UserRecord} from '~/records/UserRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import AuthenticationStore from '~/stores/AuthenticationStore';
import ChannelStore from '~/stores/ChannelStore';
import FavoritesStore from '~/stores/FavoritesStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import ReadStateStore from '~/stores/ReadStateStore';
import RelationshipStore from '~/stores/RelationshipStore';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import UserProfileMobileStore from '~/stores/UserProfileMobileStore';
import * as CallUtils from '~/utils/CallUtils';
import {getMutedText} from '~/utils/ContextMenuUtils';
import * as InviteUtils from '~/utils/InviteUtils';
import * as RelationshipActionUtils from '~/utils/RelationshipActionUtils';
import * as RouterUtils from '~/utils/RouterUtils';
import {fromTimestamp} from '~/utils/SnowflakeUtils';
import sharedStyles from './shared.module.css';

interface DMBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channel: ChannelRecord;
	recipient?: UserRecord | null;
}

export const DMBottomSheet: React.FC<DMBottomSheetProps> = observer(({isOpen, onClose, channel, recipient}) => {
	const {t, i18n} = useLingui();
	const [muteSheetOpen, setMuteSheetOpen] = React.useState(false);

	const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
	const currentUserId = AuthenticationStore.currentUserId;
	const isOwner = isGroupDM && channel.ownerId === currentUserId;

	const channelOverride = UserGuildSettingsStore.getChannelOverride(null, channel.id);
	const isMuted = channelOverride?.muted ?? false;
	const muteConfig = channelOverride?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);

	const readState = ReadStateStore.get(channel.id);
	const hasUnread = () => readState?.hasUnread?.() ?? false;

	const isFavorited = !!FavoritesStore.getChannel(channel.id);
	const isRecipientBot = recipient?.bot;
	const relationship = recipient ? RelationshipStore.getRelationship(recipient.id) : null;
	const relationshipType = relationship?.type;

	const handleMarkAsRead = () => {
		ReadStateActionCreators.ack(channel.id, true, true);
		onClose();
	};

	const handleToggleFavorite = () => {
		onClose();
		if (isFavorited) {
			FavoritesStore.removeChannel(channel.id);
			ToastActionCreators.createToast({type: 'success', children: t`Removed from Favorites`});
		} else {
			FavoritesStore.addChannel(channel.id, ME, null);
			ToastActionCreators.createToast({type: 'success', children: t`Added to Favorites`});
		}
	};

	const handleOpenMuteSheet = () => {
		setMuteSheetOpen(true);
	};

	const handleCloseMuteSheet = () => {
		setMuteSheetOpen(false);
	};

	const handlePinDM = async () => {
		onClose();
		try {
			await PrivateChannelActionCreators.pinDmChannel(channel.id);
			ToastActionCreators.createToast({
				type: 'success',
				children: isGroupDM ? t`Pinned group DM` : t`Pinned DM`,
			});
		} catch (error) {
			console.error('Failed to pin:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: isGroupDM ? t`Failed to pin group DM` : t`Failed to pin DM`,
			});
		}
	};

	const handleUnpinDM = async () => {
		onClose();
		try {
			await PrivateChannelActionCreators.unpinDmChannel(channel.id);
			ToastActionCreators.createToast({
				type: 'success',
				children: isGroupDM ? t`Unpinned group DM` : t`Unpinned DM`,
			});
		} catch (error) {
			console.error('Failed to unpin:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: isGroupDM ? t`Failed to unpin group DM` : t`Failed to unpin DM`,
			});
		}
	};

	const handleEditGroup = () => {
		onClose();
		ModalActionCreators.push(modal(() => <EditGroupModal channelId={channel.id} />));
	};

	const handleShowInvites = () => {
		onClose();
		ModalActionCreators.push(modal(() => <GroupInvitesModal channelId={channel.id} />));
	};

	const handleViewProfile = () => {
		if (!recipient) return;
		onClose();
		UserProfileMobileStore.open(recipient.id);
	};

	const handleStartVoiceCall = async () => {
		if (!recipient) return;
		onClose();
		try {
			const channelId = await PrivateChannelActionCreators.ensureDMChannel(recipient.id);
			await CallUtils.checkAndStartCall(channelId);
		} catch (error) {
			console.error('Failed to start voice call:', error);
		}
	};

	const handleAddNote = () => {
		if (!recipient) return;
		onClose();
		UserProfileActionCreators.openUserProfile(recipient.id, undefined, true);
	};

	const handleChangeFriendNickname = () => {
		if (!recipient) return;
		onClose();
		ModalActionCreators.push(modal(() => <ChangeFriendNicknameModal user={recipient} />));
	};

	const getInvitableCommunities = React.useCallback((): Array<{guild: GuildRecord; channelId: string}> => {
		if (!recipient) return [];
		return GuildStore.getGuilds()
			.filter((guild) => !GuildMemberStore.getMember(guild.id, recipient.id))
			.map((guild) => {
				const selectedChannelId = SelectedChannelStore.selectedChannelIds.get(guild.id);
				if (selectedChannelId) {
					const selectedChannel = ChannelStore.getChannel(selectedChannelId);
					if (selectedChannel?.guildId && InviteUtils.canInviteToChannel(selectedChannel.id, selectedChannel.guildId)) {
						return {guild, channelId: selectedChannel.id};
					}
				}

				const guildChannels = ChannelStore.getGuildChannels(guild.id);
				for (const guildChannel of guildChannels) {
					if (
						guildChannel.type === ChannelTypes.GUILD_TEXT &&
						InviteUtils.canInviteToChannel(guildChannel.id, guildChannel.guildId)
					) {
						return {guild, channelId: guildChannel.id};
					}
				}

				return null;
			})
			.filter((candidate): candidate is {guild: GuildRecord; channelId: string} => candidate !== null)
			.sort((a, b) => a.guild.name.localeCompare(b.guild.name));
	}, [recipient]);

	const handleInviteToCommunity = async (_guildId: string, channelId: string, guildName: string) => {
		if (!recipient) return;
		onClose();
		try {
			const invite = await InviteActionCreators.create(channelId);
			const inviteUrl = `${RuntimeConfigStore.inviteEndpoint}/${invite.code}`;
			const dmChannelId = await PrivateChannelActionCreators.ensureDMChannel(recipient.id);
			await MessageActionCreators.send(dmChannelId, {
				content: inviteUrl,
				nonce: fromTimestamp(Date.now()),
			});
			ToastActionCreators.createToast({
				type: 'success',
				children: t`Invite sent to ${guildName}`,
			});
		} catch (error) {
			console.error('Failed to send invite:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: t`Failed to send invite`,
			});
		}
	};

	const handleSendFriendRequest = () => {
		if (!recipient) return;
		RelationshipActionUtils.sendFriendRequest(i18n, recipient.id);
		onClose();
	};

	const handleAcceptFriendRequest = () => {
		if (!recipient) return;
		RelationshipActionUtils.acceptFriendRequest(i18n, recipient.id);
		onClose();
	};

	const handleRemoveFriend = () => {
		if (!recipient) return;
		onClose();
		RelationshipActionUtils.showRemoveFriendConfirmation(i18n, recipient);
	};

	const handleBlockUser = () => {
		if (!recipient) return;
		onClose();
		RelationshipActionUtils.showBlockUserConfirmation(i18n, recipient);
	};

	const handleUnblockUser = () => {
		if (!recipient) return;
		onClose();
		RelationshipActionUtils.unblockUser(i18n, recipient.id);
	};

	const handleCloseDM = () => {
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
	};

	const handleLeaveGroup = () => {
		if (!currentUserId) {
			onClose();
			return;
		}

		onClose();
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Leave Group`}
					description={t`Are you sure you want to leave this group? You will no longer be able to see any messages.`}
					primaryText={t`Leave Group`}
					primaryVariant="danger-primary"
					onPrimary={async () => {
						try {
							await PrivateChannelActionCreators.removeRecipient(channel.id, currentUserId);
							const selectedChannel = SelectedChannelStore.selectedChannelIds.get(ME);
							if (selectedChannel === channel.id) {
								RouterUtils.transitionTo(Routes.ME);
							}
							ToastActionCreators.createToast({
								type: 'success',
								children: t`Left group`,
							});
						} catch (error) {
							console.error('Failed to leave group:', error);
							ModalActionCreators.push(modal(() => <GroupLeaveFailedModal />));
						}
					}}
				/>
			)),
		);
	};

	const handleCopyChannelId = async () => {
		await TextCopyActionCreators.copy(i18n, channel.id, true);
		ToastActionCreators.createToast({
			type: 'success',
			children: t`Channel ID copied`,
		});
		onClose();
	};

	const handleCopyUserId = async () => {
		if (!recipient) return;
		await TextCopyActionCreators.copy(i18n, recipient.id, true);
		ToastActionCreators.createToast({
			type: 'success',
			children: t`User ID copied`,
		});
		onClose();
	};

	const menuGroups: Array<MenuGroupType> = [];

	if (hasUnread()) {
		menuGroups.push({
			items: [
				{
					icon: <BookOpenIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Mark as Read`,
					onClick: handleMarkAsRead,
				},
			],
		});
	}

	if (AccessibilityStore.showFavorites) {
		menuGroups.push({
			items: [
				{
					icon: <StarIcon weight={isFavorited ? 'fill' : 'regular'} className={sharedStyles.icon} />,
					label: isFavorited ? t`Remove from Favorites` : t`Add to Favorites`,
					onClick: handleToggleFavorite,
				},
			],
		});
	}

	if (recipient && !isGroupDM) {
		const recipientItems: MenuGroupType['items'] = [
			{
				icon: <UserCircleIcon weight="fill" className={sharedStyles.icon} />,
				label: t`View Profile`,
				onClick: handleViewProfile,
			},
		];

		if (!isRecipientBot) {
			recipientItems.push({
				icon: <PhoneIcon weight="fill" className={sharedStyles.icon} />,
				label: t`Voice Call`,
				onClick: handleStartVoiceCall,
			});
		}

		recipientItems.push({
			icon: <NoteIcon weight="fill" className={sharedStyles.icon} />,
			label: t`Add Note`,
			onClick: handleAddNote,
		});

		if (relationshipType === RelationshipTypes.FRIEND) {
			recipientItems.push({
				icon: <PencilIcon weight="fill" className={sharedStyles.icon} />,
				label: t`Change Friend Nickname`,
				onClick: handleChangeFriendNickname,
			});
		}

		menuGroups.push({items: recipientItems});
	}

	menuGroups.push({
		items: [
			{
				icon: isMuted ? (
					<BellSlashIcon weight="fill" className={sharedStyles.icon} />
				) : (
					<BellIcon weight="fill" className={sharedStyles.icon} />
				),
				label: isMuted ? t`Unmute Conversation` : t`Mute Conversation`,
				subtext: mutedText || undefined,
				onClick: handleOpenMuteSheet,
			},
		],
	});

	const groupActionsItems: MenuGroupType['items'] = [];

	if (isGroupDM) {
		groupActionsItems.push({
			icon: <PencilIcon weight="fill" className={sharedStyles.icon} />,
			label: t`Edit Group`,
			onClick: handleEditGroup,
		});

		if (isOwner) {
			groupActionsItems.push({
				icon: <TicketIcon weight="fill" className={sharedStyles.icon} />,
				label: t`Invites`,
				onClick: handleShowInvites,
			});
		}
	}

	groupActionsItems.push(
		channel.isPinned
			? {
					icon: <PushPinIcon weight="fill" className={sharedStyles.icon} />,
					label: isGroupDM ? t`Unpin Group DM` : t`Unpin DM`,
					onClick: handleUnpinDM,
				}
			: {
					icon: <PushPinIcon weight="fill" className={sharedStyles.icon} />,
					label: isGroupDM ? t`Pin Group DM` : t`Pin DM`,
					onClick: handlePinDM,
				},
	);

	menuGroups.push({items: groupActionsItems});

	if (recipient && !isGroupDM && !isRecipientBot) {
		const relationshipItems: MenuGroupType['items'] = [];

		const invitableCommunities = getInvitableCommunities();
		if (invitableCommunities.length > 0) {
			const communitiesToShow = invitableCommunities.slice(0, 5);
			for (const {guild, channelId} of communitiesToShow) {
				relationshipItems.push({
					icon: <UsersIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Invite to ${guild.name}`,
					onClick: () => handleInviteToCommunity(guild.id, channelId, guild.name),
				});
			}
		}

		if (relationshipType === RelationshipTypes.FRIEND) {
			relationshipItems.push({
				icon: <UserMinusIcon weight="fill" className={sharedStyles.icon} />,
				label: t`Remove Friend`,
				onClick: handleRemoveFriend,
				danger: true,
			});
		} else if (relationshipType === RelationshipTypes.INCOMING_REQUEST) {
			relationshipItems.push({
				icon: <UserPlusIcon weight="fill" className={sharedStyles.icon} />,
				label: t`Accept Friend Request`,
				onClick: handleAcceptFriendRequest,
			});
		} else if (
			relationshipType !== RelationshipTypes.OUTGOING_REQUEST &&
			relationshipType !== RelationshipTypes.BLOCKED
		) {
			relationshipItems.push({
				icon: <UserPlusIcon weight="fill" className={sharedStyles.icon} />,
				label: t`Add Friend`,
				onClick: handleSendFriendRequest,
			});
		}

		if (relationshipType === RelationshipTypes.BLOCKED) {
			relationshipItems.push({
				icon: <ProhibitIcon weight="fill" className={sharedStyles.icon} />,
				label: t`Unblock`,
				onClick: handleUnblockUser,
			});
		} else {
			relationshipItems.push({
				icon: <ProhibitIcon weight="fill" className={sharedStyles.icon} />,
				label: t`Block`,
				onClick: handleBlockUser,
				danger: true,
			});
		}

		menuGroups.push({items: relationshipItems});
	}

	const closeItems: MenuGroupType['items'] = [
		isGroupDM
			? {
					icon: <SignOutIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Leave Group`,
					onClick: handleLeaveGroup,
					danger: true,
				}
			: {
					icon: <XIcon weight="bold" className={sharedStyles.icon} />,
					label: t`Close DM`,
					onClick: handleCloseDM,
					danger: true,
				},
	];

	menuGroups.push({items: closeItems});

	const copyItems: MenuGroupType['items'] = [];

	if (recipient) {
		copyItems.push({
			icon: <IdentificationCardIcon weight="fill" className={sharedStyles.icon} />,
			label: t`Copy User ID`,
			onClick: handleCopyUserId,
		});
	}

	copyItems.push({
		icon: <CopyIcon weight="fill" className={sharedStyles.icon} />,
		label: t`Copy Channel ID`,
		onClick: handleCopyChannelId,
	});

	menuGroups.push({items: copyItems});

	return (
		<>
			<MenuBottomSheet isOpen={isOpen} onClose={onClose} groups={menuGroups} />

			<Sheet.Root isOpen={muteSheetOpen} onClose={handleCloseMuteSheet} snapPoints={[0, 1]} initialSnap={1}>
				<Sheet.Handle />
				<Sheet.Header trailing={<Sheet.CloseButton onClick={handleCloseMuteSheet} />}>
					<Sheet.Title>{isMuted ? t`Unmute Conversation` : t`Mute Conversation`}</Sheet.Title>
				</Sheet.Header>
				<Sheet.Content padding="none">
					<div style={{padding: '0 16px 16px'}}>
						{isMuted && mutedText ? (
							<>
								<div
									style={{
										padding: '12px 16px',
										backgroundColor: 'var(--background-secondary)',
										borderRadius: '8px',
										marginBottom: '12px',
									}}
								>
									<p style={{margin: 0, color: 'var(--text-secondary)', fontSize: '14px'}}>
										<Trans>Currently: {mutedText}</Trans>
									</p>
								</div>

								<div
									style={{
										backgroundColor: 'var(--background-secondary)',
										borderRadius: '8px',
										overflow: 'hidden',
									}}
								>
									<button
										type="button"
										onClick={() => {
											UserGuildSettingsActionCreators.updateChannelOverride(
												null,
												channel.id,
												{
													muted: false,
													mute_config: null,
												},
												{persistImmediately: true},
											);
											handleCloseMuteSheet();
											onClose();
										}}
										style={{
											width: '100%',
											padding: '14px 16px',
											display: 'flex',
											alignItems: 'center',
											justifyContent: 'space-between',
											background: 'none',
											border: 'none',
											cursor: 'pointer',
											color: 'var(--text-primary)',
											fontSize: '16px',
										}}
									>
										<span>
											<Trans>Unmute</Trans>
										</span>
									</button>
								</div>
							</>
						) : (
							<div
								style={{
									backgroundColor: 'var(--background-secondary)',
									borderRadius: '8px',
									overflow: 'hidden',
								}}
							>
								{[
									{label: t`For 15 minutes`, value: 15 * 60 * 1000},
									{label: t`For 1 hour`, value: 60 * 60 * 1000},
									{label: t`For 3 hours`, value: 3 * 60 * 60 * 1000},
									{label: t`For 8 hours`, value: 8 * 60 * 60 * 1000},
									{label: t`For 24 hours`, value: 24 * 60 * 60 * 1000},
									{label: t`Until I turn it back on`, value: null},
								].map((option, index, array) => {
									const isSelected =
										isMuted &&
										((option.value === null && !muteConfig?.end_time) ||
											(option.value !== null && muteConfig?.selected_time_window === option.value));

									return (
										<React.Fragment key={option.label}>
											<button
												type="button"
												onClick={() => {
													const newMuteConfig =
														option.value !== null
															? {
																	selected_time_window: option.value,
																	end_time: new Date(Date.now() + option.value).toISOString(),
																}
															: null;

													UserGuildSettingsActionCreators.updateChannelOverride(
														null,
														channel.id,
														{
															muted: true,
															mute_config: newMuteConfig,
														},
														{persistImmediately: true},
													);
													handleCloseMuteSheet();
													onClose();
												}}
												style={{
													width: '100%',
													padding: '14px 16px',
													display: 'flex',
													alignItems: 'center',
													justifyContent: 'space-between',
													background: 'none',
													border: 'none',
													cursor: 'pointer',
													color: 'var(--text-primary)',
													fontSize: '16px',
												}}
											>
												<span>{option.label}</span>
												{isSelected && <CheckIcon size={20} weight="bold" style={{color: 'var(--brand-primary)'}} />}
											</button>

											{index < array.length - 1 && (
												<div
													style={{
														height: '1px',
														backgroundColor: 'var(--background-modifier-accent)',
														marginLeft: '16px',
													}}
												/>
											)}
										</React.Fragment>
									);
								})}
							</div>
						)}
					</div>
				</Sheet.Content>
			</Sheet.Root>
		</>
	);
});
