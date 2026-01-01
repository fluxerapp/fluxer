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
	GearIcon,
	LinkIcon,
	NotePencilIcon,
	PaperPlaneIcon,
	PushPinIcon,
	SignOutIcon,
	StarIcon,
	TrashIcon,
	UserPlusIcon,
	XIcon,
} from '@phosphor-icons/react';
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
import {ChannelTypes, ME, Permissions} from '~/Constants';
import {createMuteConfig, getMuteDurationOptions} from '~/components/channel/muteOptions';
import {ChannelSettingsModal} from '~/components/modals/ChannelSettingsModal';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {EditGroupModal} from '~/components/modals/EditGroupModal';
import {GroupInvitesModal} from '~/components/modals/GroupInvitesModal';
import {GuildNotificationSettingsModal} from '~/components/modals/GuildNotificationSettingsModal';
import {InviteModal} from '~/components/modals/InviteModal';
import type {MenuGroupType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {MenuBottomSheet} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import * as Sheet from '~/components/uikit/Sheet/Sheet';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import AuthenticationStore from '~/stores/AuthenticationStore';
import FavoritesStore from '~/stores/FavoritesStore';
import PermissionStore from '~/stores/PermissionStore';
import ReadStateStore from '~/stores/ReadStateStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import {getMutedText} from '~/utils/ContextMenuUtils';
import * as InviteUtils from '~/utils/InviteUtils';
import styles from './ChannelDetailsBottomSheet.module.css';
import sharedStyles from './shared.module.css';

interface ChannelBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channel: ChannelRecord;
	guild?: GuildRecord;
}

export const ChannelBottomSheet: React.FC<ChannelBottomSheetProps> = observer(({isOpen, onClose, channel, guild}) => {
	const {t, i18n} = useLingui();

	const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
	const isDM = channel.type === ChannelTypes.DM;
	const isTextChannel = channel.type === ChannelTypes.GUILD_TEXT;
	const isVoiceChannel = channel.type === ChannelTypes.GUILD_VOICE;

	const currentUserId = AuthenticationStore.currentUserId;
	const isOwner = isGroupDM && channel.ownerId === currentUserId;
	const settingsGuildId = guild?.id ?? null;
	const channelOverride = UserGuildSettingsStore.getChannelOverride(settingsGuildId, channel.id);
	const isMuted = channelOverride?.muted ?? false;
	const muteConfig = channelOverride?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);
	const isFavorited = !!FavoritesStore.getChannel(channel.id);
	const readState = ReadStateStore.get(channel.id);
	const hasUnread = readState.hasUnread;
	const [muteSheetOpen, setMuteSheetOpen] = React.useState(false);

	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {
		channelId: channel.id,
		guildId: channel.guildId,
	});
	const canInvite = InviteUtils.canInviteToChannel(channel.id, channel.guildId);

	const handleMarkAsRead = () => {
		ReadStateActionCreators.ack(channel.id, true, true);
		onClose();
	};

	const handleToggleFavorite = () => {
		onClose();
		const guildId = channel.guildId ?? ME;
		if (isFavorited) {
			FavoritesStore.removeChannel(channel.id);
			ToastActionCreators.createToast({type: 'success', children: t`Removed from favorites`});
		} else {
			FavoritesStore.addChannel(channel.id, guildId, null);
			ToastActionCreators.createToast({type: 'success', children: t`Added to favorites`});
		}
	};

	const handleInviteMembers = () => {
		onClose();
		ModalActionCreators.push(modal(() => <InviteModal channelId={channel.id} />));
	};

	const handleCopyChannelLink = async () => {
		const link = `${window.location.origin}/channels/${channel.guildId}/${channel.id}`;
		await TextCopyActionCreators.copy(i18n, link, true);
		ToastActionCreators.createToast({
			type: 'success',
			children: t`Channel link copied`,
		});
		onClose();
	};

	const handleOpenMuteSheet = () => {
		setMuteSheetOpen(true);
	};

	const handleCloseMuteSheet = () => {
		setMuteSheetOpen(false);
	};

	const handleUpdateMute = (muted: boolean, duration: number | null) => {
		UserGuildSettingsActionCreators.updateChannelOverride(
			settingsGuildId,
			channel.id,
			{
				muted,
				mute_config: muted ? createMuteConfig(duration) : null,
			},
			{persistImmediately: true},
		);
		handleCloseMuteSheet();
	};

	const handleMuteDuration = (duration: number | null) => handleUpdateMute(true, duration);

	const handleUnmute = () => handleUpdateMute(false, null);

	const handleNotificationSettings = () => {
		onClose();
		if (guild) {
			ModalActionCreators.push(modal(() => <GuildNotificationSettingsModal guildId={guild.id} />));
		}
	};

	const handleChannelSettings = () => {
		onClose();
		ModalActionCreators.push(modal(() => <ChannelSettingsModal channelId={channel.id} />));
	};

	const handleDeleteChannel = () => {
		onClose();
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t`Delete Channel`}
					description={t`Are you sure you want to delete #${channel.name ?? channel.id}? This action cannot be undone.`}
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
	};

	const handleEditGroup = () => {
		onClose();
		ModalActionCreators.push(modal(() => <EditGroupModal channelId={channel.id} />));
	};

	const handleShowInvites = () => {
		onClose();
		ModalActionCreators.push(modal(() => <GroupInvitesModal channelId={channel.id} />));
	};

	const handlePinChannel = async () => {
		onClose();
		try {
			await PrivateChannelActionCreators.pinDmChannel(channel.id);
			ToastActionCreators.createToast({
				type: 'success',
				children: isGroupDM ? t`Pinned group` : t`Pinned DM`,
			});
		} catch (error) {
			console.error('Failed to pin:', error);
		}
	};

	const handleUnpinChannel = async () => {
		onClose();
		try {
			await PrivateChannelActionCreators.unpinDmChannel(channel.id);
			ToastActionCreators.createToast({
				type: 'success',
				children: isGroupDM ? t`Unpinned group` : t`Unpinned DM`,
			});
		} catch (error) {
			console.error('Failed to unpin:', error);
		}
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
					description={t`Are you sure you want to leave ${channel.name ?? 'this group'}?`}
					primaryText={t`Leave Group`}
					primaryVariant="danger-primary"
					onPrimary={async () => {
						try {
							await ChannelActionCreators.remove(channel.id);
						} catch (error) {
							console.error('Failed to leave group:', error);
						}
					}}
				/>
			)),
		);
	};

	const handleCloseDM = () => {
		onClose();
		ChannelActionCreators.remove(channel.id);
	};

	const handleCopyChannelId = async () => {
		await TextCopyActionCreators.copy(i18n, channel.id, true);
		ToastActionCreators.createToast({
			type: 'success',
			children: t`Channel ID copied`,
		});
		onClose();
	};

	const menuGroups: Array<MenuGroupType> = [];

	if (isGroupDM) {
		const primaryItems = [
			{
				icon: <NotePencilIcon weight="fill" className={sharedStyles.icon} />,
				label: t`Edit Group`,
				onClick: handleEditGroup,
			},
			channel.isPinned
				? {
						icon: <PushPinIcon weight="fill" className={sharedStyles.icon} />,
						label: t`Unpin Group DM`,
						onClick: handleUnpinChannel,
					}
				: {
						icon: <PushPinIcon weight="fill" className={sharedStyles.icon} />,
						label: t`Pin Group DM`,
						onClick: handlePinChannel,
					},
		];

		if (isOwner) {
			primaryItems.push({
				icon: <PaperPlaneIcon weight="fill" className={sharedStyles.icon} />,
				label: t`Invites`,
				onClick: handleShowInvites,
			});
		}

		menuGroups.push({items: primaryItems});

		menuGroups.push({
			items: [
				{
					icon: <SignOutIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Leave Group`,
					onClick: handleLeaveGroup,
					danger: true,
				},
				{
					icon: <CopyIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Copy Channel ID`,
					onClick: handleCopyChannelId,
				},
			],
		});
	} else if (isDM) {
		menuGroups.push({
			items: [
				channel.isPinned
					? {
							icon: <PushPinIcon weight="fill" className={sharedStyles.icon} />,
							label: t`Unpin DM`,
							onClick: handleUnpinChannel,
						}
					: {
							icon: <PushPinIcon weight="fill" className={sharedStyles.icon} />,
							label: t`Pin DM`,
							onClick: handlePinChannel,
						},
				{
					icon: <XIcon weight="bold" className={sharedStyles.icon} />,
					label: t`Close DM`,
					onClick: handleCloseDM,
					danger: true,
				},
				{
					icon: <CopyIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Copy Channel ID`,
					onClick: handleCopyChannelId,
				},
			],
		});
	} else if (guild && (isTextChannel || isVoiceChannel)) {
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

		const inviteItems = [];
		if (canInvite) {
			inviteItems.push({
				icon: <UserPlusIcon weight="fill" className={sharedStyles.icon} />,
				label: t`Invite People`,
				onClick: handleInviteMembers,
			});
		}
		inviteItems.push({
			icon: <LinkIcon weight="bold" className={sharedStyles.icon} />,
			label: t`Copy Channel Link`,
			onClick: handleCopyChannelLink,
		});
		menuGroups.push({items: inviteItems});

		menuGroups.push({
			items: [
				{
					icon: isMuted ? (
						<BellIcon weight="fill" className={sharedStyles.icon} />
					) : (
						<BellSlashIcon weight="fill" className={sharedStyles.icon} />
					),
					label: isMuted ? t`Unmute Channel` : t`Mute Channel`,
					onClick: handleOpenMuteSheet,
				},
				{
					icon: <BellIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Notification Settings`,
					onClick: handleNotificationSettings,
				},
			],
		});

		if (canManageChannels) {
			menuGroups.push({
				items: [
					{
						icon: <GearIcon weight="fill" className={sharedStyles.icon} />,
						label: t`Edit Channel`,
						onClick: handleChannelSettings,
					},
					{
						icon: <TrashIcon weight="fill" className={sharedStyles.icon} />,
						label: t`Delete Channel`,
						onClick: handleDeleteChannel,
						danger: true,
					},
				],
			});
		}

		menuGroups.push({
			items: [
				{
					icon: <CopyIcon weight="fill" className={sharedStyles.icon} />,
					label: t`Copy Channel ID`,
					onClick: handleCopyChannelId,
				},
			],
		});
	}

	return (
		<>
			<MenuBottomSheet
				isOpen={isOpen}
				onClose={onClose}
				groups={menuGroups}
				title={channel.name ?? t`Channel Options`}
			/>

			<Sheet.Root isOpen={muteSheetOpen} onClose={handleCloseMuteSheet} snapPoints={[0, 1]} initialSnap={1}>
				<Sheet.Handle />
				<Sheet.Header trailing={<Sheet.CloseButton onClick={handleCloseMuteSheet} />}>
					<Sheet.Title>{isMuted ? t`Unmute Channel` : t`Mute Channel`}</Sheet.Title>
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
										<button type="button" onClick={handleUnmute} className={styles.muteOptionButton}>
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
													onClick={() => handleMuteDuration(option.value)}
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
		</>
	);
});
