// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserSettingsModal} from '@app/features/app/components/dialogs/LoadableSettingsModals';
import {getMuteDurationOptions} from '@app/features/channel/components/MuteOptions';
import type {Guild} from '@app/features/guild/models/Guild';
import {
	MUTE_COMMUNITY_DESCRIPTOR,
	UNMUTE_COMMUNITY_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {EditProfileIcon, MuteIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import Users from '@app/features/user/state/Users';
import {getMutedText} from '@app/lib/overlay/OverlayContextMenu';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo} from 'react';

const EDIT_COMMUNITY_PROFILE_DESCRIPTOR = msg({
	message: 'Edit community profile',
	comment: 'Action that opens the community profile editor for the current member.',
});

interface GuildMenuItemProps {
	guild: Guild;
	onClose: () => void;
}
export const MuteCommunityMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const settings = UserGuildSettings.getSettingsForScope(guild.id);
	const isMuted = settings?.muted ?? false;
	const muteConfig = settings?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);
	const muteDurations = useMemo(() => getMuteDurationOptions(i18n), [i18n.locale]);
	const handleMute = useCallback(
		(duration: number | null) => {
			const computedMuteConfig = duration
				? {
						selected_time_window: duration,
						end_time: new Date(Date.now() + duration).toISOString(),
					}
				: null;
			UserGuildSettingsCommands.updateGuildSettings(
				guild.id,
				{
					muted: true,
					mute_config: computedMuteConfig,
				},
				{persistImmediately: true},
			);
			onClose();
		},
		[guild.id, onClose],
	);
	const handleUnmute = useCallback(() => {
		UserGuildSettingsCommands.updateGuildSettings(
			guild.id,
			{
				muted: false,
				mute_config: null,
			},
			{persistImmediately: true},
		);
		onClose();
	}, [guild.id, onClose]);
	if (isMuted) {
		return (
			<MenuItem
				icon={<MuteIcon data-flx="ui.action-menu.items.guild-menu-items.mute-community-menu-item.mute-icon" />}
				onClick={handleUnmute}
				hint={mutedText ?? undefined}
				data-flx="ui.action-menu.items.guild-menu-items.mute-community-menu-item.menu-item.unmute"
			>
				{i18n._(UNMUTE_COMMUNITY_DESCRIPTOR)}
			</MenuItem>
		);
	}
	return (
		<MenuItemSubmenu
			label={i18n._(MUTE_COMMUNITY_DESCRIPTOR)}
			onTriggerSelect={() => handleMute(null)}
			render={() => (
				<MenuGroup data-flx="ui.action-menu.items.guild-menu-items.mute-community-menu-item.menu-group">
					{muteDurations.map((duration) => (
						<MenuItem
							key={duration.value ?? 'until'}
							onClick={() => handleMute(duration.value)}
							data-flx="ui.action-menu.items.guild-menu-items.mute-community-menu-item.menu-item.mute"
						>
							{duration.label}
						</MenuItem>
					))}
				</MenuGroup>
			)}
			data-flx="ui.action-menu.items.guild-menu-items.mute-community-menu-item.menu-item-submenu"
		/>
	);
});
export const EditCommunityProfileMenuItem: React.FC<GuildMenuItemProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const currentUser = Users.getCurrentUser();
	const handleEditProfile = useCallback(() => {
		ModalCommands.push(
			modal(
				() => (
					<UserSettingsModal
						initialGuildId={guild.id}
						initialTab="my_profile"
						data-flx="ui.action-menu.items.guild-menu-items.handle-edit-profile.user-settings-modal"
					/>
				),
				'user-settings',
			),
		);
		onClose();
	}, [guild.id, onClose]);
	if (!currentUser?.isClaimed()) return null;
	return (
		<MenuItem
			icon={
				<EditProfileIcon data-flx="ui.action-menu.items.guild-menu-items.edit-community-profile-menu-item.edit-profile-icon" />
			}
			onClick={handleEditProfile}
			data-flx="ui.action-menu.items.guild-menu-items.edit-community-profile-menu-item.menu-item.edit-profile"
		>
			{i18n._(EDIT_COMMUNITY_PROFILE_DESCRIPTOR)}
		</MenuItem>
	);
});
