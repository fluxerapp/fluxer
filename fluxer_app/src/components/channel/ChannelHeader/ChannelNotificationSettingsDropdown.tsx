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

import {useLingui} from '@lingui/react/macro';

import {observer} from 'mobx-react-lite';
import React from 'react';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import {MessageNotifications} from '~/Constants';
import {ContextMenuCloseProvider} from '~/components/uikit/ContextMenu/ContextMenu';
import {MuteIcon} from '~/components/uikit/ContextMenu/ContextMenuIcons';
import itemStyles from '~/components/uikit/ContextMenu/items/MenuItems.module.css';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import menuItemStyles from '~/components/uikit/ContextMenu/MenuItem.module.css';
import {MenuItemRadio} from '~/components/uikit/ContextMenu/MenuItemRadio';
import {MenuItemSubmenu} from '~/components/uikit/ContextMenu/MenuItemSubmenu';
import type {ChannelRecord} from '~/records/ChannelRecord';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import {getMutedText, getNotificationSettingsLabel} from '~/utils/ContextMenuUtils';

interface MuteDuration {
	label: string;
	value: number | null;
}

interface Props {
	channel: ChannelRecord;
	onClose: () => void;
}

export const ChannelNotificationSettingsDropdown: React.FC<Props> = observer(({channel, onClose}) => {
	const {t} = useLingui();

	const MUTE_DURATIONS: Array<MuteDuration> = [
		{label: t`For 15 Minutes`, value: 15 * 60 * 1000},
		{label: t`For 1 Hour`, value: 60 * 60 * 1000},
		{label: t`For 3 Hours`, value: 3 * 60 * 60 * 1000},
		{label: t`For 8 Hours`, value: 8 * 60 * 60 * 1000},
		{label: t`For 24 Hours`, value: 24 * 60 * 60 * 1000},
		{label: t`Until I turn it back on`, value: null},
	];
	const guildId = channel.guildId;
	const isGuildChannel = guildId != null;

	const channelOverride = UserGuildSettingsStore.getChannelOverride(guildId ?? null, channel.id);
	const isMuted = channelOverride?.muted ?? false;
	const muteConfig = channelOverride?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);

	const channelNotifications = channelOverride?.message_notifications;
	const currentNotificationLevel =
		channelNotifications ?? (isGuildChannel ? MessageNotifications.INHERIT : MessageNotifications.ALL_MESSAGES);

	const guildNotificationLevel = guildId
		? UserGuildSettingsStore.getGuildMessageNotifications(guildId)
		: MessageNotifications.ALL_MESSAGES;

	const categoryId = channel.parentId;
	const categoryOverride = guildId ? UserGuildSettingsStore.getChannelOverride(guildId, categoryId ?? '') : null;
	const categoryNotifications = categoryId ? categoryOverride?.message_notifications : undefined;

	const resolveEffectiveLevel = (level: number | undefined, fallback: number): number => {
		if (level === undefined || level === MessageNotifications.INHERIT) {
			return fallback;
		}
		return level;
	};

	const effectiveDefaultLevel = resolveEffectiveLevel(categoryNotifications, guildNotificationLevel);
	const hasCategory = categoryId != null;

	const handleMute = React.useCallback(
		(duration: number | null) => {
			const muteConfigValue = duration
				? {
						selected_time_window: duration,
						end_time: new Date(Date.now() + duration).toISOString(),
					}
				: null;

			UserGuildSettingsActionCreators.updateChannelOverride(
				guildId ?? null,
				channel.id,
				{
					muted: true,
					mute_config: muteConfigValue,
				},
				{persistImmediately: true},
			);
			onClose();
		},
		[guildId, channel.id, onClose],
	);

	const handleUnmute = React.useCallback(() => {
		UserGuildSettingsActionCreators.updateChannelOverride(
			guildId ?? null,
			channel.id,
			{
				muted: false,
				mute_config: null,
			},
			{persistImmediately: true},
		);
		onClose();
	}, [guildId, channel.id, onClose]);

	const handleNotificationLevelChange = React.useCallback(
		(level: number) => {
			if (level === MessageNotifications.INHERIT) {
				UserGuildSettingsActionCreators.updateChannelOverride(
					guildId ?? null,
					channel.id,
					{
						message_notifications: MessageNotifications.INHERIT,
					},
					{persistImmediately: true},
				);
			} else if (guildId) {
				UserGuildSettingsActionCreators.updateMessageNotifications(guildId, level, channel.id, {
					persistImmediately: true,
				});
			} else {
				UserGuildSettingsActionCreators.updateChannelOverride(
					null,
					channel.id,
					{
						message_notifications: level,
					},
					{persistImmediately: true},
				);
			}
		},
		[guildId, channel.id],
	);

	const defaultLabelParts = React.useMemo(
		() => ({
			main: hasCategory ? t`Use Category Default` : t`Use Community Default`,
			sub: getNotificationSettingsLabel(effectiveDefaultLevel) ?? null,
		}),
		[effectiveDefaultLevel, hasCategory],
	);

	return (
		<ContextMenuCloseProvider value={onClose}>
			<MenuGroup>
				{isMuted ? (
					<MenuItem icon={<MuteIcon />} onClick={handleUnmute} hint={mutedText ?? undefined}>
						{t`Unmute Channel`}
					</MenuItem>
				) : (
					<MenuItemSubmenu
						label={t`Mute Channel`}
						icon={<MuteIcon />}
						onTriggerSelect={() => handleMute(null)}
						render={() => (
							<MenuGroup>
								{MUTE_DURATIONS.map((duration) => (
									<MenuItem key={duration.label} onClick={() => handleMute(duration.value)}>
										{duration.label}
									</MenuItem>
								))}
							</MenuGroup>
						)}
					/>
				)}
			</MenuGroup>

			{isGuildChannel && (
				<MenuGroup>
					<MenuItemRadio
						selected={currentNotificationLevel === MessageNotifications.INHERIT}
						onSelect={() => handleNotificationLevelChange(MessageNotifications.INHERIT)}
					>
						<div className={itemStyles.flexColumn}>
							<span>{defaultLabelParts.main}</span>
							{defaultLabelParts.sub && <div className={menuItemStyles.subtext}>{defaultLabelParts.sub}</div>}
						</div>
					</MenuItemRadio>
				</MenuGroup>
			)}

			<MenuGroup>
				<MenuItemRadio
					selected={currentNotificationLevel === MessageNotifications.ALL_MESSAGES}
					onSelect={() => handleNotificationLevelChange(MessageNotifications.ALL_MESSAGES)}
				>
					{t`All Messages`}
				</MenuItemRadio>
				<MenuItemRadio
					selected={currentNotificationLevel === MessageNotifications.ONLY_MENTIONS}
					onSelect={() => handleNotificationLevelChange(MessageNotifications.ONLY_MENTIONS)}
				>
					{t`Only @mentions`}
				</MenuItemRadio>
				<MenuItemRadio
					selected={currentNotificationLevel === MessageNotifications.NO_MESSAGES}
					onSelect={() => handleNotificationLevelChange(MessageNotifications.NO_MESSAGES)}
				>
					{t`Nothing`}
				</MenuItemRadio>
			</MenuGroup>
		</ContextMenuCloseProvider>
	);
});
