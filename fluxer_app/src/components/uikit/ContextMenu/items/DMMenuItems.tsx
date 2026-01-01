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

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ReadStateActionCreators from '~/actions/ReadStateActionCreators';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import type {ChannelRecord} from '~/records/ChannelRecord';
import ReadStateStore from '~/stores/ReadStateStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import * as ChannelUtils from '~/utils/ChannelUtils';
import {getMutedText} from '~/utils/ContextMenuUtils';
import {MarkAsReadIcon, MuteIcon} from '../ContextMenuIcons';
import {MenuGroup} from '../MenuGroup';
import {MenuItem} from '../MenuItem';
import {MenuItemSubmenu} from '../MenuItemSubmenu';

interface DMMenuItemProps {
	channel: ChannelRecord;
	onClose: () => void;
}

export const MarkDMAsReadMenuItem: React.FC<DMMenuItemProps> = observer(({channel, onClose}) => {
	const {t} = useLingui();
	const hasUnread = ReadStateStore.hasUnread(channel.id);

	const handleMarkAsRead = React.useCallback(() => {
		ReadStateActionCreators.ack(channel.id, true, true);
		onClose();
	}, [channel.id, onClose]);

	return (
		<MenuItem icon={<MarkAsReadIcon />} onClick={handleMarkAsRead} disabled={!hasUnread}>
			{t`Mark as Read`}
		</MenuItem>
	);
});

interface MuteDuration {
	label: MessageDescriptor;
	value: number | null;
}

const MUTE_DURATIONS: Array<MuteDuration> = [
	{label: msg`For 15 Minutes`, value: 15 * 60 * 1000},
	{label: msg`For 1 Hour`, value: 60 * 60 * 1000},
	{label: msg`For 3 Hours`, value: 3 * 60 * 60 * 1000},
	{label: msg`For 8 Hours`, value: 8 * 60 * 60 * 1000},
	{label: msg`For 24 Hours`, value: 24 * 60 * 60 * 1000},
	{label: msg`Until I turn it back on`, value: null},
];

export const MuteDMMenuItem: React.FC<DMMenuItemProps> = observer(({channel, onClose}) => {
	const {t} = useLingui();
	const channelOverride = UserGuildSettingsStore.getChannelOverride(null, channel.id);
	const isMuted = channelOverride?.muted ?? false;
	const muteConfig = channelOverride?.mute_config;

	const mutedText = getMutedText(isMuted, muteConfig);
	const dmDisplayName = ChannelUtils.getDMDisplayName(channel);
	const displayLabel = channel.isDM() ? `@${dmDisplayName}` : dmDisplayName;
	const muteLabel = t`Mute ${displayLabel}`;
	const unmuteLabel = t`Unmute ${displayLabel}`;

	const handleMute = React.useCallback(
		(duration: number | null) => {
			const muteConfig = duration
				? {
						selected_time_window: duration,
						end_time: new Date(Date.now() + duration).toISOString(),
					}
				: null;

			UserGuildSettingsActionCreators.updateChannelOverride(
				null,
				channel.id,
				{
					muted: true,
					mute_config: muteConfig,
				},
				{persistImmediately: true},
			);
			onClose();
		},
		[channel.id, onClose],
	);

	const handleUnmute = React.useCallback(() => {
		UserGuildSettingsActionCreators.updateChannelOverride(
			null,
			channel.id,
			{
				muted: false,
				mute_config: null,
			},
			{persistImmediately: true},
		);
		onClose();
	}, [channel.id, onClose]);

	if (isMuted) {
		return (
			<MenuItem icon={<MuteIcon />} onClick={handleUnmute} hint={mutedText ?? undefined}>
				{unmuteLabel}
			</MenuItem>
		);
	}

	return (
		<MenuItemSubmenu
			label={muteLabel}
			icon={<MuteIcon />}
			onTriggerSelect={() => handleMute(null)}
			render={() => (
				<MenuGroup>
					{MUTE_DURATIONS.map((duration) => (
						<MenuItem key={duration.value ?? 'until'} onClick={() => handleMute(duration.value)}>
							{t(duration.label)}
						</MenuItem>
					))}
				</MenuGroup>
			)}
		/>
	);
});
