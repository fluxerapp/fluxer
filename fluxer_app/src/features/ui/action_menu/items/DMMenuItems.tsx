// SPDX-License-Identifier: AGPL-3.0-or-later

import {createMuteConfig} from '@app/features/channel/components/MuteOptions';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {MuteTargetMenuItem} from '@app/features/ui/action_menu/items/MuteTargetMenuItem';

import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {getMutedText} from '@app/lib/overlay/OverlayContextMenu';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const MUTE_DESCRIPTOR = msg({
	message: 'Mute {displayLabel}',
	comment: 'Action that mutes the named target.',
});
const UNMUTE_DESCRIPTOR = msg({
	message: 'Unmute {displayLabel}',
	comment: 'Action that re-enables notifications for the named target.',
});

interface DMMenuItemProps {
	channel: Channel;
	onClose: () => void;
}

export const MuteDMMenuItem: React.FC<DMMenuItemProps> = observer(({channel, onClose}) => {
	const {i18n} = useLingui();

	const channelOverride = UserGuildSettings.getChannelOverride(null, channel.id);
	const isMuted = channelOverride?.muted ?? false;
	const muteConfig = channelOverride?.mute_config;
	const mutedText = getMutedText(isMuted, muteConfig);
	const dmDisplayName = ChannelUtils.getDMDisplayName(channel);
	const displayLabel = channel.isDM() ? `@${dmDisplayName}` : dmDisplayName;
	const muteLabel = i18n._(MUTE_DESCRIPTOR, {displayLabel});
	const unmuteLabel = i18n._(UNMUTE_DESCRIPTOR, {displayLabel});
	const handleMute = useCallback(
		(duration: number | null) => {
			const muteConfig = createMuteConfig(duration);
			UserGuildSettingsCommands.updateChannelOverride(
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
	const handleUnmute = useCallback(() => {
		UserGuildSettingsCommands.updateChannelOverride(
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

	return (
		<MuteTargetMenuItem
			showUnmute={isMuted}
			mutedText={mutedText}
			muteLabel={muteLabel}
			unmuteLabel={unmuteLabel}
			onMute={handleMute}
			onUnmute={handleUnmute}
			dataFlx="ui.action-menu.items.dm-menu-items.mute-dm-menu-item"
			data-flx="ui.action-menu.items.dm-menu-items.mute-target-menu-item"
		/>
	);
});
