// SPDX-License-Identifier: AGPL-3.0-or-later

import {createMuteConfig} from '@app/features/channel/components/MuteOptions';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import type {MuteConfig} from '@app/features/user/models/UserGuildSettings';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {useCallback, useState} from 'react';

interface UseMuteSheetBaseParams {
	onMuteSuccess?: () => void;
	onUnmuteSuccess?: () => void;
	onClose?: () => void;
}

interface UseMuteSheetChannelParams extends UseMuteSheetBaseParams {
	mode?: 'channel';
	guildId: string | null;
	channelId: string;
	additionalMutePayload?: Record<string, unknown>;
}

interface UseMuteSheetGuildParams extends UseMuteSheetBaseParams {
	mode: 'guild';
	guildId: string;
}

type UseMuteSheetParams = UseMuteSheetChannelParams | UseMuteSheetGuildParams;

interface UseMuteSheetReturn {
	muteSheetOpen: boolean;
	muteConfig: MuteConfig | null | undefined;
	openMuteSheet: () => void;
	closeMuteSheet: () => void;
	handleMute: (duration: number | null) => void;
	handleUnmute: () => void;
}

export function useMuteSheet(params: UseMuteSheetParams): UseMuteSheetReturn {
	const [muteSheetOpen, setMuteSheetOpen] = useState(false);
	const isGuildMode = params.mode === 'guild';
	const guildId = params.guildId;
	const channelId = isGuildMode ? null : params.channelId;
	const additionalMutePayload = isGuildMode ? undefined : params.additionalMutePayload;
	const {onMuteSuccess, onUnmuteSuccess, onClose} = params;
	const muteConfig = isGuildMode
		? UserGuildSettings.getSettingsForScope(guildId)?.mute_config
		: UserGuildSettings.getChannelOverride(guildId, channelId!)?.mute_config;
	const openMuteSheet = useCallback(() => {
		setMuteSheetOpen(true);
	}, []);
	const closeMuteSheet = useCallback(() => {
		setMuteSheetOpen(false);
	}, []);
	const handleMute = useCallback(
		(duration: number | null) => {
			if (isGuildMode) {
				UserGuildSettingsCommands.updateGuildSettings(guildId, {
					muted: true,
					mute_config: createMuteConfig(duration),
				});
			} else {
				UserGuildSettingsCommands.updateChannelOverride(
					guildId,
					channelId!,
					{
						muted: true,
						mute_config: createMuteConfig(duration),
						...additionalMutePayload,
					},
					{persistImmediately: true},
				);
			}
			setMuteSheetOpen(false);
			onMuteSuccess?.();
			onClose?.();
		},
		[isGuildMode, guildId, channelId, additionalMutePayload, onMuteSuccess, onClose],
	);
	const handleUnmute = useCallback(() => {
		if (isGuildMode) {
			UserGuildSettingsCommands.updateGuildSettings(guildId, {
				muted: false,
				mute_config: null,
			});
		} else {
			UserGuildSettingsCommands.updateChannelOverride(
				guildId,
				channelId!,
				{
					muted: false,
					mute_config: null,
				},
				{persistImmediately: true},
			);
		}
		setMuteSheetOpen(false);
		onUnmuteSuccess?.();
		onClose?.();
	}, [isGuildMode, guildId, channelId, onUnmuteSuccess, onClose]);
	return {
		muteSheetOpen,
		muteConfig,
		openMuteSheet,
		closeMuteSheet,
		handleMute,
		handleUnmute,
	};
}
