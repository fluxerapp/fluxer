// SPDX-License-Identifier: AGPL-3.0-or-later

import {MORE_OPTIONS_DESCRIPTOR, WATCH_STREAM_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {
	ChangeStreamIcon,
	LocalMuteIcon,
	PopOutIcon,
	TurnOffStreamIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import type {VoiceParticipantMenuScreenShareSource} from '@app/features/ui/action_menu/items/VoiceParticipantMenuTypes';
import {
	CHANGE_STREAM_DESCRIPTOR,
	MUTE_DESCRIPTOR,
	PAUSE_OWN_STREAM_PREVIEW_DESCRIPTOR,
	POP_OUT_STREAM_DESCRIPTOR,
	SCREEN_SHARE_PRIVACY_DESCRIPTOR,
	SHOW_MY_SCREEN_SHARE_DESCRIPTOR,
	STOP_STREAMING_DESCRIPTOR,
	STREAM_VOLUME_DESCRIPTOR,
} from '@app/features/ui/action_menu/items/voice_participant_menu_data/shared';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import type {
	MenuCheckboxType,
	MenuGroupType,
	MenuItemType,
	MenuSliderType,
	MenuSubmenuItemType,
} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import {changeActiveScreenShare, stopActiveScreenShare} from '@app/features/voice/components/ActiveScreenShareMenu';
import {openScreenSharePreviewPrivacyModal} from '@app/features/voice/components/modals/ScreenSharePickerModal';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import ActiveScreenShareSource from '@app/features/voice/state/ActiveScreenShareSource';
import PopoutWindowManager, {isVoicePopoutSupported} from '@app/features/voice/state/PopoutWindowManager';
import StreamAudioPrefs from '@app/features/voice/state/StreamAudioPrefs';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {isScreenShareRollbackIncompleteError} from '@app/features/voice/utils/ScreenShareRollbackIncompleteError';
import {handleScreenShareError} from '@app/features/voice/utils/ScreenShareUtils';
import {VOICE_STOP_WATCHING_DESCRIPTOR} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {buildVoiceParticipantIdentity} from '@app/features/voice/utils/VoiceParticipantIdentity';
import type {I18n} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const logger = new Logger('VoiceParticipantStreamMenuBuilder');

interface VoiceParticipantStreamMenuBuilderOptions {
	i18n: I18n;
	userId: string;
	guildId?: string;
	connectionId?: string;
	channelId: string | null;
	participantIdentity: string;
	displayName: string;
	isCurrentUserConnectedToVoice: boolean;
	source: VoiceParticipantMenuScreenShareSource;
	streamVolume: number;
	isStreamMuted: boolean;
	showMyOwnScreenShare: boolean;
	pauseOwnScreenSharePreviewOnUnfocus: boolean;
	onClose: () => void;
}

function buildStreamStateAction(options: VoiceParticipantStreamMenuBuilderOptions): MenuItemType {
	const {i18n, source, onClose} = options;
	const streamState = source.state;
	if (streamState.kind === 'own') {
		return {
			icon: (
				<TurnOffStreamIcon
					size={16}
					data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.build-stream-state-action.turn-off-stream-icon"
				/>
			),
			label: i18n._(STOP_STREAMING_DESCRIPTOR),
			onClick: () => {
				onClose();
				void stopActiveScreenShare().catch((error) => {
					if (isScreenShareRollbackIncompleteError(error)) handleScreenShareError(error);
					logger.error('Failed to stop active screen share from participant menu', error);
				});
			},
			danger: true,
		};
	}
	if (streamState.kind === 'remote-unwatched') {
		return {
			label: i18n._(WATCH_STREAM_DESCRIPTOR),
			onClick: () => {
				streamState.onWatch();
				onClose();
			},
		};
	}
	return {
		icon: (
			<TurnOffStreamIcon
				size={16}
				data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.build-stream-state-action.turn-off-stream-icon--2"
			/>
		),
		label: i18n._(VOICE_STOP_WATCHING_DESCRIPTOR),
		onClick: () => {
			streamState.onStopWatching();
			onClose();
		},
	};
}

function buildOwnStreamChangeAction(options: VoiceParticipantStreamMenuBuilderOptions): MenuItemType | null {
	const {i18n, source, onClose} = options;
	const streamState = source.state;
	if (streamState.kind !== 'own') return null;
	return {
		icon: (
			<ChangeStreamIcon
				size={16}
				data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.build-own-stream-change-action.change-stream-icon"
			/>
		),
		label: i18n._(CHANGE_STREAM_DESCRIPTOR),
		onClick: () => {
			onClose();
			void changeActiveScreenShare(ActiveScreenShareSource.getShareContext() ?? 'display').catch((error) => {
				logger.error('Failed to change active screen share from participant menu', error);
			});
		},
	};
}

function buildStreamAudioActions(
	options: VoiceParticipantStreamMenuBuilderOptions,
): Array<MenuCheckboxType | MenuSliderType> {
	const {i18n, source, streamVolume, isStreamMuted, userId} = options;
	const streamState = source.state;
	if (streamState.kind !== 'remote-watched') return [];
	if (!streamState.hasAudio) return [];
	return [
		{
			icon: (
				<LocalMuteIcon
					size={16}
					data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.build-stream-audio-actions.local-mute-icon"
				/>
			),
			label: i18n._(MUTE_DESCRIPTOR),
			checked: isStreamMuted,
			onChange: (checked: boolean) => {
				StreamAudioPrefs.setMuted(source.streamKey, checked);
				MediaEngine.applyLocalAudioPreferencesForUser(userId);
			},
		},
		{
			label: i18n._(STREAM_VOLUME_DESCRIPTOR),
			value: streamVolume,
			minValue: 0,
			maxValue: 200,
			onChange: (value: number) => {
				StreamAudioPrefs.setVolume(source.streamKey, value);
				MediaEngine.applyLocalAudioPreferencesForUser(userId);
			},
			onFormat: (value: number) => `${Math.round(value)}%`,
			factoryDefaultValue: 100,
		},
	];
}

function buildStreamPopoutAction(options: VoiceParticipantStreamMenuBuilderOptions): MenuItemType | null {
	const {
		i18n,
		userId,
		guildId,
		connectionId,
		channelId,
		participantIdentity,
		displayName,
		isCurrentUserConnectedToVoice,
		onClose,
	} = options;
	if (!connectionId) return null;
	if (!channelId) return null;
	if (!isCurrentUserConnectedToVoice) return null;
	if (!isVoicePopoutSupported()) return null;
	return {
		icon: (
			<PopOutIcon
				size={16}
				data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.build-stream-popout-action.pop-out-icon"
			/>
		),
		label: i18n._(POP_OUT_STREAM_DESCRIPTOR),
		onClick: () => {
			const didOpen = PopoutWindowManager.openTilePopout({
				participantIdentity,
				source: 'screen_share',
				userId,
				connectionId,
				channelId,
				guildId: guildId ?? null,
				title: displayName,
			});
			if (didOpen) onClose();
		},
	};
}

function buildStreamMoreOptions(options: VoiceParticipantStreamMenuBuilderOptions): MenuSubmenuItemType | null {
	const {i18n, guildId, source, showMyOwnScreenShare, pauseOwnScreenSharePreviewOnUnfocus, onClose} = options;
	if (source.state.kind !== 'own') return null;
	const items: Array<MenuItemType | MenuCheckboxType> = [
		{
			label: i18n._(SHOW_MY_SCREEN_SHARE_DESCRIPTOR),
			checked: showMyOwnScreenShare,
			onChange: (checked: boolean) => VoiceSettingsCommands.update({showMyOwnScreenShare: checked}),
		},
		{
			label: i18n._(PAUSE_OWN_STREAM_PREVIEW_DESCRIPTOR),
			checked: pauseOwnScreenSharePreviewOnUnfocus,
			onChange: (checked: boolean) => VoiceSettingsCommands.update({pauseOwnScreenSharePreviewOnUnfocus: checked}),
		},
	];
	if (guildId === undefined) {
		items.push({
			label: i18n._(SCREEN_SHARE_PRIVACY_DESCRIPTOR),
			onClick: () => {
				onClose();
				openScreenSharePreviewPrivacyModal();
			},
		});
	}
	if (items.length === 0) return null;
	return {
		label: i18n._(MORE_OPTIONS_DESCRIPTOR),
		items,
	};
}

export function buildVoiceParticipantStreamMenu(
	options: VoiceParticipantStreamMenuBuilderOptions,
): Array<MenuGroupType> {
	const stateActions: Array<MenuItemType> = [buildStreamStateAction(options)];
	const changeAction = buildOwnStreamChangeAction(options);
	if (changeAction) stateActions.push(changeAction);
	const groups: Array<MenuGroupType> = [{items: stateActions}];
	const audioActions = buildStreamAudioActions(options);
	if (audioActions.length > 0) groups.push({items: audioActions});
	const popoutAction = buildStreamPopoutAction(options);
	if (popoutAction) groups.push({items: [popoutAction]});
	const moreOptions = buildStreamMoreOptions(options);
	if (moreOptions) groups.push({items: [moreOptions]});
	return groups;
}

interface VoiceParticipantOwnStreamMenuTailProps {
	userId: string;
	guildId?: string;
	connectionId?: string;
	displayName: string;
	onClose: () => void;
}

export const VoiceParticipantOwnStreamMenuTail: React.FC<VoiceParticipantOwnStreamMenuTailProps> = observer(
	({userId, guildId, connectionId, displayName, onClose}) => {
		const {i18n} = useLingui();
		const connectionVoiceState = connectionId ? MediaEngine.getVoiceStateByConnectionId(connectionId) : null;
		const channelId = connectionVoiceState?.channel_id ?? null;
		const participantIdentity = connectionId ? buildVoiceParticipantIdentity(userId, connectionId) : '';
		const canPopOut = Boolean(
			connectionId && channelId && participantIdentity && MediaEngine.connectionId && isVoicePopoutSupported(),
		);
		return (
			<>
				{canPopOut && connectionId && channelId && (
					<MenuGroup data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.popout-group">
						<MenuItem
							icon={
								<PopOutIcon
									size={16}
									data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.voice-participant-own-stream-menu-tail.pop-out-icon"
								/>
							}
							onClick={() => {
								const didOpen = PopoutWindowManager.openTilePopout({
									participantIdentity,
									source: 'screen_share',
									userId,
									connectionId,
									channelId,
									guildId: guildId ?? null,
									title: displayName,
								});
								if (didOpen) onClose();
							}}
							data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.popout-stream"
						>
							{i18n._(POP_OUT_STREAM_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
				)}
				<MenuGroup data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.more-options-group">
					<MenuItemSubmenu
						label={i18n._(MORE_OPTIONS_DESCRIPTOR)}
						render={() => (
							<MenuGroup data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.more-options">
								<CheckboxItem
									checked={VoiceSettings.showMyOwnScreenShare}
									onCheckedChange={(checked: boolean) => VoiceSettingsCommands.update({showMyOwnScreenShare: checked})}
									data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.voice-participant-own-stream-menu-tail.checkbox-item"
								>
									{i18n._(SHOW_MY_SCREEN_SHARE_DESCRIPTOR)}
								</CheckboxItem>
								<CheckboxItem
									checked={VoiceSettings.pauseOwnScreenSharePreviewOnUnfocus}
									onCheckedChange={(checked: boolean) =>
										VoiceSettingsCommands.update({pauseOwnScreenSharePreviewOnUnfocus: checked})
									}
									data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.voice-participant-own-stream-menu-tail.checkbox-item--2"
								>
									{i18n._(PAUSE_OWN_STREAM_PREVIEW_DESCRIPTOR)}
								</CheckboxItem>
								{guildId === undefined && (
									<MenuItem
										onClick={() => {
											onClose();
											openScreenSharePreviewPrivacyModal();
										}}
										data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.voice-participant-own-stream-menu-tail.menu-item.close"
									>
										{i18n._(SCREEN_SHARE_PRIVACY_DESCRIPTOR)}
									</MenuItem>
								)}
							</MenuGroup>
						)}
						data-flx="ui.action-menu.items.voice-participant-stream-menu-builder.more-options-submenu"
					/>
				</MenuGroup>
			</>
		);
	},
);
