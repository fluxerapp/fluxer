// SPDX-License-Identifier: AGPL-3.0-or-later

import sharedStyles from '@app/features/app/components/bottomsheets/shared.module.css';
import * as VoiceStateCommands from '@app/features/devtools/commands/VoiceStateCommands';
import {
	DeafenIcon,
	EchoCancellationIcon,
	GridViewIcon,
	InputDeviceIcon,
	MembersIcon,
	OutputDeviceIcon,
	SettingsIcon,
	VideoSettingsIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {MenuGroupType, MenuSheetItem} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import * as VoiceCallLayoutCommands from '@app/features/voice/commands/VoiceCallLayoutCommands';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import {CameraPreviewModalInRoom} from '@app/features/voice/components/modals/CameraPreviewModal';
import MediaEngine, {useMediaEngineVersion} from '@app/features/voice/engine/MediaEngineFacade';
import VoiceCallLayout from '@app/features/voice/state/VoiceCallLayout';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	getVoiceDeafenedByModeratorsStatusLabel,
	getVoiceVideoSettingsLabel,
	VOICE_AUTOMATIC_GAIN_CONTROL_DESCRIPTOR,
	VOICE_DEAFEN_DESCRIPTOR,
	VOICE_DIRECT_INPUT_PROFILE_DESCRIPTOR,
	VOICE_ECHO_CANCELLATION_DESCRIPTOR,
	VOICE_FOCUSED_VOICE_PROFILE_DESCRIPTOR,
	VOICE_INPUT_DEVICE_DESCRIPTOR,
	VOICE_INPUT_VOLUME_DESCRIPTOR,
	VOICE_OUTPUT_DEVICE_DESCRIPTOR,
	VOICE_OUTPUT_VOLUME_DESCRIPTOR,
	VOICE_UNDEAFEN_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {getActiveVoiceProcessingMode, type VoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';
import {VOICE_VOLUME_MAX_PERCENT} from '@app/features/voice/utils/VoiceVolumeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const CUSTOM_DESCRIPTOR = msg({
	message: 'Custom',
	comment: 'Voice processing profile option label (mobile voice settings). User configures processing manually.',
});
const ENHANCED_DESCRIPTOR = msg({
	message: 'Enhanced',
	comment: 'Noise suppression option label (mobile voice settings). Enhanced engine (DeepFilterNet3).',
});
const STANDARD_DESCRIPTOR = msg({
	message: 'Standard',
	comment: 'Noise suppression option label (mobile voice settings). Browser built-in engine.',
});
const OFF_DESCRIPTOR = msg({
	message: 'Off',
	comment: 'Noise suppression option label (mobile voice settings). Suppression disabled.',
});
const INPUT_PROFILE_DESCRIPTOR = msg({
	message: 'Input profile: {processingModeLabel}',
	comment: 'Row label in the mobile voice settings input-profile picker. {processingModeLabel} is the profile name.',
});
const NOISE_SUPPRESSION_DESCRIPTOR = msg({
	message: 'Noise suppression: {noiseSuppressionLabel}',
	comment:
		'Row label in the mobile voice settings noise-suppression picker. {noiseSuppressionLabel} is the suppression choice.',
});
const VOICE_SETTINGS_DESCRIPTOR = msg({
	message: 'Voice settings',
	comment: 'Title of the mobile voice settings bottom sheet.',
});
const CAMERA_DEVICE_DESCRIPTOR = msg({
	message: 'Camera device',
	comment: 'Section header in the mobile video settings bottom sheet. Picks the camera device.',
});
const PREVIEW_CAMERA_DESCRIPTOR = msg({
	message: 'Preview camera',
	comment: 'Button label in the mobile video settings bottom sheet. Opens the camera preview.',
});
const VIDEO_SETTINGS_DESCRIPTOR = msg({
	message: 'Video settings',
	comment: 'Title of the mobile video settings bottom sheet.',
});
const GRID_VIEW_DESCRIPTOR = msg({
	message: 'Grid view',
	comment: 'Toggle label in the mobile video settings bottom sheet. Switches to grid layout.',
});
const SHOW_MY_OWN_CAMERA_DESCRIPTOR = msg({
	message: 'Show my own camera',
	comment: 'Toggle label in the mobile video settings bottom sheet. Controls whether the local camera tile is visible.',
});
const SHOW_NON_VIDEO_PARTICIPANTS_DESCRIPTOR = msg({
	message: 'Show non-video participants',
	comment:
		'Toggle label in the mobile video settings bottom sheet. Controls whether participants without video are shown in the grid.',
});

interface VoiceAudioSettingsBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
}

export const VoiceAudioSettingsBottomSheet: React.FC<VoiceAudioSettingsBottomSheetProps> = observer(
	({isOpen, onClose}) => {
		const {i18n} = useLingui();
		useMediaEngineVersion();
		const voiceSettings = VoiceSettings;
		const voiceState = MediaEngine.getCurrentUserVoiceState();
		const isGuildDeafened = voiceState?.deaf ?? false;
		const isDeafened = (voiceState?.self_deaf ?? false) || isGuildDeafened;
		const deafenLabel = isGuildDeafened
			? getVoiceDeafenedByModeratorsStatusLabel(i18n, true)
			: i18n._(isDeafened ? VOICE_UNDEAFEN_DESCRIPTOR : VOICE_DEAFEN_DESCRIPTOR);
		const processingMode = getActiveVoiceProcessingMode(voiceSettings);
		const isCustomMode = processingMode === 'custom';
		const deepFilterEnabled = voiceSettings.deepFilterNoiseSuppression;
		const browserNsEnabled = voiceSettings.noiseSuppression;
		const processingModeLabels: Record<VoiceProcessingMode, string> = {
			voice: i18n._(VOICE_FOCUSED_VOICE_PROFILE_DESCRIPTOR),
			studio: i18n._(VOICE_DIRECT_INPUT_PROFILE_DESCRIPTOR),
			custom: i18n._(CUSTOM_DESCRIPTOR),
		};
		type NoiseSuppressionChoice = 'enhanced' | 'standard' | 'none';
		const noiseSuppressionChoice: NoiseSuppressionChoice = deepFilterEnabled
			? 'enhanced'
			: browserNsEnabled
				? 'standard'
				: 'none';
		const noiseSuppressionLabels: Record<NoiseSuppressionChoice, string> = {
			enhanced: i18n._(ENHANCED_DESCRIPTOR),
			standard: i18n._(STANDARD_DESCRIPTOR),
			none: i18n._(OFF_DESCRIPTOR),
		};
		const cycleNoiseSuppression = () => {
			const order: Array<NoiseSuppressionChoice> = ['enhanced', 'standard', 'none'];
			const next = order[(order.indexOf(noiseSuppressionChoice) + 1) % order.length];
			switch (next) {
				case 'enhanced':
					VoiceSettingsCommands.update({deepFilterNoiseSuppression: true, noiseSuppression: false});
					return;
				case 'standard':
					VoiceSettingsCommands.update({deepFilterNoiseSuppression: false, noiseSuppression: true});
					return;
				case 'none':
					VoiceSettingsCommands.update({deepFilterNoiseSuppression: false, noiseSuppression: false});
					return;
			}
		};
		const handleToggleDeafen = () => {
			VoiceStateCommands.toggleSelfDeaf(null);
			onClose();
		};
		const handleOpenVoiceSettings = () => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<UserSettingsModal
						initialTab="voice_video"
						data-flx="voice.voice-settings-bottom-sheets.handle-open-voice-settings.user-settings-modal"
					/>
				)),
			);
		};
		const menuGroups: Array<MenuGroupType> = [];
		const deviceItems = [
			{
				icon: (
					<InputDeviceIcon
						className={sharedStyles.icon}
						data-flx="voice.voice-settings-bottom-sheets.voice-audio-settings-bottom-sheet.input-device-icon"
					/>
				),
				label: i18n._(VOICE_INPUT_DEVICE_DESCRIPTOR),
				onClick: () => {
					ModalCommands.pushAfterBottomSheetClose(
						onClose,
						modal(() => (
							<UserSettingsModal
								initialTab="voice_video"
								data-flx="voice.voice-settings-bottom-sheets.on-click.user-settings-modal"
							/>
						)),
					);
				},
			},
			{
				icon: (
					<OutputDeviceIcon
						className={sharedStyles.icon}
						data-flx="voice.voice-settings-bottom-sheets.voice-audio-settings-bottom-sheet.output-device-icon"
					/>
				),
				label: i18n._(VOICE_OUTPUT_DEVICE_DESCRIPTOR),
				onClick: () => {
					ModalCommands.pushAfterBottomSheetClose(
						onClose,
						modal(() => (
							<UserSettingsModal
								initialTab="voice_video"
								data-flx="voice.voice-settings-bottom-sheets.on-click.user-settings-modal--2"
							/>
						)),
					);
				},
			},
		];
		menuGroups.push({
			items: deviceItems,
		});
		const volumeItems = [
			{
				label: i18n._(VOICE_INPUT_VOLUME_DESCRIPTOR),
				value: voiceSettings.inputVolume,
				minValue: 0,
				maxValue: VOICE_VOLUME_MAX_PERCENT,
				onChange: (value: number) => {
					VoiceSettingsCommands.update({inputVolume: value});
				},
				onFormat: (value: number) => `${Math.round(value)}%`,
				factoryDefaultValue: 100,
			},
			{
				label: i18n._(VOICE_OUTPUT_VOLUME_DESCRIPTOR),
				value: voiceSettings.outputVolume,
				minValue: 0,
				maxValue: VOICE_VOLUME_MAX_PERCENT,
				onChange: (value: number) => {
					VoiceSettingsCommands.update({outputVolume: value});
				},
				onFormat: (value: number) => `${Math.round(value)}%`,
				factoryDefaultValue: 100,
			},
		];
		menuGroups.push({
			items: volumeItems,
		});
		const processingItems: Array<MenuSheetItem> = [];
		for (const mode of ['voice', 'studio', 'custom'] as const) {
			processingItems.push({
				label: i18n._(INPUT_PROFILE_DESCRIPTOR, {processingModeLabel: processingModeLabels[mode]}),
				selected: processingMode === mode,
				onSelect: () => {
					VoiceSettingsCommands.setActiveInputVoiceProcessingMode(mode);
				},
			});
		}
		if (isCustomMode) {
			processingItems.push({
				icon: (
					<InputDeviceIcon
						className={sharedStyles.icon}
						data-flx="voice.voice-settings-bottom-sheets.voice-audio-settings-bottom-sheet.input-device-icon--3"
					/>
				),
				label: i18n._(NOISE_SUPPRESSION_DESCRIPTOR, {
					noiseSuppressionLabel: noiseSuppressionLabels[noiseSuppressionChoice],
				}),
				onClick: cycleNoiseSuppression,
			});
			processingItems.push({
				icon: (
					<EchoCancellationIcon
						className={sharedStyles.icon}
						data-flx="voice.voice-settings-bottom-sheets.voice-audio-settings-bottom-sheet.echo-cancellation-icon"
					/>
				),
				label: i18n._(VOICE_ECHO_CANCELLATION_DESCRIPTOR),
				onClick: () => {
					VoiceSettingsCommands.update({echoCancellation: !voiceSettings.echoCancellation});
				},
			});
			processingItems.push({
				icon: (
					<InputDeviceIcon
						className={sharedStyles.icon}
						data-flx="voice.voice-settings-bottom-sheets.voice-audio-settings-bottom-sheet.input-device-icon--4"
					/>
				),
				label: i18n._(VOICE_AUTOMATIC_GAIN_CONTROL_DESCRIPTOR),
				onClick: () => {
					VoiceSettingsCommands.update({autoGainControl: !voiceSettings.autoGainControl});
				},
			});
		}
		menuGroups.push({
			items: processingItems,
		});
		menuGroups.push({
			items: [
				{
					icon: (
						<DeafenIcon
							className={sharedStyles.icon}
							data-flx="voice.voice-settings-bottom-sheets.voice-audio-settings-bottom-sheet.deafen-icon"
						/>
					),
					label: deafenLabel,
					disabled: isGuildDeafened,
					onClick: handleToggleDeafen,
				},
			],
		});
		menuGroups.push({
			items: [
				{
					icon: (
						<SettingsIcon
							className={sharedStyles.icon}
							data-flx="voice.voice-settings-bottom-sheets.voice-audio-settings-bottom-sheet.settings-icon"
						/>
					),
					label: i18n._(VOICE_SETTINGS_DESCRIPTOR),
					onClick: handleOpenVoiceSettings,
				},
			],
		});
		return (
			<MenuBottomSheet
				isOpen={isOpen}
				onClose={onClose}
				groups={menuGroups}
				data-flx="voice.voice-settings-bottom-sheets.voice-audio-settings-bottom-sheet.menu-bottom-sheet"
			/>
		);
	},
);

interface VoiceCameraSettingsBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
}

export const VoiceCameraSettingsBottomSheet: React.FC<VoiceCameraSettingsBottomSheetProps> = observer(
	({isOpen, onClose}) => {
		const {i18n} = useLingui();
		const handlePreviewCamera = () => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<CameraPreviewModalInRoom data-flx="voice.voice-settings-bottom-sheets.handle-preview-camera.camera-preview-modal-in-room" />
				)),
			);
		};
		const handleOpenVideoSettings = () => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<UserSettingsModal
						initialTab="voice_video"
						data-flx="voice.voice-settings-bottom-sheets.handle-open-video-settings.user-settings-modal"
					/>
				)),
			);
		};
		const menuGroups: Array<MenuGroupType> = [];
		const cameraItems = [
			{
				icon: (
					<VideoSettingsIcon
						className={sharedStyles.icon}
						data-flx="voice.voice-settings-bottom-sheets.voice-camera-settings-bottom-sheet.video-settings-icon"
					/>
				),
				label: i18n._(CAMERA_DEVICE_DESCRIPTOR),
				onClick: () => {
					ModalCommands.pushAfterBottomSheetClose(
						onClose,
						modal(() => (
							<UserSettingsModal
								initialTab="voice_video"
								data-flx="voice.voice-settings-bottom-sheets.on-click.user-settings-modal--3"
							/>
						)),
					);
				},
			},
		];
		menuGroups.push({
			items: cameraItems,
		});
		const cameraActions = [
			{
				icon: (
					<VideoSettingsIcon
						className={sharedStyles.icon}
						data-flx="voice.voice-settings-bottom-sheets.voice-camera-settings-bottom-sheet.video-settings-icon--2"
					/>
				),
				label: i18n._(PREVIEW_CAMERA_DESCRIPTOR),
				onClick: handlePreviewCamera,
			},
		];
		menuGroups.push({
			items: cameraActions,
		});
		menuGroups.push({
			items: [
				{
					icon: (
						<SettingsIcon
							className={sharedStyles.icon}
							data-flx="voice.voice-settings-bottom-sheets.voice-camera-settings-bottom-sheet.settings-icon"
						/>
					),
					label: i18n._(VIDEO_SETTINGS_DESCRIPTOR),
					onClick: handleOpenVideoSettings,
				},
			],
		});
		return (
			<MenuBottomSheet
				isOpen={isOpen}
				onClose={onClose}
				groups={menuGroups}
				data-flx="voice.voice-settings-bottom-sheets.voice-camera-settings-bottom-sheet.menu-bottom-sheet"
			/>
		);
	},
);

interface VoiceMoreOptionsBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
}

export const VoiceMoreOptionsBottomSheet: React.FC<VoiceMoreOptionsBottomSheetProps> = observer(({isOpen, onClose}) => {
	const {i18n} = useLingui();
	const voiceSettings = VoiceSettings;
	const layoutMode = VoiceCallLayout.layoutMode;
	const isGrid = layoutMode === 'grid';
	const handleToggleGrid = () => {
		if (isGrid) VoiceCallLayoutCommands.setLayoutMode('focus');
		else VoiceCallLayoutCommands.setLayoutMode('grid');
	};
	const menuGroups: Array<MenuGroupType> = [];
	const displayItems = [
		{
			icon: (
				<GridViewIcon
					className={sharedStyles.icon}
					data-flx="voice.voice-settings-bottom-sheets.voice-more-options-bottom-sheet.grid-view-icon"
				/>
			),
			label: i18n._(GRID_VIEW_DESCRIPTOR),
			onClick: () => {
				handleToggleGrid();
				onClose();
			},
		},
		{
			icon: (
				<MembersIcon
					className={sharedStyles.icon}
					data-flx="voice.voice-settings-bottom-sheets.voice-more-options-bottom-sheet.members-icon"
				/>
			),
			label: i18n._(SHOW_MY_OWN_CAMERA_DESCRIPTOR),
			onClick: () => {
				VoiceSettingsCommands.update({showMyOwnCamera: !voiceSettings.showMyOwnCamera});
			},
		},
		{
			icon: (
				<MembersIcon
					className={sharedStyles.icon}
					data-flx="voice.voice-settings-bottom-sheets.voice-more-options-bottom-sheet.members-icon--2"
				/>
			),
			label: i18n._(SHOW_NON_VIDEO_PARTICIPANTS_DESCRIPTOR),
			onClick: () => {
				VoiceSettingsCommands.update({showNonVideoParticipants: !voiceSettings.showNonVideoParticipants});
			},
		},
	];
	menuGroups.push({
		items: displayItems,
	});
	menuGroups.push({
		items: [
			{
				icon: (
					<SettingsIcon
						className={sharedStyles.icon}
						data-flx="voice.voice-settings-bottom-sheets.voice-more-options-bottom-sheet.settings-icon"
					/>
				),
				label: getVoiceVideoSettingsLabel(i18n),
				onClick: () => {
					ModalCommands.pushAfterBottomSheetClose(
						onClose,
						modal(() => (
							<UserSettingsModal
								initialTab="voice_video"
								data-flx="voice.voice-settings-bottom-sheets.on-click.user-settings-modal--4"
							/>
						)),
					);
				},
			},
		],
	});
	return (
		<MenuBottomSheet
			isOpen={isOpen}
			onClose={onClose}
			groups={menuGroups}
			data-flx="voice.voice-settings-bottom-sheets.voice-more-options-bottom-sheet.menu-bottom-sheet"
		/>
	);
});
