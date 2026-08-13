// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Combobox, type ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {SwitchGroup, SwitchGroupItem} from '@app/features/ui/components/SwitchGroup';
import PiP from '@app/features/ui/state/PiP';
import {getElectronAPI, isDesktop} from '@app/features/ui/utils/NativeUtils';
import styles from '@app/features/user/components/modals/tabs/AdvancedSettingsTab.module.css';
import {CodecSelectorSection} from '@app/features/user/components/modals/tabs/components/CodecSelectorSection';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import PrivacyPreferences from '@app/features/user/state/PrivacyPreferences';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import VoiceSettings, {DEFAULT_SCREEN_SHARE_MAX_BITRATE_MBPS} from '@app/features/voice/state/VoiceSettings';
import type {
	CodecPreference,
	ScreenShareBackupCodecMode,
	ScreenShareContentHint,
	ScreenShareEncoderMode,
	ScreenShareScalabilityModePreference,
	ScreenShareSoftwareQuality,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {setOpenH264Enabled} from '@app/features/voice/utils/OpenH264Status';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {GearIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect} from 'react';

const OPENH264_LABEL_DESCRIPTOR = msg({
	message: 'OpenH264 Video Codec provided by Cisco Systems, Inc.',
	comment:
		'Switch label for the OpenH264 codec toggle. "OpenH264" is a product name and "Cisco Systems, Inc." is a company name; do not translate either.',
});
const PAUSE_PREVIEW_BACKGROUND_DESCRIPTOR = msg({
	message: 'Pause my screen share preview in the background',
	comment: 'Short label for an advanced screen-share preview preference.',
});
const DISABLE_SCREEN_SHARE_POPOUT_DESCRIPTOR = msg({
	message: 'Disable picture-in-picture popout for screen shares',
	comment: 'Short label for an advanced screen-share preview preference.',
});
const HIDE_STREAM_PREVIEW_THUMBNAIL_DESCRIPTOR = msg({
	message: 'Hide my stream preview thumbnail',
	comment: 'Short label for an advanced stream privacy preference.',
});
const ENCODER_PATH_DESCRIPTOR = msg({
	message: 'Encoder path',
	comment: 'Label for an advanced screen-share select. Refers to hardware or software video encoding.',
});
const ENCODER_PATH_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Encoder preference for new screen shares.',
	comment: 'Description for the encoder path select.',
});
const ENCODER_PATH_AUTO_DESCRIPTOR = msg({
	message: 'Automatic',
	comment: 'Option label for an encoder path select.',
});
const ENCODER_PATH_HARDWARE_DESCRIPTOR = msg({
	message: 'Prefer hardware',
	comment: 'Option label for an encoder path select. Refers to hardware video encoders.',
});
const ENCODER_PATH_SOFTWARE_DESCRIPTOR = msg({
	message: 'Prefer software',
	comment: 'Option label for an encoder path select. Refers to software video encoders.',
});
const SOFTWARE_QUALITY_DESCRIPTOR = msg({
	message: 'Software encoder quality',
	comment: 'Label for an advanced screen-share select. Refers to software video encoder quality bias.',
});
const SOFTWARE_QUALITY_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Higher quality can cost more CPU and latency.',
	comment: 'Description for a software encoder quality select. Keep AV1 and CPU literal.',
});
const SOFTWARE_QUALITY_REALTIME_DESCRIPTOR = msg({
	message: 'Realtime',
	comment: 'Option label for a software encoder quality select. Means fastest/lower-latency.',
});
const SOFTWARE_QUALITY_BALANCED_DESCRIPTOR = msg({
	message: 'Balanced',
	comment: 'Option label for a software encoder quality select.',
});
const SOFTWARE_QUALITY_QUALITY_DESCRIPTOR = msg({
	message: 'Quality',
	comment: 'Option label for a software encoder quality select. Means slower/higher-quality.',
});
const SVC_MODE_DESCRIPTOR = msg({
	message: 'SVC mode',
	comment: 'Label for an advanced screen-share select. SVC is a WebRTC acronym and should stay uppercase.',
});
const SVC_MODE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Layering for AV1 and VP9 when available.',
	comment: 'Description for an SVC mode select. Keep SVC and AV1 literal.',
});
const SVC_MODE_AUTO_DESCRIPTOR = msg({
	message: 'Automatic',
	comment: 'Option label for an SVC mode select.',
});
const SVC_MODE_SINGLE_LAYER_DESCRIPTOR = msg({
	message: 'Single layer',
	comment: 'Option label for an SVC mode select.',
});
const SVC_MODE_TEMPORAL_DESCRIPTOR = msg({
	message: 'Temporal layers',
	comment: 'Option label for an SVC mode select.',
});
const SVC_MODE_SPATIAL_DESCRIPTOR = msg({
	message: 'Spatial and temporal layers',
	comment: 'Option label for an SVC mode select.',
});
const BACKUP_CODEC_DESCRIPTOR = msg({
	message: 'H.264 backup stream',
	comment: 'Label for an advanced screen-share select. H.264 is a codec name and should stay literal.',
});
const BACKUP_CODEC_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Adds H.264 for mixed clients. Uses more encode work.',
	comment: 'Description for an H.264 backup stream select. Keep H.264, CPU, and GPU literal.',
});
const BACKUP_CODEC_OFF_DESCRIPTOR = msg({
	message: 'Off',
	comment: 'Option label for an H.264 backup stream select.',
});
const BACKUP_CODEC_H264_SIMULCAST_DESCRIPTOR = msg({
	message: 'H.264 simulcast backup',
	comment: 'Option label for an H.264 backup stream select. H.264 is a codec name and should stay literal.',
});
const CONTENT_HINT_DESCRIPTOR = msg({
	message: 'Content hint',
	comment: 'Label for an advanced screen-share select. Refers to the WebRTC MediaStreamTrack contentHint value.',
});
const CONTENT_HINT_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Helps the browser choose motion, detail, or text handling.',
	comment: 'Description for a WebRTC content hint select. Keep Motion, Detail, and text as plain option concepts.',
});
const CONTENT_HINT_AUTO_DESCRIPTOR = msg({
	message: 'Automatic',
	comment: 'Option label for a WebRTC content hint select.',
});
const CONTENT_HINT_MOTION_DESCRIPTOR = msg({
	message: 'Motion',
	comment: 'Option label for a WebRTC content hint select.',
});
const CONTENT_HINT_DETAIL_DESCRIPTOR = msg({
	message: 'Detail',
	comment: 'Option label for a WebRTC content hint select.',
});
const CONTENT_HINT_TEXT_DESCRIPTOR = msg({
	message: 'Text',
	comment: 'Option label for a WebRTC content hint select.',
});
const MAX_BITRATE_DESCRIPTOR = msg({
	message: 'Maximum screen share bitrate',
	comment: 'Label for an advanced screen-share bitrate select.',
});
const MAX_BITRATE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Upper bitrate cap before WebRTC adapts to the network.',
	comment: 'Description for the maximum screen share bitrate select. Keep WebRTC literal.',
});
const CONFIGURE_DESCRIPTOR = msg({
	message: 'Configure',
	comment: 'Button label that opens a dedicated advanced settings modal for configuring screen-share encoder controls.',
});
const MBPS_DESCRIPTOR = msg({
	message: '{megabits} Mbps',
	comment: 'Screen share bitrate option label. Mbps means megabits per second.',
});

const DECODE_CODEC_CAP_DESCRIPTOR = msg({
	message: 'Emulate decode codec (testing)',
	comment: 'Label for an advanced select that caps the best video codec this client advertises it can decode.',
});
const DECODE_CODEC_CAP_AUTO_DESCRIPTOR = msg({
	message: 'Automatic',
	comment: 'Option label for the emulated decode codec select. Means advertise true decode support.',
});

const SCREEN_SHARE_MAX_BITRATE_OPTIONS = [1, 5, 10, 15, 25, 50] as const;

const getNearestNumericOptionValue = <V extends number>(
	value: number,
	options: ReadonlyArray<ComboboxOption<V>>,
): V | undefined => {
	let nearestOption: ComboboxOption<V> | undefined;
	let nearestDistance = Number.POSITIVE_INFINITY;
	for (const option of options) {
		const distance = Math.abs(option.value - value);
		if (distance < nearestDistance) {
			nearestDistance = distance;
			nearestOption = option;
		}
	}
	return nearestOption?.value;
};

const resolveScreenShareBitrateInput = (
	inputValue: string,
	options: ReadonlyArray<ComboboxOption<number>>,
): number | undefined => {
	const normalized = inputValue.trim().toLowerCase();
	if (!normalized) return undefined;
	const numericMatch = normalized.match(/([0-9]+(?:\.[0-9]+)?)/);
	if (!numericMatch) return undefined;
	const parsedValue = Number(numericMatch[1]);
	if (!Number.isFinite(parsedValue)) return undefined;
	const valueInMbps = normalized.includes('kbps') ? parsedValue / 1000 : parsedValue;
	return getNearestNumericOptionValue(valueInMbps, options);
};

export const ScreenShareCodecControl = observer(() => (
	<CodecSelectorSection data-flx="user.advanced-settings-tab.advanced-video-controls.screen-share-codec-control.codec-selector-section" />
));

export const EmulatedDecodeCodecCapControl = observer(() => {
	const {i18n} = useLingui();
	const options: ReadonlyArray<ComboboxOption<CodecPreference>> = [
		{value: 'auto', label: i18n._(DECODE_CODEC_CAP_AUTO_DESCRIPTOR)},
		{value: 'av1', label: 'AV1'},
		{value: 'h265', label: 'H.265'},
		{value: 'vp9', label: 'VP9'},
		{value: 'h264', label: 'H.264'},
		{value: 'vp8', label: 'VP8'},
	];
	return (
		<Combobox<CodecPreference, false>
			value={VoiceSettings.emulatedDecodeVideoCodecCap}
			options={options}
			onChange={(value) => VoiceSettingsCommands.update({emulatedDecodeVideoCodecCap: value})}
			density="compact"
			isSearchable={false}
			aria-label={i18n._(DECODE_CODEC_CAP_DESCRIPTOR)}
			data-flx="user.advanced-settings-tab.select.emulated-decode-codec-cap"
		/>
	);
});

export const OpenH264Control = observer(() => {
	const {i18n} = useLingui();
	const handleChange = useCallback((value: boolean) => {
		VoiceSettingsCommands.update({openH264Enabled: value});
		void setOpenH264Enabled(value);
	}, []);
	if (!isDesktop() || getElectronAPI()?.platform !== 'linux') return null;
	return (
		<Switch
			ariaLabel={i18n._(OPENH264_LABEL_DESCRIPTOR)}
			value={VoiceSettings.openH264Enabled}
			onChange={handleChange}
			compact
			data-flx="user.advanced-settings-tab.switch.openh264"
		/>
	);
});

export const ScreenSharePreviewBehaviorControl = observer(() => {
	const {i18n} = useLingui();
	const handleDisableScreenSharePopoutToggle = useCallback((value: boolean) => {
		VoiceSettingsCommands.update({disablePictureInPicturePopoutScreenShare: value});
		if (!value) {
			PiP.setSessionDisable(false);
		}
	}, []);
	return (
		<SwitchGroup data-flx="user.advanced-settings-tab.switch-group.screen-share-preview-behavior">
			<SwitchGroupItem
				label={i18n._(PAUSE_PREVIEW_BACKGROUND_DESCRIPTOR)}
				value={VoiceSettings.pauseOwnScreenSharePreviewOnUnfocus}
				onChange={(value) => VoiceSettingsCommands.update({pauseOwnScreenSharePreviewOnUnfocus: value})}
				data-flx="user.advanced-settings-tab.switch-group-item.pause-screen-share-preview"
			/>
			<SwitchGroupItem
				label={i18n._(DISABLE_SCREEN_SHARE_POPOUT_DESCRIPTOR)}
				value={VoiceSettings.disablePictureInPicturePopoutScreenShare}
				onChange={handleDisableScreenSharePopoutToggle}
				data-flx="user.advanced-settings-tab.switch-group-item.disable-screen-share-popout"
			/>
			<SwitchGroupItem
				label={i18n._(HIDE_STREAM_PREVIEW_THUMBNAIL_DESCRIPTOR)}
				value={PrivacyPreferences.getDisableStreamPreviews()}
				onChange={PrivacyPreferences.setDisableStreamPreviews}
				data-flx="user.advanced-settings-tab.switch-group-item.disable-stream-previews"
			/>
		</SwitchGroup>
	);
});

const ScreenShareEncoderControlsContent = observer(() => {
	const {i18n} = useLingui();
	const encoderModeOptions: ReadonlyArray<ComboboxOption<ScreenShareEncoderMode>> = [
		{value: 'auto', label: i18n._(ENCODER_PATH_AUTO_DESCRIPTOR)},
		{value: 'hardware', label: i18n._(ENCODER_PATH_HARDWARE_DESCRIPTOR)},
		{value: 'software', label: i18n._(ENCODER_PATH_SOFTWARE_DESCRIPTOR)},
	];
	const softwareQualityOptions: ReadonlyArray<ComboboxOption<ScreenShareSoftwareQuality>> = [
		{value: 'realtime', label: i18n._(SOFTWARE_QUALITY_REALTIME_DESCRIPTOR)},
		{value: 'balanced', label: i18n._(SOFTWARE_QUALITY_BALANCED_DESCRIPTOR)},
		{value: 'quality', label: i18n._(SOFTWARE_QUALITY_QUALITY_DESCRIPTOR)},
	];
	const scalabilityModeOptions: ReadonlyArray<ComboboxOption<ScreenShareScalabilityModePreference>> = [
		{value: 'auto', label: i18n._(SVC_MODE_AUTO_DESCRIPTOR)},
		{value: 'single_layer', label: i18n._(SVC_MODE_SINGLE_LAYER_DESCRIPTOR)},
		{value: 'temporal', label: i18n._(SVC_MODE_TEMPORAL_DESCRIPTOR)},
		{value: 'spatial', label: i18n._(SVC_MODE_SPATIAL_DESCRIPTOR)},
	];
	const backupCodecOptions: ReadonlyArray<ComboboxOption<ScreenShareBackupCodecMode>> = [
		{value: 'off', label: i18n._(BACKUP_CODEC_OFF_DESCRIPTOR)},
		{value: 'h264_simulcast', label: i18n._(BACKUP_CODEC_H264_SIMULCAST_DESCRIPTOR)},
	];
	const contentHintOptions: ReadonlyArray<ComboboxOption<ScreenShareContentHint>> = [
		{value: 'auto', label: i18n._(CONTENT_HINT_AUTO_DESCRIPTOR)},
		{value: 'motion', label: i18n._(CONTENT_HINT_MOTION_DESCRIPTOR)},
		{value: 'detail', label: i18n._(CONTENT_HINT_DETAIL_DESCRIPTOR)},
		{value: 'text', label: i18n._(CONTENT_HINT_TEXT_DESCRIPTOR)},
	];
	const bitrateOptions: ReadonlyArray<ComboboxOption<number>> = SCREEN_SHARE_MAX_BITRATE_OPTIONS.map((megabits) => ({
		value: megabits,
		label: i18n._(MBPS_DESCRIPTOR, {megabits}),
	}));
	const storedMaxBitrate = VoiceSettings.screenShareMaxBitrateMbps;
	const selectedMaxBitrate =
		getNearestNumericOptionValue(storedMaxBitrate, bitrateOptions) ?? DEFAULT_SCREEN_SHARE_MAX_BITRATE_MBPS;
	useEffect(() => {
		if (storedMaxBitrate !== selectedMaxBitrate) {
			VoiceSettingsCommands.update({screenShareMaxBitrateMbps: selectedMaxBitrate});
		}
	}, [selectedMaxBitrate, storedMaxBitrate]);
	return (
		<div className={styles.controlStackCompact} data-flx="user.advanced-settings-tab.screen-share-encoder-controls">
			<CompactComboboxRow<ScreenShareEncoderMode>
				label={i18n._(ENCODER_PATH_DESCRIPTOR)}
				description={i18n._(ENCODER_PATH_DESCRIPTION_DESCRIPTOR)}
				value={VoiceSettings.screenShareEncoderMode}
				options={encoderModeOptions}
				onChange={(value) => VoiceSettingsCommands.update({screenShareEncoderMode: value})}
				isSearchable={false}
				controlWidth="small"
				dataFlx="user.advanced-settings-tab.select.screen-share-encoder-path"
				data-flx="user.advanced-settings-tab.advanced-video-controls.screen-share-encoder-controls-content.compact-combobox-row.update"
			/>
			<CompactComboboxRow<ScreenShareSoftwareQuality>
				label={i18n._(SOFTWARE_QUALITY_DESCRIPTOR)}
				description={i18n._(SOFTWARE_QUALITY_DESCRIPTION_DESCRIPTOR)}
				value={VoiceSettings.screenShareSoftwareQuality}
				options={softwareQualityOptions}
				onChange={(value) => VoiceSettingsCommands.update({screenShareSoftwareQuality: value})}
				isSearchable={false}
				controlWidth="small"
				dataFlx="user.advanced-settings-tab.select.screen-share-software-quality"
				data-flx="user.advanced-settings-tab.advanced-video-controls.screen-share-encoder-controls-content.compact-combobox-row.update--2"
			/>
			<CompactComboboxRow<ScreenShareScalabilityModePreference>
				label={i18n._(SVC_MODE_DESCRIPTOR)}
				description={i18n._(SVC_MODE_DESCRIPTION_DESCRIPTOR)}
				value={VoiceSettings.screenShareScalabilityMode}
				options={scalabilityModeOptions}
				onChange={(value) => VoiceSettingsCommands.update({screenShareScalabilityMode: value})}
				isSearchable={false}
				controlWidth="large"
				dataFlx="user.advanced-settings-tab.select.screen-share-svc-mode"
				data-flx="user.advanced-settings-tab.advanced-video-controls.screen-share-encoder-controls-content.compact-combobox-row.update--3"
			/>
			<CompactComboboxRow<ScreenShareBackupCodecMode>
				label={i18n._(BACKUP_CODEC_DESCRIPTOR)}
				description={i18n._(BACKUP_CODEC_DESCRIPTION_DESCRIPTOR)}
				value={VoiceSettings.screenShareBackupCodecMode}
				options={backupCodecOptions}
				onChange={(value) => VoiceSettingsCommands.update({screenShareBackupCodecMode: value})}
				isSearchable={false}
				controlWidth="large"
				dataFlx="user.advanced-settings-tab.select.screen-share-backup-codec"
				data-flx="user.advanced-settings-tab.advanced-video-controls.screen-share-encoder-controls-content.compact-combobox-row.update--4"
			/>
			<CompactComboboxRow<ScreenShareContentHint>
				label={i18n._(CONTENT_HINT_DESCRIPTOR)}
				description={i18n._(CONTENT_HINT_DESCRIPTION_DESCRIPTOR)}
				value={VoiceSettings.screenShareContentHint}
				options={contentHintOptions}
				onChange={(value) => VoiceSettingsCommands.update({screenShareContentHint: value})}
				isSearchable={false}
				controlWidth="small"
				dataFlx="user.advanced-settings-tab.select.screen-share-content-hint"
				data-flx="user.advanced-settings-tab.advanced-video-controls.screen-share-encoder-controls-content.compact-combobox-row.update--5"
			/>
			<CompactComboboxRow<number>
				label={i18n._(MAX_BITRATE_DESCRIPTOR)}
				description={i18n._(MAX_BITRATE_DESCRIPTION_DESCRIPTOR)}
				value={selectedMaxBitrate}
				options={bitrateOptions}
				onChange={(value) => VoiceSettingsCommands.update({screenShareMaxBitrateMbps: value})}
				autoSelectValueFromInput={resolveScreenShareBitrateInput}
				controlWidth="small"
				dataFlx="user.advanced-settings-tab.select.screen-share-max-bitrate"
				data-flx="user.advanced-settings-tab.advanced-video-controls.screen-share-encoder-controls-content.compact-combobox-row.update--6"
			/>
		</div>
	);
});

interface ScreenShareEncoderControlsModalProps {
	title: string;
}

const ScreenShareEncoderControlsModal = observer(({title}: ScreenShareEncoderControlsModalProps) => {
	const handleClose = useCallback(() => {
		ModalCommands.pop();
	}, []);
	return (
		<Modal.Root
			size="medium"
			onClose={handleClose}
			data-flx="user.advanced-settings-tab.screen-share-encoder-controls-modal.modal-root"
		>
			<Modal.Header
				title={title}
				onClose={handleClose}
				data-flx="user.advanced-settings-tab.screen-share-encoder-controls-modal.modal-header"
			/>
			<Modal.Content data-flx="user.advanced-settings-tab.screen-share-encoder-controls-modal.modal-content">
				<Modal.ContentLayout data-flx="user.advanced-settings-tab.screen-share-encoder-controls-modal.modal-content-layout">
					<ScreenShareEncoderControlsContent data-flx="user.advanced-settings-tab.advanced-video-controls.screen-share-encoder-controls-modal.screen-share-encoder-controls-content" />
				</Modal.ContentLayout>
			</Modal.Content>
		</Modal.Root>
	);
});

interface ScreenShareEncoderControlsProps {
	title: string;
}

export const ScreenShareEncoderControls = observer(({title}: ScreenShareEncoderControlsProps) => {
	const {i18n} = useLingui();
	const handleOpen = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ScreenShareEncoderControlsModal
					title={title}
					data-flx="user.advanced-settings-tab.advanced-video-controls.handle-open.screen-share-encoder-controls-modal"
				/>
			)),
		);
	}, [title]);
	return (
		<Button
			variant="secondary"
			compact
			leftIcon={
				<GearIcon
					size={remFromPx(14)}
					weight="bold"
					data-flx="user.advanced-settings-tab.screen-share-encoder-controls.gear-icon"
				/>
			}
			onClick={handleOpen}
			data-flx="user.advanced-settings-tab.screen-share-encoder-controls.configure-button"
		>
			{i18n._(CONFIGURE_DESCRIPTOR)}
		</Button>
	);
});
