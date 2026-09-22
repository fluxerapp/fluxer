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
import styles from '@app/features/user/components/modals/tabs/AdvancedSettingsTab.module.css';
import {selectScreenShareEncoderPathDescription} from '@app/features/user/components/modals/tabs/advanced_settings_tab/AdvancedVideoControlsState';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import PrivacyPreferences from '@app/features/user/state/PrivacyPreferences';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import type {
	CodecPreference,
	ScreenShareContentHint,
	ScreenShareEncoderMode,
	ScreenShareScalabilityModePreference,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {
	describeScreenShareEncoderPath,
	getCodecCapabilityReport,
} from '@app/features/voice/utils/CodecCapabilityDetector';
import {
	getGpuEncoderReportSync,
	type HardwareEncodeReport,
	loadGpuEncoderReport,
} from '@app/features/voice/utils/GpuEncoderCapabilities';
import {CODEC_DISPLAY_LABEL} from '@app/features/voice/utils/ScreenShareCodecPolicy';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {GearIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';

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
const CONTENT_HINT_DESCRIPTOR = msg({
	message: 'Content hint',
	comment: 'Label for an advanced screen-share select. Refers to the WebRTC MediaStreamTrack contentHint value.',
});
const CONTENT_HINT_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Used in Custom mode. The Gaming and Screen share presets set their own hint.',
	comment:
		'Description for a WebRTC content hint select. Custom, Gaming and Screen share are the stream quality preset labels.',
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
const CONFIGURE_DESCRIPTOR = msg({
	message: 'Configure',
	comment: 'Button label that opens a dedicated advanced settings modal for configuring screen-share encoder controls.',
});

const PREFERRED_SCREEN_SHARE_CODEC_DESCRIPTOR = msg({
	message: 'Preferred screen share codec',
	comment: 'Accessible label for the advanced screen-share codec preference select.',
});
const AUTOMATIC_CODEC_OPTION_DESCRIPTOR = msg({
	message: 'Automatic ({codec})',
	comment: 'Option label for the screen-share codec select. codec is the automatically selected codec label.',
});
const AV1_REQUIRES_OPT_IN_OPTION_DESCRIPTOR = msg({
	message: 'AV1 (requires opt-in)',
	comment:
		'Option label for the screen-share codec select while the AV1 opt-in is off and the option is disabled. AV1 is a codec name and should stay literal.',
});
const H265_REQUIRES_OPT_IN_OPTION_DESCRIPTOR = msg({
	message: 'H.265 (requires opt-in)',
	comment:
		'Option label for the screen-share codec select while the HEVC opt-in is off and the option is disabled. H.265 is a codec name and should stay literal.',
});
const AV1_SCREEN_SHARE_OPT_IN_DESCRIPTOR = msg({
	message: 'Allow AV1 for screen sharing',
	comment: 'Switch label for the AV1 screen-share opt-in. AV1 is a codec name and should stay literal.',
});
const HEVC_SCREEN_SHARE_OPT_IN_DESCRIPTOR = msg({
	message: 'Allow H.265 (HEVC) for screen sharing',
	comment:
		'Switch label for the H.265/HEVC screen-share opt-in. H.265 and HEVC are codec names and should stay literal.',
});
const SCREEN_SHARE_CODEC_OPTION_ORDER = ['av1', 'h265', 'h264', 'vp9', 'vp8'] as const;

function useLoadedGpuEncoderReport(): HardwareEncodeReport | null {
	const [gpuReport, setGpuReport] = useState(() => getGpuEncoderReportSync());
	useEffect(() => {
		if (gpuReport) return;
		let cancelled = false;
		void loadGpuEncoderReport().then((report) => {
			if (!cancelled) setGpuReport(report);
		});
		return () => {
			cancelled = true;
		};
	}, [gpuReport]);
	return gpuReport;
}

export const ScreenShareCodecControl = observer(() => {
	const {i18n} = useLingui();
	const encoderMode = VoiceSettings.getScreenShareEncoderMode();
	useLoadedGpuEncoderReport();
	const automaticCodec = describeScreenShareEncoderPath(encoderMode).codec;
	const av1OptIn = VoiceSettings.getScreenShareAv1OptIn();
	const hevcOptIn = VoiceSettings.getScreenShareHevcOptIn();
	const codecCapabilities = getCodecCapabilityReport();
	const options: ReadonlyArray<ComboboxOption<CodecPreference>> = [
		{value: 'auto', label: i18n._(AUTOMATIC_CODEC_OPTION_DESCRIPTOR, {codec: CODEC_DISPLAY_LABEL[automaticCodec]})},
		...SCREEN_SHARE_CODEC_OPTION_ORDER.map((codec) => ({
			value: codec,
			label:
				codec === 'av1' && !av1OptIn
					? i18n._(AV1_REQUIRES_OPT_IN_OPTION_DESCRIPTOR)
					: codec === 'h265' && !hevcOptIn
						? i18n._(H265_REQUIRES_OPT_IN_OPTION_DESCRIPTOR)
						: CODEC_DISPLAY_LABEL[codec],
			isDisabled: !codecCapabilities[codec].supported,
		})),
	];
	return (
		<Combobox<CodecPreference, false>
			value={VoiceSettings.getPreferredScreenShareCodec()}
			options={options}
			onChange={(value) => VoiceSettingsCommands.update({preferredScreenShareCodec: value})}
			density="compact"
			isSearchable={false}
			aria-label={i18n._(PREFERRED_SCREEN_SHARE_CODEC_DESCRIPTOR)}
			data-flx="user.advanced-settings-tab.select.preferred-screen-share-codec"
		/>
	);
});

export const ScreenShareAv1OptInControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(AV1_SCREEN_SHARE_OPT_IN_DESCRIPTOR)}
			value={VoiceSettings.getScreenShareAv1OptIn()}
			onChange={(value) => VoiceSettingsCommands.update({screenShareAv1OptIn: value})}
			compact
			data-flx="user.advanced-settings-tab.switch.screen-share-av1-opt-in"
		/>
	);
});

export const ScreenShareHevcOptInControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(HEVC_SCREEN_SHARE_OPT_IN_DESCRIPTOR)}
			value={VoiceSettings.getScreenShareHevcOptIn()}
			onChange={(value) => VoiceSettingsCommands.update({screenShareHevcOptIn: value})}
			compact
			data-flx="user.advanced-settings-tab.switch.screen-share-hevc-opt-in"
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
	useLoadedGpuEncoderReport();
	const {hardwareUnavailable} = describeScreenShareEncoderPath(VoiceSettings.screenShareEncoderMode);
	const encoderModeOptions: ReadonlyArray<ComboboxOption<ScreenShareEncoderMode>> = [
		{value: 'auto', label: i18n._(ENCODER_PATH_AUTO_DESCRIPTOR)},
		{value: 'hardware', label: i18n._(ENCODER_PATH_HARDWARE_DESCRIPTOR)},
		{value: 'software', label: i18n._(ENCODER_PATH_SOFTWARE_DESCRIPTOR)},
	];
	const scalabilityModeOptions: ReadonlyArray<ComboboxOption<ScreenShareScalabilityModePreference>> = [
		{value: 'auto', label: i18n._(SVC_MODE_AUTO_DESCRIPTOR)},
		{value: 'single_layer', label: i18n._(SVC_MODE_SINGLE_LAYER_DESCRIPTOR)},
		{value: 'temporal', label: i18n._(SVC_MODE_TEMPORAL_DESCRIPTOR)},
	];
	const contentHintOptions: ReadonlyArray<ComboboxOption<ScreenShareContentHint>> = [
		{value: 'auto', label: i18n._(CONTENT_HINT_AUTO_DESCRIPTOR)},
		{value: 'motion', label: i18n._(CONTENT_HINT_MOTION_DESCRIPTOR)},
		{value: 'detail', label: i18n._(CONTENT_HINT_DETAIL_DESCRIPTOR)},
		{value: 'text', label: i18n._(CONTENT_HINT_TEXT_DESCRIPTOR)},
	];
	return (
		<div className={styles.controlStackCompact} data-flx="user.advanced-settings-tab.screen-share-encoder-controls">
			<CompactComboboxRow<ScreenShareEncoderMode>
				label={i18n._(ENCODER_PATH_DESCRIPTOR)}
				description={i18n._(selectScreenShareEncoderPathDescription(hardwareUnavailable))}
				value={VoiceSettings.screenShareEncoderMode}
				options={encoderModeOptions}
				onChange={(value) => VoiceSettingsCommands.update({screenShareEncoderMode: value})}
				isSearchable={false}
				controlWidth="small"
				dataFlx="user.advanced-settings-tab.select.screen-share-encoder-path"
				data-flx="user.advanced-settings-tab.advanced-video-controls.screen-share-encoder-controls-content.compact-combobox-row.update"
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
