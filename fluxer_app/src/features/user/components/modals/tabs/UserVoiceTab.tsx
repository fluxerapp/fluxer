// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {SettingsTabSection} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {
	DESKTOP_DOWNLOAD_URL,
	MACOS_INPUT_MONITORING_PERMISSION_NAME,
	MACOS_MICROPHONE_PERMISSION_NAME,
	MACOS_PRIVACY_AND_SECURITY_SETTINGS_NAME,
	MACOS_SYSTEM_SETTINGS_NAME,
	PRODUCT_NAME,
} from '@app/features/app/config/I18nDisplayConstants';
import {KeybindRecorder} from '@app/features/input/components/KeybindRecorder';
import Keybind, {getDefaultKeybind} from '@app/features/input/state/InputKeybind';
import {openMacPermissionsModal} from '@app/features/permissions/system/commands/MacPermissionsModalCommands';
import {MacPermissionsSettingsRow} from '@app/features/permissions/system/components/MacPermissionsSettingsRow';
import NativePermission from '@app/features/permissions/system/state/NativePermission';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR, Slider} from '@app/features/ui/components/Slider';
import {canResetSliderValue, SliderResetIconButton} from '@app/features/ui/components/slider/SliderResetIconButton';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {formatRoundedPercentage} from '@app/features/ui/utils/PercentageFormatting';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import {EntranceSoundSection} from '@app/features/user/components/modals/tabs/components/EntranceSoundSection';
import {MicTestSection} from '@app/features/user/components/modals/tabs/components/MicTestSection';
import {useMediaPermission} from '@app/features/user/components/modals/tabs/hooks/useMediaPermission';
import styles from '@app/features/user/components/modals/tabs/UserVoiceTab.module.css';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import type VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	type ExternalAudioProcessorMatch,
	findExternalProcessorForDevice,
} from '@app/features/voice/utils/ExternalAudioProcessor';
import {buildSettingsDeviceOptions} from '@app/features/voice/utils/SettingsDeviceOptions';
import {hasDeviceLabels, resolveEffectiveDeviceId} from '@app/features/voice/utils/VoiceDeviceManager';
import {
	VOICE_AUTOMATIC_GAIN_CONTROL_DESCRIPTOR,
	VOICE_DIRECT_INPUT_PROFILE_DESCRIPTOR,
	VOICE_ECHO_CANCELLATION_DESCRIPTOR,
	VOICE_FOCUSED_VOICE_PROFILE_DESCRIPTOR,
	VOICE_INPUT_DEVICE_DESCRIPTOR,
	VOICE_INPUT_VOLUME_DESCRIPTOR,
	VOICE_NOISE_SUPPRESSION_DESCRIPTOR,
	VOICE_OUTPUT_DEVICE_DESCRIPTOR,
	VOICE_OUTPUT_VOLUME_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import type {VoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';
import {VOICE_VOLUME_MAX_PERCENT} from '@app/features/voice/utils/VoiceVolumeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo} from 'react';

const RELEASE_DELAY_DESCRIPTOR = msg({
	message: 'Release delay',
	comment: 'Short label for push-to-talk release delay.',
});
const RELEASE_DELAY_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Keep transmitting briefly after release.',
	comment: 'Description for push-to-talk release delay.',
});
const FOCUSED_VOICE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Recommended. Cleans up your mic for clear speech.',
	comment: 'Description for the focused voice profile option in the voice tab.',
});
const DIRECT_INPUT_DESCRIPTION_DESCRIPTOR = msg({
	message: "Sends your audio untouched. Best if you're using external audio software.",
	comment: 'Description for the direct input profile option in the voice tab.',
});
const CUSTOM_DESCRIPTOR = msg({
	message: 'Custom',
	comment: 'Short label in the voice tab. Keep it concise.',
});
const CUSTOM_PROFILE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Adjust each setting yourself: noise suppression, echo cancellation, and gain.',
	comment: 'Description for the custom profile option in the voice tab.',
});
const NOISE_SUPPRESSION_ENHANCED_DESCRIPTOR = msg({
	message: 'Enhanced',
	comment: 'Noise suppression option label in the voice tab (neural filter). Keep it concise.',
});
const NOISE_SUPPRESSION_STANDARD_DESCRIPTOR = msg({
	message: 'Standard',
	comment: 'Noise suppression option label in the voice tab (browser default). Keep it concise.',
});
const NONE_DESCRIPTOR = msg({
	message: 'None',
	comment: 'Short label in the voice tab. Keep it concise.',
});
const PUSH_TO_TALK_LIMITED_DESCRIPTOR = msg({
	message: 'Push-to-talk (limited)',
	comment: 'Short label in the voice tab. Keep it concise.',
});
const DOWNLOAD_DESKTOP_APP_DESCRIPTOR = msg({
	message: 'Download desktop app',
	comment: 'Button or menu action label in the voice tab. Keep it concise.',
});
const I_UNDERSTAND_DESCRIPTOR = msg({
	message: 'I understand',
	comment: 'Short label in the voice tab. Keep it concise.',
});
const PUSH_TO_TALK_DESCRIPTOR = msg({
	message: 'Push-to-talk',
	comment: 'Switch label in the voice tab toggling push-to-talk mode. Keep it concise.',
});
const AUTO_SET_ACTIVITY_THRESHOLD_DESCRIPTOR = msg({
	message: 'Auto-set activity threshold',
	comment: 'Short label in the voice tab. Keep it concise.',
});
const SELECT_VOICE_PROCESSING_DESCRIPTOR = msg({
	message: 'Select voice processing mode',
	comment: 'Aria label for the voice processing radio group in the voice settings tab. Keep it concise.',
});
const ENABLE_PLATFORM_PERMISSION_DESCRIPTOR = msg({
	message: 'Enable {permissionName} permission',
	comment: 'Voice settings CTA label for opening a platform permission prompt.',
});
const PUSH_TO_TALK_BROWSER_LIMITED_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Push-to-talk only works while the browser tab is focused.',
	comment: 'Voice settings warning shown when push-to-talk is enabled in a browser.',
});
const PUSH_TO_TALK_PERMISSION_LIMITED_DESCRIPTION_DESCRIPTOR = msg({
	message:
		'Push-to-talk only works while {productName} is focused until {permissionName} is enabled. After changing the permission, fully quit and restart {productName} so system-wide hotkeys can use it.',
	comment:
		'Voice settings warning shown when macOS Input Monitoring is required for system-wide push-to-talk. {productName} is the app name and {permissionName} is the macOS permission name.',
});
const PRODUCT_NEEDS_MICROPHONE_ACCESS_DESCRIPTOR = msg({
	message: '{productName} needs microphone access to list your devices.',
	comment: 'Voice settings notice shown when microphone permission has not been granted.',
});
const PUSH_TO_TALK_RELEASE_DELAY_OPTIONS = [20, 100, 250, 500, 1000, 1500, 2000] as const;

const getNearestPushToTalkReleaseDelay = (value: number): number => {
	let nearest: number = PUSH_TO_TALK_RELEASE_DELAY_OPTIONS[0];
	let nearestDistance = Number.POSITIVE_INFINITY;
	for (const option of PUSH_TO_TALK_RELEASE_DELAY_OPTIONS) {
		const distance = Math.abs(option - value);
		if (distance < nearestDistance) {
			nearest = option;
			nearestDistance = distance;
		}
	}
	return nearest;
};

const resolvePushToTalkReleaseDelayInput = (
	inputValue: string,
	options: ReadonlyArray<ComboboxOption<number>>,
): number | undefined => {
	const normalized = inputValue.trim().toLowerCase();
	const numericMatch = normalized.match(/([0-9]+(?:\.[0-9]+)?)/);
	if (!numericMatch) return undefined;
	const parsedValue = Number(numericMatch[1]);
	if (!Number.isFinite(parsedValue)) return undefined;
	const unitMatch = normalized.match(/[0-9.]\s*([a-z]+)/);
	const unit = unitMatch?.[1] ?? 'ms';
	const valueInMs = unit.startsWith('s') ? parsedValue * 1000 : parsedValue;
	return options.reduce((nearest, option) =>
		Math.abs(option.value - valueInMs) < Math.abs(nearest.value - valueInMs) ? option : nearest,
	).value;
};
const AUTO_GAIN_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Evens out your mic volume so you are not too quiet.',
	comment: 'Description for the automatic gain control toggle in the voice processing settings.',
});
const INPUT_AND_OUTPUT_DESCRIPTOR = msg({
	message: 'Input and output',
	comment: 'Subsection title in the voice tab. Keep it concise.',
});
const VOLUME_DESCRIPTOR = msg({
	message: 'Volume',
	comment: 'Subsection title in the voice tab. Keep it concise.',
});
const MIC_TEST_DESCRIPTOR = msg({
	message: 'Mic test',
	comment: 'Subsection title in the voice tab. Keep it concise.',
});
const ENTRANCE_SOUND_DESCRIPTOR = msg({
	message: 'Entrance sound',
	comment: 'Subsection title in the voice tab. Keep it concise.',
});
const MACOS_DESCRIPTOR = msg({
	message: 'macOS',
	comment: 'Subsection title in the voice tab for macOS-specific settings. Keep it concise.',
});

type NoiseSuppressionMethod = 'enhanced' | 'standard' | 'none';

interface VoiceTabProps {
	voiceSettings: typeof VoiceSettings;
	hasPremium: boolean;
	autoRequestPermission?: boolean;
}

function resolveNoiseSuppressionMethod(deepFilterEnabled: boolean, browserNsEnabled: boolean): NoiseSuppressionMethod {
	if (deepFilterEnabled) return 'enhanced';
	if (browserNsEnabled) return 'standard';
	return 'none';
}

export const VoiceTab: React.FC<VoiceTabProps> = observer(({voiceSettings, autoRequestPermission = true}) => {
	const {i18n} = useLingui();
	const {
		inputDeviceId,
		outputDeviceId,
		inputVolume,
		outputVolume,
		echoCancellation,
		noiseSuppression,
		autoGainControl,
		deepFilterNoiseSuppression,
		deepFilterNoiseSuppressionLevel,
		vadThreshold,
		vadAutoSensitivity,
	} = voiceSettings;
	const {
		devices,
		deviceState,
		status: permissionStatus,
		requestPermission,
	} = useMediaPermission('audio', {
		autoRequest: autoRequestPermission,
	});
	const isNativeDesktop = NativePermission.isDesktop;
	const isNativeMac = NativePermission.isMacOS;
	const inputMonitoringGranted = NativePermission.isInputMonitoringGranted;
	const inputDevices = useMemo(() => devices.filter((d) => d.kind === 'audioinput'), [devices]);
	const outputDevices = useMemo(() => devices.filter((d) => d.kind === 'audiooutput'), [devices]);
	const transmitMode = Keybind.transmitMode;
	const isPushToTalk = transmitMode === 'voice_push_to_talk';
	const pttPrimary = Keybind.getPrimaryCustomKeybind('voice_push_to_talk');
	const pttCombo = pttPrimary?.combo ?? {key: '', enabled: true, global: true};
	const pttReleaseDelay = Keybind.pushToTalkReleaseDelay;
	const selectedPttReleaseDelay = getNearestPushToTalkReleaseDelay(pttReleaseDelay);
	const pttReleaseDelayOptions: ReadonlyArray<ComboboxOption<number>> = useMemo(
		() => PUSH_TO_TALK_RELEASE_DELAY_OPTIONS.map((value) => ({value, label: `${value}ms`})),
		[],
	);
	const isPttLimited = !isNativeDesktop || (isNativeMac && !inputMonitoringGranted);
	const defaultPttCombo = getDefaultKeybind('voice_push_to_talk', i18n);
	const inputHasLabels = hasDeviceLabels(inputDevices);
	const effectiveInputDeviceId = resolveEffectiveDeviceId(inputDeviceId, inputDevices) ?? 'default';
	const effectiveOutputDeviceId = resolveEffectiveDeviceId(outputDeviceId, outputDevices) ?? 'default';
	const activeInputDevice = inputDevices.find((d) => d.deviceId === effectiveInputDeviceId) ?? null;
	const activeInputLabel = activeInputDevice?.label || null;
	const voiceProcessingMode = voiceSettings.getVoiceProcessingModeForDeviceLabel(activeInputLabel);
	const externalProcessor: ExternalAudioProcessorMatch | null = inputHasLabels
		? findExternalProcessorForDevice(effectiveInputDeviceId, inputDevices)
		: null;
	const showExternalProcessorHint = externalProcessor !== null && voiceProcessingMode !== 'studio';
	useEffect(() => {
		if (pttReleaseDelay !== selectedPttReleaseDelay) Keybind.setPushToTalkReleaseDelay(selectedPttReleaseDelay);
	}, [pttReleaseDelay, selectedPttReleaseDelay]);
	const handleInputDeviceChange = (value: string) => {
		VoiceSettingsCommands.update({inputDeviceId: value});
	};
	const handleVoiceProcessingModeChange = (mode: VoiceProcessingMode) => {
		VoiceSettingsCommands.setActiveInputVoiceProcessingMode(mode);
	};
	const inputDeviceOptions = useMemo(
		() => buildSettingsDeviceOptions(deviceState, 'audioinput', i18n),
		[deviceState, i18n.locale],
	);
	const outputDeviceOptions = useMemo(
		() => buildSettingsDeviceOptions(deviceState, 'audiooutput', i18n),
		[deviceState, i18n.locale],
	);
	const resetSliderLabel = i18n._(RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR);
	const profileOptions: Array<RadioOption<VoiceProcessingMode>> = [
		{
			value: 'voice',
			name: i18n._(VOICE_FOCUSED_VOICE_PROFILE_DESCRIPTOR),
			desc: i18n._(FOCUSED_VOICE_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: 'studio',
			name: i18n._(VOICE_DIRECT_INPUT_PROFILE_DESCRIPTOR),
			desc: i18n._(DIRECT_INPUT_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: 'custom',
			name: i18n._(CUSTOM_DESCRIPTOR),
			desc: i18n._(CUSTOM_PROFILE_DESCRIPTION_DESCRIPTOR),
		},
	];
	const noiseSuppressionMethod = resolveNoiseSuppressionMethod(deepFilterNoiseSuppression, noiseSuppression);
	const noiseSuppressionOptions: Array<ComboboxOption<NoiseSuppressionMethod>> = [
		{value: 'enhanced', label: i18n._(NOISE_SUPPRESSION_ENHANCED_DESCRIPTOR)},
		{value: 'standard', label: i18n._(NOISE_SUPPRESSION_STANDARD_DESCRIPTOR)},
		{value: 'none', label: i18n._(NONE_DESCRIPTOR)},
	];
	const setNoiseSuppressionMethod = (method: NoiseSuppressionMethod) => {
		switch (method) {
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
	const setPushToTalkEnabled = (enabled: boolean) => {
		const mode = enabled ? 'voice_push_to_talk' : 'voice_activity';
		if (enabled && !isNativeDesktop) {
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(PUSH_TO_TALK_LIMITED_DESCRIPTOR)}
						description={
							<p data-flx="user.voice-tab.set-push-to-talk-enabled.p">
								<Trans>
									In a browser, push-to-talk only works while the {PRODUCT_NAME} tab is focused. Install the desktop app
									for system-wide push-to-talk.
								</Trans>
							</p>
						}
						primaryText={i18n._(DOWNLOAD_DESKTOP_APP_DESCRIPTOR)}
						primaryVariant="primary"
						secondaryText={i18n._(I_UNDERSTAND_DESCRIPTOR)}
						onPrimary={() => {
							void openExternalUrl(DESKTOP_DOWNLOAD_URL);
						}}
						onSecondary={() => {
							Keybind.setTransmitMode(mode);
							MediaEngine.handlePushToTalkModeChange();
						}}
						data-flx="user.voice-tab.set-push-to-talk-enabled.confirm-modal"
					/>
				)),
			);
			return;
		}
		if (enabled && isNativeMac && !inputMonitoringGranted) {
			openMacPermissionsModal({focus: 'input-monitoring'});
		}
		if (enabled && NativePermission.isLinuxWaylandDesktop && NativePermission.linuxInputAccessStatus !== 'granted') {
			NativePermission.requestLinuxInputAccessNagbar('push-to-talk');
		}
		Keybind.setTransmitMode(mode);
		MediaEngine.handlePushToTalkModeChange();
	};
	const handleOpenInputMonitoringModal = () => {
		openMacPermissionsModal({focus: 'input-monitoring'});
	};
	const pttAvailable = voiceProcessingMode !== 'studio';
	const showPttDetails = pttAvailable && isPushToTalk;
	const renderPttControls = () => (
		<>
			<Switch
				label={<Trans>Push-to-talk</Trans>}
				value={isPushToTalk}
				onChange={setPushToTalkEnabled}
				ariaLabel={i18n._(PUSH_TO_TALK_DESCRIPTOR)}
				data-flx="user.voice-tab.render-ptt-controls.switch.set-push-to-talk-enabled"
			/>
			{showPttDetails && (
				<div className={styles.pttSettings} data-flx="user.voice-tab.render-ptt-controls.ptt-settings">
					<p className={styles.pttSettingDescription} data-flx="user.voice-tab.render-ptt-controls.ptt-mute-note">
						<Trans>
							While push-to-talk is on, your shortcut controls your microphone, so the mute button is disabled.
						</Trans>
					</p>
					<div className={styles.pttSettingRow} data-flx="user.voice-tab.render-ptt-controls.ptt-setting-row">
						<KeybindRecorder
							label={<Trans>Shortcut</Trans>}
							labelPlacement="inline"
							action="voice_push_to_talk"
							value={pttCombo}
							defaultValue={defaultPttCombo}
							onChange={(combo) => {
								Keybind.setPrimaryCustomKeybindCombo('voice_push_to_talk', {
									...combo,
									global: pttCombo.global ?? true,
								});
							}}
							onReset={() => {
								if (defaultPttCombo) {
									Keybind.setPrimaryCustomKeybindCombo('voice_push_to_talk', {
										...defaultPttCombo,
										global: pttCombo.global ?? true,
									});
								}
							}}
							data-flx="user.voice-tab.render-ptt-controls.keybind-recorder.set-primary-custom-keybind-combo"
						/>
					</div>
					<div className={styles.pttSettingRow} data-flx="user.voice-tab.render-ptt-controls.ptt-setting-row--2">
						<CompactComboboxRow<number>
							label={i18n._(RELEASE_DELAY_DESCRIPTOR)}
							description={i18n._(RELEASE_DELAY_DESCRIPTION_DESCRIPTOR)}
							value={selectedPttReleaseDelay}
							options={pttReleaseDelayOptions}
							onChange={(value) => Keybind.setPushToTalkReleaseDelay(value)}
							autoSelectValueFromInput={resolvePushToTalkReleaseDelayInput}
							controlWidth="small"
							dataFlx="user.voice-tab.render-ptt-controls.select.release-delay"
							data-flx="user.user-voice-tab.render-ptt-controls.compact-combobox-row.set-push-to-talk-release-delay"
						/>
					</div>
				</div>
			)}
			{isPushToTalk && isPttLimited && (
				<WarningAlert
					actions={
						!isNativeDesktop ? (
							<Button
								variant="primary"
								small={true}
								onClick={() => void openExternalUrl(DESKTOP_DOWNLOAD_URL)}
								data-flx="user.voice-tab.render-ptt-controls.button.download-desktop-app"
							>
								{i18n._(DOWNLOAD_DESKTOP_APP_DESCRIPTOR)}
							</Button>
						) : (
							<Button
								variant="primary"
								small={true}
								onClick={handleOpenInputMonitoringModal}
								data-flx="user.voice-tab.render-ptt-controls.button.open-input-monitoring-modal"
							>
								{i18n._(ENABLE_PLATFORM_PERMISSION_DESCRIPTOR, {
									permissionName: MACOS_INPUT_MONITORING_PERMISSION_NAME,
								})}
							</Button>
						)
					}
					data-flx="user.voice-tab.render-ptt-controls.warning-alert"
				>
					{!isNativeDesktop
						? i18n._(PUSH_TO_TALK_BROWSER_LIMITED_DESCRIPTION_DESCRIPTOR)
						: i18n._(PUSH_TO_TALK_PERMISSION_LIMITED_DESCRIPTION_DESCRIPTOR, {
								productName: PRODUCT_NAME,
								permissionName: MACOS_INPUT_MONITORING_PERMISSION_NAME,
							})}
				</WarningAlert>
			)}
		</>
	);
	const renderAutoGainControlSwitch = () => (
		<Switch
			label={i18n._(VOICE_AUTOMATIC_GAIN_CONTROL_DESCRIPTOR)}
			description={i18n._(AUTO_GAIN_DESCRIPTION_DESCRIPTOR)}
			value={autoGainControl}
			onChange={(value) => VoiceSettingsCommands.update({autoGainControl: value})}
			ariaLabel={i18n._(VOICE_AUTOMATIC_GAIN_CONTROL_DESCRIPTOR)}
			data-flx="user.voice-tab.render-auto-gain-control-switch.switch.update-auto-gain-control"
		/>
	);
	const renderCustomProfile = () => (
		<div className={styles.profileSubSection} data-flx="user.voice-tab.render-custom-profile.profile-sub-section">
			{renderPttControls()}
			<Switch
				label={<Trans>Auto-set activity threshold</Trans>}
				value={vadAutoSensitivity}
				onChange={(value) => VoiceSettingsCommands.update({vadAutoSensitivity: value})}
				ariaLabel={i18n._(AUTO_SET_ACTIVITY_THRESHOLD_DESCRIPTOR)}
				data-flx="user.voice-tab.render-custom-profile.switch.update"
			/>
			{!vadAutoSensitivity && (
				<div
					className={styles.sensitivitySliderWrapper}
					data-flx="user.voice-tab.render-custom-profile.sensitivity-slider-wrapper"
				>
					<div className={styles.sliderLabelRow} data-flx="user.voice-tab.render-custom-profile.slider-label-row">
						<div className={styles.sliderLabel} data-flx="user.voice-tab.render-custom-profile.slider-label">
							<Trans>Activity threshold</Trans>
						</div>
						<SliderResetIconButton
							canReset={canResetSliderValue(vadThreshold, 50)}
							onReset={() => VoiceSettingsCommands.update({vadThreshold: 50})}
							ariaLabel={resetSliderLabel}
							dataFlx="user.voice-tab.render-custom-profile.reset-button.vad-threshold"
							data-flx="user.user-voice-tab.render-custom-profile.slider-reset-icon-button"
						/>
					</div>
					<Slider
						value={vadThreshold}
						defaultValue={vadThreshold}
						factoryDefaultValue={50}
						minValue={0}
						maxValue={100}
						step={1}
						onValueRender={formatRoundedPercentage}
						asValueChanges={(value) => VoiceSettingsCommands.update({vadThreshold: value})}
						onValueChange={(value) => VoiceSettingsCommands.update({vadThreshold: value})}
						data-flx="user.voice-tab.render-custom-profile.slider"
					/>
				</div>
			)}
			<CompactComboboxRow<NoiseSuppressionMethod>
				label={i18n._(VOICE_NOISE_SUPPRESSION_DESCRIPTOR)}
				value={noiseSuppressionMethod}
				options={noiseSuppressionOptions}
				onChange={setNoiseSuppressionMethod}
				isSearchable={false}
				controlWidth="medium"
				dataFlx="user.voice-tab.render-custom-profile.select.set-noise-suppression-method"
				data-flx="user.user-voice-tab.render-custom-profile.compact-combobox-row.set-noise-suppression-method"
			/>
			<Switch
				label={i18n._(VOICE_ECHO_CANCELLATION_DESCRIPTOR)}
				value={echoCancellation}
				onChange={(value) => VoiceSettingsCommands.update({echoCancellation: value})}
				ariaLabel={i18n._(VOICE_ECHO_CANCELLATION_DESCRIPTOR)}
				data-flx="user.voice-tab.render-custom-profile.switch.update--2"
			/>
			{renderAutoGainControlSwitch()}
		</div>
	);
	return (
		<>
			<SettingsTabSection title={i18n._(INPUT_AND_OUTPUT_DESCRIPTOR)} data-flx="user.voice-tab.devices-section">
				{devices.length === 0 && permissionStatus !== 'loading' && permissionStatus !== 'granted' ? (
					<div className={styles.deviceNotice} data-flx="user.voice-tab.device-notice">
						<div className={styles.deviceNoticeText} data-flx="user.voice-tab.device-notice-text">
							<div className={styles.deviceNoticeTitle} data-flx="user.voice-tab.device-notice-title">
								<Trans>No microphone found</Trans>
							</div>
							<p className={styles.deviceNoticeDescription} data-flx="user.voice-tab.device-notice-description">
								{permissionStatus === 'denied' ? (
									isNativeDesktop ? (
										<Trans>
											Allow {PRODUCT_NAME} to access your microphone in {MACOS_SYSTEM_SETTINGS_NAME} →{' '}
											{MACOS_PRIVACY_AND_SECURITY_SETTINGS_NAME} → {MACOS_MICROPHONE_PERMISSION_NAME}.
										</Trans>
									) : (
										<Trans>
											Allow {PRODUCT_NAME} to access your microphone. Check your browser's address bar or settings.
										</Trans>
									)
								) : (
									i18n._(PRODUCT_NEEDS_MICROPHONE_ACCESS_DESCRIPTOR, {productName: PRODUCT_NAME})
								)}
							</p>
						</div>
						<Button
							variant="secondary"
							small={true}
							onClick={() => {
								void requestPermission();
							}}
							data-flx="user.voice-tab.button"
						>
							<Trans>Allow microphone</Trans>
						</Button>
					</div>
				) : null}
				<CompactComboboxRow
					label={i18n._(VOICE_INPUT_DEVICE_DESCRIPTOR)}
					value={effectiveInputDeviceId}
					options={inputDeviceOptions}
					onChange={handleInputDeviceChange}
					controlWidth="wide"
					menuMinWidth={280}
					dataFlx="user.voice-tab.select.input-device-change"
					data-flx="user.user-voice-tab.voice-tab.compact-combobox-row.input-device-change"
				/>
				<CompactComboboxRow
					label={i18n._(VOICE_OUTPUT_DEVICE_DESCRIPTOR)}
					value={effectiveOutputDeviceId}
					options={outputDeviceOptions}
					onChange={(value) => VoiceSettingsCommands.update({outputDeviceId: value})}
					controlWidth="wide"
					menuMinWidth={280}
					dataFlx="user.voice-tab.select.update"
					data-flx="user.user-voice-tab.voice-tab.compact-combobox-row.update"
				/>
			</SettingsTabSection>
			<SettingsTabSection title={i18n._(VOLUME_DESCRIPTOR)} data-flx="user.voice-tab.volume-section">
				<div data-flx="user.voice-tab.div--3">
					<div className={styles.sliderLabelRow} data-flx="user.voice-tab.slider-label-row">
						<div className={styles.sliderLabel} data-flx="user.voice-tab.slider-label">
							{i18n._(VOICE_INPUT_VOLUME_DESCRIPTOR)}
						</div>
						<SliderResetIconButton
							canReset={canResetSliderValue(inputVolume, 100)}
							onReset={() => VoiceSettingsCommands.update({inputVolume: 100})}
							ariaLabel={resetSliderLabel}
							dataFlx="user.voice-tab.reset-button.input-volume"
							data-flx="user.user-voice-tab.voice-tab.slider-reset-icon-button"
						/>
					</div>
					<Slider
						value={inputVolume}
						defaultValue={inputVolume}
						factoryDefaultValue={100}
						minValue={0}
						maxValue={VOICE_VOLUME_MAX_PERCENT}
						step={1}
						markers={[0, 50, 100, 150, 200]}
						stickToMarkers={false}
						onMarkerRender={formatRoundedPercentage}
						onValueRender={formatRoundedPercentage}
						onValueChange={(value) => VoiceSettingsCommands.update({inputVolume: value})}
						data-flx="user.voice-tab.slider"
					/>
				</div>
				<div data-flx="user.voice-tab.div--4">
					<div className={styles.sliderLabelRow} data-flx="user.voice-tab.slider-label-row--2">
						<div className={styles.sliderLabel} data-flx="user.voice-tab.slider-label--2">
							{i18n._(VOICE_OUTPUT_VOLUME_DESCRIPTOR)}
						</div>
						<SliderResetIconButton
							canReset={canResetSliderValue(outputVolume, 100)}
							onReset={() => VoiceSettingsCommands.update({outputVolume: 100})}
							ariaLabel={resetSliderLabel}
							dataFlx="user.voice-tab.reset-button.output-volume"
							data-flx="user.user-voice-tab.voice-tab.slider-reset-icon-button--2"
						/>
					</div>
					<Slider
						value={outputVolume}
						defaultValue={outputVolume}
						factoryDefaultValue={100}
						minValue={0}
						maxValue={VOICE_VOLUME_MAX_PERCENT}
						step={1}
						markers={[0, 50, 100, 150, 200]}
						stickToMarkers={false}
						onMarkerRender={formatRoundedPercentage}
						onValueRender={formatRoundedPercentage}
						onValueChange={(value) => VoiceSettingsCommands.update({outputVolume: value})}
						data-flx="user.voice-tab.slider--2"
					/>
				</div>
			</SettingsTabSection>
			<SettingsTabSection title={<Trans>Voice processing</Trans>} data-flx="user.voice-tab.voice-processing-section">
				<div className={styles.audioProcessing} data-flx="user.voice-tab.audio-processing">
					{showExternalProcessorHint && externalProcessor != null && (
						<WarningAlert
							link={{
								label: <Trans>Switch to direct input</Trans>,
								onClick: () => handleVoiceProcessingModeChange('studio'),
							}}
							data-flx="user.voice-tab.warning-alert"
						>
							<Trans>
								Looks like you're using {externalProcessor.name}. Stacking {PRODUCT_NAME}'s processing on top can cause
								pumping or distortion. Direct input passes your audio through untouched.
							</Trans>
						</WarningAlert>
					)}
					<div className={styles.audioProcessingCard} data-flx="user.voice-tab.audio-processing-card">
						<RadioGroup
							options={profileOptions}
							value={voiceProcessingMode}
							onChange={handleVoiceProcessingModeChange}
							aria-label={i18n._(SELECT_VOICE_PROCESSING_DESCRIPTOR)}
							data-flx="user.voice-tab.radio-group.voice-processing-mode-change"
						/>
						{voiceProcessingMode === 'voice' && (
							<div className={styles.profileSubSection} data-flx="user.voice-tab.profile-sub-section">
								{renderPttControls()}
								{renderAutoGainControlSwitch()}
							</div>
						)}
						{voiceProcessingMode === 'studio' && pttCombo?.key && isPushToTalk && (
							<WarningAlert data-flx="user.voice-tab.warning-alert--2">
								<Trans>Push-to-talk is ignored in direct input. Switch to focused voice or custom to use it.</Trans>
							</WarningAlert>
						)}
						{voiceProcessingMode === 'custom' && renderCustomProfile()}
					</div>
				</div>
			</SettingsTabSection>
			<SettingsTabSection title={i18n._(MIC_TEST_DESCRIPTOR)} data-flx="user.voice-tab.mic-test-section-wrapper">
				<MicTestSection
					settings={{
						inputDeviceId: effectiveInputDeviceId,
						outputDeviceId: effectiveOutputDeviceId,
						inputVolume,
						outputVolume,
						echoCancellation,
						noiseSuppression,
						autoGainControl,
						deepFilterNoiseSuppression,
						deepFilterNoiseSuppressionLevel,
						voiceProcessingMode,
					}}
					data-flx="user.voice-tab.mic-test-section"
				/>
			</SettingsTabSection>
			<SettingsTabSection
				title={i18n._(ENTRANCE_SOUND_DESCRIPTOR)}
				data-flx="user.voice-tab.entrance-sound-section-wrapper"
			>
				<EntranceSoundSection data-flx="user.voice-tab.entrance-sound-section" />
			</SettingsTabSection>
			{NativePermission.isNativeMacDesktop && (
				<SettingsTabSection title={i18n._(MACOS_DESCRIPTOR)} data-flx="user.voice-tab.macos-section">
					<MacPermissionsSettingsRow data-flx="user.voice-tab.macos-permissions-row" />
				</SettingsTabSection>
			)}
		</>
	);
});
