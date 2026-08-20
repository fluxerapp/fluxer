// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR, Slider} from '@app/features/ui/components/Slider';
import {canResetSliderValue, SliderResetIconButton} from '@app/features/ui/components/slider/SliderResetIconButton';
import {RadioGroup, type RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {CompactComboboxRow} from '@app/features/user/components/modals/tabs/components/CompactComboboxRow';
import * as VoiceSettingsCommands from '@app/features/voice/commands/VoiceSettingsCommands';
import styles from '@app/features/voice/components/VoiceConnectionStatus.module.css';
import {
	type NoiseSuppressionMethod,
	resolveNoiseSuppressionMethod,
} from '@app/features/voice/components/voice_connection_status/shared';
import VoiceSettings from '@app/features/voice/state/VoiceSettings';
import {
	VOICE_AUTOMATIC_GAIN_CONTROL_DESCRIPTOR,
	VOICE_DIRECT_INPUT_PROFILE_DESCRIPTOR,
	VOICE_ECHO_CANCELLATION_DESCRIPTOR,
	VOICE_FOCUSED_VOICE_PROFILE_DESCRIPTOR,
	VOICE_NOISE_SUPPRESSION_DESCRIPTOR,
} from '@app/features/voice/utils/VoiceMessageDescriptors';
import {getActiveVoiceProcessingMode, type VoiceProcessingMode} from '@app/features/voice/utils/VoiceProcessingProfile';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const FOCUSED_VOICE_OPTION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Cleans up your mic for clear speech.',
	comment: 'Description for the focused-voice option in the voice processing settings radio group.',
});
const DIRECT_INPUT_OPTION_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Sends raw mic audio with no processing.',
	comment: 'Description for the studio / direct-input option in the voice processing settings radio group.',
});
const CUSTOM_DESCRIPTOR = msg({
	message: 'Custom',
	comment: 'Voice input processing profile where the user configures processing manually.',
	context: 'voice-processing-profile',
});
const TUNE_THE_PROCESSING_YOURSELF_DESCRIPTOR = msg({
	message: 'Tune the processing yourself.',
	comment: 'Description for the custom option in the voice processing settings radio group.',
});
const NOISE_SUPPRESSION_ENHANCED_DESCRIPTOR = msg({
	message: 'Enhanced',
	comment: 'Noise suppression option using the enhanced neural filter (DeepFilterNet3). Keep it concise.',
});
const NOISE_SUPPRESSION_STANDARD_DESCRIPTOR = msg({
	message: 'Standard',
	comment: 'Noise suppression option using the browser built-in engine. Keep it concise.',
});
const NONE_DESCRIPTOR = msg({
	message: 'None',
	comment: 'Noise suppression option that disables suppression.',
	context: 'noise-suppression-option',
});
const AUDIO_PROCESSING_DESCRIPTOR = msg({
	message: 'Audio processing',
	comment: 'Voice settings modal title for microphone processing options.',
});
const VOICE_PROCESSING_DESCRIPTOR = msg({
	message: 'Voice processing',
	comment: 'Voice settings radio group label for microphone processing profile.',
});
const STOPS_YOUR_SPEAKERS_FROM_LOOPING_BACK_INTO_YOUR_DESCRIPTOR = msg({
	message: 'Stops your speakers from looping back into your mic.',
	comment: 'Description for the echo cancellation toggle in the custom voice processing settings.',
});
const AUTO_GAIN_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Evens out your mic volume so you are not too quiet.',
	comment: 'Description for the automatic gain control toggle in the voice processing settings.',
});
const DEEP_FILTER_STRENGTH_DESCRIPTOR = msg({
	message: 'Suppression strength',
	comment: 'Label for the DeepFilterNet3 noise suppression strength slider in the custom voice processing settings.',
});
const DEEP_FILTER_STRENGTH_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Higher values remove more noise but can affect speech texture.',
	comment: 'Description for the DeepFilterNet3 noise suppression strength slider.',
});
export const AudioProcessingModal = observer(() => {
	const {i18n} = useLingui();
	const mode = getActiveVoiceProcessingMode(VoiceSettings);
	const deepFilterEnabled = VoiceSettings.deepFilterNoiseSuppression;
	const deepFilterNoiseReductionLevel = VoiceSettings.deepFilterNoiseSuppressionLevel;
	const deepFilterDefaultNoiseReductionLevel = 80;
	const browserNsEnabled = VoiceSettings.noiseSuppression;
	const method = resolveNoiseSuppressionMethod(deepFilterEnabled, browserNsEnabled);
	const modeOptions: Array<RadioOption<VoiceProcessingMode>> = [
		{
			value: 'voice',
			name: i18n._(VOICE_FOCUSED_VOICE_PROFILE_DESCRIPTOR),
			desc: i18n._(FOCUSED_VOICE_OPTION_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: 'studio',
			name: i18n._(VOICE_DIRECT_INPUT_PROFILE_DESCRIPTOR),
			desc: i18n._(DIRECT_INPUT_OPTION_DESCRIPTION_DESCRIPTOR),
		},
		{
			value: 'custom',
			name: i18n._(CUSTOM_DESCRIPTOR),
			desc: i18n._(TUNE_THE_PROCESSING_YOURSELF_DESCRIPTOR),
		},
	];
	const noiseSuppressionOptions: Array<ComboboxOption<NoiseSuppressionMethod>> = [
		{
			value: 'enhanced',
			label: i18n._(NOISE_SUPPRESSION_ENHANCED_DESCRIPTOR),
		},
		{
			value: 'standard',
			label: i18n._(NOISE_SUPPRESSION_STANDARD_DESCRIPTOR),
		},
		{
			value: 'none',
			label: i18n._(NONE_DESCRIPTOR),
		},
	];
	const setNoiseSuppressionMethod = (next: NoiseSuppressionMethod) => {
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
	return (
		<Modal.Root
			size="small"
			centered
			onClose={ModalCommands.pop}
			data-flx="voice.voice-connection-status.audio-processing-modal.modal-root"
		>
			<Modal.Header
				title={i18n._(AUDIO_PROCESSING_DESCRIPTOR)}
				data-flx="voice.voice-connection-status.audio-processing-modal.modal-header"
			/>
			<Modal.Content data-flx="voice.voice-connection-status.audio-processing-modal.modal-content">
				<div
					className={styles.nsModalContent}
					data-flx="voice.voice-connection-status.audio-processing-modal.ns-modal-content"
				>
					<RadioGroup
						options={modeOptions}
						value={mode}
						onChange={(value) => VoiceSettingsCommands.setActiveInputVoiceProcessingMode(value)}
						aria-label={i18n._(VOICE_PROCESSING_DESCRIPTOR)}
						data-flx="voice.voice-connection-status.audio-processing-modal.radio-group.update"
					/>
					{mode === 'custom' && (
						<div
							className={styles.nsOptions}
							data-flx="voice.voice-connection-status.audio-processing-modal.ns-options"
						>
							<CompactComboboxRow<NoiseSuppressionMethod>
								label={i18n._(VOICE_NOISE_SUPPRESSION_DESCRIPTOR)}
								value={method}
								options={noiseSuppressionOptions}
								onChange={setNoiseSuppressionMethod}
								isSearchable={false}
								controlWidth="small"
								dataFlx="voice.voice-connection-status.audio-processing-modal.select.set-noise-suppression-method"
								data-flx="voice.voice-connection-status.audio-processing-modal.compact-select-row.set-noise-suppression-method"
							/>
							<Switch
								label={i18n._(VOICE_ECHO_CANCELLATION_DESCRIPTOR)}
								description={i18n._(STOPS_YOUR_SPEAKERS_FROM_LOOPING_BACK_INTO_YOUR_DESCRIPTOR)}
								value={VoiceSettings.echoCancellation}
								onChange={(checked) => VoiceSettingsCommands.update({echoCancellation: checked})}
								compact
								data-flx="voice.voice-connection-status.audio-processing-modal.switch.update"
							/>
							<Switch
								label={i18n._(VOICE_AUTOMATIC_GAIN_CONTROL_DESCRIPTOR)}
								description={i18n._(AUTO_GAIN_DESCRIPTION_DESCRIPTOR)}
								value={VoiceSettings.autoGainControl}
								onChange={(checked) => VoiceSettingsCommands.update({autoGainControl: checked})}
								compact
								data-flx="voice.voice-connection-status.audio-processing-modal.switch.update--2"
							/>
							{deepFilterEnabled && (
								<div
									className={styles.nsSliderSection}
									data-flx="voice.voice-connection-status.audio-processing-modal.ns-slider-section"
								>
									<div
										className={styles.nsSliderTitleRow}
										data-flx="voice.voice-connection-status.audio-processing-modal.ns-slider-title-row"
									>
										<div
											className={styles.nsSliderLabel}
											data-flx="voice.voice-connection-status.audio-processing-modal.ns-slider-label"
										>
											{i18n._(DEEP_FILTER_STRENGTH_DESCRIPTOR)}
										</div>
										<SliderResetIconButton
											canReset={canResetSliderValue(
												deepFilterNoiseReductionLevel,
												deepFilterDefaultNoiseReductionLevel,
											)}
											onReset={() =>
												VoiceSettingsCommands.update({
													deepFilterNoiseSuppressionLevel: deepFilterDefaultNoiseReductionLevel,
												})
											}
											ariaLabel={i18n._(RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR)}
											dataFlx="voice.voice-connection-status.audio-processing-modal.reset-button.deep-filter-strength"
											data-flx="voice.voice-connection-status.audio-processing-modal.slider-reset-icon-button"
										/>
									</div>
									<div
										className={styles.nsSliderLabel}
										data-flx="voice.voice-connection-status.audio-processing-modal.ns-slider-description"
									>
										{i18n._(DEEP_FILTER_STRENGTH_DESCRIPTION_DESCRIPTOR)}
									</div>
									<Slider
										defaultValue={deepFilterNoiseReductionLevel}
										factoryDefaultValue={deepFilterDefaultNoiseReductionLevel}
										value={deepFilterNoiseReductionLevel}
										minValue={0}
										maxValue={100}
										step={1}
										onValueChange={(value) => {
											VoiceSettingsCommands.update({deepFilterNoiseSuppressionLevel: value});
										}}
										ariaLabel={i18n._(DEEP_FILTER_STRENGTH_DESCRIPTOR)}
										ariaValueText={`${Math.round(deepFilterNoiseReductionLevel)}%`}
										data-flx="voice.voice-connection-status.audio-processing-modal.deep-filter-strength-slider"
									/>
								</div>
							)}
						</div>
					)}
				</div>
			</Modal.Content>
		</Modal.Root>
	);
});
