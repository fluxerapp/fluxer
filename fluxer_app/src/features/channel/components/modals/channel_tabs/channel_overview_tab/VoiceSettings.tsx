// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/modals/channel_tabs/ChannelOverviewTab.module.css';
import {SettingsControlRow} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/SettingsControlRow';
import {
	BITRATE_KBPS_MARKERS,
	BITRATE_KBPS_MAX,
	BITRATE_KBPS_MIN,
	type FormInputs,
} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/shared';
import {RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR, Slider} from '@app/features/ui/components/Slider';
import {
	VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT,
	VOICE_CHANNEL_CONNECTION_LIMIT_MAX,
	VOICE_CHANNEL_CONNECTION_LIMIT_MIN,
} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {Controller, type UseFormReturn} from 'react-hook-form';

const PARTICIPANT_LIMIT_LABEL_DESCRIPTOR = msg({
	message: 'Participant limit',
	comment: 'Voice channel setting label for the maximum number of members in the voice channel.',
});
const CONNECTION_LIMIT_LABEL_DESCRIPTOR = msg({
	message: 'Connection limit',
	comment: 'Voice channel setting label for the maximum number of active connections per member.',
});
const CONNECTION_LIMIT_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Per member, across all devices.',
	comment: 'Helper text for the voice channel connection limit field.',
});
const VOICE_QUALITY_DESCRIPTOR = msg({
	message: 'Voice quality',
	comment: 'Voice channel setting label for the voice bitrate preset.',
});
const KBPS_DESCRIPTOR = msg({
	message: '{kilobits} kbps',
	comment: 'Voice channel bitrate option label. kbps means kilobits per second.',
});
const PARTICIPANT_LIMIT_VALUE_DESCRIPTOR = msg({
	message: '{count, plural, =0 {∞ No limit} one {# participant} other {# participants}}',
	comment: 'Displayed value for the voice channel participant limit slider. ∞ is the infinity symbol.',
});
const CONNECTION_LIMIT_VALUE_DESCRIPTOR = msg({
	message: '{count, plural, one {# connection} other {# connections}}',
	comment: 'Displayed value for the voice channel connection limit slider.',
});

interface VoiceSettingsProps {
	form: UseFormReturn<FormInputs>;
}

const formatIntegerValue = (value: number): string => String(Math.round(value));

const formatParticipantLimitMarker = (value: number): string => {
	if (value === 0) return '∞';
	return formatIntegerValue(value);
};

const formatConnectionLimitMarker = (value: number): string | null => {
	if (value === VOICE_CHANNEL_CONNECTION_LIMIT_MIN) return null;
	return formatIntegerValue(value);
};

const VoiceBitrateSlider: React.FC<{
	value: number | undefined;
	onChange: (value: number) => void;
}> = ({value, onChange}) => {
	const {i18n} = useLingui();
	const voiceQualityLabel = i18n._(VOICE_QUALITY_DESCRIPTOR);
	const resetSliderLabel = i18n._(RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR);
	let currentValue = value;
	if (typeof currentValue !== 'number') {
		currentValue = 64;
	}
	return (
		<SettingsControlRow
			label={voiceQualityLabel}
			dataFlx="channel.channel-tabs.channel-overview-tab.voice-settings.voice-bitrate-slider"
		>
			<div className={styles.settingsSliderControl}>
				<Slider
					value={currentValue}
					defaultValue={currentValue}
					factoryDefaultValue={64}
					minValue={BITRATE_KBPS_MIN}
					maxValue={BITRATE_KBPS_MAX}
					step={1}
					markers={[...BITRATE_KBPS_MARKERS]}
					ariaLabel={voiceQualityLabel}
					ariaValueText={i18n._(KBPS_DESCRIPTOR, {kilobits: Math.round(currentValue)})}
					onMarkerRender={formatIntegerValue}
					onValueRender={(kilobits) => i18n._(KBPS_DESCRIPTOR, {kilobits: Math.round(kilobits)})}
					onValueChange={(kilobits) => onChange(Math.round(kilobits))}
					showResetButton={true}
					onReset={() => onChange(64)}
					resetTooltip={resetSliderLabel}
				/>
			</div>
		</SettingsControlRow>
	);
};

export const VoiceSettings: React.FC<VoiceSettingsProps> = ({form}) => {
	const {i18n} = useLingui();
	const resetSliderLabel = i18n._(RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR);
	const participantLimitLabel = i18n._(PARTICIPANT_LIMIT_LABEL_DESCRIPTOR);
	return (
		<>
			<div data-flx="channel.channel-tabs.channel-overview-tab.div--3">
				<Controller
					name="bitrate"
					control={form.control}
					render={({field}) => <VoiceBitrateSlider value={field.value} onChange={field.onChange} />}
					data-flx="channel.channel-tabs.channel-overview-tab.controller--2"
				/>
			</div>
			<div data-flx="channel.channel-tabs.channel-overview-tab.div--4">
				<Controller
					name="user_limit"
					control={form.control}
					render={({field}) => {
						const currentValue = typeof field.value === 'number' ? field.value : 0;
						return (
							<SettingsControlRow
								label={participantLimitLabel}
								dataFlx="channel.channel-tabs.channel-overview-tab.participant-limit"
							>
								<div
									className={styles.settingsSliderControl}
									data-flx="channel.channel-tabs.channel-overview-tab.participant-limit.slider-wrap"
								>
									<Slider
										value={currentValue}
										defaultValue={currentValue}
										factoryDefaultValue={0}
										minValue={0}
										maxValue={99}
										step={1}
										markers={[0, 25, 50, 75, 99]}
										ariaLabel={participantLimitLabel}
										onMarkerRender={formatParticipantLimitMarker}
										onValueRender={(value) => i18n._(PARTICIPANT_LIMIT_VALUE_DESCRIPTOR, {count: Math.round(value)})}
										onValueChange={field.onChange}
										showResetButton={true}
										onReset={() => field.onChange(0)}
										resetTooltip={resetSliderLabel}
										data-flx="channel.channel-tabs.channel-overview-tab.participant-limit.slider"
									/>
								</div>
							</SettingsControlRow>
						);
					}}
					data-flx="channel.channel-tabs.channel-overview-tab.controller--3"
				/>
			</div>
		</>
	);
};

export const VoiceConnectionLimitControl: React.FC<VoiceSettingsProps> = ({form}) => {
	const {i18n} = useLingui();
	const resetSliderLabel = i18n._(RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR);
	const connectionLimitLabel = i18n._(CONNECTION_LIMIT_LABEL_DESCRIPTOR);
	const connectionLimitDescription = i18n._(CONNECTION_LIMIT_DESCRIPTION_DESCRIPTOR);
	return (
		<div data-flx="channel.channel-tabs.channel-overview-tab.div--5">
			<Controller
				name="voice_connection_limit"
				control={form.control}
				render={({field}) => {
					const currentValue = typeof field.value === 'number' ? field.value : VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT;
					return (
						<SettingsControlRow
							label={connectionLimitLabel}
							description={connectionLimitDescription}
							dataFlx="channel.channel-tabs.channel-overview-tab.voice-connection-limit"
						>
							<div
								className={styles.settingsSliderControl}
								data-flx="channel.channel-tabs.channel-overview-tab.voice-connection-limit.slider-wrap"
							>
								<Slider
									value={currentValue}
									defaultValue={currentValue}
									factoryDefaultValue={VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT}
									minValue={VOICE_CHANNEL_CONNECTION_LIMIT_MIN}
									maxValue={VOICE_CHANNEL_CONNECTION_LIMIT_MAX}
									step={1}
									markers={[1, 5, 25, 50, 75, 100]}
									ariaLabel={connectionLimitLabel}
									onMarkerRender={formatConnectionLimitMarker}
									onValueRender={(value) => i18n._(CONNECTION_LIMIT_VALUE_DESCRIPTOR, {count: Math.round(value)})}
									onValueChange={field.onChange}
									showResetButton={true}
									onReset={() => field.onChange(VOICE_CHANNEL_CONNECTION_LIMIT_DEFAULT)}
									resetTooltip={resetSliderLabel}
									data-flx="channel.channel-tabs.channel-overview-tab.voice-connection-limit.slider"
								/>
							</div>
						</SettingsControlRow>
					);
				}}
				data-flx="channel.channel-tabs.channel-overview-tab.controller--4"
			/>
		</div>
	);
};
