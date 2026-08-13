// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/modals/channel_tabs/ChannelOverviewTab.module.css';
import {SettingsControlRow} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/SettingsControlRow';
import type {FormInputs} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/shared';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import {RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR, Slider} from '@app/features/ui/components/Slider';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {CHANNEL_RATE_LIMIT_PER_USER_MAX, CHANNEL_RATE_LIMIT_PER_USER_MIN} from '@fluxer/constants/src/LimitConstants';
import {SECONDS_PER_HOUR, SECONDS_PER_MINUTE} from '@fluxer/date_utils/src/DateConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useMemo} from 'react';
import {Controller, type UseFormReturn} from 'react-hook-form';

const SLOWMODE_DESCRIPTOR = msg({
	message: 'Slowmode',
	comment:
		'Channel overview settings tab label, control, or validation message (name, topic, slowmode, voice region, mature content gate).',
});
const OFF_DESCRIPTOR = msg({
	message: 'Off',
	comment:
		'Channel overview settings tab label, control, or validation message (name, topic, slowmode, voice region, mature content gate).',
});
const SLOWMODE_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Wait between messages. "{bypassSlowmodePermissionLabel}" can bypass it.',
	comment:
		'Description under the slowmode slider in channel settings. bypassSlowmodePermissionLabel is the localized Bypass Slowmode permission name.',
});
const SLOWMODE_SECONDS_DESCRIPTOR = msg({
	message: '{count, plural, one {# second} other {# seconds}}',
	comment: 'Slowmode duration expressed in seconds, shown on the channel settings slowmode slider.',
});
const SLOWMODE_MINUTES_DESCRIPTOR = msg({
	message: '{count, plural, one {# minute} other {# minutes}}',
	comment: 'Slowmode duration expressed in minutes, shown on the channel settings slowmode slider.',
});
const SLOWMODE_HOURS_DESCRIPTOR = msg({
	message: '{count, plural, one {# hour} other {# hours}}',
	comment: 'Slowmode duration expressed in hours, shown on the channel settings slowmode slider.',
});

const SLOWMODE_STOP_SECONDS: ReadonlyArray<number> = [
	0, 5, 10, 15, 30, 60, 120, 300, 600, 900, 1800, 3600, 7200, 21600,
];
const SLOWMODE_MARKER_SECONDS: ReadonlyArray<number> = [0, 60, 3600, 21600];

function buildSlowmodeStops(remoteSeconds: number): Array<number> {
	const boundedSeconds = Math.min(
		Math.max(Math.round(remoteSeconds), CHANNEL_RATE_LIMIT_PER_USER_MIN),
		CHANNEL_RATE_LIMIT_PER_USER_MAX,
	);
	if (SLOWMODE_STOP_SECONDS.includes(boundedSeconds)) {
		return [...SLOWMODE_STOP_SECONDS];
	}
	return [...SLOWMODE_STOP_SECONDS, boundedSeconds].sort((left, right) => left - right);
}

function findNearestStopIndex(stops: ReadonlyArray<number>, seconds: number): number {
	let nearestIndex = 0;
	let nearestDistance = Number.POSITIVE_INFINITY;
	for (let index = 0; index < stops.length; index++) {
		const distance = Math.abs(stops[index] - seconds);
		if (distance < nearestDistance) {
			nearestIndex = index;
			nearestDistance = distance;
		}
	}
	return nearestIndex;
}

function formatSlowmodeDuration(i18n: I18n, seconds: number): string {
	if (seconds === 0) {
		return i18n._(OFF_DESCRIPTOR);
	}
	if (seconds % SECONDS_PER_HOUR === 0) {
		return i18n._(SLOWMODE_HOURS_DESCRIPTOR, {count: seconds / SECONDS_PER_HOUR});
	}
	if (seconds % SECONDS_PER_MINUTE === 0) {
		return i18n._(SLOWMODE_MINUTES_DESCRIPTOR, {count: seconds / SECONDS_PER_MINUTE});
	}
	return i18n._(SLOWMODE_SECONDS_DESCRIPTOR, {count: seconds});
}

interface SlowmodeControlProps {
	form: UseFormReturn<FormInputs>;
	remoteSlowmodeSeconds: number;
}

export const SlowmodeControl: React.FC<SlowmodeControlProps> = ({form, remoteSlowmodeSeconds}) => {
	const {i18n} = useLingui();
	const stops = useMemo(() => buildSlowmodeStops(remoteSlowmodeSeconds), [remoteSlowmodeSeconds]);
	const markerIndexes = useMemo(() => SLOWMODE_MARKER_SECONDS.map((seconds) => stops.indexOf(seconds)), [stops]);
	const slowmodeLabel = i18n._(SLOWMODE_DESCRIPTOR);
	const bypassSlowmodePermissionLabel = formatPermissionLabel(i18n, Permissions.BYPASS_SLOWMODE);
	const resetSliderLabel = i18n._(RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR);
	return (
		<Controller
			name="slowmode"
			control={form.control}
			render={({field}) => {
				let currentSeconds: number;
				if (typeof field.value === 'number') {
					currentSeconds = field.value;
				} else {
					currentSeconds = 0;
				}
				const currentIndex = findNearestStopIndex(stops, currentSeconds);
				return (
					<SettingsControlRow
						label={slowmodeLabel}
						description={i18n._(SLOWMODE_DESCRIPTION_DESCRIPTOR, {bypassSlowmodePermissionLabel})}
						dataFlx="channel.channel-tabs.channel-overview-tab.slowmode-control"
					>
						<div className={styles.settingsSliderControl}>
							<Slider
								value={currentIndex}
								defaultValue={currentIndex}
								factoryDefaultValue={0}
								minValue={0}
								maxValue={stops.length - 1}
								step={1}
								markers={markerIndexes}
								ariaLabel={slowmodeLabel}
								ariaValueText={formatSlowmodeDuration(i18n, currentSeconds)}
								onMarkerRender={(index) => formatSlowmodeDuration(i18n, stops[index])}
								onValueRender={(index) => formatSlowmodeDuration(i18n, stops[Math.round(index)])}
								onValueChange={(index) => field.onChange(stops[Math.round(index)])}
								showResetButton={true}
								onReset={() => field.onChange(0)}
								resetTooltip={resetSliderLabel}
							/>
						</div>
					</SettingsControlRow>
				);
			}}
		/>
	);
};
