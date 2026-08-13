// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/modals/channel_tabs/ChannelOverviewTab.module.css';
import {SettingsControlRow} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/SettingsControlRow';
import type {FormInputs} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/shared';
import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import {Slider} from '@app/features/ui/components/Slider';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {CHANNEL_RATE_LIMIT_PER_USER_MAX, CHANNEL_RATE_LIMIT_PER_USER_MIN} from '@fluxer/constants/src/LimitConstants';
import {SECONDS_PER_HOUR, SECONDS_PER_MINUTE} from '@fluxer/date_utils/src/DateConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {formatListWithConfig} from '@pkgs/list_utils/src/ListFormatting';
import type React from 'react';
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
const SLOWMODE_STOP_SECONDS: ReadonlyArray<number> = [
	0, 5, 10, 15, 30, 60, 120, 300, 600, 900, 1800, 3600, 7200, 21600,
];
const SLOWMODE_MARKER_SECONDS: ReadonlyArray<number> = [0, 60, 3600, 21600];

type SlowmodeDurationUnit = 'second' | 'minute' | 'hour';

function formatSlowmodeDurationPart(value: number, unit: SlowmodeDurationUnit, locale: string): string {
	return getCachedNumberFormat(locale, {style: 'unit', unit, unitDisplay: 'long'}).format(value);
}

function formatSlowmodeDuration(i18n: I18n, seconds: number): string {
	const roundedSeconds = Math.max(0, Math.round(seconds));
	if (roundedSeconds === 0) {
		return i18n._(OFF_DESCRIPTOR);
	}
	const hours = Math.floor(roundedSeconds / SECONDS_PER_HOUR);
	const minutes = Math.floor((roundedSeconds % SECONDS_PER_HOUR) / SECONDS_PER_MINUTE);
	const remainingSeconds = roundedSeconds % SECONDS_PER_MINUTE;
	const parts: Array<string> = [];
	if (hours > 0) {
		parts.push(formatSlowmodeDurationPart(hours, 'hour', i18n.locale));
	}
	if (minutes > 0) {
		parts.push(formatSlowmodeDurationPart(minutes, 'minute', i18n.locale));
	}
	if (remainingSeconds > 0) {
		parts.push(formatSlowmodeDurationPart(remainingSeconds, 'second', i18n.locale));
	}
	return formatListWithConfig(parts, {locale: i18n.locale, style: 'long', type: 'unit'});
}

interface SlowmodeControlProps {
	form: UseFormReturn<FormInputs>;
}

export const SlowmodeControl: React.FC<SlowmodeControlProps> = ({form}) => {
	const {i18n} = useLingui();
	const slowmodeLabel = i18n._(SLOWMODE_DESCRIPTOR);
	const bypassSlowmodePermissionLabel = formatPermissionLabel(i18n, Permissions.BYPASS_SLOWMODE);
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
				return (
					<SettingsControlRow
						label={slowmodeLabel}
						description={i18n._(SLOWMODE_DESCRIPTION_DESCRIPTOR, {bypassSlowmodePermissionLabel})}
						stacked
						dataFlx="channel.channel-tabs.channel-overview-tab.slowmode-control"
					>
						<div className={styles.settingsSliderControl}>
							<Slider
								value={currentSeconds}
								defaultValue={currentSeconds}
								factoryDefaultValue={0}
								minValue={CHANNEL_RATE_LIMIT_PER_USER_MIN}
								maxValue={CHANNEL_RATE_LIMIT_PER_USER_MAX}
								step={1}
								equidistant
								markers={SLOWMODE_STOP_SECONDS}
								ariaLabel={slowmodeLabel}
								ariaValueText={formatSlowmodeDuration(i18n, currentSeconds)}
								onMarkerRender={(seconds) => {
									if (!SLOWMODE_MARKER_SECONDS.includes(seconds)) {
										return null;
									}
									return formatSlowmodeDuration(i18n, seconds);
								}}
								onValueRender={(seconds) => formatSlowmodeDuration(i18n, Math.round(seconds))}
								onValueChange={(seconds) => field.onChange(Math.round(seconds))}
							/>
						</div>
					</SettingsControlRow>
				);
			}}
		/>
	);
};
