// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/SlowmodeIndicator.module.css';
import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {
	MS_PER_SECOND,
	SECONDS_PER_DAY,
	SECONDS_PER_HOUR,
	SECONDS_PER_MINUTE,
} from '@fluxer/date_utils/src/DateConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ClockIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

const SLOWMODE_IS_SET_BUT_YOU_ARE_IMMUNE_DESCRIPTOR = msg({
	message: 'Slowmode is set to {durationLabel}, but you are immune.',
	comment:
		'Tooltip on the composer slowmode indicator when the reader can bypass slowmode. Preserve {durationLabel}; it is inserted by code.',
});
const SLOWMODE_IS_SET_WAIT_BEFORE_SENDING_DESCRIPTOR = msg({
	message: 'Slowmode is set to {durationLabel}. Wait before sending another message.',
	comment:
		'Tooltip on the composer slowmode indicator while the reader is counting down. Preserve {durationLabel}; it is inserted by code.',
});
const SLOWMODE_IS_SET_FOR_THIS_CHANNEL_DESCRIPTOR = msg({
	message: 'Slowmode is set to {durationLabel} for this channel.',
	comment:
		'Tooltip on the composer slowmode indicator when the reader is free to send. Preserve {durationLabel}; it is inserted by code.',
});
const SLOWMODE_IS_ENABLED_DESCRIPTOR = msg({
	message: 'Slowmode is enabled',
	comment: 'Short label in the composer slowmode indicator when the reader is free to send. Keep it concise.',
});
const SLOWMODE_IS_ACTIVE_DESCRIPTOR = msg({
	message: 'Slowmode is active ({remaining})',
	comment:
		'Short label in the composer slowmode indicator while the countdown runs. Keep it concise. Preserve {remaining}; it is an mm:ss or hh:mm:ss timer inserted by code.',
});

interface SlowmodeIndicatorProps {
	slowmodeRemaining: number;
	slowmodeDuration: number;
	isImmune: boolean;
}

type DurationUnit = 'second' | 'minute' | 'hour' | 'day';

function formatDurationPart(value: number, unit: DurationUnit, locale: string): string {
	return getCachedNumberFormat(locale, {style: 'unit', unit, unitDisplay: 'short'}).format(value);
}

function formatTimeSegment(value: number, locale: string): string {
	return getCachedNumberFormat(locale, {minimumIntegerDigits: 2, useGrouping: false}).format(value);
}

export function formatSlowmodeTime(ms: number, locale: string): string {
	const totalSeconds = Math.ceil(ms / MS_PER_SECOND);
	const hours = Math.floor(totalSeconds / SECONDS_PER_HOUR);
	const minutes = Math.floor((totalSeconds % SECONDS_PER_HOUR) / SECONDS_PER_MINUTE);
	const seconds = totalSeconds % 60;
	if (hours > 0) {
		return `${formatTimeSegment(hours, locale)}:${formatTimeSegment(minutes, locale)}:${formatTimeSegment(seconds, locale)}`;
	}
	return `${formatTimeSegment(minutes, locale)}:${formatTimeSegment(seconds, locale)}`;
}

export function formatSlowmodeDuration(ms: number, locale: string): string {
	const totalSeconds = Math.max(1, Math.round(ms / MS_PER_SECOND));
	if (totalSeconds < SECONDS_PER_MINUTE) {
		return formatDurationPart(totalSeconds, 'second', locale);
	}
	if (totalSeconds < SECONDS_PER_HOUR) {
		return formatDurationPart(Math.round(totalSeconds / SECONDS_PER_MINUTE), 'minute', locale);
	}
	if (totalSeconds < SECONDS_PER_DAY) {
		const hours = Math.floor(totalSeconds / SECONDS_PER_HOUR);
		const minutes = Math.round((totalSeconds % SECONDS_PER_HOUR) / SECONDS_PER_MINUTE);
		const hourPart = formatDurationPart(hours, 'hour', locale);
		if (minutes === 0) return hourPart;
		return `${hourPart} ${formatDurationPart(minutes, 'minute', locale)}`;
	}
	return formatDurationPart(Math.round(totalSeconds / SECONDS_PER_DAY), 'day', locale);
}

export const SlowmodeIndicator = observer(({slowmodeRemaining, slowmodeDuration, isImmune}: SlowmodeIndicatorProps) => {
	const {i18n} = useLingui();
	const locale = i18n.locale;
	const onCooldown = !isImmune && slowmodeRemaining > 0;
	const durationLabel = formatSlowmodeDuration(slowmodeDuration, locale);
	let tooltipText: string;
	if (isImmune) {
		tooltipText = i18n._(SLOWMODE_IS_SET_BUT_YOU_ARE_IMMUNE_DESCRIPTOR, {durationLabel});
	} else if (onCooldown) {
		tooltipText = i18n._(SLOWMODE_IS_SET_WAIT_BEFORE_SENDING_DESCRIPTOR, {durationLabel});
	} else {
		tooltipText = i18n._(SLOWMODE_IS_SET_FOR_THIS_CHANNEL_DESCRIPTOR, {durationLabel});
	}
	let statusLabel: string;
	if (onCooldown) {
		statusLabel = i18n._(SLOWMODE_IS_ACTIVE_DESCRIPTOR, {remaining: formatSlowmodeTime(slowmodeRemaining, locale)});
	} else {
		statusLabel = i18n._(SLOWMODE_IS_ENABLED_DESCRIPTOR);
	}
	return (
		<Tooltip text={tooltipText} data-flx="channel.slowmode-indicator.tooltip">
			<div
				className={clsx(styles.container, onCooldown && styles.cooldown)}
				data-flx="channel.slowmode-indicator.container"
			>
				<span className={styles.label} data-flx="channel.slowmode-indicator.label">
					{statusLabel}
				</span>
				<ClockIcon
					size={remFromPx(12)}
					weight="fill"
					className={styles.icon}
					data-flx="channel.slowmode-indicator.clock-icon"
				/>
			</div>
		</Tooltip>
	);
});
