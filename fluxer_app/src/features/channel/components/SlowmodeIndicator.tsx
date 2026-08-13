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

const SLOWMODE_IS_ENABLED_BUT_YOU_ARE_IMMUNE_DESCRIPTOR = msg({
	message: 'Slowmode is enabled, but you are immune.',
	comment: 'Description text in the channel and chat slowmode indicator.',
});
const YOU_ARE_IN_SLOWMODE_PLEASE_WAIT_BEFORE_SENDING_DESCRIPTOR = msg({
	message: "You're in slowmode. Wait before sending another message.",
	comment: 'Description text in the channel and chat slowmode indicator.',
});
const SLOWMODE_IS_ENABLED_FOR_THIS_CHANNEL_DESCRIPTOR = msg({
	message: 'Slowmode is enabled for this channel.',
	comment: 'Description text in the channel and chat slowmode indicator.',
});
const SLOWMODE_DESCRIPTOR = msg({
	message: '{durationLabel} slowmode',
	comment:
		'Short label in the channel and chat slowmode indicator. Keep it concise. Preserve {durationLabel}; it is inserted by code.',
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
	const tooltipText = isImmune
		? i18n._(SLOWMODE_IS_ENABLED_BUT_YOU_ARE_IMMUNE_DESCRIPTOR)
		: onCooldown
			? i18n._(YOU_ARE_IN_SLOWMODE_PLEASE_WAIT_BEFORE_SENDING_DESCRIPTOR)
			: i18n._(SLOWMODE_IS_ENABLED_FOR_THIS_CHANNEL_DESCRIPTOR);
	const durationLabel = formatSlowmodeDuration(slowmodeDuration, locale);
	return (
		<Tooltip text={tooltipText} data-flx="channel.slowmode-indicator.tooltip">
			<div
				className={clsx(styles.container, onCooldown && styles.cooldown)}
				data-flx="channel.slowmode-indicator.container"
			>
				<ClockIcon size={remFromPx(12)} weight="fill" data-flx="channel.slowmode-indicator.clock-icon" />
				{onCooldown ? (
					<span className={styles.time} data-flx="channel.slowmode-indicator.time">
						{formatSlowmodeTime(slowmodeRemaining, locale)}
					</span>
				) : (
					<span className={styles.label} data-flx="channel.slowmode-indicator.label">
						{i18n._(SLOWMODE_DESCRIPTOR, {durationLabel})}
					</span>
				)}
			</div>
		</Tooltip>
	);
});
