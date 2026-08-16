// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	dismissScheduledMaintenanceNagbar,
	isScheduledMaintenanceNagbarDismissed,
} from '@app/features/app/components/layout/app_layout/ScheduledMaintenanceDismissal';
import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import NagbarState from '@app/features/ui/state/Nagbar';
import StatusPage, {type MaintenanceStatus, type StatusPageMaintenance} from '@app/features/user/state/StatusPage';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import {ExternalUrls} from '@fluxer/constants/src/ExternalUrls';
import {HOURS_PER_DAY, MINUTES_PER_HOUR, MS_PER_HOUR} from '@fluxer/date_utils/src/DateConstants';
import {getFormattedDateTime} from '@fluxer/date_utils/src/DateFormatting';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useMemo} from 'react';

interface MaintenanceScheduleLabels {
	startLabel: string;
	durationLabel: string;
}

export const MAINTENANCE_NAGBAR_TONE_KINDS: Record<MaintenanceStatus, NagbarToneKind> = {
	scheduled: NagbarToneKind.MAINTENANCE_SCHEDULED,
	in_progress: NagbarToneKind.MAINTENANCE_ACTIVE,
	completed: NagbarToneKind.MAINTENANCE_COMPLETED,
};
const MAINTENANCE_IN_PROGRESS_MESSAGE_DESCRIPTOR = msg({
	message: 'Maintenance is in progress. Expected duration: {duration}.',
	comment:
		'Maintenance banner message shown while a planned status-page maintenance is in progress. {duration} is already localized. Keep the tone plain.',
});
const MAINTENANCE_COMPLETE_MESSAGE_DESCRIPTOR = msg({
	message: 'Maintenance is complete.',
	comment: 'Maintenance banner message shown after a planned status-page maintenance finishes. Keep the tone plain.',
});
const MAINTENANCE_SCHEDULED_MESSAGE_DESCRIPTOR = msg({
	message: 'Maintenance is scheduled for {localizedTime}. Expected duration: {duration}.',
	comment:
		'Maintenance banner message shown before a planned status-page maintenance begins. {localizedTime} and {duration} are already localized. Keep the tone plain.',
});
const LEARN_MORE_DESCRIPTOR = msg({
	message: 'Learn more',
	comment: 'CTA on the planned-maintenance banner. Opens the external status-page maintenance notice.',
});

function formatMaintenanceTimeLabel(value: string, locale: string): string {
	const parsedDate = new Date(value);
	if (Number.isNaN(parsedDate.getTime())) {
		return value;
	}
	return getFormattedDateTime(parsedDate, locale);
}

function formatMaintenanceDurationLabel(durationMinutes: number, locale: string): string {
	const normalizedDurationMinutes = Math.max(1, Math.round(durationMinutes));
	const durationParts =
		normalizedDurationMinutes % (HOURS_PER_DAY * MINUTES_PER_HOUR) === 0
			? {value: normalizedDurationMinutes / (HOURS_PER_DAY * MINUTES_PER_HOUR), unit: 'day'}
			: normalizedDurationMinutes % MINUTES_PER_HOUR === 0
				? {value: normalizedDurationMinutes / MINUTES_PER_HOUR, unit: 'hour'}
				: {value: normalizedDurationMinutes, unit: 'minute'};
	try {
		return getCachedNumberFormat(locale, {
			style: 'unit',
			unit: durationParts.unit,
			unitDisplay: 'long',
		}).format(durationParts.value);
	} catch {
		const pluralSuffix = durationParts.value === 1 ? '' : 's';
		return `${durationParts.value} ${durationParts.unit}${pluralSuffix}`;
	}
}

function getDeveloperScheduledMaintenance(): StatusPageMaintenance {
	return {
		id: 'developer-scheduled-maintenance',
		name: 'Developer scheduled maintenance',
		status: 'scheduled',
		start: new Date(Date.now() + MS_PER_HOUR).toISOString(),
		durationMinutes: MINUTES_PER_HOUR,
		url: ExternalUrls.SERVICE_STATUS,
	};
}

function renderMaintenanceMessage(
	status: MaintenanceStatus,
	scheduleLabels: MaintenanceScheduleLabels,
	i18n: I18n,
): string {
	if (status === 'in_progress') {
		return i18n._(MAINTENANCE_IN_PROGRESS_MESSAGE_DESCRIPTOR, {duration: scheduleLabels.durationLabel});
	}
	if (status === 'completed') {
		return i18n._(MAINTENANCE_COMPLETE_MESSAGE_DESCRIPTOR);
	}
	return i18n._(MAINTENANCE_SCHEDULED_MESSAGE_DESCRIPTOR, {
		localizedTime: scheduleLabels.startLabel,
		duration: scheduleLabels.durationLabel,
	});
}

export const ScheduledMaintenanceNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const {i18n} = useLingui();
	const scheduledMaintenance =
		StatusPage.scheduledMaintenance ??
		(NagbarState.forceScheduledMaintenance ? getDeveloperScheduledMaintenance() : null);
	const locale = LocaleUtils.getCurrentLocale();
	const scheduleLabels = useMemo<MaintenanceScheduleLabels | null>(() => {
		if (!scheduledMaintenance) {
			return null;
		}
		return {
			startLabel: formatMaintenanceTimeLabel(scheduledMaintenance.start, locale),
			durationLabel: formatMaintenanceDurationLabel(scheduledMaintenance.durationMinutes, locale),
		};
	}, [locale, scheduledMaintenance]);
	const handleOpenStatusPage = useCallback(() => {
		if (!scheduledMaintenance) {
			return;
		}
		openExternalUrlWithWarning(scheduledMaintenance.url);
	}, [scheduledMaintenance]);
	const handleDismiss = useCallback(() => {
		if (!scheduledMaintenance) {
			return;
		}
		dismissScheduledMaintenanceNagbar(scheduledMaintenance.id, scheduledMaintenance.status);
		NagbarState.bumpScheduledMaintenanceDismissed();
	}, [scheduledMaintenance]);
	if (
		!scheduledMaintenance ||
		!scheduleLabels ||
		(!NagbarState.forceScheduledMaintenance &&
			isScheduledMaintenanceNagbarDismissed(scheduledMaintenance.id, scheduledMaintenance.status))
	) {
		return null;
	}
	const tone = NAGBAR_TONES[MAINTENANCE_NAGBAR_TONE_KINDS[scheduledMaintenance.status]];
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={tone.backgroundColor}
			textColor={tone.textColor}
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.scheduled-maintenance-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={renderMaintenanceMessage(scheduledMaintenance.status, scheduleLabels, i18n)}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleOpenStatusPage}
						data-flx="app.app-layout.nagbars.scheduled-maintenance-nagbar.nagbar-button.open-status-page"
					>
						{i18n._(LEARN_MORE_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.scheduled-maintenance-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
