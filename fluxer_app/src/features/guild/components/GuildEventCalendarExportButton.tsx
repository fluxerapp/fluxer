// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	guildEventCalendarFilename,
	serializeGuildEventsCalendar,
} from '@app/features/guild/utils/GuildEventCalendarUtils';
import {downloadTextFile} from '@app/features/platform/utils/DownloadFile';
import {Button} from '@app/features/ui/button/Button';
import type {GuildEvent} from '@fluxer/schema/src/domains/guild/GuildEventSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {DownloadSimpleIcon} from '@phosphor-icons/react';

const EXPORT_ERROR_DESCRIPTOR = msg({
	message: 'Unable to export community events. Please try again.',
	comment: 'Error shown when a community event calendar file cannot be downloaded.',
});

interface GuildEventCalendarExportButtonProps {
	guildId: string;
	events: ReadonlyArray<GuildEvent>;
	eventId?: string;
	disabled?: boolean;
	onError: (message: string) => void;
}

export function GuildEventCalendarExportButton({
	guildId,
	events,
	eventId,
	disabled = false,
	onError,
}: GuildEventCalendarExportButtonProps) {
	const {i18n} = useLingui();

	const handleExport = () => {
		if (disabled || events.length === 0) return;
		try {
			downloadTextFile(
				serializeGuildEventsCalendar(events),
				guildEventCalendarFilename(guildId, eventId),
				'text/calendar;charset=utf-8',
			);
		} catch (cause) {
			onError(cause instanceof Error && cause.message ? cause.message : i18n._(EXPORT_ERROR_DESCRIPTOR));
		}
	};

	return (
		<Button
			type="button"
			variant="secondary"
			small
			fitContent
			disabled={disabled || events.length === 0}
			leftIcon={<DownloadSimpleIcon size={16} aria-hidden />}
			onClick={handleExport}
			data-flx="guild.event-calendar-export-button"
		>
			{eventId ? (
				<Trans comment="Button that downloads one community event as an iCalendar file.">Export event</Trans>
			) : (
				<Trans comment="Button that downloads the listed community events as an iCalendar file.">Export calendar</Trans>
			)}
		</Button>
	);
}
