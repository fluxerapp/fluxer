/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useLingui} from '@lingui/react/macro';
import React from 'react';
import {Input} from '~/components/form/Input';
import {Select, type SelectOption} from '~/components/form/Select';
import styles from '~/components/modals/ConfirmModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';

interface ScheduleMessageModalProps {
	onClose: () => void;
	onSubmit: (scheduledLocalAt: string, timezone: string) => Promise<void>;
	initialScheduledLocalAt?: string;
	initialTimezone?: string;
	title?: string;
	submitLabel?: string;
	helpText?: React.ReactNode;
}

const formatInputValue = (value: Date): string => value.toISOString().slice(0, 16);

export const ScheduleMessageModal = ({
	onClose,
	onSubmit,
	initialScheduledLocalAt,
	initialTimezone,
	title,
	submitLabel,
	helpText,
}: ScheduleMessageModalProps) => {
	const {t} = useLingui();
	const minDateTime = React.useMemo(() => formatInputValue(new Date(Date.now() + 60_000)), []);
	const maxDateTime = React.useMemo(() => formatInputValue(new Date(Date.now() + 30 * 24 * 60 * 60 * 1000)), []);
	const defaultTimezone = React.useMemo(() => Intl.DateTimeFormat().resolvedOptions().timeZone, []);
	const timezoneOptions = React.useMemo((): Array<SelectOption<string>> => {
		const intl = Intl as typeof Intl & {supportedValuesOf?: (type: string) => Array<string>};
		const zones = typeof intl.supportedValuesOf === 'function' ? intl.supportedValuesOf('timeZone') : [defaultTimezone];
		return zones.map((zone) => ({value: zone, label: zone}));
	}, [defaultTimezone]);

	const initialScheduledAt = React.useMemo(
		() => initialScheduledLocalAt ?? formatInputValue(new Date(Date.now() + 5 * 60 * 1000)),
		[initialScheduledLocalAt],
	);
	const [scheduledLocalAt, setScheduledLocalAt] = React.useState(initialScheduledAt);
	const [timezone, setTimezone] = React.useState(initialTimezone ?? defaultTimezone);
	React.useEffect(() => {
		setScheduledLocalAt(initialScheduledLocalAt ?? formatInputValue(new Date(Date.now() + 5 * 60 * 1000)));
	}, [initialScheduledLocalAt]);
	React.useEffect(() => {
		if (initialTimezone) {
			setTimezone(initialTimezone);
		}
	}, [initialTimezone]);
	const [submitting, setSubmitting] = React.useState(false);

	const handleConfirm = React.useCallback(async () => {
		if (!scheduledLocalAt) {
			return;
		}
		setSubmitting(true);
		try {
			await onSubmit(scheduledLocalAt, timezone);
			onClose();
		} finally {
			setSubmitting(false);
		}
	}, [scheduledLocalAt, timezone, onSubmit, onClose]);

	return (
		<Modal.Root size="small" centered onClose={onClose}>
			<Modal.Header title={title ?? t`Schedule Message`} />
			<Modal.Content className={styles.content}>
				<span className={styles.descriptionText}>{helpText ?? t`Pick a time when this message should be posted.`}</span>

				<div className={styles.inputContainer}>
					<Input
						id="schedule-message-time"
						type="datetime-local"
						label={t`Date & time`}
						min={minDateTime}
						max={maxDateTime}
						value={scheduledLocalAt}
						onChange={(event) => setScheduledLocalAt(event.target.value)}
					/>

					<Select
						id="schedule-message-timezone"
						label={t`Timezone`}
						description={t`Scheduled messages can be at most 30 days in the future.`}
						value={timezone}
						options={timezoneOptions}
						onChange={setTimezone}
					/>
				</div>
			</Modal.Content>
			<Modal.Footer>
				<Button variant="secondary" onClick={onClose}>
					{t`Cancel`}
				</Button>
				<Button variant="primary" onClick={handleConfirm} submitting={submitting} disabled={!scheduledLocalAt}>
					{submitLabel ?? t`Schedule`}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
};
