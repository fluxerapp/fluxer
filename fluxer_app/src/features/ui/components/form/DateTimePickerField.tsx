// SPDX-License-Identifier: AGPL-3.0-or-later

import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/ui/components/form/DateTimePickerField.module.css';
import surfaceStyles from '@app/features/ui/components/form/FormSurface.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {CalendarBlankIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {DateTime} from 'luxon';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';
import {Button, Dialog, DialogTrigger, Popover} from 'react-aria-components';
import {DayPicker} from 'react-day-picker';

const SELECT_A_DATE_AND_TIME_DESCRIPTOR = msg({
	message: 'Select a date and time',
	comment: 'Form placeholder for a date and time field.',
});
const DATE_AND_TIME_DESCRIPTOR = msg({
	message: 'Date and time',
	comment: 'Form field label for a date and time value.',
});
const OPEN_DATE_PICKER_DESCRIPTOR = msg({
	message: 'Open date picker',
	comment: 'Accessible label for the button that opens the date picker.',
});
const DATE_PICKER_DESCRIPTOR = msg({
	message: 'Date picker',
	comment: 'Accessible label for the date picker dialog.',
});
const TIME_DESCRIPTOR = msg({
	message: 'Time',
	comment: 'Form field label for a time value.',
});

interface DateTimePickerFieldProps {
	label?: string;
	description?: string;
	value: Date | null;
	onChange: (date: Date | null) => void;
	minDate?: Date;
	maxDate?: Date;
	disabled?: boolean;
	error?: string;
	className?: string;
}

function formatDisplayDate(date: Date | null): string {
	if (!date) {
		return '';
	}
	const dt = DateTime.fromJSDate(date);
	return dt.toFormat('d LLL yyyy, HH:mm');
}

function toTimeString(date: Date): string {
	return `${String(date.getHours()).padStart(2, '0')}:${String(date.getMinutes()).padStart(2, '0')}`;
}

export const DateTimePickerField: React.FC<DateTimePickerFieldProps> = (props) => {
	const {i18n} = useLingui();
	const {label, description, value, onChange, minDate, maxDate, disabled, error, className} = props;
	const [popoutOpen, setPopoutOpen] = useState(false);
	const displayValue = useMemo(() => formatDisplayDate(value), [value]);
	const timeValue = useMemo(() => (value ? toTimeString(value) : '00:00'), [value]);
	const handleDaySelect = useCallback(
		(selected: Date | undefined) => {
			if (!selected) {
				return;
			}
			const current = value ?? new Date();
			const merged = new Date(selected);
			merged.setHours(current.getHours(), current.getMinutes(), 0, 0);
			onChange(merged);
		},
		[value, onChange],
	);
	const handleTimeChange = useCallback(
		(event: React.ChangeEvent<HTMLInputElement>) => {
			const timeStr = event.target.value;
			if (!timeStr) {
				return;
			}
			const [hours, minutes] = timeStr.split(':').map(Number);
			const current = value ?? new Date();
			const updated = new Date(current);
			updated.setHours(hours, minutes, 0, 0);
			onChange(updated);
		},
		[value, onChange],
	);
	const disabledMatcher = useMemo(() => {
		const matchers: Array<{before: Date} | {after: Date}> = [];
		if (minDate) {
			matchers.push({before: minDate});
		}
		if (maxDate) {
			matchers.push({after: maxDate});
		}
		return matchers;
	}, [minDate, maxDate]);
	const calendarClassNames = useMemo(
		() => ({
			root: styles.rdpRoot,
			months: styles.rdpMonths,
			month: styles.rdpMonth,
			nav: styles.rdpNav,
			month_caption: styles.rdpMonthCaption,
			caption_label: styles.rdpCaptionLabel,
			button_previous: styles.rdpButtonPrevious,
			button_next: styles.rdpButtonNext,
			chevron: styles.rdpChevron,
			month_grid: styles.rdpMonthGrid,
			weekday: styles.rdpWeekday,
			day: styles.rdpDay,
			day_button: styles.rdpDayButton,
			today: styles.rdpToday,
			selected: styles.rdpSelected,
			outside: styles.rdpOutside,
			disabled: styles.rdpDisabled,
			hidden: styles.rdpHidden,
		}),
		[],
	);
	return (
		<FocusRing within={true} offset={-2} enabled={!disabled} data-flx="ui.form.date-time-picker-field.focus-ring">
			<fieldset className={clsx(styles.fieldset, className)} data-flx="ui.form.date-time-picker-field.fieldset">
				{label && (
					<div className={styles.labelContainer} data-flx="ui.form.date-time-picker-field.label-container">
						<legend className={styles.label} data-flx="ui.form.date-time-picker-field.label">
							{label}
						</legend>
					</div>
				)}
				<div className={styles.inputContainer} data-flx="ui.form.date-time-picker-field.input-container">
					<div
						className={clsx(styles.inputWrapper, surfaceStyles.surface)}
						data-flx="ui.form.date-time-picker-field.input-wrapper"
					>
						<input
							type="text"
							data-flx="ui.form.date-time-picker-field.input.text"
							{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
							readOnly={true}
							value={displayValue}
							placeholder={i18n._(SELECT_A_DATE_AND_TIME_DESCRIPTOR)}
							disabled={disabled}
							className={styles.input}
							aria-label={label ?? i18n._(DATE_AND_TIME_DESCRIPTOR)}
						/>
						<div className={styles.divider} data-flx="ui.form.date-time-picker-field.divider" />
						<DialogTrigger
							isOpen={popoutOpen}
							onOpenChange={setPopoutOpen}
							data-flx="ui.form.date-time-picker-field.dialog-trigger"
						>
							<Button
								className={styles.calendarButton}
								aria-label={i18n._(OPEN_DATE_PICKER_DESCRIPTOR)}
								isDisabled={disabled}
								data-flx="ui.form.date-time-picker-field.calendar-button"
							>
								<CalendarBlankIcon size={remFromPx(18)} data-flx="ui.form.date-time-picker-field.calendar-blank-icon" />
							</Button>
							<Popover
								placement="bottom start"
								offset={8}
								className={styles.popover}
								data-flx="ui.form.date-time-picker-field.popover"
							>
								<Dialog
									className={styles.dialog}
									aria-label={i18n._(DATE_PICKER_DESCRIPTOR)}
									data-flx="ui.form.date-time-picker-field.dialog"
								>
									<div
										className={styles.calendarContainer}
										data-flx="ui.form.date-time-picker-field.calendar-container"
									>
										<DayPicker
											mode="single"
											selected={value ?? undefined}
											onSelect={handleDaySelect}
											startMonth={minDate}
											endMonth={maxDate}
											disabled={disabledMatcher}
											defaultMonth={value ?? undefined}
											showOutsideDays={true}
											classNames={calendarClassNames}
											data-flx="ui.form.date-time-picker-field.day-picker.day-select"
										/>
										<div className={styles.timeRow} data-flx="ui.form.date-time-picker-field.time-row">
											<label
												className={styles.timeLabel}
												htmlFor="rdp-time-input"
												data-flx="ui.form.date-time-picker-field.time-label"
											>
												{i18n._(TIME_DESCRIPTOR)}
											</label>
											<input
												id="rdp-time-input"
												type="time"
												data-flx="ui.form.date-time-picker-field.rdp-time-input.time-change"
												{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
												className={styles.timeInput}
												value={timeValue}
												onChange={handleTimeChange}
											/>
										</div>
									</div>
								</Dialog>
							</Popover>
						</DialogTrigger>
					</div>
					{description && (
						<p className={styles.description} data-flx="ui.form.date-time-picker-field.description">
							{description}
						</p>
					)}
					{error && (
						<p className={styles.errorText} data-flx="ui.form.date-time-picker-field.error-text">
							{error}
						</p>
					)}
				</div>
			</fieldset>
		</FocusRing>
	);
};
