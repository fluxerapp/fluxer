// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/flow/DateOfBirthField.module.css';
import {dateOfBirthYearRange, isPlausibleDateOfBirthInput} from '@app/features/auth/flow/DateOfBirthRules';
import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {getDateFieldOrder, getMonthNames} from '@fluxer/date_utils/src/DateIntrospection';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useId, useMemo} from 'react';

const DATE_OF_BIRTH_DESCRIPTOR = msg({
	message: 'Date of birth',
	comment: 'Short label in the authentication date of birth field. Keep the tone plain and specific.',
});
const MONTH_DESCRIPTOR = msg({
	message: 'Month',
	comment: 'Short label in the authentication date of birth field. Keep the tone plain and specific.',
});
const DAY_DESCRIPTOR = msg({
	message: 'Day',
	comment: 'Short label in the authentication date of birth field. Keep the tone plain and specific.',
});
const YEAR_DESCRIPTOR = msg({
	message: 'Year',
	comment: 'Short label in the authentication date of birth field. Keep the tone plain and specific.',
});

const ENTER_YOUR_REAL_DATE_OF_BIRTH_DESCRIPTOR = msg({
	message: 'Enter your real date of birth.',
	comment:
		'Calm error under the date of birth field when the date is today, in the future, impossible, or more than 120 years ago.',
});

type DateFieldType = 'month' | 'day' | 'year';

function useDateOfBirthError(year: string, month: string, day: string, error: string | undefined): string | undefined {
	const {i18n} = useLingui();
	if (error) return error;
	if (!year || !month || !day || isPlausibleDateOfBirthInput(year, month, day)) return undefined;
	return i18n._(ENTER_YOUR_REAL_DATE_OF_BIRTH_DESCRIPTOR);
}

interface DateOfBirthFieldProps {
	selectedMonth: string;
	selectedDay: string;
	selectedYear: string;
	onMonthChange: (month: string) => void;
	onDayChange: (day: string) => void;
	onYearChange: (year: string) => void;
	error?: string;
}

interface NativeDatePickerProps {
	selectedMonth: string;
	selectedDay: string;
	selectedYear: string;
	onMonthChange: (month: string) => void;
	onDayChange: (day: string) => void;
	onYearChange: (year: string) => void;
	error?: string;
}

function NativeDatePicker({
	selectedMonth,
	selectedDay,
	selectedYear,
	onMonthChange,
	onDayChange,
	onYearChange,
	error,
}: NativeDatePickerProps) {
	const {i18n} = useLingui();
	const inputId = useId();
	const dateOfBirthPlaceholder = i18n._(DATE_OF_BIRTH_DESCRIPTOR);
	const yearRange = dateOfBirthYearRange();
	const minDate = `${yearRange.oldest}-01-01`;
	const maxDate = `${yearRange.newest}-12-31`;
	const dateValue = useMemo(() => {
		if (!selectedYear || !selectedMonth || !selectedDay) {
			return '';
		}
		const year = selectedYear.padStart(4, '0');
		const month = selectedMonth.padStart(2, '0');
		const day = selectedDay.padStart(2, '0');
		return `${year}-${month}-${day}`;
	}, [selectedYear, selectedMonth, selectedDay]);
	const handleDateChange = (e: React.ChangeEvent<HTMLInputElement>) => {
		const value = e.target.value;
		if (!value) {
			onYearChange('');
			onMonthChange('');
			onDayChange('');
			return;
		}
		const [year, month, day] = value.split('-');
		onYearChange(String(parseInt(year, 10)));
		onMonthChange(String(parseInt(month, 10)));
		onDayChange(String(parseInt(day, 10)));
	};
	return (
		<fieldset className={styles.fieldset} data-flx="auth.flow.date-of-birth-field.native-date-picker.fieldset">
			<div
				className={styles.labelContainer}
				data-flx="auth.flow.date-of-birth-field.native-date-picker.label-container"
			>
				<label
					htmlFor={inputId}
					className={styles.legend}
					data-flx="auth.flow.date-of-birth-field.native-date-picker.legend"
				>
					<Trans>Date of birth</Trans>
				</label>
			</div>
			<div
				className={styles.inputsContainer}
				data-flx="auth.flow.date-of-birth-field.native-date-picker.inputs-container"
			>
				<FocusRing offset={-2} data-flx="auth.flow.date-of-birth-field.native-date-picker.focus-ring">
					<input
						id={inputId}
						type="date"
						data-flx="auth.flow.date-of-birth-field.native-date-picker.native-date-input.date-change"
						{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
						className={styles.nativeDateInput}
						value={dateValue}
						onChange={handleDateChange}
						min={minDate}
						max={maxDate}
						placeholder={dateOfBirthPlaceholder}
						aria-invalid={!!error || undefined}
						aria-describedby={error ? `${inputId}-error` : undefined}
					/>
				</FocusRing>
				{error && (
					<span
						id={`${inputId}-error`}
						className={styles.errorText}
						role="alert"
						data-flx="auth.flow.date-of-birth-field.native-date-picker.error-text"
					>
						{error}
					</span>
				)}
			</div>
		</fieldset>
	);
}

export const DateOfBirthField = observer(function DateOfBirthField({
	selectedMonth,
	selectedDay,
	selectedYear,
	onMonthChange,
	onDayChange,
	onYearChange,
	error: externalError,
}: DateOfBirthFieldProps) {
	const {i18n} = useLingui();
	const error = useDateOfBirthError(selectedYear, selectedMonth, selectedDay, externalError);
	const monthPlaceholder = i18n._(MONTH_DESCRIPTOR);
	const dayPlaceholder = i18n._(DAY_DESCRIPTOR);
	const yearPlaceholder = i18n._(YEAR_DESCRIPTOR);
	const locale = getCurrentLocale();
	const fieldOrder = useMemo(() => getDateFieldOrder(locale), [locale]);
	const dateOptions = useMemo(() => {
		const yearRange = dateOfBirthYearRange();
		const monthNames = getMonthNames(locale);
		const allMonths = Array.from({length: 12}, (_, index) => {
			return {
				value: String(index + 1),
				label: monthNames[index],
			};
		});
		const years = [];
		for (let year = yearRange.newest; year >= yearRange.oldest; year--) {
			years.push({
				value: String(year),
				label: String(year),
			});
		}
		let availableDays = Array.from({length: 31}, (_, i) => ({
			value: String(i + 1),
			label: String(i + 1),
		}));
		if (selectedYear && selectedMonth) {
			const year = Number(selectedYear);
			const month = Number(selectedMonth);
			const daysInMonth = new Date(year, month, 0).getDate();
			availableDays = availableDays.filter((day) => Number(day.value) <= daysInMonth);
		}
		return {
			months: allMonths,
			days: availableDays,
			years,
		};
	}, [selectedYear, selectedMonth, locale]);
	if (isMobileExperienceEnabled()) {
		return (
			<NativeDatePicker
				selectedMonth={selectedMonth}
				selectedDay={selectedDay}
				selectedYear={selectedYear}
				onMonthChange={onMonthChange}
				onDayChange={onDayChange}
				onYearChange={onYearChange}
				error={error}
				data-flx="auth.flow.date-of-birth-field.native-date-picker"
			/>
		);
	}
	const handleYearChange = (year: string) => {
		onYearChange(year);
		if (selectedDay && selectedYear && selectedMonth) {
			const daysInMonth = new Date(Number(year), Number(selectedMonth), 0).getDate();
			if (Number(selectedDay) > daysInMonth) {
				onDayChange('');
			}
		}
	};
	const handleMonthChange = (month: string) => {
		onMonthChange(month);
		if (selectedDay && selectedYear && month) {
			const daysInMonth = new Date(Number(selectedYear), Number(month), 0).getDate();
			if (Number(selectedDay) > daysInMonth) {
				onDayChange('');
			}
		}
	};
	const fieldComponents: Record<DateFieldType, React.ReactElement> = {
		month: (
			<div key="month" className={styles.monthField} data-flx="auth.flow.date-of-birth-field.month-field">
				<Combobox
					placeholder={monthPlaceholder}
					options={dateOptions.months}
					value={selectedMonth}
					onChange={handleMonthChange}
					tabIndex={0}
					blurInputOnSelect={false}
					openMenuOnFocus={true}
					closeMenuOnSelect={true}
					autoSelectExactMatch={true}
					data-flx="auth.flow.date-of-birth-field.select.month-change"
				/>
			</div>
		),
		day: (
			<div key="day" className={styles.dayField} data-flx="auth.flow.date-of-birth-field.day-field">
				<Combobox
					placeholder={dayPlaceholder}
					options={dateOptions.days}
					value={selectedDay}
					onChange={onDayChange}
					tabIndex={0}
					blurInputOnSelect={false}
					openMenuOnFocus={true}
					closeMenuOnSelect={true}
					autoSelectExactMatch={true}
					data-flx="auth.flow.date-of-birth-field.select.day-change"
				/>
			</div>
		),
		year: (
			<div key="year" className={styles.yearField} data-flx="auth.flow.date-of-birth-field.year-field">
				<Combobox
					placeholder={yearPlaceholder}
					options={dateOptions.years}
					value={selectedYear}
					onChange={handleYearChange}
					tabIndex={0}
					blurInputOnSelect={false}
					openMenuOnFocus={true}
					closeMenuOnSelect={true}
					autoSelectExactMatch={true}
					data-flx="auth.flow.date-of-birth-field.select.year-change"
				/>
			</div>
		),
	};
	const orderedFields = fieldOrder.map((fieldType) => fieldComponents[fieldType]);
	return (
		<fieldset className={styles.fieldset} data-flx="auth.flow.date-of-birth-field.fieldset">
			<div className={styles.labelContainer} data-flx="auth.flow.date-of-birth-field.label-container">
				<legend className={styles.legend} data-flx="auth.flow.date-of-birth-field.legend">
					<Trans>Date of birth</Trans>
				</legend>
			</div>
			<div className={styles.inputsContainer} data-flx="auth.flow.date-of-birth-field.inputs-container">
				<div className={styles.fieldsRow} data-flx="auth.flow.date-of-birth-field.fields-row">
					{orderedFields}
				</div>
				{error && (
					<span className={styles.errorText} role="alert" data-flx="auth.flow.date-of-birth-field.error-text">
						{error}
					</span>
				)}
			</div>
		</fieldset>
	);
});
