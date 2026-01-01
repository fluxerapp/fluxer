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

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo} from 'react';
import {Select} from '~/components/form/Select';
import {getCurrentLocale} from '~/utils/LocaleUtils';
import styles from './DateOfBirthField.module.css';

type DateFieldType = 'month' | 'day' | 'year';

function isMobileWebBrowser(): boolean {
	return /Android|iPhone|iPad|iPod/i.test(navigator.userAgent);
}

function getDateFieldOrder(locale: string): Array<DateFieldType> {
	const formatter = new Intl.DateTimeFormat(locale, {
		year: 'numeric',
		month: '2-digit',
		day: '2-digit',
	});

	const parts = formatter.formatToParts(new Date(2000, 0, 1));
	const order: Array<DateFieldType> = [];

	for (const part of parts) {
		if (part.type === 'month' && !order.includes('month')) {
			order.push('month');
		} else if (part.type === 'day' && !order.includes('day')) {
			order.push('day');
		} else if (part.type === 'year' && !order.includes('year')) {
			order.push('year');
		}
	}

	return order;
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
	const {t} = useLingui();
	const _monthPlaceholder = t`Month`;
	const _dayPlaceholder = t`Day`;
	const _yearPlaceholder = t`Year`;
	const dateOfBirthPlaceholder = t`Date of birth`;

	const currentYear = new Date().getFullYear();
	const minDate = `${currentYear - 150}-01-01`;
	const maxDate = `${currentYear}-12-31`;

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
		<fieldset className={styles.fieldset}>
			<div className={styles.labelContainer}>
				<legend className={styles.legend}>
					<Trans>Date of birth</Trans>
				</legend>
			</div>
			<div className={styles.inputsContainer}>
				<input
					type="date"
					className={styles.nativeDateInput}
					value={dateValue}
					onChange={handleDateChange}
					min={minDate}
					max={maxDate}
					placeholder={dateOfBirthPlaceholder}
					aria-invalid={!!error || undefined}
				/>
				{error && <span className={styles.errorText}>{error}</span>}
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
	error,
}: DateOfBirthFieldProps) {
	const {t} = useLingui();
	const monthPlaceholder = t`Month`;
	const dayPlaceholder = t`Day`;
	const yearPlaceholder = t`Year`;

	const locale = getCurrentLocale();
	const fieldOrder = useMemo(() => getDateFieldOrder(locale), [locale]);

	const dateOptions = useMemo(() => {
		const currentDate = new Date();
		const currentYear = currentDate.getFullYear();

		const allMonths = Array.from({length: 12}, (_, index) => {
			const monthDate = new Date(2000, index, 1);
			const monthName = new Intl.DateTimeFormat(locale, {month: 'long'}).format(monthDate);
			return {
				value: String(index + 1),
				label: monthName,
			};
		});

		const years = [];
		for (let year = currentYear; year >= currentYear - 150; year--) {
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

	if (isMobileWebBrowser()) {
		return (
			<NativeDatePicker
				selectedMonth={selectedMonth}
				selectedDay={selectedDay}
				selectedYear={selectedYear}
				onMonthChange={onMonthChange}
				onDayChange={onDayChange}
				onYearChange={onYearChange}
				error={error}
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
			<div key="month" className={styles.monthField}>
				<Select
					placeholder={monthPlaceholder}
					options={dateOptions.months}
					value={selectedMonth}
					onChange={handleMonthChange}
					tabIndex={0}
					tabSelectsValue={false}
					blurInputOnSelect={false}
					openMenuOnFocus={true}
					closeMenuOnSelect={true}
					autoSelectExactMatch={true}
				/>
			</div>
		),
		day: (
			<div key="day" className={styles.dayField}>
				<Select
					placeholder={dayPlaceholder}
					options={dateOptions.days}
					value={selectedDay}
					onChange={onDayChange}
					tabIndex={0}
					tabSelectsValue={false}
					blurInputOnSelect={false}
					openMenuOnFocus={true}
					closeMenuOnSelect={true}
					autoSelectExactMatch={true}
				/>
			</div>
		),
		year: (
			<div key="year" className={styles.yearField}>
				<Select
					placeholder={yearPlaceholder}
					options={dateOptions.years}
					value={selectedYear}
					onChange={handleYearChange}
					tabIndex={0}
					tabSelectsValue={false}
					blurInputOnSelect={false}
					openMenuOnFocus={true}
					closeMenuOnSelect={true}
					autoSelectExactMatch={true}
				/>
			</div>
		),
	};

	const orderedFields = fieldOrder.map((fieldType) => fieldComponents[fieldType]);

	return (
		<fieldset className={styles.fieldset}>
			<div className={styles.labelContainer}>
				<legend className={styles.legend}>
					<Trans>Date of birth</Trans>
				</legend>
			</div>
			<div className={styles.inputsContainer}>
				<div className={styles.fieldsRow}>{orderedFields}</div>
				{error && <span className={styles.errorText}>{error}</span>}
			</div>
		</fieldset>
	);
});
