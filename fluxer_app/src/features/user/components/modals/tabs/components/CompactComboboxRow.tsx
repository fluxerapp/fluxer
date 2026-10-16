// SPDX-License-Identifier: AGPL-3.0-or-later

import {Combobox, type ComboboxOption, type ComboboxProps} from '@app/features/ui/components/form/FormCombobox';
import styles from '@app/features/user/components/modals/tabs/components/CompactComboboxRow.module.css';
import {clsx} from 'clsx';
import type React from 'react';
import {useId} from 'react';

type CompactComboboxValue = string | number | null;
type CompactComboboxWidth = 'small' | 'medium' | 'large' | 'wide';

interface CompactComboboxRowProps<V extends CompactComboboxValue, O extends ComboboxOption<V> = ComboboxOption<V>> {
	label: React.ReactNode;
	description?: React.ReactNode;
	action?: React.ReactNode;
	value: V;
	options: ReadonlyArray<O>;
	onChange: (value: V) => void;
	disabled?: boolean;
	isSearchable?: boolean;
	placeholder?: string;
	isLoading?: boolean;
	isClearable?: boolean;
	autoSelectExactMatch?: boolean;
	autoSelectValueFromInput?: ComboboxProps<V, O>['autoSelectValueFromInput'];
	filterOption?: ComboboxProps<V, O>['filterOption'];
	menuMinWidth?: number;
	controlWidth?: CompactComboboxWidth;
	renderOption?: (option: O, isSelected: boolean) => React.ReactNode;
	renderValue?: (option: O | null) => React.ReactNode;
	className?: string;
	dataFlx: string;
	'aria-label'?: string;
}

const widthClass: Record<CompactComboboxWidth, string> = {
	small: styles.comboboxWrapSmall,
	medium: styles.comboboxWrapMedium,
	large: styles.comboboxWrapLarge,
	wide: styles.comboboxWrapWide,
};

export function CompactComboboxRow<V extends CompactComboboxValue, O extends ComboboxOption<V> = ComboboxOption<V>>({
	label,
	description,
	action,
	value,
	options,
	onChange,
	disabled,
	isSearchable = true,
	placeholder,
	isLoading,
	isClearable,
	autoSelectExactMatch,
	autoSelectValueFromInput,
	filterOption,
	menuMinWidth,
	controlWidth = 'medium',
	renderOption,
	renderValue,
	className,
	dataFlx,
	'aria-label': ariaLabel,
}: CompactComboboxRowProps<V, O>): React.ReactElement {
	const id = useId();
	const selectedOption = options.find((option) => Object.is(option.value, value));
	const sizingLabel = selectedOption?.label ?? placeholder ?? '';
	const comboboxWrapStyle = {
		'--compact-combobox-content-width': `${sizingLabel.length}ch`,
	} as React.CSSProperties;
	return (
		<div className={clsx(styles.row, className)} data-flx={`${dataFlx}.row`}>
			<div className={styles.text} data-flx={`${dataFlx}.text`}>
				<div className={styles.titleRow} data-flx={`${dataFlx}.title-row`}>
					<label htmlFor={id} className={clsx(styles.label, disabled && styles.disabled)} data-flx={`${dataFlx}.label`}>
						{label}
					</label>
					{action ? (
						<div className={styles.action} data-flx={`${dataFlx}.action`}>
							{action}
						</div>
					) : null}
				</div>
				{description ? (
					<p className={clsx(styles.description, disabled && styles.disabled)} data-flx={`${dataFlx}.description`}>
						{description}
					</p>
				) : null}
			</div>
			<div
				className={clsx(styles.comboboxWrap, widthClass[controlWidth])}
				style={comboboxWrapStyle}
				data-flx={`${dataFlx}.select-wrap`}
			>
				<Combobox<V, O>
					id={id}
					value={value}
					options={options}
					onChange={onChange}
					disabled={disabled}
					className={styles.combobox}
					density="compact"
					isSearchable={isSearchable}
					placeholder={placeholder}
					isLoading={isLoading}
					isClearable={isClearable}
					autoSelectExactMatch={autoSelectExactMatch}
					autoSelectValueFromInput={autoSelectValueFromInput}
					filterOption={filterOption}
					menuMinWidth={menuMinWidth}
					wrapMenuText={true}
					renderOption={renderOption}
					renderValue={renderValue}
					aria-label={ariaLabel}
					data-flx={dataFlx}
				/>
			</div>
		</div>
	);
}
