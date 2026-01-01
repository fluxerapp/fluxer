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
import {CaretDownIcon, CheckIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import type {SelectOption} from './Select';
import styles from './SelectBottomSheet.module.css';

type Primitive = string | number | null;

interface SelectBottomSheetProps<
	V extends Primitive = string,
	IsMulti extends boolean = false,
	O extends SelectOption<V> = SelectOption<V>,
> {
	label?: string;
	description?: string;
	value: IsMulti extends true ? Array<V> : V;
	options: ReadonlyArray<O>;
	onChange: (value: IsMulti extends true ? Array<V> : V) => void;
	disabled?: boolean;
	error?: string;
	placeholder?: string;
	className?: string;
	id?: string;
	isMulti?: IsMulti;
	renderOption?: (option: O, isSelected: boolean) => React.ReactNode;
	renderValue?: (option: IsMulti extends true ? Array<O> : O | null) => React.ReactNode;
}

interface OptionItemProps<V extends Primitive, O extends SelectOption<V> = SelectOption<V>> {
	option: O;
	isSelected: boolean;
	onSelect: () => void;
	customContent?: React.ReactNode;
}

const OptionItem = <V extends Primitive, O extends SelectOption<V> = SelectOption<V>>({
	option,
	isSelected,
	onSelect,
	customContent,
}: OptionItemProps<V, O>) => (
	<button
		type="button"
		onClick={onSelect}
		className={clsx(styles.optionButton, isSelected && styles.optionButtonSelected)}
		disabled={option.isDisabled}
		aria-pressed={isSelected}
	>
		{customContent ?? (
			<span className={clsx(styles.optionLabel, option.isDisabled && styles.optionDisabled)}>{option.label}</span>
		)}
		{isSelected && (
			<div className={styles.checkIconContainer}>
				<CheckIcon weight="bold" className={styles.checkIcon} />
			</div>
		)}
	</button>
);

export const SelectBottomSheet = observer(function SelectBottomSheet<
	V extends Primitive = string,
	IsMulti extends boolean = false,
	O extends SelectOption<V> = SelectOption<V>,
>({
	id,
	label,
	description,
	value,
	options,
	onChange,
	disabled = false,
	error,
	placeholder,
	className,
	isMulti,
	renderOption,
	renderValue,
}: SelectBottomSheetProps<V, IsMulti, O>) {
	const {t} = useLingui();
	const [isOpen, setIsOpen] = React.useState(false);

	const selectedOption = React.useMemo(() => {
		if (isMulti) {
			if (!Array.isArray(value)) return [];
			return options.filter((option) => (value as Array<V>).includes(option.value));
		}
		return options.find((option) => option.value === value) || null;
	}, [isMulti, options, value]);

	const displayValue = React.useMemo(() => {
		if (renderValue) {
			return renderValue(selectedOption as IsMulti extends true ? Array<O> : O | null);
		}
		if (isMulti) {
			const selected = selectedOption as Array<O>;
			if (selected.length === 0) return placeholder ?? t`Select...`;
			if (selected.length === 1) return selected[0].label;
			return t`${selected.length} selected`;
		}
		return (selectedOption as O | null)?.label ?? placeholder ?? t`Select...`;
	}, [isMulti, selectedOption, placeholder, renderValue]);

	const handleOpen = () => {
		if (!disabled) {
			setIsOpen(true);
		}
	};

	const handleClose = () => {
		setIsOpen(false);
	};

	const handleSelect = (optionValue: V) => {
		if (isMulti) {
			const currentValues = Array.isArray(value) ? (value as Array<V>) : [];
			const newValues = currentValues.includes(optionValue)
				? currentValues.filter((v) => v !== optionValue)
				: [...currentValues, optionValue];
			(onChange as (value: Array<V>) => void)(newValues);
		} else {
			(onChange as (value: V) => void)(optionValue);
			handleClose();
		}
	};

	const isOptionSelected = (optionValue: V): boolean => {
		if (isMulti) {
			return Array.isArray(value) && (value as Array<V>).includes(optionValue);
		}
		return value === optionValue;
	};

	const generatedTriggerId = React.useId();
	const triggerId = id ?? generatedTriggerId;

	return (
		<div className={styles.container}>
			{label && (
				<label htmlFor={triggerId} className={clsx(styles.label, disabled && styles.disabled)}>
					{label}
				</label>
			)}
			<div className={className}>
				<button
					id={triggerId}
					type="button"
					onClick={handleOpen}
					disabled={disabled}
					className={clsx(styles.trigger, disabled && styles.triggerDisabled, error && styles.triggerError)}
				>
					<span className={clsx(styles.triggerValue, !selectedOption && styles.triggerPlaceholder)}>
						{displayValue}
					</span>
					<CaretDownIcon weight="bold" className={styles.triggerIcon} />
				</button>
			</div>
			{description && <p className={clsx(styles.description, disabled && styles.disabled)}>{description}</p>}
			{error && <span className={styles.errorText}>{error}</span>}

			<BottomSheet
				isOpen={isOpen}
				onClose={handleClose}
				snapPoints={[0, 0.6, 1]}
				initialSnap={1}
				disableDefaultHeader
				zIndex={30000}
			>
				<div className={styles.scrollContainer}>
					<div className={styles.optionsContainer}>
						{options.map((option, index) => {
							const isSelected = isOptionSelected(option.value);
							return (
								<React.Fragment key={String(option.value)}>
									<OptionItem
										option={option}
										isSelected={isSelected}
										onSelect={() => handleSelect(option.value)}
										customContent={renderOption?.(option, isSelected)}
									/>
									{index < options.length - 1 && <div className={styles.divider} />}
								</React.Fragment>
							);
						})}
					</div>
					<div className={styles.bottomSpacer} />
				</div>
			</BottomSheet>
		</div>
	);
});
