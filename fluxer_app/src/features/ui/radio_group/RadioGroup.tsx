// SPDX-License-Identifier: AGPL-3.0-or-later

import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import styles from '@app/features/ui/radio_group/RadioGroup.module.css';
import * as RadixRadioGroup from '@radix-ui/react-radio-group';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef} from 'react';

export interface RadioOption<T> {
	value: T;
	name: string | React.ReactNode;
	desc?: string | React.ReactNode;
	disabled?: boolean;
}

interface RadioGroupProps<T> {
	options: ReadonlyArray<RadioOption<T>>;
	value: T | null;
	disabled?: boolean;
	className?: string;
	onChange: (value: T) => void;
	renderContent?: (option: RadioOption<T>, checked: boolean) => React.ReactNode;
	optionAlign?: 'start' | 'center';
	'aria-label'?: string;
}

interface RadioOptionItemProps<T> {
	option: RadioOption<T>;
	value: string;
	renderContent?: (option: RadioOption<T>, checked: boolean) => React.ReactNode;
	isSelected: boolean;
	groupDisabled: boolean;
	align: 'start' | 'center';
}

const RadioOptionItem = <T,>({
	option,
	value,
	renderContent,
	isSelected,
	groupDisabled,
	align,
}: RadioOptionItemProps<T>) => {
	const radioRef = useRef<HTMLButtonElement | null>(null);
	return (
		<FocusRing
			focusTarget={radioRef}
			ringTarget={radioRef}
			offset={-2}
			ringClassName={styles.focusRing}
			enabled={!option.disabled && !groupDisabled}
			data-flx="ui.radio-group.radio-group.radio-option-item.focus-ring"
		>
			<RadixRadioGroup.Item
				ref={radioRef}
				value={value}
				disabled={option.disabled || groupDisabled}
				className={clsx(styles.radioGroupOption, align === 'center' && styles.radioGroupOptionCenter)}
				data-flx="ui.radio-group.radio-group.radio-option-item.radio-group-option"
			>
				<svg
					className={styles.radioIndicator}
					width="20"
					height="20"
					viewBox="0 0 40 40"
					fill="none"
					shapeRendering="geometricPrecision"
					aria-hidden="true"
					data-flx="ui.radio-group.radio-group.radio-option-item.radio-indicator"
				>
					<circle
						cx="20"
						cy="20"
						r="20"
						className={styles.outerRadioBase}
						data-flx="ui.radio-group.radio-group.radio-option-item.outer-radio-base"
					/>
					<circle
						cx="20"
						cy="20"
						r="20"
						className={styles.outerRadioFill}
						data-flx="ui.radio-group.radio-group.radio-option-item.outer-radio-fill"
					/>
					<circle
						cx="20"
						cy="20"
						r="8"
						className={styles.innerDotRadio}
						data-flx="ui.radio-group.radio-group.radio-option-item.inner-dot-radio"
					/>
				</svg>
				<div className={styles.stack} data-flx="ui.radio-group.radio-group.radio-option-item.stack">
					{renderContent ? (
						<div
							className={styles.customContent}
							data-flx="ui.radio-group.radio-group.radio-option-item.custom-content"
						>
							{renderContent(option, isSelected)}
						</div>
					) : (
						<>
							<span className={styles.label} data-flx="ui.radio-group.radio-group.radio-option-item.label">
								<div className={styles.labelText} data-flx="ui.radio-group.radio-group.radio-option-item.label-text">
									{option.name}
								</div>
							</span>
							{option.desc && (
								<div className={styles.description} data-flx="ui.radio-group.radio-group.radio-option-item.description">
									{option.desc}
								</div>
							)}
						</>
					)}
				</div>
			</RadixRadioGroup.Item>
		</FocusRing>
	);
};
export const RadioGroup = observer(
	<T,>({
		options,
		value,
		disabled = false,
		className,
		onChange,
		renderContent,
		optionAlign = 'start',
		'aria-label': ariaLabel,
	}: RadioGroupProps<T>) => {
		const valueToString = (val: T): string => {
			if (typeof val === 'string') return val;
			if (typeof val === 'number') return String(val);
			return JSON.stringify(val);
		};
		const stringToValue = (str: string): T | undefined => {
			const option = options.find((opt) => valueToString(opt.value) === str);
			return option?.value;
		};
		const currentStringValue = value !== null ? valueToString(value) : undefined;
		const handleChange = (newStringValue: string) => {
			const nextValue = stringToValue(newStringValue);
			if (nextValue !== undefined) {
				onChange(nextValue);
			}
		};
		return (
			<RadixRadioGroup.Root
				className={clsx(styles.group, className)}
				value={currentStringValue}
				onValueChange={handleChange}
				disabled={disabled}
				orientation="vertical"
				aria-label={ariaLabel}
				data-flx="ui.radio-group.radio-group.group"
			>
				{options.map((option) => {
					const stringValue = valueToString(option.value);
					const isSelected = currentStringValue === stringValue;
					return (
						<RadioOptionItem
							key={stringValue}
							option={option}
							value={stringValue}
							renderContent={renderContent}
							isSelected={isSelected}
							groupDisabled={disabled}
							align={optionAlign}
							data-flx="ui.radio-group.radio-group.radio-option-item"
						/>
					);
				})}
			</RadixRadioGroup.Root>
		);
	},
);
