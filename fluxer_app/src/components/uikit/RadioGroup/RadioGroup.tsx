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

import * as RadixRadioGroup from '@radix-ui/react-radio-group';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import styles from './RadioGroup.module.css';

export interface RadioOption<T = unknown> {
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
	'aria-label'?: string;
}

interface RadioOptionItemProps<T> {
	option: RadioOption<T>;
	value: string;
	renderContent?: (option: RadioOption<T>, checked: boolean) => React.ReactNode;
	isSelected: boolean;
	groupDisabled: boolean;
}

const RadioOptionItem = <T,>({option, value, renderContent, isSelected, groupDisabled}: RadioOptionItemProps<T>) => {
	const radioRef = React.useRef<HTMLButtonElement | null>(null);

	return (
		<FocusRing
			focusTarget={radioRef}
			ringTarget={radioRef}
			offset={-2}
			ringClassName={styles.focusRing}
			enabled={!option.disabled && !groupDisabled}
		>
			<RadixRadioGroup.Item
				ref={radioRef}
				value={value}
				disabled={option.disabled || groupDisabled}
				className={styles.radioGroupOption}
			>
				<svg
					className={styles.radioIndicator}
					width="20"
					height="20"
					viewBox="0 0 40 40"
					fill="none"
					shapeRendering="geometricPrecision"
					aria-hidden="true"
				>
					<circle cx="20" cy="20" r="20" className={styles.outerRadioBase} />
					<circle cx="20" cy="20" r="20" className={styles.outerRadioFill} />
					<circle cx="20" cy="20" r="8" className={styles.innerDotRadio} />
				</svg>
				<div className={styles.stack}>
					{renderContent ? (
						<div className={styles.customContent}>{renderContent(option, isSelected)}</div>
					) : (
						<>
							<span className={styles.label}>
								<div className={styles.labelText}>{option.name}</div>
							</span>
							{option.desc && <div className={styles.description}>{option.desc}</div>}
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
						/>
					);
				})}
			</RadixRadioGroup.Root>
		);
	},
);
