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

import {clsx} from 'clsx';
import React from 'react';
import {MenuItem as AriaMenuItem} from 'react-aria-components';
import {Slider} from '../Slider';
import styles from './MenuItem.module.css';

interface MenuItemSliderProps {
	label: string;
	value: number;
	minValue?: number;
	maxValue?: number;
	disabled?: boolean;
	onChange?: (value: number) => void;
	onFormat?: (value: number) => string;
}

export const MenuItemSlider = React.forwardRef<HTMLDivElement, MenuItemSliderProps>(
	({label, value, minValue = 0, maxValue = 100, disabled = false, onChange, onFormat}, forwardedRef) => {
		const [localValue, setLocalValue] = React.useState(value);

		React.useEffect(() => {
			setLocalValue(value);
		}, [value]);

		const formattedValue = onFormat ? onFormat(localValue) : `${Math.round(localValue)}%`;

		const handleValueChange = React.useCallback(
			(newValue: number) => {
				setLocalValue(newValue);
				onChange?.(newValue);
			},
			[onChange],
		);

		const handleValueCommit = React.useCallback(
			(newValue: number) => {
				onChange?.(newValue);
			},
			[onChange],
		);

		const handleSliderInteraction = React.useCallback((e: React.SyntheticEvent) => {
			e.stopPropagation();
			e.preventDefault();
		}, []);

		return (
			<AriaMenuItem
				ref={forwardedRef}
				className={clsx(styles.sliderItem, {
					[styles.disabled]: disabled,
				})}
				isDisabled={disabled}
				textValue={label}
			>
				{/* biome-ignore lint/a11y/noStaticElementInteractions: interactive slider element */}
				<div
					onClick={handleSliderInteraction}
					onKeyDown={(e) => {
						if (e.key === 'Enter' || e.key === ' ') {
							e.preventDefault();
						}
					}}
					style={{width: '100%'}}
				>
					<div className={styles.sliderHeader}>
						<span className={styles.sliderLabel}>{label}</span>
						<span className={styles.sliderValue}>{formattedValue}</span>
					</div>
					<div className={styles.sliderContainer}>
						<Slider
							defaultValue={localValue}
							factoryDefaultValue={100}
							minValue={minValue}
							maxValue={maxValue}
							disabled={disabled}
							onValueChange={handleValueCommit}
							asValueChanges={handleValueChange}
							mini={true}
							value={localValue}
						/>
					</div>
				</div>
			</AriaMenuItem>
		);
	},
);

MenuItemSlider.displayName = 'MenuItemSlider';
