// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/ui/action_menu/MenuItem.module.css';
import {Slider} from '@app/features/ui/components/Slider';
import {formatRoundedPercentage} from '@app/features/ui/utils/PercentageFormatting';
import {getFormattingLocale} from '@app/features/user/utils/LocaleUtils';
import {ContextMenu as BaseContextMenu} from '@base-ui/react/context-menu';
import {useLingui} from '@lingui/react';
import {clsx} from 'clsx';
import React, {useCallback, useEffect, useRef, useState} from 'react';

interface MenuItemSliderProps {
	label: string;
	value: number;
	minValue?: number;
	maxValue?: number;
	disabled?: boolean;
	onChange?: (value: number) => void;
	onFormat?: (value: number) => string;
	step?: number;
}

export const MenuItemSlider = React.forwardRef<HTMLDivElement, MenuItemSliderProps>(
	({label, value, minValue = 0, maxValue = 100, disabled = false, onChange, onFormat, step = 1}, forwardedRef) => {
		useLingui();
		const [localValue, setLocalValue] = useState(value);
		const localValueRef = useRef(value);
		localValueRef.current = localValue;
		useEffect(() => {
			setLocalValue(value);
		}, [value]);
		const formattedValue = onFormat ? onFormat(localValue) : formatRoundedPercentage(getFormattingLocale(), localValue);
		const handleValueChange = useCallback(
			(newValue: number) => {
				localValueRef.current = newValue;
				setLocalValue(newValue);
				onChange?.(newValue);
			},
			[onChange],
		);
		const handleValueCommit = useCallback(
			(newValue: number) => {
				onChange?.(newValue);
			},
			[onChange],
		);
		const stopPropagation = useCallback((e: React.SyntheticEvent) => {
			e.stopPropagation();
		}, []);
		const handleKeyDown = useCallback(
			(event: React.KeyboardEvent<HTMLDivElement>) => {
				if (disabled) return;
				const current = localValueRef.current;
				let next: number;
				switch (event.key) {
					case 'ArrowRight':
						next = current + step;
						break;
					case 'ArrowLeft':
						next = current - step;
						break;
					case 'PageUp':
						next = current + step * 10;
						break;
					case 'PageDown':
						next = current - step * 10;
						break;
					case 'Home':
						next = minValue;
						break;
					case 'End':
						next = maxValue;
						break;
					default:
						return;
				}
				event.preventDefault();
				event.stopPropagation();
				handleValueChange(Math.min(maxValue, Math.max(minValue, next)));
			},
			[disabled, step, minValue, maxValue, handleValueChange],
		);
		return (
			<BaseContextMenu.Item
				ref={forwardedRef}
				className={clsx(styles.sliderItem, {
					[styles.disabled]: disabled,
				})}
				disabled={disabled}
				closeOnClick={false}
				label={label}
				role="slider"
				aria-label={label}
				aria-valuemin={minValue}
				aria-valuemax={maxValue}
				aria-valuenow={localValue}
				aria-valuetext={formattedValue}
				aria-orientation="horizontal"
				onKeyDown={handleKeyDown}
				data-flx="ui.action-menu.menu-item-slider.slider-item"
			>
				{/* biome-ignore lint/a11y/noStaticElementInteractions: presentational wrapper that only stops propagation of focus-stealing events. */}
				<div
					role="presentation"
					onPointerDown={stopPropagation}
					onMouseDown={stopPropagation}
					onClick={stopPropagation}
					style={{width: '100%'}}
					data-flx="ui.action-menu.menu-item-slider.presentation.stop-propagation"
				>
					<div className={styles.sliderHeader} data-flx="ui.action-menu.menu-item-slider.slider-header">
						<span className={styles.sliderLabel} data-flx="ui.action-menu.menu-item-slider.slider-label">
							{label}
						</span>
						<span className={styles.sliderValue} data-flx="ui.action-menu.menu-item-slider.slider-value">
							{formattedValue}
						</span>
					</div>
					<div className={styles.sliderContainer} data-flx="ui.action-menu.menu-item-slider.slider-container">
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
							step={step}
							data-flx="ui.action-menu.menu-item-slider.slider"
						/>
					</div>
				</div>
			</BaseContextMenu.Item>
		);
	},
);

MenuItemSlider.displayName = 'MenuItemSlider';
