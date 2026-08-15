// SPDX-License-Identifier: AGPL-3.0-or-later

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/ui/components/Slider.module.css';
import {ArrowCounterClockwiseIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';

interface SliderResetIconButtonProps {
	canReset: boolean;
	onReset: () => void;
	ariaLabel: string;
	className?: string;
	iconSize?: number;
	dataFlx?: string;
	alwaysVisible?: boolean;
}

export function canResetSliderValue(value: number, factoryDefaultValue: number, disabled = false): boolean {
	return !disabled && value !== factoryDefaultValue;
}

export function SliderResetIconButton({
	canReset,
	onReset,
	ariaLabel,
	className,
	iconSize = 14,
	dataFlx = 'ui.slider.slider-reset-icon-button.button',
	alwaysVisible = false,
}: SliderResetIconButtonProps): React.ReactElement | null {
	if (!canReset && !alwaysVisible) return null;
	return (
		<button
			type="button"
			className={clsx(styles.resetButton, className)}
			onClick={onReset}
			disabled={!canReset}
			aria-label={ariaLabel}
			data-flx={dataFlx}
		>
			<ArrowCounterClockwiseIcon
				size={remFromPx(iconSize)}
				weight="bold"
				data-flx="ui.slider.slider-reset-icon-button.arrow-counter-clockwise-icon"
			/>
		</button>
	);
}
