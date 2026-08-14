// SPDX-License-Identifier: AGPL-3.0-or-later

import {Button} from '@app/features/ui/button/Button';
import styles from '@app/features/ui/components/Slider.module.css';
import {SliderResetIconButton} from '@app/features/ui/components/slider/SliderResetIconButton';
import type {SliderResetButtonPlacement} from '@app/features/ui/components/slider/SliderTypes';
import {ArrowCounterClockwiseIcon} from '@phosphor-icons/react';
import type React from 'react';

interface SliderResetControlsProps {
	placement: SliderResetButtonPlacement;
	canReset: boolean;
	onReset: () => void;
	inlineLabel: string;
	label: React.ReactNode;
	accessory?: React.ReactNode;
	children: React.ReactNode;
}

export function SliderResetControls({
	placement,
	canReset,
	onReset,
	inlineLabel,
	label,
	accessory,
	children,
}: SliderResetControlsProps): React.ReactElement {
	if (placement === 'below') {
		return (
			<>
				{children}
				{canReset || accessory ? (
					<div className={styles.resetRow} data-flx="ui.slider.slider-reset-controls.reset-row">
						{canReset ? (
							<Button
								variant="secondary"
								small
								fitContent
								onClick={onReset}
								leftIcon={
									<ArrowCounterClockwiseIcon
										size={14}
										weight="bold"
										data-flx="ui.slider.slider-reset-controls.arrow-counter-clockwise-icon"
									/>
								}
								data-flx="ui.slider.slider-reset-controls.button.reset"
							>
								{label}
							</Button>
						) : null}
						{accessory ? (
							<span className={styles.resetAccessory} data-flx="ui.slider.slider-reset-controls.reset-accessory">
								{accessory}
							</span>
						) : null}
					</div>
				) : null}
			</>
		);
	}
	return (
		<div className={styles.sliderRow} data-flx="ui.slider.slider-reset-controls.slider-row">
			<div className={styles.sliderResetSlot} data-flx="ui.slider.slider-reset-controls.reset-slot">
				<SliderResetIconButton
					canReset={canReset}
					onReset={onReset}
					ariaLabel={inlineLabel}
					alwaysVisible={true}
					dataFlx="ui.slider.slider-reset-controls.reset-button"
					data-flx="ui.slider.slider-reset-controls.slider-reset-icon-button"
				/>
			</div>
			<div className={styles.sliderRowMain} data-flx="ui.slider.slider-reset-controls.slider-row-main">
				{children}
			</div>
		</div>
	);
}
