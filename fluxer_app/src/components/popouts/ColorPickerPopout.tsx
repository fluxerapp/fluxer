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

import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {ColorArea, ColorPicker, ColorSlider, ColorThumb, parseColor, SliderTrack} from 'react-aria-components';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import styles from './ColorPickerPopout.module.css';

export const ColorPickerPopout = observer(
	({
		color,
		onChange,
		onReset,
	}: {
		popoutKey?: string | number;
		color: string;
		onChange: (color: string) => void;
		onReset: () => void;
	}) => {
		const hasCustomColor = color !== null && color !== '#4641D9';

		const handleColorChange = React.useCallback(
			(newColor: ReturnType<typeof parseColor>) => {
				onChange(newColor.toString('hex'));
			},
			[onChange],
		);

		const parsedColor = React.useMemo(() => {
			try {
				return parseColor(color).toFormat('hsb');
			} catch {
				return parseColor('#4641D9').toFormat('hsb');
			}
		}, [color]);

		return (
			<div className={styles.container}>
				<ColorPicker value={parsedColor} onChange={handleColorChange}>
					<div className={hasCustomColor ? styles.pickerContainerWithMargin : styles.pickerContainer}>
						<div className={styles.pickerWrapper}>
							<ColorArea colorSpace="hsb" xChannel="saturation" yChannel="brightness" className={styles.colorArea}>
								<ColorThumb className={styles.colorThumb} />
							</ColorArea>
							<ColorSlider channel="hue" className={styles.colorSlider}>
								<SliderTrack className={styles.sliderTrack}>
									<ColorThumb className={styles.colorThumb} />
								</SliderTrack>
							</ColorSlider>
						</div>
					</div>
				</ColorPicker>
				<FocusRing offset={-2}>
					<button type="button" className={styles.resetButton} onClick={onReset} disabled={!hasCustomColor}>
						<span className={styles.resetButtonText}>
							<Trans>Reset</Trans>
						</span>
					</button>
				</FocusRing>
			</div>
		);
	},
);
