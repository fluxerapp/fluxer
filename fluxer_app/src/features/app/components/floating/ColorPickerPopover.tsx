// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getColorWithAlpha,
	getColorWithHue,
	getColorWithSaturationBrightness,
	getUnitPointFromClientPosition,
	getUnitValueFromClientX,
	shouldSyncPickerColorFromProp,
} from '@app/features/app/components/floating/ColorPickerMath';
import styles from '@app/features/app/components/floating/ColorPickerPopover.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Trans} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import {
	type Color,
	ColorArea,
	ColorPicker,
	ColorSlider,
	ColorThumb,
	parseColor,
	SliderTrack,
} from 'react-aria-components';

type DragTarget = 'color-area' | 'hue-slider' | 'alpha-slider';

export const ColorPickerPopover = observer(
	({
		color,
		onChange,
		onReset,
		showAlpha = false,
		children,
	}: {
		popoutKey?: string | number;
		color: string;
		onChange: (color: string) => void;
		onReset: () => void;
		showAlpha?: boolean;
		children?: React.ReactNode;
	}) => {
		const hasCustomColor = color !== null && color !== '#4641D9';
		const parsedColor = useMemo(() => {
			try {
				return parseColor(color).toFormat('hsb');
			} catch {
				return parseColor('#4641D9').toFormat('hsb');
			}
		}, [color]);
		const activeDragRef = useRef<{pointerId: number; target: DragTarget} | null>(null);
		const colorRef = useRef<Color>(parsedColor);
		const [pickerColor, setPickerColor] = useState(parsedColor);
		const handleColorChange = useCallback(
			(newColor: Color) => {
				colorRef.current = newColor;
				setPickerColor(newColor);
				onChange(newColor.toString(showAlpha ? 'hexa' : 'hex'));
			},
			[onChange, showAlpha],
		);
		useEffect(() => {
			if (!shouldSyncPickerColorFromProp(parsedColor, colorRef.current)) return;
			colorRef.current = parsedColor;
			setPickerColor(parsedColor);
		}, [parsedColor]);
		const focusGestureInput = useCallback((element: HTMLElement) => {
			element.querySelector<HTMLInputElement>('input')?.focus({preventScroll: true});
		}, []);
		const updateColorAreaFromPointer = useCallback(
			(clientX: number, clientY: number, element: HTMLElement) => {
				const point = getUnitPointFromClientPosition(clientX, clientY, element.getBoundingClientRect());
				if (!point) return;
				handleColorChange(getColorWithSaturationBrightness(colorRef.current, point));
			},
			[handleColorChange],
		);
		const updateSliderFromPointer = useCallback(
			(clientX: number, element: HTMLElement, target: DragTarget) => {
				const unitValue = getUnitValueFromClientX(clientX, element.getBoundingClientRect());
				if (unitValue === null) return;
				handleColorChange(
					target === 'alpha-slider'
						? getColorWithAlpha(colorRef.current, unitValue)
						: getColorWithHue(colorRef.current, unitValue),
				);
			},
			[handleColorChange],
		);
		const capturePointer = useCallback(
			(event: React.PointerEvent<HTMLElement>, target: DragTarget) => {
				if (event.pointerType === 'mouse' && (event.button !== 0 || event.altKey || event.ctrlKey || event.metaKey)) {
					return;
				}
				event.preventDefault();
				event.stopPropagation();
				const element = event.currentTarget;
				activeDragRef.current = {pointerId: event.pointerId, target};
				element.setPointerCapture?.(event.pointerId);
				focusGestureInput(element);
				if (target === 'color-area') {
					updateColorAreaFromPointer(event.clientX, event.clientY, element);
				} else {
					updateSliderFromPointer(event.clientX, element, target);
				}
			},
			[focusGestureInput, updateColorAreaFromPointer, updateSliderFromPointer],
		);
		const updateCapturedPointer = useCallback(
			(event: React.PointerEvent<HTMLElement>, target: DragTarget) => {
				const activeDrag = activeDragRef.current;
				if (!activeDrag || activeDrag.pointerId !== event.pointerId || activeDrag.target !== target) return;
				event.preventDefault();
				event.stopPropagation();
				if (target === 'color-area') {
					updateColorAreaFromPointer(event.clientX, event.clientY, event.currentTarget);
				} else {
					updateSliderFromPointer(event.clientX, event.currentTarget, target);
				}
			},
			[updateColorAreaFromPointer, updateSliderFromPointer],
		);
		const releaseCapturedPointer = useCallback(
			(event: React.PointerEvent<HTMLElement>, target: DragTarget) => {
				const activeDrag = activeDragRef.current;
				if (!activeDrag || activeDrag.pointerId !== event.pointerId || activeDrag.target !== target) return;
				event.preventDefault();
				event.stopPropagation();
				if (target === 'color-area') {
					updateColorAreaFromPointer(event.clientX, event.clientY, event.currentTarget);
				} else {
					updateSliderFromPointer(event.clientX, event.currentTarget, target);
				}
				activeDragRef.current = null;
				if (event.currentTarget.hasPointerCapture?.(event.pointerId)) {
					event.currentTarget.releasePointerCapture(event.pointerId);
				}
			},
			[updateColorAreaFromPointer, updateSliderFromPointer],
		);
		return (
			<div className={styles.container} data-flx="app.floating.color-picker-popover.container">
				<ColorPicker
					value={pickerColor}
					onChange={handleColorChange}
					data-flx="app.floating.color-picker-popover.color-picker.color-change"
				>
					<div
						className={hasCustomColor ? styles.pickerContainerWithMargin : styles.pickerContainer}
						data-flx="app.floating.color-picker-popover.picker-container"
					>
						<div className={styles.pickerWrapper} data-flx="app.floating.color-picker-popover.picker-wrapper">
							<ColorArea
								colorSpace="hsb"
								xChannel="saturation"
								yChannel="brightness"
								className={styles.colorArea}
								onPointerDownCapture={(event) => capturePointer(event, 'color-area')}
								onPointerMoveCapture={(event) => updateCapturedPointer(event, 'color-area')}
								onPointerUpCapture={(event) => releaseCapturedPointer(event, 'color-area')}
								onPointerCancelCapture={(event) => releaseCapturedPointer(event, 'color-area')}
								onLostPointerCaptureCapture={(event) => releaseCapturedPointer(event, 'color-area')}
								data-flx="app.floating.color-picker-popover.color-area"
							>
								<ColorThumb className={styles.colorThumb} data-flx="app.floating.color-picker-popover.color-thumb" />
							</ColorArea>
							<ColorSlider
								channel="hue"
								className={styles.colorSlider}
								data-flx="app.floating.color-picker-popover.color-slider"
							>
								<SliderTrack
									className={styles.sliderTrack}
									onPointerDownCapture={(event) => capturePointer(event, 'hue-slider')}
									onPointerMoveCapture={(event) => updateCapturedPointer(event, 'hue-slider')}
									onPointerUpCapture={(event) => releaseCapturedPointer(event, 'hue-slider')}
									onPointerCancelCapture={(event) => releaseCapturedPointer(event, 'hue-slider')}
									onLostPointerCaptureCapture={(event) => releaseCapturedPointer(event, 'hue-slider')}
									data-flx="app.floating.color-picker-popover.slider-track"
								>
									<ColorThumb
										className={styles.colorThumb}
										data-flx="app.floating.color-picker-popover.color-thumb--2"
									/>
								</SliderTrack>
							</ColorSlider>
							{showAlpha ? (
								<ColorSlider
									channel="alpha"
									className={styles.colorSlider}
									data-flx="app.floating.color-picker-popover.alpha-slider"
								>
									<SliderTrack
										className={styles.sliderTrack}
										onPointerDownCapture={(event) => capturePointer(event, 'alpha-slider')}
										onPointerMoveCapture={(event) => updateCapturedPointer(event, 'alpha-slider')}
										onPointerUpCapture={(event) => releaseCapturedPointer(event, 'alpha-slider')}
										onPointerCancelCapture={(event) => releaseCapturedPointer(event, 'alpha-slider')}
										onLostPointerCaptureCapture={(event) => releaseCapturedPointer(event, 'alpha-slider')}
										data-flx="app.floating.color-picker-popover.alpha-slider-track"
									>
										<ColorThumb
											className={styles.colorThumb}
											data-flx="app.floating.color-picker-popover.alpha-color-thumb"
										/>
									</SliderTrack>
								</ColorSlider>
							) : null}
						</div>
					</div>
				</ColorPicker>
				{children}
				<FocusRing offset={-2} data-flx="app.floating.color-picker-popover.focus-ring">
					<button
						type="button"
						className={styles.resetButton}
						onClick={onReset}
						disabled={!hasCustomColor}
						data-flx="app.floating.color-picker-popover.reset-button"
					>
						<span className={styles.resetButtonText} data-flx="app.floating.color-picker-popover.reset-button-text">
							<Trans>Reset</Trans>
						</span>
					</button>
				</FocusRing>
			</div>
		);
	},
);
