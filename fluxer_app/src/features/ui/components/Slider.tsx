// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/ui/components/Slider.module.css';
import {
	buildMarkerState,
	clamp,
	scaleValueEquidistant,
	snapValueToMarker,
	unscaleValueEquidistant,
} from '@app/features/ui/components/slider/SliderMath';
import {SliderResetControls} from '@app/features/ui/components/slider/SliderResetControls';
import {SliderTooltipPortal} from '@app/features/ui/components/slider/SliderTooltipPortal';
import type {SliderProps} from '@app/features/ui/components/slider/SliderTypes';
import {useSliderTooltip} from '@app/features/ui/components/slider/useSliderTooltip';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {Slider as BaseSlider} from '@base-ui/react/slider';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import React, {useCallback, useEffect, useMemo, useRef, useState} from 'react';

export type {SliderProps} from '@app/features/ui/components/slider/SliderTypes';

export const RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR = msg({
	message: 'Reset slider to default value',
	comment: 'Accessible label for a button that resets a slider to its default value.',
});
const SLIDER_VALUE_DESCRIPTOR = msg({
	message: 'Slider value',
	comment: 'Accessible label announcing the current slider value.',
});

function getSingleValue(value: number | ReadonlyArray<number>): number {
	return typeof value === 'number' ? value : (value[0] ?? 0);
}

function isPointerInteractionReason(reason: string): boolean {
	return reason === 'track-press' || reason === 'drag';
}

export const Slider = React.forwardRef<HTMLDivElement, SliderProps>(
	(
		{
			defaultValue = 10,
			factoryDefaultValue,
			minValue = 0,
			maxValue = 100,
			disabled = false,
			equidistant = false,
			markers,
			className,
			ariaLabel,
			stickToMarkers = false,
			mini = false,
			markerPosition = 'above',
			orientation = 'horizontal',
			onValueChange,
			onValueRender,
			onMarkerRender,
			asValueChanges,
			onPointerInteractionChange,
			stopEventPropagation = false,
			barStyles = {},
			fillStyles = {},
			ariaLabelledBy,
			ariaValueText,
			children,
			value: controlledValue,
			step,
			showResetButton = false,
			onReset,
			resetTooltip,
			resetButtonPlacement = 'inline',
			resetLabel,
			resetAccessory,
		},
		ref,
	) => {
		const {i18n} = useLingui();
		const thumbRef = useRef<HTMLDivElement>(null);
		const thumbInputRef = useRef<HTMLInputElement>(null);
		const pointerInteractionRef = useRef(false);
		const [uncontrolledValue, setUncontrolledValue] = useState(defaultValue);
		const [showTooltip, setShowTooltip] = useState(false);
		const [isPointerInteracting, setIsPointerInteracting] = useState(false);
		const rawValue = controlledValue !== undefined ? controlledValue : uncontrolledValue;
		const markerState = useMemo(
			() => buildMarkerState({markers, minValue, maxValue, value: rawValue, equidistant}),
			[equidistant, markers, maxValue, minValue, rawValue],
		);
		const range = markerState.max - markerState.min;
		const baseMin = markerState.min;
		const baseMax = range > 0 ? markerState.max : markerState.min + 1;
		const resolvedStep = step ?? Math.max((baseMax - baseMin) / 100, Number.EPSILON);
		const normalizeValue = useCallback(
			(nextValue: number) => {
				const boundedValue = clamp(nextValue, markerState.min, markerState.max);
				return stickToMarkers ? snapValueToMarker(boundedValue, markerState) : boundedValue;
			},
			[markerState, stickToMarkers],
		);
		const value = normalizeValue(rawValue);
		const usesEquidistantScale = equidistant && markerState.sortedMarkers.length > 1;
		const visualPercent = usesEquidistantScale ? scaleValueEquidistant(markerState.sortedMarkers, value) : undefined;
		const resolveBaseValue = useCallback(
			(newValue: number | ReadonlyArray<number>, reason: string) => {
				const baseValue = getSingleValue(newValue);
				if (!usesEquidistantScale || !isPointerInteractionReason(reason)) {
					return normalizeValue(baseValue);
				}
				const basePercent = ((baseValue - baseMin) / (baseMax - baseMin)) * 100;
				return normalizeValue(unscaleValueEquidistant(markerState.sortedMarkers, basePercent));
			},
			[baseMax, baseMin, markerState.sortedMarkers, normalizeValue, usesEquidistantScale],
		);
		const indicatorStyle = useMemo(() => {
			if (visualPercent === undefined) return fillStyles;
			return orientation === 'vertical'
				? {...fillStyles, height: `${visualPercent}%`}
				: {...fillStyles, width: `${visualPercent}%`};
		}, [fillStyles, orientation, visualPercent]);
		const thumbStyle = useMemo(() => {
			if (visualPercent === undefined) return undefined;
			return orientation === 'vertical' ? {bottom: `${visualPercent}%`} : {insetInlineStart: `${visualPercent}%`};
		}, [orientation, visualPercent]);
		const isAtFactoryDefault = factoryDefaultValue !== undefined && value === factoryDefaultValue;
		const canReset = showResetButton && !isAtFactoryDefault && !disabled;
		const inlineResetLabel = resetTooltip ?? i18n._(RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR);
		const belowResetLabel = resetLabel ?? resetTooltip ?? i18n._(RESET_SLIDER_TO_DEFAULT_VALUE_DESCRIPTOR);
		const tooltip = useSliderTooltip({
			showTooltip,
			value,
			isDragging: isPointerInteracting,
			thumbRef,
		});
		const endPointerInteraction = useCallback(() => {
			if (!pointerInteractionRef.current) return;
			pointerInteractionRef.current = false;
			setIsPointerInteracting(false);
			setShowTooltip(false);
			onPointerInteractionChange?.(false);
			document.dispatchEvent(new CustomEvent('slider-drag-end'));
		}, [onPointerInteractionChange]);
		const beginPointerInteraction = useCallback(() => {
			if (pointerInteractionRef.current) return;
			pointerInteractionRef.current = true;
			setIsPointerInteracting(true);
			setShowTooltip(true);
			onPointerInteractionChange?.(true);
			document.dispatchEvent(new CustomEvent('slider-drag-start'));
		}, [onPointerInteractionChange]);
		const commitControlledValue = useCallback(
			(newValue: number) => {
				if (controlledValue === undefined) {
					setUncontrolledValue(newValue);
				}
				if (asValueChanges) {
					asValueChanges(newValue);
				} else {
					onValueChange?.(newValue);
				}
			},
			[asValueChanges, controlledValue, onValueChange],
		);
		const handleBaseValueChange = useCallback(
			(newValue: number | ReadonlyArray<number>, eventDetails: BaseSlider.Root.ChangeEventDetails) => {
				commitControlledValue(resolveBaseValue(newValue, eventDetails.reason));
			},
			[commitControlledValue, resolveBaseValue],
		);
		const handleBaseValueCommitted = useCallback(
			(newValue: number | ReadonlyArray<number>, eventDetails: BaseSlider.Root.CommitEventDetails) => {
				const nextValue = resolveBaseValue(newValue, eventDetails.reason);
				if (asValueChanges) {
					onValueChange?.(nextValue);
				}
				endPointerInteraction();
			},
			[asValueChanges, endPointerInteraction, onValueChange, resolveBaseValue],
		);
		const handleControlPointerDownCapture = useCallback(
			(event: React.PointerEvent<HTMLDivElement>) => {
				if (disabled || event.button !== 0) return;
				const target = event.target;
				if (target instanceof HTMLElement && target.closest('button')) return;
				beginPointerInteraction();
			},
			[beginPointerInteraction, disabled],
		);
		const handleThumbPointerEnter = useCallback(() => {
			setShowTooltip(true);
		}, []);
		const handleThumbPointerLeave = useCallback(() => {
			if (!pointerInteractionRef.current) {
				setShowTooltip(false);
			}
		}, []);
		const handleThumbFocus = useCallback(() => {
			if (KeyboardMode.keyboardModeEnabled) {
				setShowTooltip(true);
			}
		}, []);
		const handleThumbBlur = useCallback(() => {
			if (!pointerInteractionRef.current) {
				setShowTooltip(false);
			}
		}, []);
		const handleThumbKeyDown = useCallback(() => {
			setShowTooltip(true);
		}, []);
		const handleReset = useCallback(() => {
			if (disabled || factoryDefaultValue === undefined) return;
			const resetValue = normalizeValue(factoryDefaultValue);
			if (onReset) {
				onReset();
			} else {
				setUncontrolledValue(resetValue);
				onValueChange?.(resetValue);
			}
			thumbInputRef.current?.focus({preventScroll: true});
		}, [disabled, factoryDefaultValue, normalizeValue, onReset, onValueChange]);
		const handleRootEventPropagation = useCallback(
			(event: React.SyntheticEvent<HTMLDivElement>) => {
				if (stopEventPropagation) {
					event.stopPropagation();
				}
			},
			[stopEventPropagation],
		);
		useEffect(() => {
			setUncontrolledValue(defaultValue);
		}, [defaultValue]);
		useEffect(() => {
			if (!isPointerInteracting) return;
			document.addEventListener('pointerup', endPointerInteraction, true);
			document.addEventListener('pointercancel', endPointerInteraction, true);
			window.addEventListener('blur', endPointerInteraction);
			return () => {
				document.removeEventListener('pointerup', endPointerInteraction, true);
				document.removeEventListener('pointercancel', endPointerInteraction, true);
				window.removeEventListener('blur', endPointerInteraction);
			};
		}, [endPointerInteraction, isPointerInteracting]);
		useEffect(() => {
			if (disabled) {
				endPointerInteraction();
			}
		}, [disabled, endPointerInteraction]);
		const hasMarks = markerState.markerPositions.length > 0;
		const sliderLabel = ariaLabel ?? i18n._(SLIDER_VALUE_DESCRIPTOR);
		const renderedSlider = (
			<BaseSlider.Root
				className={clsx(
					styles.slider,
					mini && styles.mini,
					orientation === 'vertical' && styles.vertical,
					hasMarks && styles.hasMarks,
					markerPosition === 'below' ? styles.marksBelow : styles.marksAbove,
					className,
				)}
				value={value}
				min={baseMin}
				max={baseMax}
				step={resolvedStep}
				largeStep={resolvedStep * 10}
				orientation={orientation}
				disabled={disabled}
				onValueChange={handleBaseValueChange}
				onValueCommitted={handleBaseValueCommitted}
				onPointerDown={handleRootEventPropagation}
				onClick={handleRootEventPropagation}
				data-dragging={isPointerInteracting ? '' : undefined}
				data-flx="ui.slider.base-slider.root"
			>
				{hasMarks ? (
					<div className={styles.marks} aria-hidden="true" data-flx="ui.slider.marks">
						{markerState.markerPositions.map((position, i) => {
							const markerValue = markerState.sortedMarkers[i];
							const renderedValue = onMarkerRender ? onMarkerRender(markerValue) : markerValue;
							const isFactoryDefaultValue = markerValue === factoryDefaultValue;
							return (
								<div
									key={`${markerValue}-${i}`}
									className={clsx(
										styles.mark,
										i === 0 && styles.markEndpointStart,
										i === markerState.markerPositions.length - 1 && i !== 0 && styles.markEndpointEnd,
										isFactoryDefaultValue && styles.defaultValue,
									)}
									style={{left: `${position}%`}}
									data-flx="ui.slider.mark"
								>
									<div className={styles.markValue} data-flx="ui.slider.mark-value">
										{renderedValue}
									</div>
									{renderedValue != null ? <div className={styles.markDash} data-flx="ui.slider.mark-dash" /> : null}
								</div>
							);
						})}
					</div>
				) : null}
				<BaseSlider.Control
					className={styles.sliderControl}
					onPointerDownCapture={handleControlPointerDownCapture}
					data-flx="ui.slider.base-slider.control.pointer-down"
				>
					<BaseSlider.Track className={styles.track} style={barStyles} data-flx="ui.slider.base-slider.track">
						<BaseSlider.Indicator
							className={styles.barFill}
							style={indicatorStyle}
							data-flx="ui.slider.base-slider.indicator"
						/>
						{children}
						<BaseSlider.Thumb
							ref={thumbRef}
							inputRef={thumbInputRef}
							className={styles.grabber}
							style={thumbStyle}
							aria-label={ariaLabelledBy ? undefined : sliderLabel}
							aria-labelledby={ariaLabelledBy}
							getAriaValueText={ariaValueText ? () => ariaValueText : undefined}
							onPointerEnter={handleThumbPointerEnter}
							onPointerLeave={handleThumbPointerLeave}
							onFocus={handleThumbFocus}
							onBlur={handleThumbBlur}
							onKeyDown={handleThumbKeyDown}
							data-slider-thumb="true"
							data-flx="ui.slider.base-slider.thumb"
						/>
					</BaseSlider.Track>
				</BaseSlider.Control>
			</BaseSlider.Root>
		);
		const track = (
			<div className={styles.sliderFrame} data-flx="ui.slider.slider-frame">
				{renderedSlider}
			</div>
		);
		const sliderControl = showResetButton ? (
			<SliderResetControls
				placement={resetButtonPlacement}
				canReset={canReset}
				onReset={handleReset}
				inlineLabel={inlineResetLabel}
				label={belowResetLabel}
				accessory={resetAccessory}
				data-flx="ui.slider.slider-reset-controls"
			>
				{track}
			</SliderResetControls>
		) : (
			track
		);
		return (
			<div className={styles.control} ref={ref} data-flx="ui.slider.control">
				{sliderControl}
				<SliderTooltipPortal
					showTooltip={showTooltip}
					shouldRender={!stickToMarkers}
					value={value}
					onValueRender={onValueRender}
					tooltip={tooltip}
					data-flx="ui.slider.slider-tooltip-portal"
				/>
			</div>
		);
	},
);

Slider.displayName = 'Slider';
