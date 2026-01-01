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

import {FloatingPortal} from '@floating-ui/react';
import {arrow, autoUpdate, computePosition, flip, offset, shift} from '@floating-ui/react-dom';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import React from 'react';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {useTooltipPortalRoot} from '~/components/uikit/Tooltip';
import {useMergeRefs} from '~/hooks/useMergeRefs';
import styles from './Slider.module.css';
import tooltipStyles from './SliderTooltip.module.css';

interface SliderProps {
	defaultValue: number;
	factoryDefaultValue: number;
	minValue?: number;
	maxValue?: number;
	handleSize?: number;
	disabled?: boolean;
	equidistant?: boolean;
	markers?: Array<number>;
	className?: string;
	stickToMarkers?: boolean;
	mini?: boolean;
	markerPosition?: 'above' | 'below';
	onValueChange?: (value: number) => void;
	onValueRender?: (value: number) => React.ReactNode;
	onMarkerRender?: (value: number) => React.ReactNode;
	asValueChanges?: (value: number) => void;
	barStyles?: React.CSSProperties;
	fillStyles?: React.CSSProperties;
	children?: React.ReactNode;
	value?: number;
	step?: number;
}

interface MarkerState {
	min: number;
	max: number;
	range: number;
	sortedMarkers: Array<number>;
	markerPositions: Array<number>;
	closestMarkerIndex?: number;
}

export const Slider = React.forwardRef<HTMLDivElement, SliderProps>(
	(
		{
			defaultValue = 10,
			factoryDefaultValue,
			minValue = 0,
			maxValue = 100,
			handleSize = 10,
			disabled = false,
			equidistant = false,
			markers,
			className,
			stickToMarkers = false,
			mini = false,
			markerPosition = 'above',
			onValueChange,
			onValueRender,
			onMarkerRender,
			asValueChanges,
			barStyles = {},
			fillStyles = {},
			children,
			value: controlledValue,
			step,
		},
		ref,
	) => {
		const {t} = useLingui();
		const sliderRef = React.useRef<HTMLDivElement>(null);
		const thumbRef = React.useRef<HTMLButtonElement>(null);
		const tooltipRef = React.useRef<HTMLDivElement>(null);
		const arrowRef = React.useRef<HTMLDivElement>(null);
		const cleanupRef = React.useRef<(() => void) | null>(null);
		const isCalculatingRef = React.useRef(false);
		const rafRef = React.useRef<number | null>(null);
		const mergedRef = useMergeRefs([ref]);
		const tooltipPortalRoot = useTooltipPortalRoot();
		const [uncontrolledValue, setUncontrolledValue] = React.useState(defaultValue);
		const [isDragging, setIsDragging] = React.useState(false);
		const [showTooltip, setShowTooltip] = React.useState(false);
		const [tooltipPosition, setTooltipPosition] = React.useState({x: 0, y: 0, arrowX: 0, arrowY: 0, isReady: false});

		const value = controlledValue !== undefined ? controlledValue : uncontrolledValue;
		const [mouseState, setMouseState] = React.useState<{
			x: number;
			boundingRect: DOMRect | null;
		}>({x: 0, boundingRect: null});
		const [newClosestIndex, setNewClosestIndex] = React.useState<number | null>(null);

		const applyStep = React.useCallback(
			(val: number): number => {
				if (step === undefined || step === 0) return val;
				return Math.round(val / step) * step;
			},
			[step],
		);

		const findClosestMarkerByIndex = React.useCallback((val: number, markerArray: Array<number>): number => {
			if (!markerArray || markerArray.length === 0) return 0;

			let diff = 0;

			for (let i = 0; i < markerArray.length; i++) {
				const currentMarker = markerArray[i];

				if (val === currentMarker) {
					return i;
				}

				if (val < currentMarker) {
					if (diff === 0) {
						return i;
					}

					if (currentMarker - val < diff) {
						return i;
					}
					return i - 1;
				}

				diff = val - currentMarker;
			}

			return markerArray.length - 1;
		}, []);

		const markerState = React.useMemo((): MarkerState => {
			if (!markers || markers.length === 0) {
				return {
					min: minValue,
					max: maxValue,
					range: maxValue - minValue,
					sortedMarkers: [],
					markerPositions: [],
				};
			}

			const sortedMarkers = [...markers].sort((a, b) => a - b);
			const closestMarkerIndex = findClosestMarkerByIndex(value, sortedMarkers);

			const min = sortedMarkers[0];
			const max = sortedMarkers[sortedMarkers.length - 1];
			const range = max - min;

			let markerPositions: Array<number>;
			if (equidistant) {
				const markerInterval = 100 / (sortedMarkers.length - 1);
				markerPositions = sortedMarkers.map((_, i) => i * markerInterval);
			} else {
				const scaleValueFn = (val: number) => (100.0 * (val - min)) / range;
				markerPositions = sortedMarkers.map((marker) => scaleValueFn(marker));
			}

			return {
				min,
				max,
				range,
				sortedMarkers,
				markerPositions,
				closestMarkerIndex,
			};
		}, [markers, minValue, maxValue, value, equidistant, findClosestMarkerByIndex]);

		const scaleValue = React.useCallback(
			(val: number): number => {
				if (markerState.range === 0) return 0;
				return (100.0 * (val - markerState.min)) / markerState.range;
			},
			[markerState.min, markerState.range],
		);

		const unscaleValue = React.useCallback(
			(val: number): number => {
				if (markerState.range === 0) return markerState.min;
				return (val * markerState.range) / 100 + markerState.min;
			},
			[markerState.range, markerState.min],
		);

		let valueScaled = 0;
		if (!stickToMarkers) {
			valueScaled = scaleValue(value);
		} else if (markerState.markerPositions.length > 0) {
			if (newClosestIndex != null) {
				valueScaled = markerState.markerPositions[newClosestIndex];
			} else if (markerState.closestMarkerIndex !== undefined) {
				valueScaled = markerState.markerPositions[markerState.closestMarkerIndex];
			}
		}

		const moveSmoothly = React.useCallback(
			(clientX: number) => {
				if (!mouseState.boundingRect) return;

				if (rafRef.current !== null) {
					cancelAnimationFrame(rafRef.current);
				}

				rafRef.current = requestAnimationFrame(() => {
					const {left, right} = mouseState.boundingRect!;
					const delta = mouseState.x - clientX;
					let scaledValue = (value - minValue) / (maxValue - minValue);

					if (!(clientX <= left && delta < 0) && !(clientX >= right && delta > 0)) {
						const totalWidth = right - left - handleSize;
						const width = totalWidth * scaledValue;
						const newWidth = Math.min(Math.max(width - delta, 0), totalWidth);
						scaledValue = newWidth / totalWidth;
					}

					const rawValue = minValue + scaledValue * (maxValue - minValue);
					const newValue = applyStep(rawValue);
					setUncontrolledValue(newValue);
					asValueChanges?.(newValue);
					setMouseState((prev) => ({...prev, x: clientX}));
					rafRef.current = null;
				});
			},
			[value, minValue, maxValue, handleSize, asValueChanges, mouseState.boundingRect, mouseState.x, applyStep],
		);

		const moveStaggered = React.useCallback(
			(clientX: number) => {
				if (!mouseState.boundingRect || markerState.closestMarkerIndex === undefined) return;

				const {left, right} = mouseState.boundingRect;

				if (clientX <= left || clientX >= right) {
					return;
				}

				const sliderWidth = right - left;
				const deltaPixels = clientX - mouseState.x;
				const percentMoved = deltaPixels / sliderWidth;
				const posDestination = markerState.markerPositions[markerState.closestMarkerIndex] + percentMoved * 100;

				let closestIndex: number;
				if (equidistant) {
					closestIndex = findClosestMarkerByIndex(posDestination, markerState.markerPositions);
				} else {
					const deltaValue = unscaleValue(posDestination);
					closestIndex = findClosestMarkerByIndex(deltaValue, markerState.sortedMarkers);
				}

				setNewClosestIndex(closestIndex);
			},
			[mouseState, markerState, equidistant, findClosestMarkerByIndex, unscaleValue],
		);

		const handleTrackClick = (e: React.MouseEvent<HTMLDivElement>) => {
			if (disabled) return;

			if ((e.target as HTMLElement).closest('button')) return;

			const rect = sliderRef.current?.getBoundingClientRect();
			if (!rect) return;

			const trackPadding = 5;
			const clickX = e.clientX - rect.left - trackPadding;
			const trackWidth = rect.width - trackPadding * 2;
			const percentage = Math.max(0, Math.min(100, (clickX / trackWidth) * 100));

			if (stickToMarkers && markerState.markerPositions.length > 0) {
				const closestIndex = findClosestMarkerByIndex(percentage, markerState.markerPositions);
				const newValue = markerState.sortedMarkers[closestIndex];
				setUncontrolledValue(newValue);
				onValueChange?.(newValue);
			} else {
				const rawValue = unscaleValue(percentage);
				const newValue = applyStep(rawValue);
				setUncontrolledValue(newValue);
				onValueChange?.(newValue);
			}
		};

		const handleMouseDown = (e: React.MouseEvent<HTMLElement>) => {
			if (disabled) return;

			e.preventDefault();
			e.stopPropagation();
			setIsDragging(true);
			setShowTooltip(true);

			document.dispatchEvent(new CustomEvent('slider-drag-start'));

			const rect = sliderRef.current?.getBoundingClientRect() || null;
			setMouseState({
				x: e.clientX,
				boundingRect: rect,
			});

			if (stickToMarkers) {
				setNewClosestIndex(markerState.closestMarkerIndex || null);
			}
		};

		const handleMouseMove = React.useCallback(
			(e: MouseEvent) => {
				e.preventDefault();

				if (stickToMarkers) {
					moveStaggered(e.clientX);
				} else {
					moveSmoothly(e.clientX);
				}
			},
			[stickToMarkers, moveSmoothly, moveStaggered],
		);

		const handleMouseUp = React.useCallback(() => {
			setIsDragging(false);
			setShowTooltip(false);
			setTooltipPosition((prev) => ({...prev, isReady: false}));

			document.dispatchEvent(new CustomEvent('slider-drag-end'));

			if (stickToMarkers && newClosestIndex != null) {
				onValueChange?.(markerState.sortedMarkers[newClosestIndex]);
				setNewClosestIndex(null);
			} else {
				onValueChange?.(value);
			}
		}, [stickToMarkers, newClosestIndex, value, onValueChange, markerState.sortedMarkers]);

		const handleKeyDown = (e: React.KeyboardEvent<HTMLDivElement>) => {
			if (disabled) return;

			let newValue = value;
			const keyStep =
				stickToMarkers && markerState.sortedMarkers.length > 0
					? (markerState.max - markerState.min) / (markerState.sortedMarkers.length - 1)
					: (step ?? (maxValue - minValue) / 100);

			switch (e.key) {
				case 'ArrowLeft':
				case 'ArrowDown':
					e.preventDefault();
					if (stickToMarkers && markerState.sortedMarkers.length > 0) {
						const currentIndex = findClosestMarkerByIndex(value, markerState.sortedMarkers);
						const newIndex = Math.max(0, currentIndex - 1);
						newValue = markerState.sortedMarkers[newIndex];
					} else {
						newValue = applyStep(Math.max(minValue, value - keyStep));
					}
					break;
				case 'ArrowRight':
				case 'ArrowUp':
					e.preventDefault();
					if (stickToMarkers && markerState.sortedMarkers.length > 0) {
						const currentIndex = findClosestMarkerByIndex(value, markerState.sortedMarkers);
						const newIndex = Math.min(markerState.sortedMarkers.length - 1, currentIndex + 1);
						newValue = markerState.sortedMarkers[newIndex];
					} else {
						newValue = applyStep(Math.min(maxValue, value + keyStep));
					}
					break;
				case 'Home':
					e.preventDefault();
					newValue = stickToMarkers && markerState.sortedMarkers.length > 0 ? markerState.sortedMarkers[0] : minValue;
					break;
				case 'End':
					e.preventDefault();
					newValue =
						stickToMarkers && markerState.sortedMarkers.length > 0
							? markerState.sortedMarkers[markerState.sortedMarkers.length - 1]
							: maxValue;
					break;
				default:
					return;
			}

			setUncontrolledValue(newValue);
			onValueChange?.(newValue);
		};

		const handleTouchStart = (e: React.TouchEvent<HTMLElement>) => {
			if (disabled) return;

			e.stopPropagation();
			setIsDragging(true);
			setShowTooltip(true);

			const rect = sliderRef.current?.getBoundingClientRect() || null;
			setMouseState({
				x: e.touches[0].clientX,
				boundingRect: rect,
			});

			if (stickToMarkers) {
				setNewClosestIndex(markerState.closestMarkerIndex || null);
			}
		};

		const handleTouchMove = React.useCallback(
			(e: TouchEvent) => {
				if (stickToMarkers) {
					moveStaggered(e.touches[0].clientX);
				} else {
					moveSmoothly(e.touches[0].clientX);
				}
			},
			[stickToMarkers, moveSmoothly, moveStaggered],
		);

		const handleTouchEnd = React.useCallback(() => {
			setIsDragging(false);

			document.dispatchEvent(new CustomEvent('slider-drag-end'));

			if (stickToMarkers && newClosestIndex != null) {
				onValueChange?.(markerState.sortedMarkers[newClosestIndex]);
				setNewClosestIndex(null);
			} else {
				onValueChange?.(value);
			}
		}, [stickToMarkers, newClosestIndex, value, onValueChange, markerState.sortedMarkers]);

		React.useEffect(() => {
			if (isDragging) {
				document.addEventListener('mousemove', handleMouseMove);
				document.addEventListener('mouseup', handleMouseUp);
				document.addEventListener('touchmove', handleTouchMove);
				document.addEventListener('touchend', handleTouchEnd);

				return () => {
					document.removeEventListener('mousemove', handleMouseMove);
					document.removeEventListener('mouseup', handleMouseUp);
					document.removeEventListener('touchmove', handleTouchMove);
					document.removeEventListener('touchend', handleTouchEnd);

					if (rafRef.current !== null) {
						cancelAnimationFrame(rafRef.current);
						rafRef.current = null;
					}
				};
			}
			return;
		}, [isDragging, handleMouseMove, handleMouseUp, handleTouchMove, handleTouchEnd]);

		React.useEffect(() => {
			setUncontrolledValue(defaultValue);
		}, [defaultValue]);

		const updateTooltipPosition = React.useCallback(async () => {
			if (!showTooltip || !thumbRef.current || !tooltipRef.current || isCalculatingRef.current) {
				return;
			}

			isCalculatingRef.current = true;

			try {
				const target = thumbRef.current;
				const tooltip = tooltipRef.current;

				Object.assign(tooltip.style, {
					position: 'fixed',
					left: '-9999px',
					top: '-9999px',
				});

				const middleware = [offset(8), flip(), shift({padding: 8}), arrow({element: arrowRef})];

				const {x, y, middlewareData} = await computePosition(target, tooltip, {
					placement: 'top',
					middleware,
				});

				Object.assign(tooltip.style, {
					left: `${x}px`,
					top: `${y}px`,
				});

				setTooltipPosition({
					x,
					y,
					arrowX: middlewareData.arrow?.x ?? 0,
					arrowY: middlewareData.arrow?.y ?? 0,
					isReady: true,
				});
			} catch (error) {
				console.error('Error positioning slider tooltip:', error);
				if (tooltipRef.current) {
					tooltipRef.current.style.visibility = 'visible';
				}
			} finally {
				isCalculatingRef.current = false;
			}
		}, [showTooltip]);

		React.useLayoutEffect(() => {
			if (!showTooltip || !thumbRef.current || !tooltipRef.current) {
				if (cleanupRef.current) {
					cleanupRef.current();
					cleanupRef.current = null;
				}
				return;
			}

			updateTooltipPosition();

			if (document.contains(thumbRef.current)) {
				cleanupRef.current = autoUpdate(thumbRef.current, tooltipRef.current, updateTooltipPosition);
			}

			return () => {
				if (cleanupRef.current) {
					cleanupRef.current();
					cleanupRef.current = null;
				}
			};
		}, [showTooltip, updateTooltipPosition]);

		React.useEffect(() => {
			if (isDragging && showTooltip) {
				updateTooltipPosition();
			}
		}, [value, isDragging, showTooltip, updateTooltipPosition]);

		const renderMarks = () => {
			if (!markerState.markerPositions.length || !markerState.sortedMarkers.length) return null;

			return markerState.markerPositions.map((position: number, i: number) => {
				const markValue = onMarkerRender ? onMarkerRender(markerState.sortedMarkers[i]) : markerState.sortedMarkers[i];
				const isFactoryDefaultValue =
					factoryDefaultValue !== undefined && markerState.sortedMarkers[i] === factoryDefaultValue;

				return (
					<div
						key={i}
						className={clsx(
							styles.mark,
							markerPosition === 'above' ? styles.markAbove : styles.markBelow,
							isFactoryDefaultValue && styles.defaultValue,
						)}
						style={{left: `${position}%`}}
					>
						<div className={styles.markValue}>{markValue}</div>
						{markValue != null ? <div className={styles.markDash} /> : <div />}
					</div>
				);
			});
		};

		const valuePercent = `${valueScaled}%`;
		const combinedFillStyles = {
			...fillStyles,
			width: valuePercent,
		};

		const hasMarks = markers && markers.length > 0;

		return (
			<div className={styles.control} ref={mergedRef}>
				<FocusRing ringTarget={thumbRef} offset={4}>
					<div
						ref={sliderRef}
						className={clsx(
							styles.slider,
							mini && styles.mini,
							hasMarks && styles.hasMarks,
							disabled && styles.disabled,
							className,
						)}
						role="slider"
						aria-valuemin={markerState.min}
						aria-valuemax={markerState.max}
						aria-valuenow={value}
						aria-disabled={disabled}
						aria-label={t`Slider value`}
						tabIndex={0}
						onClick={handleTrackClick}
						onKeyDown={handleKeyDown}
					>
						<div className={styles.track}>{renderMarks()}</div>

						<div className={styles.bar} style={barStyles}>
							<div className={styles.barFill} style={combinedFillStyles} />
						</div>

						{children}

						<div className={styles.track}>
							<button
								ref={thumbRef}
								type="button"
								className={styles.grabber}
								style={{left: valuePercent}}
								onMouseDown={handleMouseDown}
								onTouchStart={handleTouchStart}
								onClick={(e) => e.stopPropagation()}
								onMouseEnter={() => setShowTooltip(true)}
								onMouseLeave={() => !isDragging && setShowTooltip(false)}
								aria-label={t`Slider handle`}
								tabIndex={-1}
							/>
						</div>
					</div>
				</FocusRing>
				{showTooltip && onValueRender && !stickToMarkers && (
					<FloatingPortal root={tooltipPortalRoot}>
						<AnimatePresence mode="wait">
							<motion.div
								key="slider-tooltip"
								ref={(node) => {
									(tooltipRef as any).current = node;
									if (node && thumbRef.current) {
										updateTooltipPosition();
									}
								}}
								style={{
									position: 'fixed',
									left: tooltipPosition.x,
									top: tooltipPosition.y,
									zIndex: 'var(--z-index-tooltip)',
									visibility: tooltipPosition.isReady ? 'visible' : 'hidden',
								}}
								initial={{opacity: 0, scale: 0.98}}
								animate={{opacity: 1, scale: 1}}
								exit={{opacity: 0, scale: 0.98}}
								transition={{
									opacity: {duration: 0.1},
									scale: {type: 'spring', damping: 25, stiffness: 500},
								}}
							>
								<div className={tooltipStyles.tooltip}>
									<div
										ref={arrowRef}
										className={clsx(tooltipStyles.tooltipPointer, tooltipStyles.tooltipPointerBg)}
										style={{
											left: tooltipPosition.arrowX != null ? `${tooltipPosition.arrowX}px` : undefined,
											marginLeft: tooltipPosition.arrowX != null ? '0' : undefined,
										}}
									/>
									<div
										className={tooltipStyles.tooltipPointer}
										style={{
											left: tooltipPosition.arrowX != null ? `${tooltipPosition.arrowX}px` : undefined,
											marginLeft: tooltipPosition.arrowX != null ? '0' : undefined,
										}}
									/>
									<div className={tooltipStyles.tooltipContent}>{onValueRender(value)}</div>
								</div>
							</motion.div>
						</AnimatePresence>
					</FloatingPortal>
				)}
			</div>
		);
	},
);

Slider.displayName = 'Slider';
