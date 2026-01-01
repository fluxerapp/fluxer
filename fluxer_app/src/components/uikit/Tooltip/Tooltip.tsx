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
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import type {TooltipPosition, TooltipType} from '~/components/uikit/Tooltip';
import {useMergeRefs} from '~/hooks/useMergeRefs';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import {elementSupportsRef} from '~/utils/react';
import styles from './Tooltip.module.css';

let tooltipPortalRoot: HTMLElement | null = null;

const getTooltipPortalRoot = (): HTMLElement => {
	if (!tooltipPortalRoot) {
		tooltipPortalRoot = document.createElement('div');
		tooltipPortalRoot.className = styles.tooltips;
		tooltipPortalRoot.setAttribute('data-tooltip-portal-root', 'true');
		document.body.appendChild(tooltipPortalRoot);
	}
	return tooltipPortalRoot!;
};

export const useTooltipPortalRoot = (): HTMLElement | undefined => {
	const [root, setRoot] = React.useState<HTMLElement | undefined>(undefined);

	React.useEffect(() => {
		setRoot(getTooltipPortalRoot());
	}, []);

	return root;
};

const MAX_WIDTH_MAP = {
	default: 190,
	xl: 350,
	none: 'none' as const,
};

const TooltipPositionToStyle: Record<TooltipPosition, string> = {
	top: styles.tooltipTop,
	left: styles.tooltipLeft,
	right: styles.tooltipRight,
	bottom: styles.tooltipBottom,
};

const TooltipTypeToStyle: Record<TooltipType, string> = {
	normal: styles.tooltipPrimary,
	error: styles.tooltipRed,
};

interface TooltipProps {
	text: string | (() => React.ReactNode);
	type?: TooltipType;
	position?: TooltipPosition;
	align?: 'center' | 'top' | 'bottom' | 'left' | 'right';
	nudge?: number;
	delay?: number;
	padding?: number;
	maxWidth?: 'default' | 'xl' | 'none';
	size?: 'default' | 'large';
	children?: React.ReactElement;
}

const TooltipContainer = React.forwardRef<
	HTMLDivElement,
	{
		text: string | (() => React.ReactNode);
		type: TooltipType;
		position: TooltipPosition;
		maxWidth: 'default' | 'xl' | 'none';
		size: 'default' | 'large';
		arrowRef: React.RefObject<HTMLDivElement | null> | React.RefObject<HTMLDivElement>;
		arrowX?: number;
		arrowY?: number;
	}
>(({text, type, position, maxWidth, size, arrowRef, arrowX, arrowY}, ref) => {
	const content = typeof text === 'function' ? text() : text;
	if (
		!content ||
		(Array.isArray(content) && content.length === 0) ||
		(typeof content === 'string' && content.trim().length === 0)
	) {
		return null;
	}

	const arrowStyle: React.CSSProperties = {};
	if (position === 'top' || position === 'bottom') {
		if (arrowX != null) {
			arrowStyle.left = `${arrowX}px`;
			arrowStyle.marginLeft = 0;
		}
	} else {
		if (arrowY != null) {
			arrowStyle.top = `${arrowY}px`;
			arrowStyle.marginTop = 0;
		}
	}

	return (
		<div
			ref={ref}
			className={clsx(styles.tooltip, TooltipPositionToStyle[position], TooltipTypeToStyle[type])}
			style={{maxWidth: MAX_WIDTH_MAP[maxWidth]}}
		>
			<div ref={arrowRef} className={clsx(styles.tooltipPointer, styles.tooltipPointerBg)} style={arrowStyle} />
			<div className={clsx(styles.tooltipPointer)} style={arrowStyle} />
			<div className={size === 'large' ? styles.tooltipContentLarge : styles.tooltipContent}>{content}</div>
		</div>
	);
});
TooltipContainer.displayName = 'TooltipContainer';

export const Tooltip = observer(
	({
		text,
		type = 'normal',
		position = 'top',
		nudge = 0,
		delay = 0,
		padding = 8,
		maxWidth = 'default',
		size = 'default',
		children,
	}: TooltipProps) => {
		const tooltipId = React.useId();
		const portalRoot = useTooltipPortalRoot();

		const [isOpen, setIsOpen] = React.useState(false);
		const timerRef = React.useRef<number | null>(null);
		const longPressTimeoutRef = React.useRef<number | null>(null);
		const arrowRef = React.useRef<HTMLDivElement>(null);
		const tooltipRef = React.useRef<HTMLDivElement>(null);
		const targetRef = React.useRef<HTMLElement>(null);
		const cleanupRef = React.useRef<(() => void) | null>(null);
		const rafRef = React.useRef<number | null>(null);
		const isCalculatingRef = React.useRef(false);
		const unmountedRef = React.useRef(false);

		const mobileLayout = MobileLayoutStore;

		const [tooltipState, setTooltipState] = React.useState({
			x: 0,
			y: 0,
			arrowX: 0,
			arrowY: 0,
			isReady: false,
			actualPlacement: position,
		});

		const textRef = React.useRef(text);
		React.useEffect(() => {
			textRef.current = text;
		}, [text]);

		const getTextString = React.useCallback((): string => {
			const t = textRef.current;
			return typeof t === 'string' ? t : '';
		}, []);

		const hasRenderableContent = React.useCallback(() => {
			const t = textRef.current;
			if (typeof t === 'function') return true;
			if (typeof t === 'string') return t.trim().length > 0;
			return !!t;
		}, []);

		const clearTimer = React.useCallback(() => {
			if (timerRef.current != null) {
				window.clearTimeout(timerRef.current);
				timerRef.current = null;
			}
		}, []);

		const cancelRaf = React.useCallback(() => {
			if (rafRef.current != null) {
				cancelAnimationFrame(rafRef.current);
				rafRef.current = null;
			}
		}, []);

		const safeSetOpen = React.useCallback((open: boolean) => {
			if (!unmountedRef.current) setIsOpen(open);
		}, []);

		const updatePositionNow = React.useCallback(async () => {
			if (
				!isOpen ||
				!targetRef.current ||
				!tooltipRef.current ||
				isCalculatingRef.current ||
				mobileLayout.enabled ||
				!hasRenderableContent()
			) {
				return;
			}

			isCalculatingRef.current = true;

			try {
				const target = targetRef.current;
				const tooltip = tooltipRef.current;

				Object.assign(tooltip.style, {
					position: 'fixed',
					left: '-9999px',
					top: '-9999px',
				} as CSSStyleDeclaration);

				const middleware = [offset(padding + nudge), flip(), shift({padding: 8}), arrow({element: arrowRef.current})];

				const {x, y, placement, middlewareData} = await computePosition(target, tooltip, {
					placement: position,
					middleware,
				});

				setTooltipState((prev) => {
					const next = {
						x,
						y,
						arrowX: middlewareData.arrow?.x ?? 0,
						arrowY: middlewareData.arrow?.y ?? 0,
						isReady: true,
						actualPlacement: placement.split('-')[0] as TooltipPosition,
					};
					return prev.x !== next.x ||
						prev.y !== next.y ||
						prev.arrowX !== next.arrowX ||
						prev.arrowY !== next.arrowY ||
						prev.isReady !== next.isReady ||
						prev.actualPlacement !== next.actualPlacement
						? next
						: prev;
				});

				Object.assign(tooltip.style, {left: `${x}px`, top: `${y}px`} as CSSStyleDeclaration);
			} catch (error) {
				console.error('Error positioning tooltip:', error);
				if (tooltipRef.current) {
					tooltipRef.current.style.visibility = 'visible';
				}
			} finally {
				isCalculatingRef.current = false;
			}
		}, [isOpen, position, padding, nudge, mobileLayout.enabled, hasRenderableContent]);

		const updatePosition = React.useCallback(() => {
			cancelRaf();
			rafRef.current = requestAnimationFrame(() => {
				rafRef.current = null;
				void updatePositionNow();
			});
		}, [updatePositionNow, cancelRaf]);

		const showTooltip = React.useCallback(() => {
			clearTimer();
			if (!hasRenderableContent()) return;
			safeSetOpen(true);
		}, [clearTimer, hasRenderableContent, safeSetOpen]);

		const hideTooltip = React.useCallback(() => {
			clearTimer();
			cancelRaf();
			safeSetOpen(false);
			setTooltipState((prev) => (prev.isReady ? {...prev, isReady: false, actualPlacement: position} : prev));
		}, [clearTimer, cancelRaf, safeSetOpen, position]);

		const showDelayedTooltip = React.useCallback(() => {
			if (timerRef.current == null) {
				timerRef.current = window.setTimeout(() => {
					timerRef.current = null;
					showTooltip();
				}, delay);
			}
		}, [showTooltip, delay]);

		const showMobileToast = React.useCallback(() => {
			const textContent = getTextString();
			if (textContent) {
				ToastActionCreators.createToast({
					type: 'info',
					children: textContent,
					timeout: 3000,
				});
			}
		}, [getTextString]);

		const handleLongPressStart = React.useCallback(() => {
			if (!mobileLayout.enabled || !getTextString()) return;
			longPressTimeoutRef.current = window.setTimeout(() => {
				showMobileToast();
			}, 500);
		}, [mobileLayout.enabled, showMobileToast, getTextString]);

		const handleLongPressEnd = React.useCallback(() => {
			if (longPressTimeoutRef.current != null) {
				window.clearTimeout(longPressTimeoutRef.current);
				longPressTimeoutRef.current = null;
			}
		}, []);

		React.useLayoutEffect(() => {
			if (!isOpen || !targetRef.current || !tooltipRef.current || mobileLayout.enabled) {
				if (cleanupRef.current) {
					cleanupRef.current();
					cleanupRef.current = null;
				}
				return;
			}

			updatePosition();

			if (document.contains(targetRef.current)) {
				cleanupRef.current = autoUpdate(targetRef.current, tooltipRef.current, updatePosition);
			}

			return () => {
				if (cleanupRef.current) {
					cleanupRef.current();
					cleanupRef.current = null;
				}
			};
		}, [isOpen, updatePosition, mobileLayout.enabled]);

		React.useEffect(() => {
			return () => {
				unmountedRef.current = true;
				hideTooltip();
				if (longPressTimeoutRef.current != null) {
					window.clearTimeout(longPressTimeoutRef.current);
					longPressTimeoutRef.current = null;
				}
				if (cleanupRef.current) {
					cleanupRef.current();
					cleanupRef.current = null;
				}
				cancelRaf();
			};
		}, [hideTooltip, cancelRaf]);

		type TooltipChildProps = React.HTMLAttributes<HTMLElement> & {disabled?: boolean; ref?: React.Ref<HTMLElement>};
		const child = children && React.isValidElement<TooltipChildProps>(children) ? React.Children.only(children) : null;
		const childSupportsRef = child ? elementSupportsRef(child) : false;
		const childRef = childSupportsRef && child ? (child.props.ref ?? null) : null;
		const mergedRef = useMergeRefs([targetRef, childSupportsRef ? childRef : null]);
		const wrapperRef = useMergeRefs([targetRef]);

		if (!child) return null;

		const childProps = child.props as TooltipChildProps;
		const {
			onMouseEnter: childMouseEnter,
			onMouseLeave: childMouseLeave,
			onClick: childClick,
			onTouchStart: childTouchStart,
			onTouchEnd: childTouchEnd,
			onTouchCancel: childTouchCancel,
		} = childProps;

		const isDisabled = Boolean(childProps.disabled || childProps['aria-disabled']);

		const mobileEventHandlers = mobileLayout.enabled
			? {
					onTouchStart: (event: React.TouchEvent<HTMLElement>) => {
						handleLongPressStart();
						childTouchStart?.(event);
					},
					onTouchEnd: (event: React.TouchEvent<HTMLElement>) => {
						handleLongPressEnd();
						childTouchEnd?.(event);
					},
					onTouchCancel: (event: React.TouchEvent<HTMLElement>) => {
						handleLongPressEnd();
						childTouchCancel?.(event);
					},
					onClick: (event: React.MouseEvent<HTMLElement>) => {
						childClick?.(event);
					},
				}
			: {};

		const desktopEventHandlers = !mobileLayout.enabled
			? {
					onMouseEnter: (event: React.MouseEvent<HTMLElement>) => {
						if (isDisabled) return;
						if (delay) {
							showDelayedTooltip();
						} else {
							showTooltip();
						}
						childMouseEnter?.(event);
					},
					onMouseLeave: (event: React.MouseEvent<HTMLElement>) => {
						hideTooltip();
						childMouseLeave?.(event);
					},
					onClick: (event: React.MouseEvent<HTMLElement>) => {
						hideTooltip();
						childClick?.(event);
					},
				}
			: {};

		const childWithHandlers = React.cloneElement(child, {
			...(childSupportsRef ? {ref: mergedRef} : {}),
			'aria-describedby': !mobileLayout.enabled && isOpen ? tooltipId : undefined,
			'aria-haspopup': 'true',
			'aria-expanded': !mobileLayout.enabled && isOpen ? 'true' : 'false',
			'aria-controls': !mobileLayout.enabled && isOpen ? tooltipId : undefined,
			'aria-label': typeof text === 'string' ? text : undefined,
			'aria-labelledby': typeof text === 'string' ? undefined : tooltipId,
			...desktopEventHandlers,
			...mobileEventHandlers,
		});

		const triggerNode = childSupportsRef ? (
			childWithHandlers
		) : (
			<span className={styles.triggerWrapper} ref={wrapperRef}>
				{childWithHandlers}
			</span>
		);

		return (
			<>
				{triggerNode}
				{isOpen && !mobileLayout.enabled && (
					<FloatingPortal root={portalRoot || undefined}>
						<AnimatePresence mode="wait">
							<motion.div
								key="tooltip"
								id={tooltipId}
								ref={(node) => {
									(tooltipRef as any).current = node;
									if (node && targetRef.current) {
										updatePosition();
									}
								}}
								style={{
									position: 'fixed',
									left: tooltipState.x,
									top: tooltipState.y,
									zIndex: 'var(--z-index-tooltip)',
									visibility: tooltipState.isReady ? 'visible' : 'hidden',
								}}
								initial={{opacity: 0, scale: 0.98}}
								animate={{opacity: 1, scale: 1}}
								exit={{opacity: 0, scale: 0.98}}
								transition={{
									opacity: {duration: 0.1},
									scale: {type: 'spring', damping: 25, stiffness: 500},
								}}
							>
								<TooltipContainer
									text={textRef.current}
									type={type}
									position={tooltipState.actualPlacement}
									maxWidth={maxWidth}
									size={size}
									arrowRef={arrowRef}
									arrowX={tooltipState.arrowX}
									arrowY={tooltipState.arrowY}
								/>
							</motion.div>
						</AnimatePresence>
					</FloatingPortal>
				)}
			</>
		);
	},
);
