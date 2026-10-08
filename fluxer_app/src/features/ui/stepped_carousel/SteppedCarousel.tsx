// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/ui/stepped_carousel/SteppedCarousel.module.css';
import {
	createSteppedCarouselSnapshot,
	getSteppedCarouselStateValue,
	resolveSteppedCarouselHeight,
	selectSteppedCarouselModel,
	transitionSteppedCarouselSnapshot,
} from '@app/features/ui/stepped_carousel/SteppedCarouselStateMachine';
import {flxElementClassName} from '@app/lib/react';
import {AnimatePresence, motion, type Transition, useReducedMotion, type Variants} from 'framer-motion';
import type React from 'react';
import {useCallback, useLayoutEffect, useRef, useState} from 'react';

export const SteppedCarouselARIALive = Object.freeze({
	OFF: 'off',
	POLITE: 'polite',
} as const);

export type SteppedCarouselARIALive = (typeof SteppedCarouselARIALive)[keyof typeof SteppedCarouselARIALive];

interface SteppedCarouselProps<Step extends string> {
	step: Step;
	steps: ReadonlyArray<Step>;
	children: React.ReactNode;
	direction?: number;
	focusOnStepChange?: boolean;
	ariaLabel?: string;
	ariaLive?: SteppedCarouselARIALive;
	'data-flx'?: string;
}

interface SteppedCarouselActionsProps extends React.HTMLAttributes<HTMLElement> {
	children: React.ReactNode;
}

const slideVariants: Variants = {
	enter: (direction: number) => ({
		opacity: 0,
		x: direction > 0 ? 24 : direction < 0 ? -24 : 0,
	}),
	center: {
		opacity: 1,
		x: 0,
		transition: {type: 'spring', stiffness: 520, damping: 42, mass: 0.7},
	},
	exit: (direction: number) => ({
		opacity: 0,
		x: direction > 0 ? -24 : direction < 0 ? 24 : 0,
		transition: {duration: 0.1, ease: 'easeIn'},
	}),
};
const heightTransition: Transition = {
	type: 'spring',
	stiffness: 460,
	damping: 40,
	mass: 0.7,
};
const instantTransition: Transition = {
	duration: 0,
};
const reducedMotionVariants: Variants = {
	enter: {
		opacity: 0,
		x: 0,
	},
	center: {
		opacity: 1,
		x: 0,
	},
	exit: {
		opacity: 0,
		x: 0,
	},
};
const focusableSelector = [
	'[data-step-focus="true"]',
	'input:not([type="hidden"]):not([disabled]):not([tabindex="-1"])',
	'button:not([disabled]):not([tabindex="-1"])',
	'a[href]:not([tabindex="-1"])',
	'select:not([disabled]):not([tabindex="-1"])',
	'textarea:not([disabled]):not([tabindex="-1"])',
	'[tabindex]:not([tabindex="-1"])',
].join(',');

function stepsEqual(a: ReadonlyArray<string>, b: ReadonlyArray<string>): boolean {
	return a === b || (a.length === b.length && a.every((value, index) => value === b[index]));
}

export function SteppedCarousel<Step extends string>({
	step,
	steps,
	children,
	direction: directionProp,
	focusOnStepChange = false,
	ariaLabel,
	ariaLive,
	'data-flx': dataFlx,
}: SteppedCarouselProps<Step>): React.ReactElement {
	const shouldReduceMotion = useReducedMotion();
	const [snapshot, setSnapshot] = useState(() =>
		createSteppedCarouselSnapshot({step, steps, direction: directionProp, focusOnStepChange}),
	);
	const context = snapshot.context;
	const nextSnapshot =
		context.step !== step ||
		context.focusOnStepChange !== focusOnStepChange ||
		(directionProp !== undefined && context.direction !== directionProp) ||
		!stepsEqual(context.steps, steps)
			? transitionSteppedCarouselSnapshot(snapshot, {
					type: 'carousel.propsChanged',
					step,
					steps,
					direction: directionProp,
					focusOnStepChange,
				})
			: snapshot;
	if (nextSnapshot !== snapshot) {
		setSnapshot(nextSnapshot);
	}
	const model = selectSteppedCarouselModel(nextSnapshot);
	const paneRef = useRef<HTMLDivElement | null>(null);
	const measureNode = useCallback((node: HTMLElement) => {
		const offsetHeight = node.offsetHeight;
		const scrollHeight = node.scrollHeight;
		setSnapshot((current) =>
			getSteppedCarouselStateValue(current) === 'ready' &&
			current.context.contentHeight === resolveSteppedCarouselHeight({offsetHeight, scrollHeight})
				? current
				: transitionSteppedCarouselSnapshot(current, {type: 'carousel.measured', offsetHeight, scrollHeight}),
		);
	}, []);
	const setMeasureNode = useCallback(
		(node: HTMLDivElement) => {
			paneRef.current = node;
			let observer: ResizeObserver | null = null;
			if (typeof ResizeObserver !== 'undefined') {
				observer = new ResizeObserver(() => {
					if (paneRef.current === node) {
						measureNode(node);
					}
				});
				observer.observe(node);
			}
			return () => {
				observer?.disconnect();
				if (paneRef.current === node) {
					paneRef.current = null;
				}
			};
		},
		[measureNode],
	);
	useLayoutEffect(() => {
		if (!focusOnStepChange || model.focusRequestId === 0) return;
		const frame = window.requestAnimationFrame(() => {
			const pane = paneRef.current;
			if (!pane) return;
			if (pane.contains(document.activeElement)) return;
			const focusTarget = pane.querySelector<HTMLElement>(focusableSelector) ?? pane;
			focusTarget.focus({preventScroll: true});
		});
		return () => window.cancelAnimationFrame(frame);
	}, [focusOnStepChange, model.focusRequestId]);
	return (
		<motion.div
			className={styles.container}
			animate={{height: model.contentHeight}}
			transition={shouldReduceMotion ? instantTransition : heightTransition}
			aria-label={ariaLabel}
			aria-live={ariaLive}
			aria-atomic={ariaLive ? true : undefined}
			role={ariaLabel ? 'group' : undefined}
			data-flx={dataFlx ?? 'ui.stepped-carousel.container'}
		>
			<AnimatePresence
				mode="popLayout"
				initial={false}
				custom={model.direction}
				data-flx="ui.stepped-carousel.stepped-carousel.animate-presence"
			>
				<motion.div
					key={model.step}
					ref={setMeasureNode}
					className={styles.pane}
					custom={model.direction}
					variants={shouldReduceMotion ? reducedMotionVariants : slideVariants}
					initial="enter"
					animate="center"
					exit="exit"
					transition={shouldReduceMotion ? instantTransition : undefined}
					tabIndex={focusOnStepChange ? -1 : undefined}
					data-flx="ui.stepped-carousel.pane"
				>
					{children}
				</motion.div>
			</AnimatePresence>
		</motion.div>
	);
}

export function SteppedCarouselActions({
	children,
	className,
	...props
}: SteppedCarouselActionsProps): React.ReactElement {
	return (
		<flx-ui-stepped-carousel-actions
			className={flxElementClassName(styles.actions, className)}
			data-flx="ui.stepped-carousel.actions"
			{...props}
		>
			{children}
		</flx-ui-stepped-carousel-actions>
	);
}
