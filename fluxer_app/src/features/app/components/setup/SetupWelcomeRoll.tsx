// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WelcomeRotationEntry} from '@app/features/app/components/setup/SetupWizardWelcomeRotation';
import {flxElementClassName} from '@app/lib/react';
import {AnimatePresence, motion, useReducedMotion} from 'framer-motion';
import {useCallback, useLayoutEffect, useRef, useState} from 'react';

const WELCOME_WORD_INLINE_PADDING_PX = 8;
const WELCOME_SCALE_EPSILON = 0.005;

interface SetupWelcomeRollProps {
	entry: WelcomeRotationEntry;
	upcoming: WelcomeRotationEntry;
	frameClassName: string;
	measureClassName: string;
	wordClassName: string;
}

interface WelcomeMotionState {
	initial: {opacity: number; y?: number; scale: number};
	animate: {opacity: number; y?: number; scale: number};
	exit: {opacity: number; y?: number; scale: number};
}

function resolveMotionState(scale: number, prefersReducedMotion: boolean): WelcomeMotionState {
	if (prefersReducedMotion) {
		return {
			initial: {opacity: 0, scale},
			animate: {opacity: 1, scale},
			exit: {opacity: 0, scale},
		};
	}
	return {
		initial: {opacity: 0, y: 14, scale: scale * 0.98},
		animate: {opacity: 1, y: 0, scale},
		exit: {opacity: 0, y: -14, scale: scale * 0.98},
	};
}

export function SetupWelcomeRoll({
	entry,
	upcoming,
	frameClassName,
	measureClassName,
	wordClassName,
}: SetupWelcomeRollProps) {
	const prefersReducedMotion = useReducedMotion();
	const frameRef = useRef<HTMLElement | null>(null);
	const measureRef = useRef<HTMLSpanElement | null>(null);
	const [scale, setScale] = useState(1);
	const measureScale = useCallback(() => {
		const frame = frameRef.current;
		const measure = measureRef.current;
		if (frame == null || measure == null) return;
		const availableWidth = Math.max(0, frame.clientWidth - WELCOME_WORD_INLINE_PADDING_PX);
		const measuredWidth = Math.max(measure.scrollWidth, measure.getBoundingClientRect().width);
		const nextScale = measuredWidth > 0 && availableWidth > 0 ? Math.min(1, availableWidth / measuredWidth) : 1;
		setScale((currentScale) => (Math.abs(currentScale - nextScale) > WELCOME_SCALE_EPSILON ? nextScale : currentScale));
	}, []);
	useLayoutEffect(() => {
		measureScale();
		const frame = frameRef.current;
		if (frame == null) return undefined;
		const ownerWindow = frame.ownerDocument.defaultView;
		const resizeObserver =
			typeof ownerWindow?.ResizeObserver === 'function' ? new ownerWindow.ResizeObserver(measureScale) : null;
		resizeObserver?.observe(frame);
		ownerWindow?.addEventListener('resize', measureScale);
		void frame.ownerDocument.fonts?.ready.then(measureScale);
		return () => {
			resizeObserver?.disconnect();
			ownerWindow?.removeEventListener('resize', measureScale);
		};
	}, [measureScale, entry]);
	const motionState = resolveMotionState(scale, Boolean(prefersReducedMotion));
	return (
		<flx-app-setup-welcome-roll
			ref={frameRef}
			className={flxElementClassName(frameClassName)}
			data-flx="app.self-hosted-setup-wizard-gate.welcome-word-frame"
		>
			<span
				ref={measureRef}
				className={measureClassName}
				aria-hidden="true"
				data-flx="app.self-hosted-setup-wizard-gate.welcome-word-measure"
			>
				{entry.text}
			</span>
			<span
				className={measureClassName}
				aria-hidden="true"
				data-flx="app.self-hosted-setup-wizard-gate.welcome-word-upcoming"
			>
				{upcoming.text}
			</span>
			<AnimatePresence
				mode="wait"
				initial={false}
				data-flx="app.setup.setup-wizard-steps.welcome-step.animate-presence"
			>
				<motion.h2
					key={entry.code}
					className={wordClassName}
					initial={motionState.initial}
					animate={motionState.animate}
					exit={motionState.exit}
					transition={{duration: prefersReducedMotion ? 0.22 : 0.42, ease: [0.22, 1, 0.36, 1]}}
					data-flx="app.self-hosted-setup-wizard-gate.welcome-word"
				>
					{entry.text}
				</motion.h2>
			</AnimatePresence>
		</flx-app-setup-welcome-roll>
	);
}
