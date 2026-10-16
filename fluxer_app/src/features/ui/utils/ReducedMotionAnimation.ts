// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Transition} from 'framer-motion';

type MotionTarget = Record<string, string | number>;

export interface MotionAnimation {
	initial: MotionTarget;
	animate: MotionTarget;
	exit: MotionTarget;
	transition: Transition;
}

export function getReducedMotionProps(animation: MotionAnimation, prefersReducedMotion: boolean): MotionAnimation {
	if (!prefersReducedMotion) {
		return animation;
	}
	return {
		initial: animation.animate,
		animate: animation.animate,
		exit: animation.animate,
		transition: {duration: 0},
	};
}

export const TOOLTIP_MOTION: MotionAnimation = {
	initial: {opacity: 0, scale: 0.98},
	animate: {opacity: 1, scale: 1},
	exit: {opacity: 0, scale: 0.98},
	transition: {
		opacity: {duration: 0.1},
		scale: {type: 'spring', damping: 25, stiffness: 500},
	},
};
