// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ModalBackdropTone, ModalTransitionPreset} from '@app/features/ui/utils/ModalUtils';

export interface ModalMotionInput {
	transitionPreset: ModalTransitionPreset;
	prefersReducedMotion: boolean;
	isFullscreenOnMobile: boolean;
}

export interface ModalBackdropMotionInput {
	transitionPreset: ModalTransitionPreset;
	prefersReducedMotion: boolean;
	isMobile: boolean;
	backdropTone: ModalBackdropTone;
}

const SETTLED_SURFACE = {opacity: 1, scale: 1} as const;

const BACKDROP_RESTING_OPACITY: Record<ModalBackdropTone, number> = {
	default: 0.85,
	strong: 0.92,
};

const BACKDROP_DISMISS_SPRING = {type: 'spring', stiffness: 1000, damping: 48, mass: 1} as const;

const QUICK_TRANSITION = {duration: 0.12, ease: 'easeOut'} as const;

const QUICK_SURFACE_OFFSET = 0.98;

export function resolveModalMotionSpec({
	transitionPreset,
	prefersReducedMotion,
	isFullscreenOnMobile,
}: ModalMotionInput) {
	if (transitionPreset === 'instant') {
		return {
			initial: SETTLED_SURFACE,
			animate: SETTLED_SURFACE,
			exit: SETTLED_SURFACE,
			transition: {duration: 0},
		};
	}
	if (prefersReducedMotion) {
		return {
			initial: {opacity: 0},
			animate: {opacity: 1},
			exit: {opacity: 0},
			transition: {duration: 0},
		};
	}
	if (isFullscreenOnMobile) {
		return {
			initial: {opacity: 0},
			animate: {opacity: 1},
			exit: {opacity: 0},
			transition: {duration: 0.15},
		};
	}
	if (transitionPreset === 'quick') {
		return {
			initial: {opacity: 0, scale: QUICK_SURFACE_OFFSET},
			animate: {opacity: 1, scale: 1},
			exit: {opacity: 0, scale: QUICK_SURFACE_OFFSET},
			transition: QUICK_TRANSITION,
		};
	}
	if (transitionPreset === 'profile-slide') {
		return {
			initial: {opacity: 0, x: 14},
			animate: {opacity: 1, x: 0},
			exit: {opacity: 0, x: 14},
			transition: {duration: 0.14, ease: 'easeOut'},
		};
	}
	return {
		initial: {opacity: 0, scale: 0.95},
		animate: {opacity: 1, scale: 1},
		exit: {opacity: 0, scale: 0.95},
		transition: {type: 'spring', stiffness: 400, damping: 30, mass: 0.8},
	};
}

export function resolveModalBackdropMotionSpec({
	transitionPreset,
	prefersReducedMotion,
	isMobile,
	backdropTone,
}: ModalBackdropMotionInput) {
	const mountsAtRest = (isMobile && !prefersReducedMotion) || transitionPreset === 'instant';
	const restingOpacity = BACKDROP_RESTING_OPACITY[backdropTone];
	const surface = {
		initial: {opacity: mountsAtRest ? restingOpacity : 0},
		animate: {opacity: restingOpacity},
		exit: {opacity: 0},
	};
	if (prefersReducedMotion) {
		return {...surface, transition: {duration: 0}};
	}
	if (transitionPreset === 'instant') {
		return {...surface, transition: BACKDROP_DISMISS_SPRING};
	}
	if (transitionPreset === 'quick') {
		return {...surface, transition: QUICK_TRANSITION};
	}
	if (isMobile) {
		return {...surface, transition: {duration: 0.15}};
	}
	return {...surface, transition: {duration: 0.2}};
}
