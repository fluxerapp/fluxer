// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {flxElementClassName} from '@app/lib/react';
import {animate} from 'animejs';
import {observer} from 'mobx-react-lite';
import {type ReactNode, useLayoutEffect, useRef} from 'react';

export const DiscoveryMotionKind = Object.freeze({
	NAVBAR: 'navbar',
	RESULTS: 'results',
	FILTERS: 'filters',
} as const);

export type DiscoveryMotionKind = (typeof DiscoveryMotionKind)[keyof typeof DiscoveryMotionKind];

interface DiscoveryMotionState {
	readonly translateY: number;
	readonly duration: number;
}

const DISCOVERY_MOTION: Record<DiscoveryMotionKind, DiscoveryMotionState> = {
	navbar: {translateY: 6, duration: 180},
	results: {translateY: 0, duration: 200},
	filters: {translateY: 8, duration: 220},
};

interface DiscoveryTransitionProps {
	readonly transitionKey: string;
	readonly motionKind: DiscoveryMotionKind;
	readonly className: string;
	readonly children: ReactNode;
}

function settleSurface(surface: HTMLElement): void {
	surface.style.opacity = '';
	surface.style.transform = '';
}

export const DiscoveryTransition = observer(function DiscoveryTransition({
	transitionKey,
	motionKind,
	className,
	children,
}: DiscoveryTransitionProps) {
	const surfaceRef = useRef<HTMLDivElement | null>(null);
	const settledKeyRef = useRef<string | null>(null);
	const prefersReducedMotion = Accessibility.useReducedMotion;
	useLayoutEffect(() => {
		const surface = surfaceRef.current;
		if (surface == null) {
			return undefined;
		}
		const previousKey = settledKeyRef.current;
		settledKeyRef.current = transitionKey;
		if (previousKey === transitionKey || prefersReducedMotion) {
			settleSurface(surface);
			return undefined;
		}
		const motionState = DISCOVERY_MOTION[motionKind];
		const animation = animate(surface, {
			opacity: [0, 1],
			translateY: [motionState.translateY, 0],
			duration: motionState.duration,
			ease: 'out(3)',
			onComplete: () => settleSurface(surface),
		});
		return () => {
			animation.revert();
			settleSurface(surface);
		};
	}, [motionKind, prefersReducedMotion, transitionKey]);
	return (
		<div
			ref={surfaceRef}
			className={flxElementClassName(className)}
			data-flx="discovery.discovery.discovery-transition.div"
		>
			{children}
		</div>
	);
});
