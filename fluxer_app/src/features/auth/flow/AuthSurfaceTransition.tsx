// SPDX-License-Identifier: AGPL-3.0-or-later

import {usePrefersReducedMotion} from '@app/features/ui/hooks/usePrefersReducedMotion';
import {flxElementClassName} from '@app/lib/react';
import {animate} from 'animejs';
import {type ReactNode, useLayoutEffect, useRef} from 'react';

export const AuthSurfaceMotionKind = Object.freeze({
	FULLSCREEN: 'fullscreen',
	CARD: 'card',
} as const);

export type AuthSurfaceMotionKind = (typeof AuthSurfaceMotionKind)[keyof typeof AuthSurfaceMotionKind];

interface AuthSurfaceMotionState {
	translateY: number;
	scale: number;
	duration: number;
}

interface AuthSurfaceTransitionProps {
	surfaceKey: string;
	motionKind: AuthSurfaceMotionKind;
	suppressed: boolean;
	anchorsContentOnInteraction: boolean;
	className: string;
	children: ReactNode;
}

const AUTH_SURFACE_MOTION: Record<AuthSurfaceMotionKind, AuthSurfaceMotionState> = {
	fullscreen: {translateY: 28, scale: 1, duration: 420},
	card: {translateY: 10, scale: 0.995, duration: 260},
};

function setSurfaceFinalStyle(surface: HTMLElement): void {
	surface.style.opacity = '1';
	surface.style.transform = 'translateY(0px) scale(1)';
}

function anchorSurfaceContent(surface: HTMLElement): void {
	const content = surface.firstElementChild;
	if (surface.style.paddingTop !== '' || !(content instanceof HTMLElement)) {
		return;
	}
	const style = getComputedStyle(surface);
	const paddingTop = Number.parseFloat(style.paddingTop);
	const freeHeight = surface.clientHeight - paddingTop - Number.parseFloat(style.paddingBottom) - content.offsetHeight;
	surface.style.alignItems = 'start';
	surface.style.paddingTop = `${paddingTop + Math.max(0, freeHeight / 2)}px`;
}

function releaseSurfaceContent(surface: HTMLElement): void {
	surface.style.removeProperty('align-items');
	surface.style.removeProperty('padding-top');
}

export function AuthSurfaceTransition({
	surfaceKey,
	motionKind,
	suppressed,
	anchorsContentOnInteraction,
	className,
	children,
}: AuthSurfaceTransitionProps) {
	const surfaceRef = useRef<HTMLElement | null>(null);
	const mountedSurfaceKeyRef = useRef<string | null>(null);
	const prefersReducedMotion = usePrefersReducedMotion();
	useLayoutEffect(() => {
		const surface = surfaceRef.current;
		if (surface == null) return;
		releaseSurfaceContent(surface);
		if (suppressed) {
			setSurfaceFinalStyle(surface);
			return;
		}
		const mountedSurfaceKey = mountedSurfaceKeyRef.current;
		mountedSurfaceKeyRef.current = surfaceKey;
		if (mountedSurfaceKey == null || mountedSurfaceKey === surfaceKey || prefersReducedMotion) {
			setSurfaceFinalStyle(surface);
			return;
		}
		const motionState = AUTH_SURFACE_MOTION[motionKind];
		const animation = animate(surface, {
			opacity: [0, 1],
			translateY: [motionState.translateY, 0],
			scale: [motionState.scale, 1],
			duration: motionState.duration,
			ease: 'out(4)',
		});
		return () => {
			animation.revert();
		};
	}, [motionKind, prefersReducedMotion, suppressed, surfaceKey]);
	useLayoutEffect(() => {
		const surface = surfaceRef.current;
		if (surface == null || suppressed || !anchorsContentOnInteraction) {
			return;
		}
		const anchor = (): void => anchorSurfaceContent(surface);
		const release = (): void => releaseSurfaceContent(surface);
		surface.addEventListener('pointerdown', anchor, true);
		surface.addEventListener('keydown', anchor, true);
		window.addEventListener('resize', release);
		return () => {
			surface.removeEventListener('pointerdown', anchor, true);
			surface.removeEventListener('keydown', anchor, true);
			window.removeEventListener('resize', release);
			release();
		};
	}, [anchorsContentOnInteraction, suppressed]);
	return (
		<flx-auth-surface-transition
			ref={surfaceRef}
			className={flxElementClassName(className)}
			data-flx="auth.flow.auth-surface-transition.surface"
		>
			{children}
		</flx-auth-surface-transition>
	);
}
