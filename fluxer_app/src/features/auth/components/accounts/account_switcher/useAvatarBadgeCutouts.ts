// SPDX-License-Identifier: AGPL-3.0-or-later

import {observeResize} from '@app/features/platform/utils/SharedResizeObserver';
import {type RefObject, useLayoutEffect, useState} from 'react';

const BADGE_CUTOUT_SELECTOR = '[data-badge-cutout]';

const BADGE_CUTOUT_GAP_REM = 0.1875;

interface BadgeCutout {
	readonly x: number;
	readonly y: number;
	readonly width: number;
	readonly height: number;
	readonly radius: number;
}

export interface AvatarCutoutMask {
	readonly width: number;
	readonly height: number;
	readonly cutouts: ReadonlyArray<BadgeCutout>;
}

function cornerRadius(element: HTMLElement, width: number, height: number): number {
	const value = getComputedStyle(element).borderTopLeftRadius.split(' ')[0] ?? '0';
	const parsed = Number.parseFloat(value);
	const shortest = Math.min(width, height);
	const radius = value.endsWith('%') ? (parsed / 100) * shortest : parsed;
	return Math.min(Number.isFinite(radius) ? radius : 0, shortest / 2);
}

function measureMask(frame: HTMLElement): AvatarCutoutMask | null {
	const elements = frame.querySelectorAll<HTMLElement>(BADGE_CUTOUT_SELECTOR);
	if (elements.length === 0) {
		return null;
	}
	const frameStyle = getComputedStyle(frame);
	const width = Number.parseFloat(frameStyle.width);
	const height = Number.parseFloat(frameStyle.height);
	const frameRect = frame.getBoundingClientRect();
	if (!(width > 0 && height > 0 && frameRect.width > 0)) {
		return null;
	}
	const scale = frameRect.width / width;
	const gap = Number.parseFloat(getComputedStyle(frame.ownerDocument.documentElement).fontSize) * BADGE_CUTOUT_GAP_REM;
	const cutouts = Array.from(elements, (element) => {
		const rect = element.getBoundingClientRect();
		const badgeWidth = rect.width / scale;
		const badgeHeight = rect.height / scale;
		return {
			x: (rect.left - frameRect.left) / scale - gap,
			y: (rect.top - frameRect.top) / scale - gap,
			width: badgeWidth + gap * 2,
			height: badgeHeight + gap * 2,
			radius: cornerRadius(element, badgeWidth, badgeHeight) + gap,
		};
	});
	return {width, height, cutouts};
}

function sameMask(a: AvatarCutoutMask | null, b: AvatarCutoutMask | null): boolean {
	if (a == null || b == null) {
		return a === b;
	}
	return (
		a.width === b.width &&
		a.height === b.height &&
		a.cutouts.length === b.cutouts.length &&
		a.cutouts.every((cutout, index) => {
			const other = b.cutouts[index];
			return (
				other != null &&
				cutout.x === other.x &&
				cutout.y === other.y &&
				cutout.width === other.width &&
				cutout.height === other.height &&
				cutout.radius === other.radius
			);
		})
	);
}

export function useAvatarBadgeCutouts(
	frameRef: RefObject<HTMLElement | null>,
	badgeKey: string,
): AvatarCutoutMask | null {
	const [mask, setMask] = useState<AvatarCutoutMask | null>(null);
	useLayoutEffect(() => {
		const frame = frameRef.current;
		if (frame == null || badgeKey.length === 0) {
			setMask(null);
			return;
		}
		const update = () => {
			const next = measureMask(frame);
			setMask((previous) => (sameMask(previous, next) ? previous : next));
		};
		update();
		const targets = [frame, ...frame.querySelectorAll<HTMLElement>(BADGE_CUTOUT_SELECTOR)];
		const releases = targets.map((target) => observeResize(target, update));
		return () => {
			for (const release of releases) {
				release();
			}
		};
	}, [frameRef, badgeKey]);
	return mask;
}
