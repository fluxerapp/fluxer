// SPDX-License-Identifier: AGPL-3.0-or-later

import {observeIntersection} from '@app/features/platform/utils/SharedIntersectionObserver';

type PlaybackToggle = (playing: boolean) => void;

const PLAYBACK_VISIBLE_RATIO = 0.7;
const PLAYBACK_MAX_ROOT_HEIGHTS = 8;
const MAX_CONCURRENT_PLAYBACK = 100;

function buildPlaybackRatioSteps(): Array<number> {
	const steps = [0, PLAYBACK_VISIBLE_RATIO];
	let ratio = PLAYBACK_VISIBLE_RATIO;
	while (PLAYBACK_VISIBLE_RATIO / ratio < PLAYBACK_MAX_ROOT_HEIGHTS) {
		ratio *= PLAYBACK_VISIBLE_RATIO;
		steps.push(ratio);
	}
	return steps.sort((a, b) => a - b);
}

const PLAYBACK_RATIO_STEPS = buildPlaybackRatioSteps();

const trackedNodes = new WeakMap<Element, PlaybackToggle>();
const playingNodes = new Set<Element>();

let onScreenNodes = new Set<Element>();

function fillsPlaybackRoot(entry: IntersectionObserverEntry): boolean {
	const rootBounds = entry.rootBounds;
	if (rootBounds == null || rootBounds.height <= 0) return false;
	return entry.intersectionRect.height >= rootBounds.height * PLAYBACK_VISIBLE_RATIO;
}

function isPlaybackVisible(entry: IntersectionObserverEntry): boolean {
	if (!entry.isIntersecting) return false;
	if (entry.intersectionRatio >= PLAYBACK_VISIBLE_RATIO) return true;
	return fillsPlaybackRoot(entry);
}

function isNearLeadingEdge(entry: IntersectionObserverEntry): boolean {
	const rootBounds = entry.rootBounds;
	if (rootBounds == null) return false;
	const bottomGap = Math.abs(entry.intersectionRect.bottom - rootBounds.bottom);
	const topGap = Math.abs(entry.intersectionRect.top - rootBounds.top);
	return bottomGap < topGap;
}

function stopTrailingPlayback(entered: Element): void {
	const ordered = Array.from(onScreenNodes);
	for (let index = ordered.length - 1; index >= 0; index--) {
		const node = ordered[index];
		if (node === entered) continue;
		if (!playingNodes.has(node)) continue;
		playingNodes.delete(node);
		trackedNodes.get(node)?.(false);
		return;
	}
}

function startNextWaitingPlayback(): void {
	if (playingNodes.size >= MAX_CONCURRENT_PLAYBACK) return;
	if (onScreenNodes.size <= playingNodes.size) return;
	for (const node of onScreenNodes) {
		if (playingNodes.has(node)) continue;
		playingNodes.add(node);
		trackedNodes.get(node)?.(true);
		return;
	}
}

function handleEntered(entry: IntersectionObserverEntry, toggle: PlaybackToggle): void {
	const node = entry.target;
	if (onScreenNodes.has(node)) return;
	const leading = isNearLeadingEdge(entry);
	if (leading) {
		onScreenNodes = new Set<Element>([node, ...onScreenNodes]);
	} else {
		onScreenNodes.add(node);
	}
	const shouldPlay = leading || playingNodes.size < MAX_CONCURRENT_PLAYBACK;
	if (shouldPlay) {
		playingNodes.add(node);
	} else {
		playingNodes.delete(node);
	}
	toggle(shouldPlay);
	if (shouldPlay && playingNodes.size > MAX_CONCURRENT_PLAYBACK) {
		stopTrailingPlayback(node);
	}
}

function handleExited(node: Element, toggle: PlaybackToggle): void {
	if (!onScreenNodes.has(node)) return;
	onScreenNodes.delete(node);
	playingNodes.delete(node);
	toggle(false);
	startNextWaitingPlayback();
}

export function trackViewportPlayback(
	element: Element,
	toggle: PlaybackToggle,
	clipRoot: Element | null = null,
): () => void {
	if (trackedNodes.has(element)) {
		throw new Error('trackViewportPlayback: element is already tracked, untrack it before tracking it again');
	}
	trackedNodes.set(element, toggle);
	const release = observeIntersection(
		element,
		(entry) => {
			const current = trackedNodes.get(entry.target);
			if (current == null) return;
			if (isPlaybackVisible(entry)) {
				handleEntered(entry, current);
			} else {
				handleExited(entry.target, current);
			}
		},
		{root: clipRoot, threshold: PLAYBACK_RATIO_STEPS},
	);
	let untracked = false;
	return () => {
		if (untracked) return;
		untracked = true;
		release();
		trackedNodes.delete(element);
		onScreenNodes.delete(element);
		if (playingNodes.delete(element)) {
			startNextWaitingPlayback();
		}
	};
}
