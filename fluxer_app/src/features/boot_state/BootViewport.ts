// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterBootReveal} from '@app/features/boot_state/BootReveal';
import {getMessageSelector} from '@app/features/messaging/utils/MessageNodeSelectors';

type ViewportSource = () => HTMLElement | null;

export interface MeasuredChannelViewport {
	readonly anchorMessageId: string;
	readonly anchorOffsetPx: number;
	readonly messageTopsPx: ReadonlyMap<string, number>;
	readonly lastBottomPx: number;
}

const AT_BOTTOM_TOLERANCE_PX = 4;

const sources = new Map<string, ViewportSource>();

export function registerChannelViewport(channelId: string, source: ViewportSource): () => void {
	sources.set(channelId, source);
	return () => {
		if (sources.get(channelId) === source) {
			sources.delete(channelId);
		}
	};
}

export function channelViewportAtBottom(channelId: string): boolean | null {
	const viewport = sources.get(channelId)?.() ?? null;
	if (viewport === null) {
		return null;
	}
	return viewport.scrollTop >= viewport.scrollHeight - viewport.clientHeight - AT_BOTTOM_TOLERANCE_PX;
}

export function measureChannelViewport(channelId: string): MeasuredChannelViewport | null {
	const viewport = sources.get(channelId)?.() ?? null;
	if (viewport === null) {
		return null;
	}
	const viewportTop = viewport.getBoundingClientRect().top;
	const messageTopsPx = new Map<string, number>();
	let anchor: {id: string; offset: number} | null = null;
	let lastBottomPx = 0;
	for (const node of viewport.querySelectorAll<HTMLElement>(getMessageSelector(channelId))) {
		const messageId = node.dataset.messageId;
		if (messageId == null) {
			continue;
		}
		const rect = node.getBoundingClientRect();
		messageTopsPx.set(messageId, rect.top - viewportTop);
		lastBottomPx = Math.max(lastBottomPx, rect.bottom - viewportTop);
		if (anchor === null && rect.bottom > viewportTop) {
			anchor = {id: messageId, offset: rect.top - viewportTop};
		}
	}
	if (anchor === null) {
		return null;
	}
	return {anchorMessageId: anchor.id, anchorOffsetPx: anchor.offset, messageTopsPx, lastBottomPx};
}

export type RestoredScroll =
	| {readonly kind: 'bottom'}
	| {readonly kind: 'anchor'; readonly messageId: string; readonly offsetPx: number};

const restoredScrolls = new Map<string, RestoredScroll>();

export function rememberRestoredScroll(channelId: string, scroll: RestoredScroll): void {
	restoredScrolls.set(channelId, scroll);
}

export function takeRestoredScroll(channelId: string): RestoredScroll | null {
	const scroll = restoredScrolls.get(channelId) ?? null;
	restoredScrolls.delete(channelId);
	return scroll;
}

export function forgetRestoredScrolls(): void {
	restoredScrolls.clear();
}

let userAreaOverlayHeightPx: number | null = null;

export function recordUserAreaOverlayHeight(heightPx: number | null): void {
	userAreaOverlayHeightPx = heightPx;
}

export function currentUserAreaOverlayHeight(): number | null {
	return userAreaOverlayHeightPx;
}

const MAX_RESTORED_IMAGE_SOURCES = 1000;
const restoredImageSources = new Set<string>();

export function captureVisibleImageSources(): Array<string> {
	const sources = new Set<string>();
	for (const image of document.images) {
		const source = image.getAttribute('src');
		if (source === null || !image.complete || image.naturalWidth === 0) {
			continue;
		}
		const rect = image.getBoundingClientRect();
		if (rect.bottom <= 0 || rect.top >= window.innerHeight || rect.width === 0) {
			continue;
		}
		sources.add(source);
		if (sources.size === MAX_RESTORED_IMAGE_SOURCES) {
			break;
		}
	}
	return Array.from(sources);
}

const MAX_WARMED_IMAGE_SOURCES = 128;

export function warmRestoredImages(sources: ReadonlyArray<string>): void {
	const warming = sources.slice(0, MAX_WARMED_IMAGE_SOURCES).map((source) => {
		const image = new Image();
		image.decoding = 'async';
		image.src = source;
		image.decode().catch(() => undefined);
		return image;
	});
	afterBootReveal(() => {
		warming.length = 0;
	});
}

export function rememberRestoredImageSources(sources: ReadonlyArray<string>): void {
	for (const source of sources.slice(0, MAX_RESTORED_IMAGE_SOURCES)) {
		restoredImageSources.add(source);
	}
}

export function wasShownBeforeRestart(source: string): boolean {
	return restoredImageSources.has(source);
}

export function forgetRestoredImageSource(source: string): void {
	restoredImageSources.delete(source);
}

const FRAME_LAYOUT_COLUMNS = 16;
const FRAME_LAYOUT_ROWS = 10;
const FRAME_LAYOUT_TARGET_SELECTOR =
	'[data-message-id], [data-channel-id], [data-guild-id], [data-user-id], [data-flx]';

function frameLayoutTarget(x: number, y: number): string {
	const target = document.elementFromPoint(x, y)?.closest<HTMLElement>(FRAME_LAYOUT_TARGET_SELECTOR);
	if (target == null) {
		return '';
	}
	const {messageId, channelId, guildId, userId, flx} = target.dataset;
	const rect = target.getBoundingClientRect();
	return `${messageId ?? channelId ?? guildId ?? userId ?? flx}@${Math.round(rect.left)},${Math.round(rect.top)}`;
}

export function measureFrameLayout(): string {
	const targets: Array<string> = [];
	for (let row = 0; row < FRAME_LAYOUT_ROWS; row += 1) {
		for (let column = 0; column < FRAME_LAYOUT_COLUMNS; column += 1) {
			targets.push(
				frameLayoutTarget(
					((column + 0.5) * window.innerWidth) / FRAME_LAYOUT_COLUMNS,
					((row + 0.5) * window.innerHeight) / FRAME_LAYOUT_ROWS,
				),
			);
		}
	}
	return targets.join('|');
}

export function frameLayoutsAllowWheel(restored: string, current: string): boolean {
	const previous = restored.split('|');
	const next = current.split('|');
	if (previous.length !== FRAME_LAYOUT_COLUMNS * FRAME_LAYOUT_ROWS || next.length !== previous.length) return false;
	let stableContentCells = 0;
	for (let index = 0; index < previous.length; index += 1) {
		const before = previous[index]!;
		const after = next[index]!;
		if (before === '' || after === '') {
			if (before !== after) return false;
			continue;
		}
		const original = /^(.+)@(-?\d+),(-?\d+)$/.exec(before);
		const actual = /^(.+)@(-?\d+),(-?\d+)$/.exec(after);
		if (original === null || actual === null) return false;
		const coordinates = [Number(original[2]), Number(original[3]), Number(actual[2]), Number(actual[3])];
		if (!coordinates.every(Number.isSafeInteger)) return false;
		if (original[1]!.startsWith('ui.scroller.scroller-track.') || actual[1]!.startsWith('ui.scroller.scroller-track.'))
			continue;
		if (
			original[1] === 'channel.messages.scroller-inner' &&
			actual[1] === original[1] &&
			Math.abs(coordinates[0]! - coordinates[2]!) <= 1
		)
			continue;
		if (
			original[1] !== actual[1] ||
			Math.abs(coordinates[0]! - coordinates[2]!) > 1 ||
			Math.abs(coordinates[1]! - coordinates[3]!) > 1
		)
			return false;
		stableContentCells += 1;
	}
	return stableContentCells > 0;
}

let restoredFrameLayout: string | null = null;

export function rememberRestoredFrameLayout(layout: string | null): void {
	restoredFrameLayout = layout;
}

export function takeRestoredFrameLayout(): string | null {
	const layout = restoredFrameLayout;
	restoredFrameLayout = null;
	return layout;
}
