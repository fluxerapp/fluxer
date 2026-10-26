// SPDX-License-Identifier: AGPL-3.0-or-later

import {AppStorageKey} from '@app/features/platform/state/AppStorageKeys';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {makeAutoObservable} from 'mobx';

export type QuickCssPanelMode = 'floating' | 'dock-left' | 'dock-right';

export interface PanelRect {
	x: number;
	y: number;
	width: number;
	height: number;
}

export interface PanelViewport {
	width: number;
	height: number;
	top: number;
}

export const PANEL_EDGE_PADDING = 16;
export const PANEL_MIN_SIZE = {width: 420, height: 360};
const PANEL_DEFAULT_SIZE = {width: 600, height: 640};
const DOCK_DEFAULT_WIDTH = 480;
const DOCK_MIN_WIDTH = 360;
const DOCK_MAX_VIEWPORT_RATIO = 0.6;
const APP_MIN_WIDTH_BESIDE_DOCK = 940;

function clamp(value: number, min: number, max: number): number {
	if (!Number.isFinite(value)) return min;
	if (max < min) return min;
	return Math.min(max, Math.max(min, value));
}

export function clampFloatingRect(rect: PanelRect, viewport: PanelViewport): PanelRect {
	const maxWidth = Math.max(0, viewport.width - PANEL_EDGE_PADDING * 2);
	const maxHeight = Math.max(0, viewport.height - viewport.top - PANEL_EDGE_PADDING * 2);
	const width = Math.round(clamp(rect.width, Math.min(PANEL_MIN_SIZE.width, maxWidth), maxWidth));
	const height = Math.round(clamp(rect.height, Math.min(PANEL_MIN_SIZE.height, maxHeight), maxHeight));
	const minY = viewport.top + PANEL_EDGE_PADDING;
	return {
		x: Math.round(clamp(rect.x, PANEL_EDGE_PADDING, viewport.width - width - PANEL_EDGE_PADDING)),
		y: Math.round(clamp(rect.y, minY, viewport.height - height - PANEL_EDGE_PADDING)),
		width,
		height,
	};
}

function defaultFloatingRect(viewport: PanelViewport): PanelRect {
	return clampFloatingRect(
		{
			x: viewport.width - PANEL_DEFAULT_SIZE.width - PANEL_EDGE_PADDING,
			y: viewport.height - PANEL_DEFAULT_SIZE.height - PANEL_EDGE_PADDING,
			...PANEL_DEFAULT_SIZE,
		},
		viewport,
	);
}

export function clampDockWidth(width: number, viewport: PanelViewport): number {
	const max = Math.min(
		Math.floor(viewport.width * DOCK_MAX_VIEWPORT_RATIO),
		viewport.width - APP_MIN_WIDTH_BESIDE_DOCK,
	);
	return Math.round(clamp(width, DOCK_MIN_WIDTH, Math.max(DOCK_MIN_WIDTH, max)));
}

export function canDockPanel(viewport: PanelViewport): boolean {
	return viewport.width - APP_MIN_WIDTH_BESIDE_DOCK >= DOCK_MIN_WIDTH;
}

interface StoredLayout {
	mode: QuickCssPanelMode;
	floatingRect: PanelRect | null;
	dockWidth: number;
}

function isFiniteRect(value: unknown): value is PanelRect {
	if (typeof value !== 'object' || value === null) return false;
	const rect = value as Record<string, unknown>;
	return ['x', 'y', 'width', 'height'].every((key) => typeof rect[key] === 'number' && Number.isFinite(rect[key]));
}

function readStoredLayout(): StoredLayout {
	const fallback: StoredLayout = {mode: 'floating', floatingRect: null, dockWidth: DOCK_DEFAULT_WIDTH};
	try {
		const raw = AppStorage.getItem(AppStorageKey.QUICK_CSS_EDITOR_PANEL_LAYOUT);
		if (raw === null) return fallback;
		const parsed = JSON.parse(raw) as Record<string, unknown>;
		const mode =
			parsed.mode === 'dock-left' || parsed.mode === 'dock-right' || parsed.mode === 'floating'
				? parsed.mode
				: fallback.mode;
		return {
			mode,
			floatingRect: isFiniteRect(parsed.floatingRect) ? parsed.floatingRect : null,
			dockWidth:
				typeof parsed.dockWidth === 'number' && Number.isFinite(parsed.dockWidth)
					? parsed.dockWidth
					: fallback.dockWidth,
		};
	} catch {
		return fallback;
	}
}

class QuickCssPanelLayout {
	mode: QuickCssPanelMode;
	floatingRect: PanelRect | null;
	dockWidth: number;

	constructor() {
		const stored = readStoredLayout();
		this.mode = stored.mode;
		this.floatingRect = stored.floatingRect;
		this.dockWidth = stored.dockWidth;
		makeAutoObservable(this, {}, {autoBind: true});
	}

	effectiveMode(viewport: PanelViewport): QuickCssPanelMode {
		return this.mode !== 'floating' && !canDockPanel(viewport) ? 'floating' : this.mode;
	}

	rectFor(viewport: PanelViewport): PanelRect {
		const mode = this.effectiveMode(viewport);
		if (mode === 'floating') {
			return this.floatingRect ? clampFloatingRect(this.floatingRect, viewport) : defaultFloatingRect(viewport);
		}
		const width = clampDockWidth(this.dockWidth, viewport);
		return {
			x: mode === 'dock-left' ? 0 : viewport.width - width,
			y: viewport.top,
			width,
			height: Math.max(0, viewport.height - viewport.top),
		};
	}

	setMode(mode: QuickCssPanelMode): void {
		this.mode = mode;
		this.persist();
	}

	setFloatingRect(rect: PanelRect): void {
		this.floatingRect = rect;
		this.persist();
	}

	resetFloatingRect(): void {
		this.floatingRect = null;
		this.persist();
	}

	setDockWidth(width: number): void {
		this.dockWidth = width;
		this.persist();
	}

	private persist(): void {
		try {
			AppStorage.setItem(
				AppStorageKey.QUICK_CSS_EDITOR_PANEL_LAYOUT,
				JSON.stringify({mode: this.mode, floatingRect: this.floatingRect, dockWidth: this.dockWidth}),
			);
		} catch {}
	}
}

export default new QuickCssPanelLayout();
