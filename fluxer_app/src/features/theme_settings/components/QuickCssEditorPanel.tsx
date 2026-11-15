// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {excludeUserStylesheets} from '@app/features/theme/hooks/useCustomThemeStyle';
import styles from '@app/features/theme_settings/components/QuickCssEditorPanel.module.css';
import QuickCssPanelLayout, {
	canDockPanel,
	clampDockWidth,
	clampFloatingRect,
	PANEL_EDGE_PADDING,
	PANEL_MIN_SIZE,
	type PanelRect,
	type PanelViewport,
	type QuickCssPanelMode,
} from '@app/features/theme_settings/state/QuickCssPanelLayout';
import {FloatingPaneResizeHandles, type ResizeEdge} from '@app/features/ui/floating_pane';
import {computeFreeResize} from '@app/features/ui/floating_pane/FloatingPaneMath';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {
	MirroredDocumentPortal,
	type MirroredDocumentTarget,
} from '@app/features/ui/popout_window/MirroredDocumentPortal';
import {type MirroredDocument, mountMirroredDocument} from '@app/features/ui/popout_window/PopoutWindowDocument';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {createContext, useCallback, useContext, useEffect, useLayoutEffect, useRef, useState} from 'react';
import {createPortal} from 'react-dom';

const QUICK_CSS_EDITOR_DESCRIPTOR = msg({
	message: 'Quick CSS editor',
	comment: 'Accessible name of the floating quick CSS editor panel and its frame.',
});
const RESIZE_EDITOR_DESCRIPTOR = msg({
	message: 'Resize the editor',
	comment: 'Accessible label of the handle that resizes the floating quick CSS editor panel.',
});

const FRAME_DOCUMENT = '<!doctype html><html><head><meta charset="utf-8"></head><body></body></html>';
const KEYBOARD_STEP = 16;
const KEYBOARD_LARGE_STEP = 64;
const TITLEBAR_SELECTOR = '[data-native-titlebar]';
const DOCK_LEFT_PROPERTY = '--quick-css-dock-left';
const DOCK_RIGHT_PROPERTY = '--quick-css-dock-right';

interface PanelControls {
	mode: QuickCssPanelMode;
	canDock: boolean;
	setMode: (mode: QuickCssPanelMode) => void;
	startMove: (event: React.PointerEvent<HTMLElement>) => void;
	moveBy: (dx: number, dy: number) => void;
}

const PanelControlsContext = createContext<PanelControls | null>(null);

export function useQuickCssPanelControls(): PanelControls | null {
	return useContext(PanelControlsContext);
}

type Interaction =
	| {kind: 'move'; pointerStartX: number; pointerStartY: number; rect: PanelRect}
	| {kind: 'resize'; edge: ResizeEdge; pointerStartX: number; pointerStartY: number; rect: PanelRect};

function readViewport(): PanelViewport {
	const titlebar = document.querySelector<HTMLElement>(TITLEBAR_SELECTOR);
	const titlebarBottom = titlebar ? Math.max(0, titlebar.getBoundingClientRect().bottom) : 0;
	return {width: window.innerWidth, height: window.innerHeight, top: titlebarBottom};
}

function useViewport(): PanelViewport {
	const [viewport, setViewport] = useState(readViewport);
	useEffect(() => {
		const update = (): void => setViewport(readViewport());
		window.addEventListener('resize', update);
		update();
		return () => window.removeEventListener('resize', update);
	}, []);
	return viewport;
}

const IMPORTANT_HOST_STYLES: ReadonlyArray<[string, string]> = [
	['display', 'block'],
	['visibility', 'visible'],
	['opacity', '1'],
	['position', 'fixed'],
	['margin', '0'],
	['padding', '0'],
	['border', '0'],
	['overflow', 'visible'],
	['background', 'transparent'],
	['transform', 'none'],
	['filter', 'none'],
	['clip-path', 'none'],
	['pointer-events', 'auto'],
];

export const QuickCssEditorPanel: React.FC<{children: React.ReactNode}> = observer(({children}) => {
	const {i18n} = useLingui();
	const hostRef = useRef<HTMLDivElement | null>(null);
	const frameRef = useRef<HTMLIFrameElement | null>(null);
	const [target, setTarget] = useState<MirroredDocumentTarget | null>(null);
	const [interaction, setInteraction] = useState<Interaction | null>(null);
	const [liveRect, setLiveRect] = useState<PanelRect | null>(null);
	const viewport = useViewport();
	const mode = QuickCssPanelLayout.effectiveMode(viewport);
	const rect = liveRect ?? QuickCssPanelLayout.rectFor(viewport);
	const animate = interaction === null && !Accessibility.useReducedMotion;
	const label = i18n._(QUICK_CSS_EDITOR_DESCRIPTOR);
	const [portalRoot] = useState(() => {
		const root = document.createElement('div');
		root.style.setProperty('display', 'contents', 'important');
		return root;
	});

	useLayoutEffect(() => {
		document.documentElement.appendChild(portalRoot);
		return () => portalRoot.remove();
	}, [portalRoot]);

	useLayoutEffect(() => {
		const host = hostRef.current;
		if (!host) return;
		for (const [property, value] of IMPORTANT_HOST_STYLES) {
			host.style.setProperty(property, value, 'important');
		}
		host.style.setProperty('inset', 'auto', 'important');
		host.style.setProperty('left', `${rect.x}px`, 'important');
		host.style.setProperty('top', `${rect.y}px`, 'important');
		host.style.setProperty('width', `${rect.width}px`, 'important');
		host.style.setProperty('height', `${rect.height}px`, 'important');
		host.style.setProperty(
			'transition',
			animate ? 'left 150ms ease, top 150ms ease, width 150ms ease, height 150ms ease' : 'none',
			'important',
		);
		const frame = frameRef.current;
		if (frame) {
			frame.style.setProperty('display', 'block', 'important');
			frame.style.setProperty('visibility', 'visible', 'important');
			frame.style.setProperty('pointer-events', interaction ? 'none' : 'auto', 'important');
		}
	});

	useLayoutEffect(() => {
		const host = hostRef.current;
		if (!host) return;
		host.showPopover();
		return () => {
			if (host.matches(':popover-open')) host.hidePopover();
		};
	}, []);

	useEffect(() => {
		const root = document.documentElement;
		root.style.setProperty(DOCK_LEFT_PROPERTY, mode === 'dock-left' ? `${rect.width}px` : '0px');
		root.style.setProperty(DOCK_RIGHT_PROPERTY, mode === 'dock-right' ? `${rect.width}px` : '0px');
	}, [mode, rect.width]);

	useEffect(
		() => () => {
			document.documentElement.style.removeProperty(DOCK_LEFT_PROPERTY);
			document.documentElement.style.removeProperty(DOCK_RIGHT_PROPERTY);
		},
		[],
	);

	const mirroredRef = useRef<MirroredDocument | null>(null);
	const handleFrameLoad = useCallback(() => {
		const frameWindow = frameRef.current?.contentWindow;
		if (!frameWindow || mirroredRef.current || frameWindow.document.URL !== 'about:srcdoc') return;
		const mirrored = mountMirroredDocument(frameWindow, {title: label, exclude: excludeUserStylesheets});
		mirroredRef.current = mirrored;
		setTarget({childWindow: frameWindow, container: mirrored.container});
	}, [label]);

	useEffect(
		() => () => {
			mirroredRef.current?.dispose();
			mirroredRef.current = null;
		},
		[],
	);

	const commitRect = useCallback(
		(next: PanelRect) => {
			if (mode === 'floating') {
				QuickCssPanelLayout.setFloatingRect(next);
				return;
			}
			QuickCssPanelLayout.setDockWidth(next.width);
		},
		[mode],
	);

	useEffect(() => {
		if (!interaction || !target) return;
		const frameWindow = target.childWindow;
		const frameDocument = target.container.ownerDocument;
		let latest: PanelRect | null = null;
		const toMainPoint = (event: PointerEvent): {x: number; y: number} => {
			if (event.view === frameWindow) {
				const frameRect = frameRef.current?.getBoundingClientRect();
				return {x: event.clientX + (frameRect?.left ?? 0), y: event.clientY + (frameRect?.top ?? 0)};
			}
			return {x: event.clientX, y: event.clientY};
		};
		const handleMove = (event: PointerEvent): void => {
			if ((event.buttons & 1) === 0) {
				handleEnd();
				return;
			}
			const point = toMainPoint(event);
			const dx = point.x - interaction.pointerStartX;
			const dy = point.y - interaction.pointerStartY;
			const currentViewport = readViewport();
			if (interaction.kind === 'move') {
				latest = clampFloatingRect(
					{...interaction.rect, x: interaction.rect.x + dx, y: interaction.rect.y + dy},
					currentViewport,
				);
			} else if (mode === 'floating') {
				latest = computeFreeResize(
					interaction,
					point.x,
					point.y,
					{
						left: PANEL_EDGE_PADDING,
						top: currentViewport.top + PANEL_EDGE_PADDING,
						right: currentViewport.width - PANEL_EDGE_PADDING,
						bottom: currentViewport.height - PANEL_EDGE_PADDING,
					},
					PANEL_MIN_SIZE,
				);
			} else {
				const width = clampDockWidth(interaction.rect.width + (mode === 'dock-left' ? dx : -dx), currentViewport);
				latest = {
					...interaction.rect,
					x: mode === 'dock-left' ? 0 : currentViewport.width - width,
					width,
				};
			}
			setLiveRect(latest);
		};
		const handleEnd = (): void => {
			if (latest) commitRect(latest);
			setLiveRect(null);
			setInteraction(null);
		};
		for (const eventTarget of [window, frameWindow]) {
			eventTarget.addEventListener('pointermove', handleMove as EventListener);
			eventTarget.addEventListener('pointerup', handleEnd);
			eventTarget.addEventListener('pointercancel', handleEnd);
		}
		window.addEventListener('blur', handleEnd);
		frameDocument.documentElement.style.cursor = interaction.kind === 'move' ? 'grabbing' : '';
		return () => {
			for (const eventTarget of [window, frameWindow]) {
				eventTarget.removeEventListener('pointermove', handleMove as EventListener);
				eventTarget.removeEventListener('pointerup', handleEnd);
				eventTarget.removeEventListener('pointercancel', handleEnd);
			}
			window.removeEventListener('blur', handleEnd);
			frameDocument.documentElement.style.cursor = '';
		};
	}, [interaction, target, mode, commitRect]);

	const startMove = useCallback(
		(event: React.PointerEvent<HTMLElement>) => {
			if (event.button !== 0 || mode !== 'floating') return;
			const frameRect = frameRef.current?.getBoundingClientRect();
			event.preventDefault();
			setInteraction({
				kind: 'move',
				pointerStartX: event.clientX + (frameRect?.left ?? 0),
				pointerStartY: event.clientY + (frameRect?.top ?? 0),
				rect,
			});
		},
		[mode, rect],
	);

	const createResizeHandler = useCallback(
		(edge: ResizeEdge) => (event: React.PointerEvent<HTMLElement>) => {
			if (event.button !== 0) return;
			event.preventDefault();
			event.stopPropagation();
			setInteraction({kind: 'resize', edge, pointerStartX: event.clientX, pointerStartY: event.clientY, rect});
		},
		[rect],
	);

	const moveBy = useCallback(
		(dx: number, dy: number) => {
			if (mode !== 'floating') return;
			QuickCssPanelLayout.setFloatingRect(clampFloatingRect({...rect, x: rect.x + dx, y: rect.y + dy}, readViewport()));
		},
		[mode, rect],
	);

	const resizeBy = useCallback(
		(dx: number, dy: number) => {
			if (mode === 'floating') {
				QuickCssPanelLayout.setFloatingRect(
					clampFloatingRect({...rect, width: rect.width + dx, height: rect.height + dy}, readViewport()),
				);
				return;
			}
			QuickCssPanelLayout.setDockWidth(clampDockWidth(rect.width + (mode === 'dock-left' ? dx : -dx), readViewport()));
		},
		[mode, rect],
	);

	const handleGripKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLElement>) => {
			const step = event.shiftKey ? KEYBOARD_LARGE_STEP : KEYBOARD_STEP;
			const deltas: Record<string, [number, number]> = {
				ArrowLeft: [-step, 0],
				ArrowRight: [step, 0],
				ArrowUp: [0, -step],
				ArrowDown: [0, step],
			};
			const delta = deltas[event.key];
			if (!delta) return;
			event.preventDefault();
			resizeBy(delta[0], delta[1]);
		},
		[resizeBy],
	);

	const setMode = useCallback((next: QuickCssPanelMode) => {
		QuickCssPanelLayout.setMode(next);
	}, []);

	const controls: PanelControls = {mode, canDock: canDockPanel(viewport), setMode, startMove, moveBy};

	return createPortal(
		<div
			ref={hostRef}
			popover="manual"
			className={styles.host}
			data-quick-css-panel=""
			data-mode={mode}
			data-flx="theme-settings.quick-css-editor-panel.host"
		>
			<FocusRingScope containerRef={hostRef} data-flx="theme-settings.quick-css-editor-panel.focus-ring-scope">
				<iframe
					ref={frameRef}
					title={label}
					srcDoc={FRAME_DOCUMENT}
					onLoad={handleFrameLoad}
					className={styles.frame}
					data-flx="theme-settings.quick-css-editor-panel.frame"
				/>
				{interaction ? (
					<div
						className={styles.captureLayer}
						data-cursor={interaction.kind === 'move' ? 'move' : interaction.edge}
						data-flx="theme-settings.quick-css-editor-panel.capture-layer"
					/>
				) : null}
				{mode === 'floating' ? (
					<FloatingPaneResizeHandles
						label={i18n._(RESIZE_EDITOR_DESCRIPTOR)}
						createResizeHandler={createResizeHandler}
						gripCorner={rect.x > viewport.width / 2 - rect.width / 2 ? 'bottom-left' : 'bottom-right'}
						onGripKeyDown={handleGripKeyDown}
						onGripDoubleClick={QuickCssPanelLayout.resetFloatingRect}
						data-flx="theme-settings.quick-css-editor-panel.floating-pane-resize-handles"
					/>
				) : (
					<button
						type="button"
						aria-label={i18n._(RESIZE_EDITOR_DESCRIPTOR)}
						className={styles.dockHandle}
						data-edge={mode === 'dock-left' ? 'right' : 'left'}
						onPointerDown={createResizeHandler(mode === 'dock-left' ? 'right' : 'left')}
						onKeyDown={handleGripKeyDown}
						data-flx="theme-settings.quick-css-editor-panel.dock-handle"
					/>
				)}
			</FocusRingScope>
			{target ? (
				<MirroredDocumentPortal
					target={target}
					data-flx="theme-settings.quick-css-editor-panel.mirrored-document-portal"
				>
					<PanelControlsContext.Provider value={controls}>{children}</PanelControlsContext.Provider>
				</MirroredDocumentPortal>
			) : null}
		</div>,
		portalRoot,
	);
});
