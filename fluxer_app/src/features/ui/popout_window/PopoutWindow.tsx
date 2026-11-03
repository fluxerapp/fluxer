// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	MirroredDocumentPortal,
	type MirroredDocumentTarget,
} from '@app/features/ui/popout_window/MirroredDocumentPortal';
import {PopoutTitlebar} from '@app/features/ui/popout_window/PopoutTitlebar';
import styles from '@app/features/ui/popout_window/PopoutWindow.module.css';
import {mountMirroredDocument, type StylesheetExclude} from '@app/features/ui/popout_window/PopoutWindowDocument';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';

const logger = new Logger('PopoutWindow');

const PAGE_HIDE_POLL_MS = 50;
const PAGE_HIDE_SETTLE_MS = 2000;

function isDocumentReplaced(childWindow: Window, mountedDocument: Document): boolean {
	try {
		return childWindow.document !== mountedDocument;
	} catch {
		return true;
	}
}

export type PopoutWindowCloseReason = 'closed' | 'blocked' | 'replaced';

interface PopoutWindowProps {
	windowKey: string;
	windowGeneration: number;
	title: string;
	showTitlebarTitle?: boolean;
	width: number;
	height: number;
	isAlwaysOnTop: boolean;
	onToggleAlwaysOnTop?: () => void;
	onRestore?: () => void;
	onClosed: (windowGeneration: number, reason: PopoutWindowCloseReason) => void;
	onCloseRequest?: (close: () => void) => void;
	onWindowOpened?: (childWindow: Window, windowGeneration: number) => void;
	existingWindow?: Window | null;
	stylesheetExclude?: StylesheetExclude;
	children: React.ReactNode;
}

export const PopoutWindow: React.FC<PopoutWindowProps> = ({
	windowKey,
	windowGeneration,
	title,
	showTitlebarTitle = true,
	width,
	height,
	isAlwaysOnTop,
	onToggleAlwaysOnTop,
	onRestore,
	onClosed,
	onCloseRequest,
	onWindowOpened,
	existingWindow,
	stylesheetExclude,
	children,
}) => {
	const [target, setTarget] = useState<MirroredDocumentTarget | null>(null);
	const onClosedRef = useRef(onClosed);
	const onWindowOpenedRef = useRef(onWindowOpened);
	const initialSizeRef = useRef({width, height});
	const initialTitleRef = useRef(title);
	const initialWindowRef = useRef(existingWindow);
	const stylesheetExcludeRef = useRef(stylesheetExclude);
	onClosedRef.current = onClosed;
	onWindowOpenedRef.current = onWindowOpened;
	useEffect(() => {
		const features = `popup=yes,width=${initialSizeRef.current.width},height=${initialSizeRef.current.height}`;
		const adoptedWindow = initialWindowRef.current;
		const childWindow = adoptedWindow && !adoptedWindow.closed ? adoptedWindow : window.open('', windowKey, features);
		if (!childWindow) {
			logger.warn('Failed to open popout window', {windowKey});
			onClosedRef.current(windowGeneration, 'blocked');
			return;
		}
		const mirrored = mountMirroredDocument(childWindow, {
			title: initialTitleRef.current,
			exclude: stylesheetExcludeRef.current,
		});
		const mountedDocument = childWindow.document;
		let active = true;
		let pageHideTimer: ReturnType<typeof setTimeout> | null = null;
		const settlePageHide = (startedAt: number): void => {
			pageHideTimer = null;
			if (!active) return;
			if (!childWindow.closed && !isDocumentReplaced(childWindow, mountedDocument)) {
				if (performance.now() - startedAt < PAGE_HIDE_SETTLE_MS) {
					pageHideTimer = setTimeout(() => settlePageHide(startedAt), PAGE_HIDE_POLL_MS);
					return;
				}
			}
			active = false;
			if (childWindow.closed) {
				onClosedRef.current(windowGeneration, 'closed');
				return;
			}
			logger.warn('Popout window document was replaced', {windowKey});
			childWindow.close();
			onClosedRef.current(windowGeneration, 'replaced');
		};
		const handlePageHide = (): void => {
			if (!active || pageHideTimer !== null) return;
			const startedAt = performance.now();
			pageHideTimer = setTimeout(() => settlePageHide(startedAt), 0);
		};
		childWindow.addEventListener('pagehide', handlePageHide);
		childWindow.focus();
		onWindowOpenedRef.current?.(childWindow, windowGeneration);
		setTarget({childWindow, container: mirrored.container});
		return () => {
			active = false;
			if (pageHideTimer !== null) clearTimeout(pageHideTimer);
			mirrored.dispose();
			childWindow.removeEventListener('pagehide', handlePageHide);
			if (!childWindow.closed) {
				childWindow.close();
			}
		};
	}, [windowGeneration, windowKey]);
	useEffect(() => {
		if (!target || target.childWindow.closed) return;
		target.childWindow.document.title = title;
	}, [target, title]);
	const handleCloseClick = useCallback(() => {
		if (!target || target.childWindow.closed) {
			onClosedRef.current(windowGeneration, 'closed');
			return;
		}
		const {childWindow} = target;
		const close = (): void => childWindow.close();
		if (onCloseRequest) {
			onCloseRequest(close);
			return;
		}
		close();
	}, [target, onCloseRequest, windowGeneration]);
	if (!target) {
		return null;
	}
	return (
		<MirroredDocumentPortal target={target} data-flx="ui.popout-window.mirrored-document-portal">
			{isDesktop() || onRestore ? (
				<PopoutTitlebar
					title={title}
					showTitle={showTitlebarTitle}
					isAlwaysOnTop={isAlwaysOnTop}
					onToggleAlwaysOnTop={onToggleAlwaysOnTop}
					onRestore={onRestore}
					onClose={handleCloseClick}
					data-flx="voice.popout-window.popout-titlebar"
				/>
			) : null}
			<div className={styles.content} data-voice-popout-window="true" data-flx="voice.popout-window.content">
				{children}
			</div>
		</MirroredDocumentPortal>
	);
};
