// SPDX-License-Identifier: AGPL-3.0-or-later

import {ModalStack} from '@app/features/app/components/dialogs/ModalStack';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {ContextMenu} from '@app/features/ui/action_menu/ContextMenu';
import {SVGMasks} from '@app/features/ui/components/SVGMasks';
import {getActivePortalHost, PortalHostContext, setActivePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import {Popouts} from '@app/features/ui/popover/PopoverPopouts';
import {
	createWindowFocusInteractionGuard,
	UNFOCUSED_FULLY_INTERACTIVE_CLASS,
} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import {PopoutTitlebar} from '@app/features/voice/components/popout/PopoutTitlebar';
import styles from '@app/features/voice/components/popout/PopoutWindow.module.css';
import {
	copyStylesheetsIntoDocument,
	observeDocumentStylesheets,
	observeDocumentThemeAttributes,
	syncDocumentThemeAttributes,
} from '@app/features/voice/components/popout/PopoutWindowDocument';
import type React from 'react';
import {useCallback, useEffect, useRef, useState} from 'react';
import {createPortal} from 'react-dom';

const logger = new Logger('PopoutWindow');

interface PopoutWindowProps {
	windowKey: string;
	windowGeneration: number;
	title: string;
	showTitlebarTitle?: boolean;
	width: number;
	height: number;
	isAlwaysOnTop: boolean;
	onToggleAlwaysOnTop?: () => void;
	onRestore: () => void;
	onClosed: (windowGeneration: number) => void;
	onWindowOpened?: (childWindow: Window, windowGeneration: number) => void;
	children: React.ReactNode;
}

interface PopoutChildState {
	childWindow: Window;
	container: HTMLElement;
	windowGeneration: number;
}

interface PopoutWindowOverlayTreeProps {
	ownerDocument: Document;
}

const PopoutWindowOverlayTree: React.FC<PopoutWindowOverlayTreeProps> = ({ownerDocument}) => (
	<>
		<ModalStack ownerDocument={ownerDocument} data-flx="voice.popout-window.modal-stack" />
		<Popouts ownerDocument={ownerDocument} data-flx="voice.popout-window.popouts" />
		<ContextMenu ownerDocument={ownerDocument} data-flx="voice.popout-window.context-menu" />
	</>
);

function prepareChildDocument(childWindow: Window, title: string): HTMLElement {
	const childDocument = childWindow.document;
	childDocument.title = title;
	syncDocumentThemeAttributes(document, childDocument);
	childDocument.documentElement.classList.add(UNFOCUSED_FULLY_INTERACTIVE_CLASS);
	copyStylesheetsIntoDocument(document, childDocument);
	childDocument.body.style.margin = '0';
	const container = childDocument.createElement('div');
	container.className = styles.root;
	childDocument.body.appendChild(container);
	return container;
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
	onWindowOpened,
	children,
}) => {
	const [childState, setChildState] = useState<PopoutChildState | null>(null);
	const onClosedRef = useRef(onClosed);
	const onWindowOpenedRef = useRef(onWindowOpened);
	const initialSizeRef = useRef({width, height});
	const initialTitleRef = useRef(title);
	onClosedRef.current = onClosed;
	onWindowOpenedRef.current = onWindowOpened;
	useEffect(() => {
		const features = `width=${initialSizeRef.current.width},height=${initialSizeRef.current.height}`;
		const childWindow = window.open('about:blank', windowKey, features);
		if (!childWindow) {
			logger.warn('Failed to open popout window', {windowKey});
			onClosedRef.current(windowGeneration);
			return;
		}
		const container = prepareChildDocument(childWindow, initialTitleRef.current);
		const disconnectThemeObserver = observeDocumentThemeAttributes(document, childWindow.document);
		const disconnectStylesheetObserver = observeDocumentStylesheets(document, childWindow.document);
		let closed = false;
		const handlePageHide = (): void => {
			if (closed) return;
			closed = true;
			onClosedRef.current(windowGeneration);
		};
		childWindow.addEventListener('pagehide', handlePageHide);
		childWindow.focus();
		onWindowOpenedRef.current?.(childWindow, windowGeneration);
		setChildState({childWindow, container, windowGeneration});
		return () => {
			disconnectThemeObserver();
			disconnectStylesheetObserver();
			childWindow.removeEventListener('pagehide', handlePageHide);
			closed = true;
			if (!childWindow.closed) {
				childWindow.close();
			}
		};
	}, [windowGeneration, windowKey]);
	useEffect(() => {
		if (!childState || childState.childWindow.closed) return;
		childState.childWindow.document.title = title;
	}, [childState, title]);
	useEffect(() => {
		if (!childState) return;
		const {childWindow, container} = childState;
		const childDocument = childWindow.document;
		const focusGuard = createWindowFocusInteractionGuard({
			root: childDocument.documentElement,
			windowTarget: childWindow,
			initiallyFocused: true,
		});
		const activatePortalHost = (): void => {
			setActivePortalHost(container);
		};
		const handleFocus = (): void => {
			focusGuard.setFocused(true);
			activatePortalHost();
		};
		const handleBlur = (): void => {
			focusGuard.setFocused(false);
		};
		activatePortalHost();
		focusGuard.setFocused(true);
		childWindow.addEventListener('focus', handleFocus);
		childWindow.addEventListener('blur', handleBlur);
		childDocument.addEventListener('pointerdown', activatePortalHost, true);
		childDocument.addEventListener('focusin', activatePortalHost, true);
		childDocument.addEventListener('keydown', activatePortalHost, true);
		return () => {
			childWindow.removeEventListener('focus', handleFocus);
			childWindow.removeEventListener('blur', handleBlur);
			childDocument.removeEventListener('pointerdown', activatePortalHost, true);
			childDocument.removeEventListener('focusin', activatePortalHost, true);
			childDocument.removeEventListener('keydown', activatePortalHost, true);
			focusGuard.destroy();
			if (getActivePortalHost() === container) {
				setActivePortalHost(null);
			}
		};
	}, [childState]);
	const handleCloseClick = useCallback(() => {
		if (!childState || childState.childWindow.closed) {
			onClosedRef.current(childState?.windowGeneration ?? windowGeneration);
			return;
		}
		childState.childWindow.close();
	}, [childState, windowGeneration]);
	if (!childState) {
		return null;
	}
	return createPortal(
		<PortalHostContext.Provider value={childState.container}>
			<SVGMasks data-flx="voice.popout-window.svg-masks" />
			<PopoutTitlebar
				title={title}
				showTitle={showTitlebarTitle}
				isAlwaysOnTop={isAlwaysOnTop}
				onToggleAlwaysOnTop={onToggleAlwaysOnTop}
				onRestore={onRestore}
				onClose={handleCloseClick}
				data-flx="voice.popout-window.popout-titlebar"
			/>
			<div className={styles.content} data-voice-popout-window="true" data-flx="voice.popout-window.content">
				{children}
			</div>
			<PopoutWindowOverlayTree
				ownerDocument={childState.container.ownerDocument}
				data-flx="voice.popout.popout-window.popout-window-overlay-tree"
			/>
		</PortalHostContext.Provider>,
		childState.container,
	);
};
