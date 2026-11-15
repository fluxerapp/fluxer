// SPDX-License-Identifier: AGPL-3.0-or-later

import {ModalStack} from '@app/features/app/components/dialogs/ModalStack';
import {ContextMenu} from '@app/features/ui/action_menu/ContextMenu';
import {SVGMasks} from '@app/features/ui/components/SVGMasks';
import {getActivePortalHost, PortalHostContext, setActivePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import {Popouts} from '@app/features/ui/popover/PopoverPopouts';
import LayerManager from '@app/features/ui/state/LayerManager';
import {Toasts} from '@app/features/ui/toast/Toasts';
import {attachMiddleClickAutoscrollPasteGuard} from '@app/features/ui/utils/DesktopWindowBehaviorUtils';
import {createWindowFocusInteractionGuard} from '@app/features/ui/utils/WindowFocusInteractionGuard';
import type React from 'react';
import {useEffect} from 'react';
import {createPortal} from 'react-dom';

export interface MirroredDocumentTarget {
	childWindow: Window;
	container: HTMLElement;
}

function useMirroredDocumentPortalHost({childWindow, container}: MirroredDocumentTarget): void {
	useEffect(() => {
		const childDocument = container.ownerDocument;
		const focusGuard = createWindowFocusInteractionGuard({
			root: childDocument.documentElement,
			windowTarget: childWindow,
			initiallyFocused: childDocument.hasFocus(),
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
		if (childDocument.hasFocus()) activatePortalHost();
		const stopObservingLayers = LayerManager.observeDocument(childDocument);
		const detachPasteGuard = attachMiddleClickAutoscrollPasteGuard(childWindow);
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
			stopObservingLayers();
			detachPasteGuard();
			focusGuard.destroy();
			if (getActivePortalHost() === container) {
				setActivePortalHost(null);
			}
		};
	}, [childWindow, container]);
}

interface MirroredDocumentPortalProps {
	target: MirroredDocumentTarget;
	children: React.ReactNode;
}

export const MirroredDocumentPortal: React.FC<MirroredDocumentPortalProps> = ({target, children}) => {
	useMirroredDocumentPortalHost(target);
	const ownerDocument = target.container.ownerDocument;
	return createPortal(
		<PortalHostContext.Provider value={target.container}>
			<SVGMasks data-flx="ui.mirrored-document-portal.svg-masks" />
			{children}
			<ModalStack ownerDocument={ownerDocument} data-flx="ui.mirrored-document-portal.modal-stack" />
			<Popouts ownerDocument={ownerDocument} data-flx="ui.mirrored-document-portal.popouts" />
			<ContextMenu ownerDocument={ownerDocument} data-flx="ui.mirrored-document-portal.context-menu" />
			<Toasts ownerDocument={ownerDocument} data-flx="ui.mirrored-document-portal.toasts" />
		</PortalHostContext.Provider>,
		target.container,
	);
};
