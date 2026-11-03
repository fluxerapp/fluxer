// SPDX-License-Identifier: AGPL-3.0-or-later

import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {scopePortalHostToDocument, usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import ToastState from '@app/features/ui/state/Toast';
import {Toast} from '@app/features/ui/toast/Toast';
import {getToastAnchorBottom} from '@app/features/ui/toast/ToastAnchor';
import styles from '@app/features/ui/toast/Toasts.module.css';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import {clsx} from 'clsx';
import {AnimatePresence} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';
import {createPortal} from 'react-dom';

interface ToastsProps {
	ownerDocument?: Document;
}

export const Toasts: React.FC<ToastsProps> = observer(({ownerDocument = document}) => {
	const [bodyPortalRoot, setBodyPortalRoot] = useState<HTMLElement | null>(null);
	const overrideHost = scopePortalHostToDocument(usePortalHost(), ownerDocument);
	useEffect(() => {
		const container = ownerDocument.createElement('div');
		container.id = 'toast-portal-root';
		ownerDocument.body.appendChild(container);
		setBodyPortalRoot(container);
		return () => {
			ownerDocument.body.removeChild(container);
		};
	}, [ownerDocument]);
	const isMobileExperience = isMobileExperienceEnabled();
	const closeToast = useCallback((id: string) => {
		ToastCommands.destroyToast(id);
	}, []);
	const portalRoot = overrideHost ?? bodyPortalRoot;
	if (!portalRoot) return null;
	const currentToast = ToastState.currentToast?.ownerDocument === ownerDocument ? ToastState.currentToast : null;
	const anchorBottom = currentToast ? getToastAnchorBottom() : null;
	return createPortal(
		<div
			className={clsx(styles.container, isMobileExperience ? styles.containerMobile : styles.containerDesktop)}
			style={anchorBottom == null ? undefined : ({'--toast-anchor-bottom': `${anchorBottom}px`} as React.CSSProperties)}
			data-flx="ui.toast.toasts.container"
		>
			<AnimatePresence mode="wait" data-flx="ui.toast.toasts.animate-presence">
				{currentToast && (
					<div key={currentToast.id} className={styles.toastWrapper} data-flx="ui.toast.toasts.toast-wrapper">
						<Toast
							id={currentToast.id}
							closeToast={closeToast}
							data-flx="ui.toast.toasts.toast"
							{...currentToast.data}
						/>
					</div>
				)}
			</AnimatePresence>
		</div>,
		portalRoot,
	);
});
