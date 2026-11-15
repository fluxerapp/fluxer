// SPDX-License-Identifier: AGPL-3.0-or-later

import {useModalBackHandler} from '@app/features/app/hooks/useModalBackHandler';
import styles from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherOverlay.module.css';
import {
	captureSwitcherReturnTarget,
	restoreFocusAfterSwitcher,
} from '@app/features/auth/components/accounts/account_switcher/AccountSwitcherReturnFocus';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {usePrefersReducedMotion} from '@app/features/ui/hooks/usePrefersReducedMotion';
import {usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import LayerManager, {LayerType} from '@app/features/ui/state/LayerManager';
import OverlayStack from '@app/features/ui/state/OverlayStack';
import {ModalStackContext} from '@app/features/ui/utils/ModalStackContext';
import {FloatingFocusManager, FloatingPortal, useFloating} from '@floating-ui/react';
import {AnimatePresence, motion, useIsPresent} from 'framer-motion';
import type React from 'react';
import {useCallback, useContext, useEffect, useId, useRef, useState} from 'react';

const STAGE_FADE_SECONDS = 0.16;
const INTERACTIVE_SELECTOR = 'button, a, input, textarea, select, label, [data-switcher-card]';

function isEmptyAreaTarget(target: EventTarget | null): boolean {
	return target instanceof Element && target.closest(INTERACTIVE_SELECTOR) == null;
}
const VIEW_FADE_SECONDS = 0.18;
const VIEW_RISE_PX = 12;

interface AccountSwitcherOverlayProps {
	readonly viewKey: string;
	readonly labelledBy: string;
	readonly isBusy: boolean;
	readonly hasSwitchedAccount: () => boolean;
	readonly onEmptyAreaClick: () => void;
	readonly onEscape: () => void;
	readonly onKeyDown: (event: React.KeyboardEvent<HTMLDivElement>) => void;
	readonly children: React.ReactNode;
}

export function AccountSwitcherOverlay({
	viewKey,
	labelledBy,
	isBusy,
	hasSwitchedAccount,
	onEmptyAreaClick,
	onEscape,
	onKeyDown,
	children,
}: AccountSwitcherOverlayProps): React.ReactElement {
	const reducedMotion = usePrefersReducedMotion();
	const portalHost = usePortalHost();
	const ownerDocument = portalHost?.ownerDocument ?? document;
	const {isVisible, isTopmost} = useContext(ModalStackContext);
	const isInteractive = isVisible && isTopmost;
	const pointerDownOnEmptyRef = useRef(false);
	const handlePointerDownCapture = (event: React.PointerEvent<HTMLDivElement>) => {
		pointerDownOnEmptyRef.current = isEmptyAreaTarget(event.target);
	};
	const handleClick = (event: React.MouseEvent<HTMLDivElement>) => {
		const startedOnEmpty = pointerDownOnEmptyRef.current;
		pointerDownOnEmptyRef.current = false;
		if (event.button !== 0 || !startedOnEmpty || !isEmptyAreaTarget(event.target)) {
			return;
		}
		onEmptyAreaClick();
	};
	const rootRef = useRef<HTMLDivElement | null>(null);
	const dialogRef = useRef<HTMLDivElement | null>(null);
	const [returnTarget] = useState(() => captureSwitcherReturnTarget(ownerDocument));
	const hasSwitchedAccountRef = useRef(hasSwitchedAccount);
	hasSwitchedAccountRef.current = hasSwitchedAccount;
	const focusRestoredRef = useRef(false);
	const restoreFocus = useCallback(() => {
		if (focusRestoredRef.current) return;
		focusRestoredRef.current = true;
		restoreFocusAfterSwitcher(ownerDocument, {
			overlay: rootRef.current,
			returnTarget,
			switchedAccount: hasSwitchedAccountRef.current(),
		});
	}, [ownerDocument, returnTarget]);
	const isPresent = useIsPresent();
	useEffect(() => {
		if (!isPresent) restoreFocus();
	}, [isPresent, restoreFocus]);
	useEffect(() => restoreFocus, [restoreFocus]);
	const {refs, context} = useFloating({open: true});
	const setDialogRef = useCallback(
		(node: HTMLDivElement | null) => {
			dialogRef.current = node;
			refs.setFloating(node);
		},
		[refs],
	);
	const [zIndex, setZIndex] = useState<number | null>(null);
	useEffect(() => {
		setZIndex(OverlayStack.acquire());
		return () => OverlayStack.release();
	}, []);
	useEffect(() => {
		PopoutCommands.closeAllForDocument(ownerDocument);
	}, [ownerDocument]);
	const escapeRef = useRef(onEscape);
	escapeRef.current = onEscape;
	const layerKey = useId();
	useEffect(() => {
		LayerManager.addLayer(LayerType.MODAL, layerKey, () => escapeRef.current());
		return () => LayerManager.removeLayer(LayerType.MODAL, layerKey);
	}, [layerKey]);
	useModalBackHandler(onEscape);
	const fade = reducedMotion ? {duration: 0} : {duration: STAGE_FADE_SECONDS, ease: 'easeOut' as const};
	const viewInitial = reducedMotion ? {opacity: 0} : {opacity: 0, y: VIEW_RISE_PX};
	const viewExit = reducedMotion ? {opacity: 0} : {opacity: 0, y: -VIEW_RISE_PX};
	return (
		<FloatingPortal root={portalHost ?? undefined} data-flx="auth.accounts.account-switcher.overlay.portal">
			<motion.div
				ref={rootRef}
				className={styles.root}
				style={{
					zIndex: zIndex ?? OverlayStack.peek(),
					pointerEvents: isInteractive ? 'auto' : 'none',
					visibility: isVisible ? 'visible' : 'hidden',
				}}
				data-reduced-motion={reducedMotion ? 'true' : 'false'}
				initial={{opacity: 0}}
				animate={{opacity: 1}}
				exit={{opacity: 0}}
				transition={fade}
				onPointerDownCapture={handlePointerDownCapture}
				onClick={handleClick}
				data-flx="auth.accounts.account-switcher.overlay.root"
			>
				<div
					className={styles.backdrop}
					aria-hidden="true"
					data-flx="auth.accounts.account-switcher.overlay.backdrop"
				/>
				<FloatingFocusManager
					context={context}
					initialFocus={-1}
					disabled={!isInteractive}
					returnFocus={false}
					getInsideElements={() => {
						const inside: Array<Element> = [];
						const popoutsRoot = ownerDocument.querySelector('[data-popouts-root]');
						if (popoutsRoot != null) inside.push(popoutsRoot);
						ownerDocument.querySelectorAll('[data-floating-ui-portal]').forEach((element) => inside.push(element));
						return inside;
					}}
					data-flx="auth.accounts.account-switcher.overlay.focus-manager"
				>
					<div
						ref={setDialogRef}
						className={styles.dialog}
						role="dialog"
						aria-modal="true"
						aria-labelledby={labelledBy}
						aria-busy={isBusy}
						tabIndex={-1}
						onKeyDown={onKeyDown}
						data-account-switcher-overlay="true"
						data-flx="auth.accounts.account-switcher.overlay.dialog"
					>
						<FocusRingScope containerRef={dialogRef} data-flx="auth.accounts.account-switcher.overlay.focus-ring-scope">
							<AnimatePresence mode="wait" initial={false} data-flx="auth.accounts.account-switcher.overlay.views">
								<motion.div
									key={viewKey}
									className={styles.view}
									initial={viewInitial}
									animate={{opacity: 1, y: 0}}
									exit={viewExit}
									transition={
										reducedMotion ? {duration: VIEW_FADE_SECONDS} : {duration: VIEW_FADE_SECONDS, ease: 'easeOut'}
									}
									data-flx="auth.accounts.account-switcher.overlay.view"
								>
									{children}
								</motion.div>
							</AnimatePresence>
						</FocusRingScope>
					</div>
				</FloatingFocusManager>
			</motion.div>
		</FloatingPortal>
	);
}
