// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/dialogs/Modal.module.css';
import {resolveModalBackdropMotionSpec, resolveModalMotionSpec} from '@app/features/app/components/dialogs/ModalMotion';
import {useModalBackHandler} from '@app/features/app/hooks/useModalBackHandler';
import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as PopoutCommands from '@app/features/ui/commands/PopoutCommands';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import FocusRingManager from '@app/features/ui/focus_ring/FocusRingManager';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {usePortalHost} from '@app/features/ui/overlay/PortalHostContext';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import OverlayStack from '@app/features/ui/state/OverlayStack';
import {ModalStackContext} from '@app/features/ui/utils/ModalStackContext';
import {
	type HeaderProps,
	type ModalContextValue,
	type ModalProps,
	type ScreenReaderLabelProps,
	useHeaderLogic,
	useModalLogic,
	useScreenReaderLabelLogic,
} from '@app/features/ui/utils/ModalUtils';
import {wasPointerDownInside} from '@app/lib/overlay/DismissGuard';
import {FloatingFocusManager, FloatingOverlay, FloatingPortal, useFloating} from '@floating-ui/react';
import {useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {motion, type Transition} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useContext, useEffect, useMemo, useRef, useState} from 'react';

const ModalContext = React.createContext<ModalContextValue | null>(null);
const useModalContext = () => {
	const context = useContext(ModalContext);
	if (!context) {
		throw new Error('Modal components must be used within a Modal.Root');
	}
	return context;
};
const RootComponent = React.forwardRef<HTMLDivElement, ModalProps>(
	(
		{
			children,
			className,
			size = 'medium',
			initialFocusRef,
			centered = false,
			onClose,
			onAnimationComplete,
			backdropSlot,
			transitionPreset = 'default',
			disableHistoryManagement = false,
			...props
		},
		ref,
	) => {
		const modalSurfaceRef = useRef<HTMLDivElement | null>(null);
		const backdropContaminatedRef = useRef<boolean>(false);
		const [labelRegistry, setLabelRegistry] = useState<Partial<Record<'header' | 'screen-reader', string>>>({});
		const {refs, context} = useFloating({
			open: true,
		});
		const portalHost = usePortalHost();
		const modalDocument = portalHost?.ownerDocument ?? document;
		const {stackIndex, isVisible, needsBackdrop, isTopmost, restoreFocusOnClose} = useContext(ModalStackContext);
		const {
			isMobile,
			isFullscreenOnMobile,
			useFullscreenLayer,
			useMobileEdgeToEdge,
			prefersReducedMotion,
			modalContextValue,
			handleBackdropClick,
			handleClose,
		} = useModalLogic({
			size,
			centered,
			onClose,
			onAnimationComplete,
		});
		useModalBackHandler(handleClose, disableHistoryManagement);
		const isFirstModal = stackIndex === 0;
		useEffect(() => {
			if (!isFirstModal) {
				return;
			}
			PopoutCommands.closeAllForDocument(modalDocument);
		}, [isFirstModal, modalDocument]);
		const isInstantTransition = transitionPreset === 'instant';
		const shouldAnimateRoot = !prefersReducedMotion && !isInstantTransition;
		const [isRootAnimating, setIsRootAnimating] = useState(shouldAnimateRoot);
		const setModalSurfaceWrapperRef = useCallback((node: HTMLDivElement | null) => {
			modalSurfaceRef.current = node;
		}, []);
		const setMotionElementRef = useCallback(
			(node: HTMLDivElement | null) => {
				if (typeof ref === 'function') {
					ref(node);
				} else if (ref) {
					(ref as React.MutableRefObject<HTMLDivElement | null>).current = node;
				}
			},
			[ref],
		);
		const motionSpec = resolveModalMotionSpec({
			transitionPreset,
			prefersReducedMotion,
			isFullscreenOnMobile,
		});
		const backdropMotionSpec = resolveModalBackdropMotionSpec({
			transitionPreset,
			prefersReducedMotion,
			isMobile,
		});
		const handleBackdropClickEvent = useCallback(
			(event: React.MouseEvent) => {
				if (event.target === event.currentTarget) {
					event.preventDefault();
					event.stopPropagation();
					if (wasPointerDownInside(modalSurfaceRef.current)) {
						return;
					}
					if (backdropContaminatedRef.current) {
						return;
					}
					backdropContaminatedRef.current = true;
					setTimeout(() => {
						backdropContaminatedRef.current = false;
					}, 100);
					handleBackdropClick(onClose);
				}
			},
			[onClose, handleBackdropClick],
		);
		const handleAnimationStart = useCallback(() => {
			FocusRingManager.setRingsEnabled(false);
			setIsRootAnimating(shouldAnimateRoot);
		}, [shouldAnimateRoot]);
		const handleAnimationComplete = useCallback(() => {
			FocusRingManager.setRingsEnabled(KeyboardMode.keyboardModeEnabled);
			setIsRootAnimating(false);
			onAnimationComplete?.();
		}, [onAnimationComplete]);
		const enhancedModalContextValue = useMemo(() => {
			const originalRegisterLabel = modalContextValue.registerLabel;
			return {
				...modalContextValue,
				registerLabel: (source: 'header' | 'screen-reader', id: string) => {
					setLabelRegistry((current) => ({...current, [source]: id}));
					return originalRegisterLabel(source, id);
				},
			};
		}, [modalContextValue]);
		const labelledBy = useMemo(() => {
			const ids = Object.values(labelRegistry).filter(Boolean);
			return ids.length > 0 ? ids.join(' ') : undefined;
		}, [labelRegistry]);
		const isIOS =
			/iPhone|iPad|iPod/.test(navigator.userAgent) ||
			(navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1);
		const [acquiredZIndex, setAcquiredZIndex] = useState<number | null>(null);
		useEffect(() => {
			const zIndex = OverlayStack.acquire();
			setAcquiredZIndex(zIndex);
			return () => {
				OverlayStack.release();
			};
		}, []);
		const fallbackZIndex = OverlayStack.peek();
		const modalZIndex = acquiredZIndex ?? fallbackZIndex;
		const backdropZIndex = acquiredZIndex != null ? acquiredZIndex - 1 : fallbackZIndex - 1;
		const isInteractive = isVisible && isTopmost;
		const overlayStyle = useMemo(
			() =>
				({
					zIndex: modalZIndex,
					display: 'flex',
					alignItems: 'center',
					justifyContent: 'center',
					inset: 0,
					pointerEvents: isInteractive ? 'auto' : 'none',
				}) as const,
			[isInteractive, modalZIndex],
		);
		const layerVisibilityStyle = useMemo(() => {
			const visibility: React.CSSProperties['visibility'] = isVisible ? 'visible' : 'hidden';
			return {
				opacity: isVisible ? 1 : 0,
				visibility,
			};
		}, [isVisible]);
		const isCenteredOnMobile = isMobile && !useFullscreenLayer;
		return (
			<FloatingPortal root={portalHost ?? undefined} data-flx="app.modal.floating-portal">
				{needsBackdrop && isVisible && (
					<motion.div
						className={styles.modalBackdrop}
						style={{zIndex: backdropZIndex}}
						initial={backdropMotionSpec.initial}
						animate={backdropMotionSpec.animate}
						exit={backdropMotionSpec.exit}
						transition={backdropMotionSpec.transition as Transition}
						data-flx="app.modal.modal-backdrop"
					/>
				)}
				<FloatingOverlay
					lockScroll={!isIOS && isInteractive}
					className="modal-backdrop"
					aria-hidden={!isInteractive}
					style={overlayStyle}
					onClick={handleBackdropClickEvent}
					data-flx="app.modal.modal-backdrop.backdrop-click-event"
				>
					{isCenteredOnMobile && (
						<motion.div
							className={clsx(styles.backdropCentered, styles.positionAbsoluteInsetZero)}
							initial={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							animate={{opacity: 1}}
							exit={prefersReducedMotion ? {opacity: 1} : {opacity: 0}}
							transition={{duration: prefersReducedMotion ? 0 : 0.15}}
							data-flx="app.modal.backdrop-centered"
						/>
					)}
					{backdropSlot ? (
						<div className={styles.backdropSlot} data-flx="app.modal.backdrop-slot">
							{backdropSlot}
						</div>
					) : null}
					<div
						className={clsx(
							styles.layer,
							useFullscreenLayer && styles.layerFullscreen,
							useMobileEdgeToEdge && styles.layerFullscreenMobile,
							isCenteredOnMobile && styles.layerCentered,
						)}
						style={layerVisibilityStyle}
						data-flx="app.modal.layer"
					>
						<FloatingFocusManager
							context={context}
							initialFocus={initialFocusRef}
							disabled={!isInteractive}
							outsideElementsInert={isInteractive}
							returnFocus={restoreFocusOnClose}
							visuallyHiddenDismiss={isInteractive}
							getInsideElements={() => {
								if (!isInteractive) {
									return [];
								}
								const inside: Array<Element> = [];
								modalDocument.querySelectorAll('iframe[src*="hcaptcha"], .h-captcha').forEach((el) => inside.push(el));
								const popoutsRoot = modalDocument.querySelector('[data-popouts-root]');
								if (popoutsRoot) inside.push(popoutsRoot);
								const mediaViewerPortalRoot = modalDocument.querySelector('[data-media-viewer-portal-root]');
								if (mediaViewerPortalRoot) inside.push(mediaViewerPortalRoot);
								modalDocument.querySelectorAll('[data-floating-ui-portal]').forEach((el) => inside.push(el));
								modalDocument
									.querySelectorAll('[data-rsbs-root], [data-rsbs-backdrop], [data-rsbs-overlay]')
									.forEach((el) => inside.push(el));
								const nativeTitlebar = modalDocument.querySelector('[data-native-titlebar]');
								if (nativeTitlebar) inside.push(nativeTitlebar);
								return inside;
							}}
							data-flx="app.modal.floating-focus-manager"
						>
							<div
								ref={refs.setFloating}
								aria-labelledby={labelledBy}
								aria-modal={true}
								className={styles.focusLock}
								role="dialog"
								tabIndex={-1}
								data-flx="app.modal.focus-lock"
							>
								<div ref={setModalSurfaceWrapperRef} className={styles.surface} data-flx="app.modal.surface">
									<FocusRingScope containerRef={modalSurfaceRef} data-flx="app.modal.focus-ring-scope">
										<motion.div
											className={clsx(
												styles.root,
												isRootAnimating && styles.rootAnimating,
												isFullscreenOnMobile ? styles.fullscreen : styles[size as keyof typeof styles],
												isCenteredOnMobile && styles.centeredOnMobile,
												className,
											)}
											data-flx="app.modal.root"
											initial={motionSpec.initial}
											animate={motionSpec.animate}
											exit={motionSpec.exit}
											transition={motionSpec.transition as Transition}
											onAnimationStart={handleAnimationStart}
											onAnimationComplete={handleAnimationComplete}
											ref={setMotionElementRef}
											{...props}
										>
											<ModalContext.Provider value={enhancedModalContextValue}>{children}</ModalContext.Provider>
										</motion.div>
									</FocusRingScope>
								</div>
							</div>
						</FloatingFocusManager>
					</div>
				</FloatingOverlay>
			</FloatingPortal>
		);
	},
);

RootComponent.displayName = 'ModalRoot';

export const Root = observer(RootComponent);
export const Header = React.forwardRef<HTMLDivElement, HeaderProps>(
	({children, icon, title, variant = 'light', hideCloseButton = false, onClose, id, ...props}, ref) => {
		const {i18n} = useLingui();
		const modalContextValue = useModalContext();
		const {headingId, handleClose} = useHeaderLogic({
			title,
			onClose,
			id,
			modalContextValue,
		});
		return (
			<div
				className={clsx(styles.layout, styles.header, styles[variant as keyof typeof styles])}
				ref={ref}
				data-flx="app.modal.header.layout"
				{...props}
			>
				<div className={styles.headerInner} data-flx="app.modal.header.header-inner">
					<div className={styles.headerText} data-flx="app.modal.header.header-text">
						{icon}
						<h3 id={headingId} data-flx="app.modal.header.h3">
							{title}
						</h3>
					</div>
					{!hideCloseButton && (
						<FocusRing offset={-2} data-flx="app.modal.header.focus-ring">
							<button
								type="button"
								aria-label={i18n._(CLOSE_DESCRIPTOR)}
								onClick={handleClose}
								data-flx="app.modal.header.button.close"
							>
								<XIcon weight="bold" width={24} height={24} data-flx="app.modal.header.x-icon" />
							</button>
						</FocusRing>
					)}
				</div>
				{children}
			</div>
		);
	},
);

Header.displayName = 'ModalHeader';

type ContentProps = React.ComponentPropsWithoutRef<typeof Scroller> & {
	children: React.ReactNode;
	className?: string;
	padding?: 'default' | 'none';
};

export const Content = React.forwardRef<ScrollerHandle, ContentProps>(
	({children, className, padding = 'default', ...props}, ref) => (
		<Scroller
			className={clsx(styles.content, padding === 'none' && styles.contentNoPadding, className)}
			ref={ref}
			key="modal-content-scroller"
			data-flx="app.modal.content.content"
			{...props}
		>
			{children}
		</Scroller>
	),
);

Content.displayName = 'ModalContent';

interface FooterProps {
	children: React.ReactNode;
	className?: string;
	stretchButtons?: boolean;
}

export const Footer = React.forwardRef<HTMLDivElement, FooterProps>(
	({children, className, stretchButtons, ...props}, ref) => (
		<div
			className={clsx(styles.layout, styles.footer, stretchButtons && styles.stretchButtons, className)}
			ref={ref}
			data-flx="app.modal.footer.layout"
			{...props}
		>
			{children}
		</div>
	),
);

Footer.displayName = 'ModalFooter';

export const ScreenReaderLabel: React.FC<ScreenReaderLabelProps> = ({text, id}) => {
	const modalContextValue = useModalContext();
	const {labelId} = useScreenReaderLabelLogic({
		text,
		id,
		modalContextValue,
	});
	return (
		<span
			id={labelId}
			className={styles.screenReaderLabel}
			data-flx="app.modal.screen-reader-label.screen-reader-label"
		>
			{text}
		</span>
	);
};

ScreenReaderLabel.displayName = 'ModalScreenReaderLabel';

type InsetCloseButtonProps = Omit<React.ButtonHTMLAttributes<HTMLButtonElement>, 'type'> & {
	ariaLabel?: string;
};

export const InsetCloseButton = React.forwardRef<HTMLButtonElement, InsetCloseButtonProps>(
	({ariaLabel, className, ...props}, ref) => {
		const {i18n} = useLingui();
		return (
			<div
				className={styles.insetCloseButtonContainer}
				data-flx="app.modal.inset-close-button.inset-close-button-container"
			>
				<FocusRing offset={-2} data-flx="app.modal.inset-close-button.focus-ring">
					<button
						ref={ref}
						type="button"
						aria-label={ariaLabel ?? i18n._(CLOSE_DESCRIPTOR)}
						className={clsx(styles.insetCloseButton, className)}
						data-flx="app.modal.inset-close-button.inset-close-button"
						{...props}
					>
						<XIcon weight="bold" width={22} height={22} data-flx="app.modal.inset-close-button.x-icon" />
					</button>
				</FocusRing>
			</div>
		);
	},
);

InsetCloseButton.displayName = 'ModalInsetCloseButton';

interface ContentLayoutProps {
	children: React.ReactNode;
	className?: string;
}

export const ContentLayout = React.forwardRef<HTMLDivElement, ContentLayoutProps>(
	({children, className, ...props}, ref) => (
		<div
			className={clsx(styles.contentLayout, className)}
			ref={ref}
			data-flx="app.modal.content-layout.content-layout"
			{...props}
		>
			{children}
		</div>
	),
);

ContentLayout.displayName = 'ModalContentLayout';

interface DescriptionProps extends React.HTMLAttributes<HTMLDivElement> {
	children: React.ReactNode;
}

export const Description = React.forwardRef<HTMLDivElement, DescriptionProps>(
	({children, className, ...props}, ref) => (
		<div
			className={clsx(styles.description, className)}
			ref={ref}
			data-flx="app.modal.description.description"
			{...props}
		>
			{children}
		</div>
	),
);

Description.displayName = 'ModalDescription';

interface InputGroupProps {
	children: React.ReactNode;
	className?: string;
}

export const InputGroup = React.forwardRef<HTMLDivElement, InputGroupProps>(({children, className, ...props}, ref) => (
	<div className={clsx(styles.inputGroup, className)} ref={ref} data-flx="app.modal.input-group.input-group" {...props}>
		{children}
	</div>
));

InputGroup.displayName = 'ModalInputGroup';

interface FormFooterProps {
	children: React.ReactNode;
	className?: string;
	stretchButtons?: boolean;
}

export const FormFooter = React.forwardRef<HTMLDivElement, FormFooterProps>(
	({children, className, stretchButtons, ...props}, ref) => (
		<div
			className={clsx(styles.layout, styles.formFooter, stretchButtons && styles.stretchButtons, className)}
			ref={ref}
			data-flx="app.modal.form-footer.layout"
			{...props}
		>
			{children}
		</div>
	),
);

FormFooter.displayName = 'ModalFormFooter';
