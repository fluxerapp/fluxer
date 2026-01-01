/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {FloatingFocusManager, FloatingOverlay, FloatingPortal, useFloating} from '@floating-ui/react';
import {useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as PopoutActionCreators from '~/actions/PopoutActionCreators';
import styles from '~/components/modals/Modal.module.css';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import FocusRingManager from '~/components/uikit/FocusRing/FocusRingManager';
import FocusRingScope from '~/components/uikit/FocusRing/FocusRingScope';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import KeyboardModeStore from '~/stores/KeyboardModeStore';
import {getBackdropZIndexForStack, getZIndexForStack} from '~/stores/ModalStore';
import OverlayStackStore from '~/stores/OverlayStackStore';
import {
	type HeaderProps,
	type ModalContextValue,
	type ModalProps,
	ModalStackContext,
	type ScreenReaderLabelProps,
	useHeaderLogic,
	useModalLogic,
	useScreenReaderLabelLogic,
} from '~/utils/modals/ModalUtils';

const ModalContext = React.createContext<ModalContextValue | null>(null);

const useModalContext = () => {
	const context = React.useContext(ModalContext);
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
			...props
		},
		ref,
	) => {
		const modalSurfaceRef = React.useRef<HTMLDivElement | null>(null);
		const backdropContaminatedRef = React.useRef<boolean>(false);
		const [labelRegistry, setLabelRegistry] = React.useState<Partial<Record<'header' | 'screen-reader', string>>>({});
		const {refs, context} = useFloating({
			open: true,
		});
		const {stackIndex, isVisible, needsBackdrop} = React.useContext(ModalStackContext);

		const {
			isMobile,
			isFullscreenOnMobile,
			useFullscreenLayer,
			useMobileEdgeToEdge,
			prefersReducedMotion,
			modalContextValue,
			handleBackdropClick,
		} = useModalLogic({
			size,
			centered,
			onClose,
			onAnimationComplete,
		});

		const isFirstModal = stackIndex === 0;

		React.useEffect(() => {
			if (!isFirstModal) {
				return;
			}
			PopoutActionCreators.closeAll();
		}, [isFirstModal]);

		const setModalSurfaceWrapperRef = React.useCallback((node: HTMLDivElement | null) => {
			modalSurfaceRef.current = node;
		}, []);

		const setMotionElementRef = React.useCallback(
			(node: HTMLDivElement | null) => {
				if (typeof ref === 'function') {
					ref(node);
				} else if (ref) {
					(ref as React.MutableRefObject<HTMLDivElement | null>).current = node;
				}
			},
			[ref],
		);

		const mobileFullscreenAnimations = {
			initial: {opacity: 0},
			animate: {opacity: 1},
			exit: {opacity: 0},
		};

		const defaultAnimations = prefersReducedMotion
			? {
					initial: {opacity: 0},
					animate: {opacity: 1},
					exit: {opacity: 0},
				}
			: {
					initial: {opacity: 0, scale: 0.95},
					animate: {opacity: 1, scale: 1},
					exit: {opacity: 0, scale: 0.95},
				};

		const animations = isFullscreenOnMobile ? mobileFullscreenAnimations : defaultAnimations;

		const handleBackdropMouseDown = React.useCallback((event: React.MouseEvent) => {
			if (event.target !== event.currentTarget) {
				backdropContaminatedRef.current = true;
			}
		}, []);

		const handleBackdropMouseUp = React.useCallback(() => {
			setTimeout(() => {
				backdropContaminatedRef.current = false;
			}, 0);
		}, []);

		const handleBackdropClickEvent = React.useCallback(
			(event: React.MouseEvent) => {
				if (event.target === event.currentTarget) {
					event.preventDefault();
					event.stopPropagation();

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

		const handleAnimationStart = React.useCallback(() => {
			FocusRingManager.setRingsEnabled(false);
		}, []);

		const handleAnimationComplete = React.useCallback(() => {
			FocusRingManager.setRingsEnabled(KeyboardModeStore.keyboardModeEnabled);
			onAnimationComplete?.();
		}, [onAnimationComplete]);

		const enhancedModalContextValue = React.useMemo(() => {
			const originalRegisterLabel = modalContextValue.registerLabel;

			return {
				...modalContextValue,
				registerLabel: (source: 'header' | 'screen-reader', id: string) => {
					setLabelRegistry((current) => ({...current, [source]: id}));
					return originalRegisterLabel(source, id);
				},
			};
		}, [modalContextValue]);

		const labelledBy = React.useMemo(() => {
			const ids = Object.values(labelRegistry).filter(Boolean);
			return ids.length > 0 ? ids.join(' ') : undefined;
		}, [labelRegistry]);

		const isIOS =
			/iPhone|iPad|iPod/.test(navigator.userAgent) ||
			(navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1);

		const [acquiredZIndex, setAcquiredZIndex] = React.useState<number | null>(null);

		React.useEffect(() => {
			const zIndex = OverlayStackStore.acquire();
			setAcquiredZIndex(zIndex);
			return () => {
				OverlayStackStore.release();
			};
		}, []);

		const modalZIndex = acquiredZIndex ?? getZIndexForStack(stackIndex);
		const backdropZIndex = acquiredZIndex != null ? acquiredZIndex - 1 : getBackdropZIndexForStack(stackIndex);

		const overlayStyle = React.useMemo(
			() => ({
				zIndex: modalZIndex,
				display: 'flex',
				alignItems: 'center',
				justifyContent: 'center',
				inset: 0,
			}),
			[modalZIndex],
		);

		const layerVisibilityStyle = React.useMemo(() => {
			const visibility: React.CSSProperties['visibility'] = isVisible ? 'visible' : 'hidden';
			return {
				opacity: isVisible ? 1 : 0,
				visibility,
			};
		}, [isVisible]);

		const isCenteredOnMobile = isMobile && !useFullscreenLayer;

		const shouldInstantBackdrop = isMobile && !prefersReducedMotion;
		const isInstantTransition = transitionPreset === 'instant';

		return (
			<FloatingPortal>
				{needsBackdrop && (
					<motion.div
						className={styles.modalBackdrop}
						style={{zIndex: backdropZIndex}}
						initial={{opacity: shouldInstantBackdrop || isInstantTransition ? 0.85 : 0}}
						animate={{opacity: 0.85}}
						exit={{opacity: 0}}
						transition={
							prefersReducedMotion || isInstantTransition
								? {duration: 0.05}
								: shouldInstantBackdrop
									? {duration: 0.15}
									: {duration: 0.2}
						}
					/>
				)}
				<FloatingOverlay
					lockScroll={!isIOS}
					className="modal-backdrop"
					aria-hidden={!isVisible}
					style={overlayStyle}
					onMouseDown={handleBackdropMouseDown}
					onMouseUp={handleBackdropMouseUp}
					onClick={handleBackdropClickEvent}
				>
					{isCenteredOnMobile && (
						<motion.div
							className="modal-backdrop-centered"
							initial={{opacity: 0}}
							animate={{opacity: 1}}
							exit={{opacity: 0}}
							transition={{duration: 0.15}}
							style={{position: 'absolute', inset: 0, pointerEvents: 'none'}}
						/>
					)}
					{backdropSlot ? <div className={styles.backdropSlot}>{backdropSlot}</div> : null}
					<div
						className={clsx(
							styles.layer,
							useFullscreenLayer && styles.layerFullscreen,
							useMobileEdgeToEdge && styles.layerFullscreenMobile,
							isCenteredOnMobile && styles.layerCentered,
						)}
						style={layerVisibilityStyle}
					>
						<FloatingFocusManager
							context={context}
							initialFocus={initialFocusRef}
							outsideElementsInert
							visuallyHiddenDismiss
							getInsideElements={() => {
								const inside: Array<Element> = [];
								document.querySelectorAll('iframe[src*="hcaptcha"], .h-captcha').forEach((el) => inside.push(el));
								const popoutsRoot = document.querySelector('[data-popouts-root]');
								if (popoutsRoot) inside.push(popoutsRoot);
								document.querySelectorAll('[data-floating-ui-portal]').forEach((el) => inside.push(el));
								document
									.querySelectorAll('[data-rsbs-root], [data-rsbs-backdrop], [data-rsbs-overlay]')
									.forEach((el) => inside.push(el));
								return inside;
							}}
						>
							<div
								ref={refs.setFloating}
								aria-labelledby={labelledBy}
								aria-modal={true}
								className={styles.focusLock}
								role="dialog"
								tabIndex={-1}
							>
								<div ref={setModalSurfaceWrapperRef} className={styles.surface}>
									<FocusRingScope containerRef={modalSurfaceRef}>
										<motion.div
											className={clsx(
												styles.root,
												isFullscreenOnMobile ? styles.fullscreen : styles[size],
												isCenteredOnMobile && styles.centeredOnMobile,
												className,
											)}
											{...animations}
											transition={
												prefersReducedMotion || transitionPreset === 'instant'
													? {duration: 0.05}
													: isFullscreenOnMobile
														? {duration: 0.15}
														: {
																type: 'spring',
																stiffness: 400,
																damping: 30,
																mass: 0.8,
															}
											}
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
		const {t} = useLingui();
		const modalContextValue = useModalContext();
		const {headingId, handleClose} = useHeaderLogic({
			title,
			onClose,
			id,
			modalContextValue,
		});

		return (
			<div className={clsx(styles.layout, styles.header, styles[variant])} ref={ref} {...props}>
				<div className={styles.headerInner}>
					<div className={styles.headerText}>
						{icon}
						<h3 id={headingId}>{title}</h3>
					</div>
					{!hideCloseButton && (
						<FocusRing offset={-2}>
							<button type="button" aria-label={t`Close`} onClick={handleClose}>
								<XIcon weight="regular" width={24} height={24} />
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
	({children, className, padding = 'default', reserveScrollbarTrack = false, ...props}, ref) => (
		<Scroller
			className={clsx(styles.content, padding === 'none' && styles.contentNoPadding, className)}
			ref={ref}
			key="modal-content-scroller"
			reserveScrollbarTrack={reserveScrollbarTrack}
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
}

export const Footer = React.forwardRef<HTMLDivElement, FooterProps>(({children, className, ...props}, ref) => (
	<div className={clsx(styles.layout, styles.footer, className)} ref={ref} {...props}>
		{children}
	</div>
));

Footer.displayName = 'ModalFooter';

export const ScreenReaderLabel: React.FC<ScreenReaderLabelProps> = ({text, id}) => {
	const modalContextValue = useModalContext();
	const {labelId} = useScreenReaderLabelLogic({
		text,
		id,
		modalContextValue,
	});

	return (
		<span id={labelId} className={styles.screenReaderLabel}>
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
		const {t} = useLingui();
		return (
			<div className={styles.insetCloseButtonContainer}>
				<FocusRing offset={-2}>
					<button
						ref={ref}
						type="button"
						aria-label={ariaLabel ?? t`Close`}
						className={clsx(styles.insetCloseButton, className)}
						{...props}
					>
						<XIcon weight="regular" width={22} height={22} />
					</button>
				</FocusRing>
			</div>
		);
	},
);

InsetCloseButton.displayName = 'ModalInsetCloseButton';

export type {ModalProps};
