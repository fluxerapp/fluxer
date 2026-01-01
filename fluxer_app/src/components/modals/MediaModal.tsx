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

import {useLingui} from '@lingui/react/macro';
import {
	ArrowSquareOutIcon,
	CaretLeftIcon,
	CaretRightIcon,
	DownloadSimpleIcon,
	InfoIcon,
	MagnifyingGlassMinusIcon,
	MagnifyingGlassPlusIcon,
	StarIcon,
	XIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type {
	CSSProperties,
	FC,
	KeyboardEvent as ReactKeyboardEvent,
	MouseEvent as ReactMouseEvent,
	ReactNode,
} from 'react';
import {createElement, forwardRef, useCallback, useEffect, useMemo, useRef, useState} from 'react';
import {createPortal} from 'react-dom';
import {TransformComponent, TransformWrapper} from 'react-zoom-pan-pinch';
import * as MediaViewerActionCreators from '~/actions/MediaViewerActionCreators';
import {ExpiryFootnote} from '~/components/common/ExpiryFootnote';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip';
import AccessibilityStore from '~/stores/AccessibilityStore';
import LayerManager from '~/stores/LayerManager';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import styles from './MediaModal.module.css';

interface MediaModalProps {
	title: string;
	fileName?: string;
	fileSize?: string;
	dimensions?: string;
	expiryInfo?: {
		expiresAt: Date | null;
		isExpired: boolean;
	};
	isFavorited?: boolean;
	onFavorite?: () => void;
	onSave?: () => void;
	onOpenInBrowser?: () => void;
	onInfo?: () => void;
	additionalActions?: ReactNode;
	children: ReactNode;
	enablePanZoom?: boolean;
	currentIndex?: number;
	totalAttachments?: number;
	onPrevious?: () => void;
	onNext?: () => void;
	thumbnails?: Array<{
		src: string;
		alt?: string;
		type?: 'image' | 'gif' | 'gifv' | 'video' | 'audio';
	}>;
	onSelectThumbnail?: (index: number) => void;
}

interface ControlButtonProps {
	icon: ReactNode;
	label: string;
	onClick: () => void;
	variant?: 'default' | 'primary' | 'danger';
	active?: boolean;
	disabled?: boolean;
}

const ControlButton = observer(
	forwardRef<HTMLButtonElement, ControlButtonProps>(
		({icon, label, onClick, variant = 'default', active = false, disabled = false}, ref) => {
			const getVariantClass = () => {
				if (active) {
					if (variant === 'primary') return styles.controlButtonPrimaryActive;
					if (variant === 'danger') return styles.controlButtonDangerActive;
					return styles.controlButtonDefaultActive;
				}
				if (variant === 'primary') return styles.controlButtonPrimary;
				if (variant === 'danger') return styles.controlButtonDanger;
				return styles.controlButtonDefault;
			};

			return (
				<FocusRing offset={-2} enabled={!disabled}>
					<button
						ref={ref}
						type="button"
						onClick={disabled ? undefined : onClick}
						className={clsx(styles.controlButton, getVariantClass(), disabled && styles.controlButtonDisabled)}
						aria-label={label}
						aria-pressed={active}
						disabled={disabled}
					>
						{icon}
					</button>
				</FocusRing>
			);
		},
	),
);

ControlButton.displayName = 'ControlButton';

interface FileInfoProps {
	fileName?: string;
	fileSize?: string;
	dimensions?: string;
	expiryInfo?: {
		expiresAt: Date | null;
		isExpired: boolean;
	};
	currentIndex?: number;
	totalAttachments?: number;
	onPrevious?: () => void;
	onNext?: () => void;
}

const FileInfo: FC<FileInfoProps> = observer(
	({fileName, fileSize, dimensions, expiryInfo, currentIndex, totalAttachments, onPrevious, onNext}) => {
		const {t} = useLingui();
		const hasNavigation = currentIndex !== undefined && totalAttachments !== undefined && totalAttachments > 1;

		if (!fileName && !hasNavigation) {
			return null;
		}

		return (
			<div className={styles.fileInfoInline}>
				{fileName && (
					<div className={styles.fileInfoContent}>
						<p className={styles.fileInfoName}>{fileName}</p>
						<p className={styles.fileInfoMeta}>
							{[fileSize, dimensions].filter(Boolean).join(' \u2022 ')}
							{expiryInfo?.expiresAt && AccessibilityStore.showAttachmentExpiryIndicator && (
								<>
									{(fileSize || dimensions) && ' \u2022 '}
									<ExpiryFootnote expiresAt={expiryInfo.expiresAt} isExpired={expiryInfo.isExpired} inline />
								</>
							)}
						</p>
					</div>
				)}

				{hasNavigation && (
					<div className={styles.fileInfoNavigation}>
						<ControlButton
							icon={<CaretLeftIcon size={16} weight="bold" />}
							label={t`Previous attachment`}
							onClick={onPrevious ?? (() => {})}
							disabled={currentIndex === 0}
						/>
						<span className={styles.fileInfoNavigationText}>{t`${currentIndex + 1}/${totalAttachments}`}</span>
						<ControlButton
							icon={<CaretRightIcon size={16} weight="bold" />}
							label={t`Next attachment`}
							onClick={onNext ?? (() => {})}
							disabled={currentIndex === totalAttachments - 1}
						/>
					</div>
				)}
			</div>
		);
	},
);

interface ControlsProps {
	isFavorited?: boolean;
	onFavorite?: () => void;
	onSave?: () => void;
	onOpenInBrowser?: () => void;
	onInfo?: () => void;
	onClose: () => void;
	additionalActions?: ReactNode;
	zoomState?: 'fit' | 'zoomed';
	onZoom?: (state: 'fit' | 'zoomed') => void;
	enableZoomControls?: boolean;
}

const clamp = (value: number, min: number, max: number) => Math.min(max, Math.max(min, value));

const getViewportPadding = () => {
	const minSide = Math.min(window.innerWidth, window.innerHeight);
	return Math.round(clamp(minSide * 0.05, 16, 64));
};

const Controls: FC<ControlsProps> = observer(
	({
		isFavorited,
		onFavorite,
		onSave,
		onOpenInBrowser,
		onInfo,
		onClose,
		additionalActions,
		zoomState = 'fit',
		onZoom,
		enableZoomControls = false,
	}) => {
		const {t} = useLingui();
		const [isHoveringActions, setIsHoveringActions] = useState(false);
		const hasActions =
			onFavorite || onSave || onOpenInBrowser || onInfo || (enableZoomControls && onZoom) || additionalActions;
		const isZoomed = zoomState === 'zoomed';
		const showActions = hasActions && (!isZoomed || isHoveringActions);

		return (
			<div
				className={styles.controlsBox}
				role="toolbar"
				aria-label={t`Media controls`}
				onMouseEnter={() => setIsHoveringActions(true)}
				onMouseLeave={() => setIsHoveringActions(false)}
			>
				{showActions && (
					<div className={styles.actionControlsBox}>
						{enableZoomControls && onZoom && (
							<Tooltip text={zoomState === 'fit' ? t`Zoom in` : t`Zoom out`} position="bottom">
								<span>
									<ControlButton
										icon={
											zoomState === 'fit' ? (
												<MagnifyingGlassPlusIcon size={18} weight="bold" />
											) : (
												<MagnifyingGlassMinusIcon size={18} weight="bold" />
											)
										}
										label={zoomState === 'fit' ? t`Zoom in` : t`Zoom out`}
										onClick={() => onZoom(zoomState === 'fit' ? 'zoomed' : 'fit')}
									/>
								</span>
							</Tooltip>
						)}

						{onFavorite && (
							<Tooltip text={isFavorited ? t`Remove from favorites` : t`Add to favorites`} position="bottom">
								<span>
									<ControlButton
										icon={<StarIcon size={18} weight={isFavorited ? 'fill' : 'bold'} />}
										label={isFavorited ? t`Remove from favorites` : t`Add to favorites`}
										onClick={onFavorite}
										variant={isFavorited ? 'primary' : 'default'}
										active={isFavorited}
									/>
								</span>
							</Tooltip>
						)}

						{onSave && (
							<Tooltip text={t`Save media`} position="bottom">
								<span>
									<ControlButton
										icon={<DownloadSimpleIcon size={18} weight="bold" />}
										label={t`Save media`}
										onClick={onSave}
									/>
								</span>
							</Tooltip>
						)}

						{onOpenInBrowser && (
							<Tooltip text={t`Open in browser`} position="bottom">
								<span>
									<ControlButton
										icon={<ArrowSquareOutIcon size={18} weight="bold" />}
										label={t`Open in browser`}
										onClick={onOpenInBrowser}
									/>
								</span>
							</Tooltip>
						)}

						{onInfo && (
							<Tooltip text={t`Show media information`} position="bottom">
								<span>
									<ControlButton
										icon={<InfoIcon size={18} weight="bold" />}
										label={t`Show media information`}
										onClick={onInfo}
									/>
								</span>
							</Tooltip>
						)}

						{additionalActions}
					</div>
				)}

				<div className={styles.closeControlBox}>
					<Tooltip text={t`Close modal`} position="bottom">
						<span>
							<ControlButton icon={<XIcon size={18} weight="bold" />} label={t`Close modal`} onClick={onClose} />
						</span>
					</Tooltip>
				</div>
			</div>
		);
	},
);

type ZoomState = 'fit' | 'zoomed';

interface DesktopMediaViewerProps {
	children: ReactNode;
	onClose: () => void;
	onZoomStateChange?: (state: ZoomState) => void;
	zoomState?: ZoomState;
	onZoom?: (state: ZoomState) => void;
}

const DesktopMediaViewer: FC<DesktopMediaViewerProps> = observer(
	({children, onClose, onZoomStateChange, zoomState: externalZoomState, onZoom}) => {
		const [internalZoomState, setInternalZoomState] = useState<ZoomState>('fit');
		const [panX, setPanX] = useState(0);
		const [panY, setPanY] = useState(0);
		const [isDragging, setIsDragging] = useState(false);
		const [isHoveringContent, setIsHoveringContent] = useState(false);
		const [dragStart, setDragStart] = useState({x: 0, y: 0});
		const containerRef = useRef<HTMLButtonElement>(null);
		const contentRef = useRef<HTMLDivElement>(null);
		const dragStartedOnImageRef = useRef(false);
		const dragDistanceRef = useRef(0);
		const zoomStateBeforeDragRef = useRef<ZoomState>('fit');
		const currentZoomStateRef = useRef<ZoomState>('fit');

		const zoomState = externalZoomState ?? internalZoomState;
		currentZoomStateRef.current = zoomState;

		const updateZoomState = useCallback(
			(newState: ZoomState) => {
				currentZoomStateRef.current = newState;
				setPanX(0);
				setPanY(0);

				if (externalZoomState !== undefined) {
					onZoom?.(newState);
				} else {
					setInternalZoomState(newState);
					onZoomStateChange?.(newState);
				}
			},
			[onZoomStateChange, onZoom, externalZoomState],
		);

		const handleMouseDown = useCallback(
			(e: ReactMouseEvent) => {
				if (e.button !== 0) {
					return;
				}

				const currentZoom = currentZoomStateRef.current;

				if (e.target === containerRef.current) {
					if (currentZoom === 'fit') {
						return;
					}
					return;
				}

				dragStartedOnImageRef.current = true;
				dragDistanceRef.current = 0;
				zoomStateBeforeDragRef.current = currentZoom;

				if (currentZoom === 'zoomed') {
					setIsDragging(true);
					setDragStart({x: e.clientX - panX, y: e.clientY - panY});
				} else {
					updateZoomState('zoomed');
				}
			},
			[panX, panY, updateZoomState],
		);

		const handleMouseMove = useCallback(
			(e: ReactMouseEvent) => {
				if (isDragging && dragStartedOnImageRef.current) {
					const dx = e.clientX - dragStart.x - panX;
					const dy = e.clientY - dragStart.y - panY;
					dragDistanceRef.current += Math.sqrt(dx * dx + dy * dy);
				}
			},
			[isDragging, dragStart, panX, panY],
		);

		const handleMouseUp = useCallback(() => {
			setIsDragging(false);

			if (dragStartedOnImageRef.current && dragDistanceRef.current < 5 && zoomStateBeforeDragRef.current === 'zoomed') {
				updateZoomState('fit');
			}

			dragStartedOnImageRef.current = false;
			dragDistanceRef.current = 0;
		}, [updateZoomState]);

		const handleDoubleClick = useCallback(() => {
			updateZoomState(currentZoomStateRef.current === 'fit' ? 'zoomed' : 'fit');
		}, [updateZoomState]);

		const handleBackdropClick = useCallback(
			(e: React.SyntheticEvent<HTMLButtonElement>) => {
				if (e.target === e.currentTarget && !dragStartedOnImageRef.current) {
					onClose();
				}
			},
			[onClose],
		);

		useEffect(() => {
			const handleGlobalMouseMove = (e: MouseEvent) => {
				if (isDragging && currentZoomStateRef.current === 'zoomed' && containerRef.current && contentRef.current) {
					const newX = e.clientX - dragStart.x;
					const newY = e.clientY - dragStart.y;

					const containerRect = containerRef.current.getBoundingClientRect();
					const contentRect = contentRef.current.getBoundingClientRect();

					const zoomScaleValue = 2.5;
					const maxX = Math.max(0, (contentRect.width * zoomScaleValue - containerRect.width) / 2);
					const maxY = Math.max(0, (contentRect.height * zoomScaleValue - containerRect.height) / 2);

					const clampedX = Math.max(-maxX, Math.min(maxX, newX));
					const clampedY = Math.max(-maxY, Math.min(maxY, newY));

					setPanX(clampedX);
					setPanY(clampedY);

					const dx = e.clientX - (dragStart.x + panX);
					const dy = e.clientY - (dragStart.y + panY);
					dragDistanceRef.current += Math.sqrt(dx * dx + dy * dy);
				}
			};

			const handleGlobalMouseUp = () => {
				setIsDragging(false);

				if (
					dragStartedOnImageRef.current &&
					dragDistanceRef.current < 5 &&
					zoomStateBeforeDragRef.current === 'zoomed'
				) {
					updateZoomState('fit');
				}

				dragStartedOnImageRef.current = false;
				dragDistanceRef.current = 0;
			};

			if (isDragging) {
				document.addEventListener('mousemove', handleGlobalMouseMove, {passive: true});
				document.addEventListener('mouseup', handleGlobalMouseUp);
				return () => {
					document.removeEventListener('mousemove', handleGlobalMouseMove);
					document.removeEventListener('mouseup', handleGlobalMouseUp);
				};
			}
			return;
		}, [isDragging, dragStart, panX, panY, updateZoomState]);

		const zoomScale = zoomState === 'zoomed' ? 2.5 : 1;

		const getCursor = () => {
			if (!isHoveringContent) return 'default';
			if (isDragging) return 'grabbing';
			if (zoomState === 'zoomed') return 'zoom-out';
			return 'zoom-in';
		};

		return (
			<button
				type="button"
				ref={containerRef}
				className={styles.desktopViewerContainer}
				style={{cursor: getCursor()}}
				onMouseDown={handleMouseDown}
				onMouseMove={handleMouseMove}
				onMouseUp={handleMouseUp}
				onDoubleClick={handleDoubleClick}
				onClick={handleBackdropClick}
				onKeyDown={(e) => {
					if (e.key === 'Enter' || e.key === ' ') {
						handleBackdropClick(e);
					}
				}}
			>
				<div
					ref={contentRef}
					className={styles.desktopViewerContent}
					role="img"
					style={{
						transform: `translate3d(${panX / zoomScale}px, ${panY / zoomScale}px, 0) scale(${zoomScale})`,
						willChange: isDragging ? 'transform' : 'auto',
					}}
					onMouseEnter={() => setIsHoveringContent(true)}
					onMouseLeave={() => setIsHoveringContent(false)}
					onKeyDown={(e) => e.stopPropagation()}
					onClick={(e) => e.stopPropagation()}
				>
					{children}
				</div>
			</button>
		);
	},
);

const MobileMediaViewer: FC<{children: ReactNode}> = observer(({children}) => {
	return (
		<div className={styles.mobileViewerContainer}>
			<TransformWrapper
				initialScale={1}
				minScale={1}
				maxScale={5}
				wheel={{step: 0.1}}
				pinch={{step: 5}}
				doubleClick={{
					disabled: false,
					step: 0.7,
					mode: 'toggle',
				}}
				centerOnInit
				centerZoomedOut
				panning={{disabled: false}}
				disabled={false}
			>
				<TransformComponent wrapperClass={styles.transformWrapper} contentClass={styles.transformContent}>
					{children}
				</TransformComponent>
			</TransformWrapper>
		</div>
	);
});

export const MediaModal: FC<MediaModalProps> = observer(
	({
		title,
		fileName,
		fileSize,
		dimensions,
		expiryInfo,
		isFavorited,
		onFavorite,
		onSave,
		onOpenInBrowser,
		onInfo,
		additionalActions,
		children,
		enablePanZoom = false,
		currentIndex,
		totalAttachments,
		onPrevious,
		onNext,
		thumbnails,
		onSelectThumbnail,
	}) => {
		const {enabled: isMobile} = MobileLayoutStore;
		const modalKey = useRef(Math.random().toString(36).substring(7));
		const prefersReducedMotion = AccessibilityStore.useReducedMotion;
		const [zoomState, setZoomState] = useState<ZoomState>('fit');
		const [viewportPadding, setViewportPadding] = useState(getViewportPadding);
		const handleClose = useCallback(() => {
			MediaViewerActionCreators.closeMediaViewer();
		}, []);

		const handleZoom = useCallback((state: ZoomState) => {
			setZoomState(state);
		}, []);

		useEffect(() => {
			LayerManager.addLayer('modal', modalKey.current, handleClose);
			return () => {
				LayerManager.removeLayer('modal', modalKey.current);
			};
		}, [handleClose]);

		useEffect(() => {
			const originalOverflow = document.body.style.overflow;
			document.body.style.overflow = 'hidden';
			return () => {
				document.body.style.overflow = originalOverflow;
			};
		}, []);

		useEffect(() => {
			const updateViewportPadding = () => setViewportPadding(getViewportPadding());
			updateViewportPadding();

			window.addEventListener('resize', updateViewportPadding);
			return () => window.removeEventListener('resize', updateViewportPadding);
		}, []);

		useEffect(() => {
			const handleKeyDown = (e: KeyboardEvent) => {
				if (e.key === 'Escape') {
					handleClose();
				} else if (e.key === 'ArrowLeft' && onPrevious) {
					e.preventDefault();
					onPrevious();
				} else if (e.key === 'ArrowRight' && onNext) {
					e.preventDefault();
					onNext();
				}
			};
			window.addEventListener('keydown', handleKeyDown);
			return () => window.removeEventListener('keydown', handleKeyDown);
		}, [handleClose, onPrevious, onNext]);

		const contentSizingStyle = useMemo(
			() => ({'--media-content-padding': `${viewportPadding}px`}) as CSSProperties,
			[viewportPadding],
		);

		const hasThumbnailCarousel =
			thumbnails && thumbnails.length > 1 && currentIndex !== undefined && onSelectThumbnail !== undefined;

		const handleThumbnailSelect = useCallback(
			(index: number) => {
				if (!onSelectThumbnail || currentIndex === undefined) return;
				setZoomState('fit');
				onSelectThumbnail(index);
			},
			[onSelectThumbnail, currentIndex],
		);

		const {t} = useLingui();
		const wrappedChildren = useMemo(() => <div className={styles.mediaContainer}>{children}</div>, [children]);

		const mediaContent = enablePanZoom
			? isMobile
				? createElement(MobileMediaViewer, null, wrappedChildren)
				: createElement(DesktopMediaViewer, {
						onClose: handleClose,
						onZoomStateChange: setZoomState,
						zoomState,
						onZoom: handleZoom,
						// biome-ignore lint/correctness/noChildrenProp: Desktop viewer expects children prop to wrap content
						children: wrappedChildren,
					})
			: createElement(
					'div',
					{className: styles.nonZoomMediaContainer},
					createElement('div', {
						className: styles.nonZoomBackdrop,
						role: 'button',
						tabIndex: 0,
						onClick: handleClose,
						onKeyDown: (e: ReactKeyboardEvent<HTMLDivElement>) => {
							if (e.key === 'Enter' || e.key === ' ') {
								e.preventDefault();
								handleClose();
							}
						},
						'aria-label': 'Close media viewer',
					}),
					createElement(
						'div',
						{className: styles.nonZoomContent},
						createElement('div', {className: styles.nonZoomContentInner}, wrappedChildren),
					),
				);

		const modalContent = (
			<AnimatePresence>
				<div className={styles.modalOverlay}>
					<motion.div
						className={styles.modalBackdrop}
						initial={{opacity: 0}}
						animate={{opacity: 1}}
						exit={{opacity: 0}}
						transition={prefersReducedMotion ? {duration: 0.05} : {duration: 0.2}}
						aria-hidden="true"
						onClick={handleClose}
					/>

					<motion.div
						className={styles.modalContent}
						initial={{opacity: 0}}
						animate={{opacity: 1}}
						exit={{opacity: 0}}
						transition={prefersReducedMotion ? {duration: 0.05} : {duration: 0.2}}
						role="dialog"
						aria-modal="true"
						aria-label={title}
					>
						<div className={clsx(styles.modalContentInner, zoomState === 'zoomed' && styles.modalContentInnerZoomed)}>
							<div className={styles.headerBar}>
								{zoomState !== 'zoomed' && (
									<div className={styles.headerMeta}>
										<FileInfo
											fileName={fileName}
											fileSize={fileSize}
											dimensions={dimensions}
											expiryInfo={expiryInfo}
											currentIndex={currentIndex}
											totalAttachments={totalAttachments}
											onPrevious={onPrevious}
											onNext={onNext}
										/>
									</div>
								)}

								<div className={styles.headerControls}>
									<Controls
										isFavorited={isFavorited}
										onFavorite={onFavorite}
										onSave={onSave}
										onOpenInBrowser={onOpenInBrowser}
										onInfo={onInfo}
										onClose={handleClose}
										additionalActions={additionalActions}
										zoomState={zoomState}
										onZoom={enablePanZoom && !isMobile ? handleZoom : undefined}
										enableZoomControls={enablePanZoom && !isMobile}
									/>
								</div>
							</div>

							<div
								className={clsx(styles.mediaArea, zoomState === 'zoomed' && styles.mediaAreaZoomed)}
								style={contentSizingStyle}
							>
								{mediaContent}
							</div>

							{hasThumbnailCarousel && (
								<div className={styles.thumbnailCarousel} role="listbox" aria-label={t`Attachment thumbnails`}>
									{thumbnails?.map((thumb, index) => {
										const isSelected = currentIndex === index;
										const badgeText =
											thumb.type === 'audio'
												? t`Audio`
												: thumb.type === 'video' || thumb.type === 'gifv'
													? t`Video`
													: thumb.type === 'gif'
														? t`GIF`
														: undefined;

										return (
											<FocusRing key={`${thumb.src}-${index}`} offset={-2}>
												<button
													type="button"
													role="option"
													aria-selected={isSelected}
													aria-label={thumb.alt ?? t`Attachment ${index + 1}`}
													className={clsx(styles.thumbnailButton, isSelected && styles.thumbnailButtonSelected)}
													onClick={() => handleThumbnailSelect(index)}
												>
													<div className={styles.thumbnailImageWrapper}>
														{thumb.type === 'video' || thumb.type === 'gifv' ? (
															<video
																className={styles.thumbnailVideo}
																src={thumb.src}
																muted
																playsInline
																preload="metadata"
																aria-label={thumb.alt ?? t`Video preview`}
															/>
														) : thumb.type === 'audio' ? (
															<div className={styles.thumbnailPlaceholder}>{t`Audio`}</div>
														) : (
															<img
																src={thumb.src}
																alt={thumb.alt ?? ''}
																className={styles.thumbnailImage}
																draggable={false}
															/>
														)}
														{badgeText && <span className={styles.thumbnailBadge}>{badgeText}</span>}
													</div>
												</button>
											</FocusRing>
										);
									})}
								</div>
							)}

							{!hasThumbnailCarousel &&
								currentIndex !== undefined &&
								totalAttachments !== undefined &&
								totalAttachments > 1 && (
									<div className={styles.navigationOverlay}>
										<Tooltip text={t`Previous attachment`} position="top">
											<span>
												<ControlButton
													icon={<CaretLeftIcon size={20} weight="bold" />}
													label={t`Previous attachment`}
													onClick={onPrevious ?? (() => {})}
													disabled={currentIndex === 0}
												/>
											</span>
										</Tooltip>

										<span className={styles.navigationText}>{t`${currentIndex + 1} of ${totalAttachments}`}</span>

										<Tooltip text={t`Next attachment`} position="top">
											<span>
												<ControlButton
													icon={<CaretRightIcon size={20} weight="bold" />}
													label={t`Next attachment`}
													onClick={onNext ?? (() => {})}
													disabled={currentIndex === totalAttachments - 1}
												/>
											</span>
										</Tooltip>
									</div>
								)}
						</div>
					</motion.div>
				</div>
			</AnimatePresence>
		);

		return createPortal(modalContent, document.body);
	},
);
