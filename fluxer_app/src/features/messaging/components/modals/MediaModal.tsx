// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {AUDIO_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import styles from '@app/features/messaging/components/modals/MediaModal.module.css';
import {MediaOverlayActions} from '@app/features/messaging/components/modals/media_modal/MediaControls';
import {
	getNearestDefaultRotationDegrees,
	isDefaultRotationDegrees,
	isSidewaysRotationDegrees,
	rotateAnticlockwiseDegrees,
	rotateClockwiseDegrees,
} from '@app/features/messaging/components/modals/media_modal/MediaRotationMath';
import {
	DesktopMediaViewer,
	MobileMediaViewer,
} from '@app/features/messaging/components/modals/media_modal/MediaViewers';
import {MobileMediaActions} from '@app/features/messaging/components/modals/media_modal/MobileMediaActions';
import type {PanZoomSurfaceHandle} from '@app/features/messaging/components/modals/media_modal/pan_zoom/PanZoomSurface';
import type {PanZoomTransformSnapshot} from '@app/features/messaging/components/modals/media_modal/pan_zoom/usePanZoomSurface';
import {
	ATTACHMENT_DESCRIPTOR,
	ATTACHMENT_THUMBNAILS_DESCRIPTOR,
	CLOSE_MEDIA_VIEWER_DESCRIPTOR,
	clamp,
	getNativeTitlebarHeight,
	getViewportPadding,
	type MediaModalProps,
	type MediaThumbnail,
	NEXT_ATTACHMENT_2_DESCRIPTOR,
	PREVIOUS_ATTACHMENT_2_DESCRIPTOR,
	VIDEO_PREVIEW_DESCRIPTOR,
	type ZoomState,
} from '@app/features/messaging/components/modals/media_modal/shared';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as MediaViewerCommands from '@app/features/ui/commands/MediaViewerCommands';
import {Scroller} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import LayerManager from '@app/features/ui/state/LayerManager';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import OverlayStack from '@app/features/ui/state/OverlayStack';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {MobileVideoViewer} from '@app/features/voice/components/modals/MobileVideoViewer';
import {useLingui} from '@lingui/react/macro';
import {CaretLeftIcon, CaretRightIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import type {CSSProperties, FC, KeyboardEvent as ReactKeyboardEvent} from 'react';
import {createElement, useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState} from 'react';

export const MediaModal: FC<MediaModalProps> = observer(
	({
		title,
		fileName,
		fileSize,
		dimensions,
		isFavorited,
		onFavorite,
		onDownload,
		onOpenInBrowser,
		onCopyLink,
		onCopyMedia,
		onReply,
		onForward,
		children,
		enablePanZoom = false,
		currentIndex,
		totalAttachments,
		onPrevious,
		onNext,
		thumbnails,
		onSelectThumbnail,
		videoSrc,
		initialTime,
		mediaType,
		onMenuOpen,
	}: MediaModalProps) => {
		const {enabled: isMobile} = MobileLayout;
		const modalKey = useRef(Math.random().toString(36).substring(7));
		const prefersReducedMotion = Accessibility.useReducedMotion;
		const [zoomState, setZoomState] = useState<ZoomState>('fit');
		const [rotation, setRotation] = useState(0);
		const [panZoomInfo, setPanZoomInfo] = useState({zoomPercent: 100, isDefault: true});
		const [isHudHovered, setIsHudHovered] = useState(false);
		const [viewportPadding, setViewportPadding] = useState(getViewportPadding);
		const [nativeTitlebarHeight, setNativeTitlebarHeight] = useState(getNativeTitlebarHeight);
		const panZoomHandleRef = useRef<PanZoomSurfaceHandle>(null);
		const topActionBarRef = useRef<HTMLDivElement>(null);
		const bottomActionBarRef = useRef<HTMLDivElement>(null);
		const bottomInfoBarRef = useRef<HTMLDivElement>(null);
		const thumbnailCarouselRef = useRef<HTMLDivElement>(null);
		const latestIndexRef = useRef(currentIndex ?? 0);
		const transformFrameRef = useRef<number | null>(null);
		const pendingPanZoomInfoRef = useRef(panZoomInfo);
		const [topOverlayHeight, setTopOverlayHeight] = useState(0);
		const [bottomOverlayHeight, setBottomOverlayHeight] = useState(0);
		const measureOverlayHeights = useCallback(() => {
			const nextTopOverlayHeight = Math.ceil(
				Math.max(
					thumbnailCarouselRef.current?.getBoundingClientRect().height ?? 0,
					topActionBarRef.current?.getBoundingClientRect().height ?? 0,
				),
			);
			const nextBottomOverlayHeight = Math.ceil(
				Math.max(
					bottomActionBarRef.current?.getBoundingClientRect().height ?? 0,
					bottomInfoBarRef.current?.getBoundingClientRect().height ?? 0,
				),
			);
			setTopOverlayHeight((previousHeight) =>
				previousHeight === nextTopOverlayHeight ? previousHeight : nextTopOverlayHeight,
			);
			setBottomOverlayHeight((previousHeight) =>
				previousHeight === nextBottomOverlayHeight ? previousHeight : nextBottomOverlayHeight,
			);
		}, []);
		const handleClose = useCallback(() => {
			MediaViewerCommands.closeMediaViewer();
		}, []);
		const handleZoomStateChange = useCallback((state: ZoomState) => {
			setZoomState((previousState) => (previousState === state ? previousState : state));
		}, []);
		const handleTransformChange = useCallback((snapshot: PanZoomTransformSnapshot) => {
			const nextInfo = {
				zoomPercent: Math.round(snapshot.scale * 100),
				isDefault: snapshot.isDefault,
			};
			const pendingInfo = pendingPanZoomInfoRef.current;
			if (pendingInfo.zoomPercent === nextInfo.zoomPercent && pendingInfo.isDefault === nextInfo.isDefault) {
				return;
			}
			pendingPanZoomInfoRef.current = nextInfo;
			if (transformFrameRef.current != null) {
				return;
			}
			transformFrameRef.current = window.requestAnimationFrame(() => {
				transformFrameRef.current = null;
				setPanZoomInfo((previousInfo) => {
					const latestInfo = pendingPanZoomInfoRef.current;
					if (previousInfo.zoomPercent === latestInfo.zoomPercent && previousInfo.isDefault === latestInfo.isDefault) {
						return previousInfo;
					}
					return latestInfo;
				});
			});
		}, []);
		const handleResetMedia = useCallback(() => {
			panZoomHandleRef.current?.reset();
			setRotation(getNearestDefaultRotationDegrees);
		}, []);
		const handleZoomIn = useCallback(() => {
			panZoomHandleRef.current?.zoomIn();
		}, []);
		const handleZoomOut = useCallback(() => {
			panZoomHandleRef.current?.zoomOut();
		}, []);
		const handleRotateCW = useCallback(() => {
			setRotation(rotateClockwiseDegrees);
		}, []);
		const handleRotateCCW = useCallback(() => {
			setRotation(rotateAnticlockwiseDegrees);
		}, []);
		const handleHudPointerEnter = useCallback(() => {
			setIsHudHovered(true);
		}, []);
		const handleHudPointerLeave = useCallback(() => {
			setIsHudHovered(false);
		}, []);
		useEffect(() => {
			pendingPanZoomInfoRef.current = panZoomInfo;
		}, [panZoomInfo]);
		useEffect(() => {
			return () => {
				if (transformFrameRef.current != null) {
					window.cancelAnimationFrame(transformFrameRef.current);
				}
			};
		}, []);
		useLayoutEffect(() => {
			if (currentIndex !== undefined) {
				latestIndexRef.current = currentIndex;
				const defaultInfo = {zoomPercent: 100, isDefault: true};
				pendingPanZoomInfoRef.current = defaultInfo;
				setPanZoomInfo(defaultInfo);
				setRotation(0);
			}
		}, [currentIndex]);
		useEffect(() => {
			LayerManager.addLayer('modal', modalKey.current, handleClose);
			return () => {
				LayerManager.removeLayer('modal', modalKey.current);
			};
		}, [handleClose]);
		useEffect(() => OverlayStack.enableAboveOverlayBase(), []);
		useEffect(() => {
			const originalOverflow = document.body.style.overflow;
			document.body.style.overflow = 'hidden';
			return () => {
				document.body.style.overflow = originalOverflow;
			};
		}, []);
		useEffect(() => {
			const updateViewportInsets = () => {
				setViewportPadding(getViewportPadding());
				setNativeTitlebarHeight(getNativeTitlebarHeight());
			};
			updateViewportInsets();
			window.addEventListener('resize', updateViewportInsets);
			return () => window.removeEventListener('resize', updateViewportInsets);
		}, []);
		const thumbnailCount = thumbnails?.length ?? 0;
		const thumbnailButtonRefs = useRef<Array<HTMLButtonElement | null>>([]);
		const [rovingThumbnailIndex, setRovingThumbnailIndex] = useState<number>(() => currentIndex ?? 0);
		useEffect(() => {
			const handleKeyDown = (e: KeyboardEvent) => {
				if (e.defaultPrevented) return;
				if (e.key === 'Escape') {
					handleClose();
					return;
				}
				const target = e.target as HTMLElement | null;
				if (target && (target.tagName === 'INPUT' || target.tagName === 'TEXTAREA' || target.isContentEditable)) {
					return;
				}
				if ((e.key === 'ArrowLeft' || e.key === 'ArrowRight') && currentIndex !== undefined && onSelectThumbnail) {
					const count = thumbnails?.length ?? 0;
					if (count > 1) {
						e.preventDefault();
						const delta = e.key === 'ArrowRight' ? 1 : -1;
						const latest = latestIndexRef.current;
						const nextIndex = (latest + delta + count) % count;
						latestIndexRef.current = nextIndex;
						setZoomState('fit');
						setRovingThumbnailIndex(nextIndex);
						onSelectThumbnail(nextIndex);
						return;
					}
				}
				if (e.key === 'ArrowLeft' && onPrevious) {
					e.preventDefault();
					onPrevious();
					return;
				}
				if (e.key === 'ArrowRight' && onNext) {
					e.preventDefault();
					onNext();
					return;
				}
			};
			window.addEventListener('keydown', handleKeyDown);
			return () => window.removeEventListener('keydown', handleKeyDown);
		}, [handleClose, onPrevious, onNext, onSelectThumbnail, thumbnails, currentIndex]);
		const hasThumbnailCarousel =
			thumbnails && thumbnails.length > 1 && currentIndex !== undefined && onSelectThumbnail !== undefined;
		const isMobileVideo = Boolean(isMobile && mediaType === 'video' && videoSrc);
		const contentSizingStyle = useMemo(() => {
			const minimumTopOverlayHeight = isMobile && !isMobileVideo ? 46 : hasThumbnailCarousel ? 54 : 0;
			const minimumBottomOverlayHeight = isMobile ? 0 : 44;
			const hasSideNavButtons =
				currentIndex !== undefined && totalAttachments !== undefined && totalAttachments > 1 && !isMobile;
			const navButtonInset = 8 + 48 + 8;
			const sideOverlayWidth = hasSideNavButtons ? Math.max(0, navButtonInset - viewportPadding) : 0;
			const hudInlineOffset = isMobile ? 12 : Math.round(clamp(viewportPadding * 0.24, 10, 16));
			const hudTopOffset = isMobile ? 12 : Math.round(clamp(viewportPadding * 0.22, 10, 16)) + nativeTitlebarHeight;
			const hudBottomOffset = isMobile ? 12 : Math.round(clamp(viewportPadding * 0.28, 12, 22));
			return {
				'--media-content-padding': `${viewportPadding}px`,
				'--media-hud-inline-offset': `${hudInlineOffset}px`,
				'--media-hud-top-offset': `${hudTopOffset}px`,
				'--media-hud-bottom-offset': `${hudBottomOffset}px`,
				'--media-top-overlay-height': `${Math.max(topOverlayHeight, minimumTopOverlayHeight)}px`,
				'--media-bottom-overlay-height': `${Math.max(bottomOverlayHeight, minimumBottomOverlayHeight)}px`,
				'--media-overlay-gap': isMobile ? '8px' : '12px',
				'--media-side-overlay-width': `${sideOverlayWidth}px`,
			} as CSSProperties;
		}, [
			viewportPadding,
			nativeTitlebarHeight,
			topOverlayHeight,
			bottomOverlayHeight,
			isMobile,
			currentIndex,
			totalAttachments,
			hasThumbnailCarousel,
			isMobileVideo,
		]);
		useLayoutEffect(() => {
			measureOverlayHeights();
			const observer = new ResizeObserver(() => {
				measureOverlayHeights();
			});
			if (topActionBarRef.current) observer.observe(topActionBarRef.current);
			if (thumbnailCarouselRef.current) observer.observe(thumbnailCarouselRef.current);
			if (bottomActionBarRef.current) observer.observe(bottomActionBarRef.current);
			if (bottomInfoBarRef.current) observer.observe(bottomInfoBarRef.current);
			return () => observer.disconnect();
		}, [measureOverlayHeights, hasThumbnailCarousel, currentIndex, totalAttachments, isMobile, isMobileVideo]);
		const shouldHideHud = enablePanZoom && !panZoomInfo.isDefault && !isHudHovered;
		useEffect(() => {
			thumbnailButtonRefs.current = thumbnailButtonRefs.current.slice(0, thumbnailCount);
		}, [thumbnailCount]);
		useEffect(() => {
			if (!thumbnailCount && rovingThumbnailIndex !== 0) {
				setRovingThumbnailIndex(0);
				return;
			}
			if (thumbnailCount && rovingThumbnailIndex >= thumbnailCount) {
				setRovingThumbnailIndex(thumbnailCount - 1);
			}
		}, [thumbnailCount, rovingThumbnailIndex]);
		useEffect(() => {
			if (currentIndex !== undefined && currentIndex !== rovingThumbnailIndex) {
				setRovingThumbnailIndex(currentIndex);
			}
		}, [currentIndex, rovingThumbnailIndex]);
		const handleThumbnailSelect = useCallback(
			(index: number) => {
				if (!onSelectThumbnail || currentIndex === undefined) return;
				setZoomState('fit');
				setRovingThumbnailIndex(index);
				onSelectThumbnail(index);
			},
			[onSelectThumbnail, currentIndex],
		);
		const focusThumbnailButton = useCallback((index: number) => {
			const button = thumbnailButtonRefs.current[index];
			button?.focus();
		}, []);
		const handleThumbnailKeyDown = useCallback(
			(e: ReactKeyboardEvent<HTMLElement>) => {
				if (!thumbnailCount) return;
				let nextIndex = rovingThumbnailIndex;
				if (e.key === 'ArrowRight' || e.key === 'ArrowDown') {
					nextIndex = (rovingThumbnailIndex + 1) % thumbnailCount;
				} else if (e.key === 'ArrowLeft' || e.key === 'ArrowUp') {
					nextIndex = (rovingThumbnailIndex - 1 + thumbnailCount) % thumbnailCount;
				} else if (e.key === 'Home') {
					nextIndex = 0;
				} else if (e.key === 'End') {
					nextIndex = thumbnailCount - 1;
				} else {
					return;
				}
				if (nextIndex === rovingThumbnailIndex) return;
				e.preventDefault();
				e.stopPropagation();
				handleThumbnailSelect(nextIndex);
				focusThumbnailButton(nextIndex);
			},
			[focusThumbnailButton, handleThumbnailSelect, rovingThumbnailIndex, thumbnailCount],
		);
		const {i18n} = useLingui();
		const isRotationSwapped = isSidewaysRotationDegrees(rotation);
		const hasCustomRotation = !isDefaultRotationDegrees(rotation);
		const wrappedChildren = useMemo(
			() => (
				<div
					className={styles.mediaContainer}
					style={{'--media-rotation': `${rotation}deg`} as CSSProperties}
					data-media-rotation-active={hasCustomRotation ? 'true' : undefined}
					data-rotation-swapped={isRotationSwapped ? 'true' : undefined}
					data-flx="messaging.media-modal.wrapped-children.media-container"
				>
					{children}
				</div>
			),
			[children, rotation, hasCustomRotation, isRotationSwapped],
		);
		const mediaContent = isMobileVideo
			? createElement(MobileVideoViewer, {
					src: videoSrc ?? '',
					initialTime,
					loop: true,
					onClose: handleClose,
					onMenuOpen,
				})
			: enablePanZoom
				? isMobile
					? createElement(MobileMediaViewer, {
							ref: panZoomHandleRef,
							zoomState,
							onZoomStateChange: handleZoomStateChange,
							onTransformChange: handleTransformChange,
							resetKey: currentIndex,
							children: wrappedChildren,
						})
					: createElement(DesktopMediaViewer, {
							ref: panZoomHandleRef,
							onClose: handleClose,
							onZoomStateChange: handleZoomStateChange,
							onTransformChange: handleTransformChange,
							resetKey: currentIndex,
							zoomState,
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
								if (isKeyboardActivationKey(e.key)) {
									e.preventDefault();
									handleClose();
								}
							},
							'aria-label': i18n._(CLOSE_MEDIA_VIEWER_DESCRIPTOR),
						}),
						createElement(
							'div',
							{className: styles.nonZoomContent},
							createElement('div', {className: styles.nonZoomContentInner}, wrappedChildren),
						),
					);
		const modalContent = (
			<AnimatePresence data-flx="messaging.media-modal.animate-presence">
				<div className={styles.modalOverlay} data-flx="messaging.media-modal.modal-overlay">
					<motion.div
						className={styles.modalBackdrop}
						initial={{opacity: 0}}
						animate={{opacity: 1}}
						exit={{opacity: 0}}
						transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2}}
						aria-hidden="true"
						onClick={handleClose}
						data-flx="messaging.media-modal.modal-backdrop.close"
					/>
					<motion.div
						className={styles.modalContent}
						initial={{opacity: 0}}
						animate={{opacity: 1}}
						exit={{opacity: 0}}
						transition={prefersReducedMotion ? {duration: 0} : {duration: 0.2}}
						role="dialog"
						aria-modal="true"
						aria-label={title}
						data-flx="messaging.media-modal.modal-content"
					>
						<div
							className={styles.modalContentInner}
							style={contentSizingStyle}
							data-zoom-state={zoomState}
							data-media-hud-hidden={shouldHideHud ? 'true' : undefined}
							data-flx="messaging.media-modal.modal-content-inner"
						>
							<div className={styles.mediaArea} data-flx="messaging.media-modal.media-area">
								{mediaContent}
							</div>
							{currentIndex !== undefined && totalAttachments !== undefined && totalAttachments > 1 && !isMobile && (
								<>
									<div
										className={styles.floatingNavButtonLeft}
										onPointerEnter={handleHudPointerEnter}
										onPointerLeave={handleHudPointerLeave}
										data-flx="messaging.media-modal.floating-nav-button-left"
									>
										<Tooltip
											text={i18n._(PREVIOUS_ATTACHMENT_2_DESCRIPTOR)}
											position="right"
											data-flx="messaging.media-modal.tooltip"
										>
											<span data-flx="messaging.media-modal.span">
												<FocusRing offset={-2} data-flx="messaging.media-modal.focus-ring--2">
													<button
														type="button"
														className={styles.floatingNavButton}
														onClick={onPrevious ?? (() => {})}
														aria-label={i18n._(PREVIOUS_ATTACHMENT_2_DESCRIPTOR)}
														data-flx="messaging.media-modal.floating-nav-button"
													>
														<CaretLeftIcon
															size={remFromPx(24)}
															weight="bold"
															data-flx="messaging.media-modal.caret-left-icon"
														/>
													</button>
												</FocusRing>
											</span>
										</Tooltip>
									</div>
									<div
										className={styles.floatingNavButtonRight}
										onPointerEnter={handleHudPointerEnter}
										onPointerLeave={handleHudPointerLeave}
										data-flx="messaging.media-modal.floating-nav-button-right"
									>
										<Tooltip
											text={i18n._(NEXT_ATTACHMENT_2_DESCRIPTOR)}
											position="left"
											data-flx="messaging.media-modal.tooltip--2"
										>
											<span data-flx="messaging.media-modal.span--2">
												<FocusRing offset={-2} data-flx="messaging.media-modal.focus-ring--3">
													<button
														type="button"
														className={styles.floatingNavButton}
														onClick={onNext ?? (() => {})}
														aria-label={i18n._(NEXT_ATTACHMENT_2_DESCRIPTOR)}
														data-flx="messaging.media-modal.floating-nav-button--2"
													>
														<CaretRightIcon
															size={remFromPx(24)}
															weight="bold"
															data-flx="messaging.media-modal.caret-right-icon"
														/>
													</button>
												</FocusRing>
											</span>
										</Tooltip>
									</div>
								</>
							)}
							{hasThumbnailCarousel && (
								<div
									ref={thumbnailCarouselRef}
									className={styles.thumbnailCarouselWrapper}
									onPointerEnter={handleHudPointerEnter}
									onPointerLeave={handleHudPointerLeave}
									data-flx="messaging.media-modal.thumbnail-carousel-wrapper"
								>
									<Scroller
										className={styles.thumbnailCarouselScroller}
										orientation="horizontal"
										overflow="auto"
										fade={false}
										key="media-modal-thumbnail-carousel-scroller"
										role="listbox"
										aria-label={i18n._(ATTACHMENT_THUMBNAILS_DESCRIPTOR)}
										onKeyDown={handleThumbnailKeyDown}
										data-flx="messaging.media-modal.thumbnail-carousel-scroller.thumbnail-key-down"
									>
										<div className={styles.thumbnailCarousel} data-flx="messaging.media-modal.thumbnail-carousel">
											{thumbnails?.map((thumb: MediaThumbnail, index: number) => {
												const isSelected = currentIndex === index;
												const isRovingTarget = rovingThumbnailIndex === index;
												const isFirstThumbnail = index === 0;
												const isLastThumbnail = index === thumbnailCount - 1;
												return (
													<FocusRing
														key={`${thumb.src}-${index}`}
														offset={-2}
														data-flx="messaging.media-modal.focus-ring--4"
													>
														<button
															ref={(el) => {
																thumbnailButtonRefs.current[index] = el;
															}}
															type="button"
															role="option"
															aria-selected={isSelected}
															aria-label={thumb.alt ?? i18n._(ATTACHMENT_DESCRIPTOR, {index1: index + 1})}
															className={clsx(styles.thumbnailButton, isSelected && styles.thumbnailButtonSelected)}
															tabIndex={isRovingTarget ? 0 : -1}
															onClick={() => handleThumbnailSelect(index)}
															onKeyDown={handleThumbnailKeyDown}
															data-flx="messaging.media-modal.thumbnail-button.thumbnail-select"
														>
															<div
																className={clsx(
																	styles.thumbnailImageWrapper,
																	isFirstThumbnail && styles.thumbnailImageWrapperFirst,
																	isLastThumbnail && styles.thumbnailImageWrapperLast,
																)}
																data-flx="messaging.media-modal.thumbnail-image-wrapper"
															>
																{thumb.type === 'video' || thumb.type === 'gifv' ? (
																	<video
																		className={styles.thumbnailVideo}
																		src={thumb.src}
																		muted
																		playsInline
																		preload="metadata"
																		aria-label={thumb.alt ?? i18n._(VIDEO_PREVIEW_DESCRIPTOR)}
																		data-flx="messaging.media-modal.thumbnail-video"
																	/>
																) : thumb.type === 'audio' ? (
																	<div
																		className={styles.thumbnailPlaceholder}
																		data-flx="messaging.media-modal.thumbnail-placeholder"
																	>
																		{i18n._(AUDIO_DESCRIPTOR)}
																	</div>
																) : (
																	<img
																		src={thumb.src}
																		alt={thumb.alt ?? ''}
																		className={styles.thumbnailImage}
																		draggable={false}
																		data-flx="messaging.media-modal.thumbnail-image"
																	/>
																)}
															</div>
														</button>
													</FocusRing>
												);
											})}
										</div>
									</Scroller>
								</div>
							)}
							{isMobile && !isMobileVideo ? (
								<MobileMediaActions
									rootRef={topActionBarRef}
									isFavorited={isFavorited}
									onFavorite={onFavorite}
									onDownload={onDownload}
									onOpenInBrowser={onOpenInBrowser}
									onCopyLink={onCopyLink}
									onCopyMedia={onCopyMedia}
									onReset={handleResetMedia}
									onZoomIn={handleZoomIn}
									onZoomOut={handleZoomOut}
									onRotateCW={handleRotateCW}
									onRotateCCW={handleRotateCCW}
									onReply={onReply}
									onForward={onForward}
									onClose={handleClose}
									canReset={!panZoomInfo.isDefault || hasCustomRotation}
									enableZoomControls={enablePanZoom}
									onPointerEnter={handleHudPointerEnter}
									onPointerLeave={handleHudPointerLeave}
									data-flx="messaging.media-modal.mobile-media-actions"
								/>
							) : !isMobile ? (
								<MediaOverlayActions
									rootRef={bottomActionBarRef}
									isFavorited={isFavorited}
									onFavorite={onFavorite}
									onDownload={onDownload}
									onOpenInBrowser={onOpenInBrowser}
									onCopyLink={onCopyLink}
									onCopyMedia={onCopyMedia}
									onReset={handleResetMedia}
									onZoomIn={handleZoomIn}
									onZoomOut={handleZoomOut}
									onRotateCW={handleRotateCW}
									onRotateCCW={handleRotateCCW}
									onReply={onReply}
									onForward={onForward}
									onClose={handleClose}
									canReset={!panZoomInfo.isDefault || hasCustomRotation}
									enableZoomControls={enablePanZoom}
									onPointerEnter={handleHudPointerEnter}
									onPointerLeave={handleHudPointerLeave}
									data-flx="messaging.media-modal.media-overlay-actions"
								/>
							) : null}
							{!isMobile && (
								<div
									ref={bottomInfoBarRef}
									className={styles.bottomInfoBar}
									onPointerEnter={handleHudPointerEnter}
									onPointerLeave={handleHudPointerLeave}
									data-flx="messaging.media-modal.bottom-info-bar"
								>
									{[fileName, dimensions, fileSize, `${panZoomInfo.zoomPercent}%`]
										.filter(Boolean)
										.map((part, index) => (
											<span
												key={`${part}-${index}`}
												className={styles.bottomInfoItem}
												data-flx="messaging.media-modal.bottom-info-item"
											>
												{part}
											</span>
										))}
								</div>
							)}
						</div>
					</motion.div>
				</div>
			</AnimatePresence>
		);
		return modalContent;
	},
);
