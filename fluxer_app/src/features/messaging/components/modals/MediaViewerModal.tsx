// SPDX-License-Identifier: AGPL-3.0-or-later

import {useBottomSheetBackHandler} from '@app/features/app/hooks/useBottomSheetBackHandler';
import {deriveDefaultNameFromMessage} from '@app/features/channel/components/embeds/EmbedUtils';
import {useMessageActionMenuData} from '@app/features/channel/components/MessageActionMenu';
import {
	getMessagePermissions,
	requestMessageForward,
	requestMessageReply,
} from '@app/features/channel/components/MessageActionUtils';
import type {Channel} from '@app/features/channel/models/Channel';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import type {ForwardModalSuccess} from '@app/features/messaging/components/modals/ForwardModal';
import {MediaModal} from '@app/features/messaging/components/modals/MediaModal';
import styles from '@app/features/messaging/components/modals/MediaViewerModal.module.css';
import {getMediaViewerPortalRoot} from '@app/features/messaging/components/modals/MediaViewerPortal';
import {useMediaFavorite} from '@app/features/messaging/hooks/useMediaFavorite';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {formatAttachmentDate} from '@app/features/messaging/utils/AttachmentExpiryUtils';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import {createDownloadHandler} from '@app/features/messaging/utils/FileDownloadUtils';
import {formatFileSize} from '@app/features/messaging/utils/FileUtils';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {
	buildMediaProxyURL,
	resolvePreferredImageFormat,
	stripMediaProxyParams,
} from '@app/features/messaging/utils/MediaProxyUtils';
import {
	copyMediaLinkToClipboard,
	copyMediaToClipboard,
	useMediaMenuData,
} from '@app/features/ui/action_menu/items/MediaMenuData';
import {MediaContextMenu} from '@app/features/ui/action_menu/MediaContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as MediaViewerCommands from '@app/features/ui/commands/MediaViewerCommands';
import {Spinner} from '@app/features/ui/components/Spinner';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {PortalHostContext} from '@app/features/ui/overlay/PortalHostContext';
import LayerManager from '@app/features/ui/state/LayerManager';
import MediaViewer, {type MediaViewerItem} from '@app/features/ui/state/MediaViewer';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {getNativePlatformSync, isNativeWindows} from '@app/features/ui/utils/NativeUtils';
import {AudioPlayer} from '@app/features/voice/components/media_player/components/AudioPlayer';
import {VideoPlayer} from '@app/features/voice/components/media_player/components/VideoPlayer';
import {resolveDevicePixelRatio} from '@app/features/voice/DevicePixelRatio';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {untracked} from 'mobx';
import {observer} from 'mobx-react-lite';
import {
	type CSSProperties,
	type FC,
	type ImgHTMLAttributes,
	type MouseEvent,
	type SyntheticEvent,
	useCallback,
	useEffect,
	useLayoutEffect,
	useMemo,
	useRef,
	useState,
} from 'react';
import {createPortal} from 'react-dom';

const MEDIA_OPTIONS_DESCRIPTOR = msg({
	message: 'Media options',
	comment: 'Accessible label for the overflow menu button in the media viewer modal.',
});
const ATTACHMENT_DESCRIPTOR = msg({
	message: 'Attachment {index1}',
	comment: 'Accessible label for the active attachment in the media viewer. index1 is the 1-based index.',
});
const ANIMATED_IMAGE_PREVIEW_DESCRIPTOR = msg({
	message: 'Animated image preview',
	comment: 'Accessible label for an animated image (APNG/AVIF) shown in the media viewer.',
});
const IMAGE_PREVIEW_DESCRIPTOR = msg({
	message: 'Image preview',
	comment: 'Accessible label for a static image shown in the media viewer.',
});
const GIF_PREVIEW_DESCRIPTOR = msg({
	message: 'GIF preview',
	comment: 'Accessible label for an animated GIF shown in the media viewer.',
});
const VIDEO_PREVIEW_DESCRIPTOR = msg({
	message: 'Video preview',
	comment: 'Accessible label for a video shown in the media viewer.',
});
const AUDIO_PREVIEW_DESCRIPTOR = msg({
	message: 'Audio preview',
	comment: 'Accessible label for an audio clip shown in the media viewer.',
});
const MEDIA_PREVIEW_DESCRIPTOR = msg({
	message: 'Media preview',
	comment: 'Generic accessible label for unknown media in the media viewer.',
});
const ANIMATED_GIF_DESCRIPTOR = msg({
	message: 'Animated GIF',
	comment: 'Media type chip shown in the media viewer info panel for animated GIFs.',
});
const ANIMATED_VIDEO_DESCRIPTOR = msg({
	message: 'Animated video',
	comment: 'Media type chip shown in the media viewer info panel for short looping video clips.',
});
const ANIMATED_IMAGE_DESCRIPTOR = msg({
	message: 'Animated image',
	comment: 'Media type chip shown in the media viewer info panel for animated images.',
});
const IMAGE_DESCRIPTOR = msg({
	message: 'Image',
	comment: 'Media type chip shown in the media viewer info panel for static images.',
});

interface MobileMediaOptionsSheetProps {
	currentItem: MediaViewerItem;
	defaultName: string;
	isOpen: boolean;
	message: Message;
	onClose: () => void;
	onDelete: (bypassConfirm?: boolean) => void;
	sourceChannel?: Channel | null;
}

function getBaseProxyURL(src: string): string {
	if (src.startsWith('blob:')) {
		return src;
	}
	return stripMediaProxyParams(src);
}

function normalizeContentType(contentType?: string): string | undefined {
	return contentType?.toLowerCase().split(';')[0]?.trim() || undefined;
}

function inferImageContentTypeFromURL(src: string): string | undefined {
	try {
		const path = new URL(src).pathname.toLowerCase();
		if (path.endsWith('.png')) return 'image/png';
		if (path.endsWith('.jpg') || path.endsWith('.jpeg')) return 'image/jpeg';
		if (path.endsWith('.webp')) return 'image/webp';
		if (path.endsWith('.gif')) return 'image/gif';
		if (path.endsWith('.svg')) return 'image/svg+xml';
		if (path.endsWith('.avif')) return 'image/avif';
		if (path.endsWith('.jxl')) return 'image/jxl';
	} catch {
		return undefined;
	}
	return undefined;
}

function resolveViewerStaticImageFormat(contentType: string | undefined, src: string): 'webp' | undefined {
	const normalizedContentType = normalizeContentType(contentType) ?? inferImageContentTypeFromURL(src);
	switch (normalizedContentType) {
		case 'image/png':
		case 'image/jpeg':
		case 'image/webp':
		case 'image/gif':
		case 'image/svg+xml':
			return undefined;
		default:
			return resolvePreferredImageFormat(normalizedContentType);
	}
}

function buildViewerStaticImageURL(item: MediaViewerItem): string {
	if (item.src.startsWith('blob:')) {
		return item.src;
	}
	const baseProxyURL = getBaseProxyURL(item.src);
	return buildMediaProxyURL(baseProxyURL, {
		format: resolveViewerStaticImageFormat(item.contentType, baseProxyURL),
	});
}

const SHARP_CANVAS_MAX_SIDE = 8192;
const SHARP_CANVAS_MAX_PIXELS = 32_000_000;
const USE_SHARP_CANVAS_FOR_PLATFORM = isNativeWindows(getNativePlatformSync());
const MEDIA_VIEWER_ANIMATION_SESSION_PARAM = 'flx_viewer_session';

type RenderReadyImageProps = ImgHTMLAttributes<HTMLImageElement> & {
	sharpenWhenZoomed?: boolean;
};

function shouldUseSharpCanvasForItem(item: MediaViewerItem, src: string): boolean {
	const contentType = normalizeContentType(item.contentType) ?? inferImageContentTypeFromURL(src);
	return USE_SHARP_CANVAS_FOR_PLATFORM && item.type === 'image' && !item.animated && contentType !== 'image/svg+xml';
}

function appendMediaViewerAnimationSession(src: string, sessionId: number): string {
	if (!src || src.startsWith('blob:')) {
		return src;
	}
	try {
		const url = new URL(src);
		url.searchParams.set(MEDIA_VIEWER_ANIMATION_SESSION_PARAM, sessionId.toString());
		return url.toString();
	} catch {
		return src;
	}
}

function getSharpCanvasSize(width: number, height: number, dpr: number): {height: number; width: number} | null {
	const canvasWidth = Math.max(1, Math.round(width * dpr));
	const canvasHeight = Math.max(1, Math.round(height * dpr));
	if (
		canvasWidth > SHARP_CANVAS_MAX_SIDE ||
		canvasHeight > SHARP_CANVAS_MAX_SIDE ||
		canvasWidth * canvasHeight > SHARP_CANVAS_MAX_PIXELS
	) {
		return null;
	}
	return {height: canvasHeight, width: canvasWidth};
}

const RenderReadyImage: FC<RenderReadyImageProps> = ({
	className,
	onError,
	onLoad,
	sharpenWhenZoomed = false,
	src,
	...imageProps
}: RenderReadyImageProps) => {
	const [isReady, setIsReady] = useState(false);
	const [canvasPortalTarget, setCanvasPortalTarget] = useState<HTMLElement | null>(null);
	const [isSharpCanvasActive, setIsSharpCanvasActive] = useState(false);
	const [isSharpCanvasVisible, setIsSharpCanvasVisible] = useState(false);
	const imageRef = useRef<HTMLImageElement | null>(null);
	const canvasRef = useRef<HTMLCanvasElement | null>(null);
	const readyTokenRef = useRef(0);
	const markReady = useCallback((image: HTMLImageElement) => {
		const token = ++readyTokenRef.current;
		const decodePromise =
			typeof image.decode === 'function' ? image.decode().catch(() => undefined) : Promise.resolve();
		void decodePromise.then(() => {
			if (readyTokenRef.current !== token) return;
			const ownerWindow = image.ownerDocument.defaultView;
			if (!ownerWindow) {
				setIsReady(true);
				return;
			}
			ownerWindow.requestAnimationFrame(() => {
				if (readyTokenRef.current === token) {
					setIsReady(true);
				}
			});
		});
	}, []);
	const handleLoad = useCallback(
		(event: SyntheticEvent<HTMLImageElement>) => {
			onLoad?.(event);
			markReady(event.currentTarget);
		},
		[markReady, onLoad],
	);
	const handleError = useCallback(
		(event: SyntheticEvent<HTMLImageElement>) => {
			onError?.(event);
			setIsReady(true);
		},
		[onError],
	);
	useEffect(() => {
		readyTokenRef.current += 1;
		setIsReady(false);
		const image = imageRef.current;
		if (image?.complete && image.naturalWidth > 0) {
			markReady(image);
		}
		return () => {
			readyTokenRef.current += 1;
		};
	}, [markReady, src]);
	const drawSharpCanvas = useCallback((): boolean => {
		const image = imageRef.current;
		const canvas = canvasRef.current;
		if (!image || !canvas || !image.complete || image.naturalWidth <= 0 || image.naturalHeight <= 0) return false;
		const ownerWindow = image.ownerDocument.defaultView;
		if (!ownerWindow) return false;
		const imageRect = image.getBoundingClientRect();
		const clipElement = image.closest('[data-flx*="pan-zoom-surface"]') as HTMLElement | null;
		const clipRect = clipElement?.getBoundingClientRect();
		const visibleLeft = Math.max(imageRect.left, clipRect?.left ?? imageRect.left);
		const visibleTop = Math.max(imageRect.top, clipRect?.top ?? imageRect.top);
		const visibleRight = Math.min(imageRect.right, clipRect?.right ?? imageRect.right);
		const visibleBottom = Math.min(imageRect.bottom, clipRect?.bottom ?? imageRect.bottom);
		const visibleWidth = visibleRight - visibleLeft;
		const visibleHeight = visibleBottom - visibleTop;
		if (imageRect.width <= 0 || imageRect.height <= 0 || visibleWidth <= 0 || visibleHeight <= 0) return false;
		const canvasSize = getSharpCanvasSize(visibleWidth, visibleHeight, resolveDevicePixelRatio(ownerWindow));
		if (!canvasSize) return false;
		canvas.style.left = `${visibleLeft}px`;
		canvas.style.top = `${visibleTop}px`;
		canvas.style.width = `${visibleWidth}px`;
		canvas.style.height = `${visibleHeight}px`;
		if (canvas.width !== canvasSize.width) canvas.width = canvasSize.width;
		if (canvas.height !== canvasSize.height) canvas.height = canvasSize.height;
		const context = canvas.getContext('2d');
		if (!context) return false;
		const sourceX = ((visibleLeft - imageRect.left) / imageRect.width) * image.naturalWidth;
		const sourceY = ((visibleTop - imageRect.top) / imageRect.height) * image.naturalHeight;
		const sourceWidth = (visibleWidth / imageRect.width) * image.naturalWidth;
		const sourceHeight = (visibleHeight / imageRect.height) * image.naturalHeight;
		context.imageSmoothingEnabled = true;
		context.imageSmoothingQuality = 'high';
		context.clearRect(0, 0, canvas.width, canvas.height);
		try {
			context.drawImage(image, sourceX, sourceY, sourceWidth, sourceHeight, 0, 0, canvas.width, canvas.height);
		} catch {
			return false;
		}
		return true;
	}, []);
	useEffect(() => {
		if (!sharpenWhenZoomed) {
			setCanvasPortalTarget(null);
			return;
		}
		setCanvasPortalTarget(imageRef.current?.ownerDocument.body ?? null);
	}, [sharpenWhenZoomed, src]);
	useEffect(() => {
		if (!sharpenWhenZoomed || !isReady) {
			setIsSharpCanvasActive(false);
			return;
		}
		const image = imageRef.current;
		const ownerWindow = image?.ownerDocument.defaultView;
		if (!image || !ownerWindow) {
			setIsSharpCanvasActive(false);
			return;
		}
		const surface = image.closest('[data-flx*="pan-zoom-surface"], [data-zoom-state]') as HTMLElement | null;
		const mediaContainer = image.closest(
			'[data-flx="messaging.media-modal.wrapped-children.media-container"]',
		) as HTMLElement | null;
		const updateActiveState = () => {
			setIsSharpCanvasActive(
				surface?.getAttribute('data-zoom-state') === 'zoomed' &&
					mediaContainer?.getAttribute('data-media-rotation-active') !== 'true',
			);
		};
		updateActiveState();
		const observer = new ownerWindow.MutationObserver(updateActiveState);
		if (surface) observer.observe(surface, {attributes: true, attributeFilter: ['data-zoom-state']});
		if (mediaContainer) {
			observer.observe(mediaContainer, {attributes: true, attributeFilter: ['data-media-rotation-active']});
		}
		return () => observer.disconnect();
	}, [isReady, sharpenWhenZoomed, src]);
	useLayoutEffect(() => {
		if (!isReady || !isSharpCanvasActive || !canvasPortalTarget) {
			setIsSharpCanvasVisible(false);
			return;
		}
		const image = imageRef.current;
		const canvas = canvasRef.current;
		const ownerWindow = image?.ownerDocument.defaultView;
		if (!image || !canvas || !ownerWindow) {
			setIsSharpCanvasVisible(false);
			return;
		}
		let animationFrame: number | null = null;
		let disposed = false;
		const scheduleDraw = () => {
			if (disposed || animationFrame != null) return;
			animationFrame = ownerWindow.requestAnimationFrame(() => {
				animationFrame = null;
				if (disposed) return;
				setIsSharpCanvasVisible(drawSharpCanvas());
			});
		};
		const surface = image.closest('[data-flx*="pan-zoom-surface"], [data-zoom-state]') as HTMLElement | null;
		const mutationObserver = new ownerWindow.MutationObserver(scheduleDraw);
		if (surface) {
			mutationObserver.observe(surface, {
				subtree: true,
				attributes: true,
				attributeFilter: ['style', 'class', 'data-zoom-state', 'data-dragging'],
			});
		}
		const ResizeObserverConstructor = ownerWindow.ResizeObserver;
		const resizeObserver = ResizeObserverConstructor ? new ResizeObserverConstructor(scheduleDraw) : null;
		resizeObserver?.observe(image);
		if (surface) resizeObserver?.observe(surface);
		ownerWindow.addEventListener('resize', scheduleDraw);
		scheduleDraw();
		return () => {
			disposed = true;
			if (animationFrame != null) {
				ownerWindow.cancelAnimationFrame(animationFrame);
			}
			mutationObserver.disconnect();
			resizeObserver?.disconnect();
			ownerWindow.removeEventListener('resize', scheduleDraw);
		};
	}, [canvasPortalTarget, drawSharpCanvas, isReady, isSharpCanvasActive, src]);
	const imageClassName =
		[
			className,
			isReady ? undefined : styles.imagePending,
			isReady && isSharpCanvasActive && isSharpCanvasVisible ? styles.sharpCanvasSourceHidden : undefined,
		]
			.filter(Boolean)
			.join(' ') || undefined;
	return (
		<>
			{!isReady && (
				<span
					className={styles.imageSpinnerOverlay}
					aria-hidden="true"
					data-flx="messaging.media-viewer-modal.render-ready-image.image-spinner-overlay"
				>
					<Spinner size="large" data-flx="messaging.media-viewer-modal.render-ready-image.spinner" />
				</span>
			)}
			{isReady &&
				isSharpCanvasActive &&
				canvasPortalTarget &&
				createPortal(
					<canvas
						ref={canvasRef}
						className={styles.sharpCanvasLayer}
						data-flx="messaging.media-viewer-modal.render-ready-image.sharp-canvas"
					/>,
					canvasPortalTarget,
				)}
			<img
				data-flx="messaging.media-viewer-modal.render-ready-image.img"
				{...imageProps}
				alt={imageProps.alt ?? ''}
				ref={imageRef}
				src={src}
				className={imageClassName}
				onLoad={handleLoad}
				onError={handleError}
			/>
		</>
	);
};

const MobileMediaOptionsSheet: FC<MobileMediaOptionsSheetProps> = observer(function MobileMediaOptionsSheet({
	currentItem,
	defaultName,
	isOpen,
	message,
	onClose,
	onDelete,
	sourceChannel,
}: MobileMediaOptionsSheetProps) {
	const {i18n} = useLingui();
	const mediaMenuData = useMediaMenuData(
		{
			message,
			originalSrc: currentItem.originalSrc,
			proxyURL: currentItem.src,
			type: currentItem.type,
			contentHash: currentItem.contentHash,
			attachmentId: currentItem.attachmentId,
			embedIndex: currentItem.embedIndex,
			defaultName,
			defaultAltText: undefined,
			naturalWidth: currentItem.naturalWidth,
			naturalHeight: currentItem.naturalHeight,
		},
		{
			onClose,
		},
	);
	const {groups: messageGroups} = useMessageActionMenuData(message, {
		onClose,
		onDelete,
		sourceChannel,
	});
	const visibleMessageGroups = useMemo(() => messageGroups.filter((group) => group.items.length > 0), [messageGroups]);
	const mediaMenuGroupsWithMessageActions = useMemo(
		() => [...mediaMenuData.groups, ...visibleMessageGroups],
		[mediaMenuData.groups, visibleMessageGroups],
	);
	return (
		<MenuBottomSheet
			isOpen={isOpen}
			onClose={onClose}
			groups={mediaMenuGroupsWithMessageActions}
			title={i18n._(MEDIA_OPTIONS_DESCRIPTOR)}
			data-flx="messaging.media-viewer-modal.mobile-media-options-sheet.menu-bottom-sheet"
		/>
	);
});
const MediaViewerModalComponent: FC = observer(() => {
	const {i18n} = useLingui();
	const {isOpen, items, currentIndex, sessionId, channelId, messageId, message, sourceChannel} = MediaViewer;
	const {enabled: isMobile} = MobileLayout;
	const [isMediaMenuOpen, setIsMediaMenuOpen] = useState(false);
	const currentItem = items[currentIndex];
	const currentGifvIsActualGif =
		currentItem?.type === 'gifv' && (currentItem.src.endsWith('.gif') || currentItem.originalSrc.endsWith('.gif'));
	useBottomSheetBackHandler(isOpen, MediaViewerCommands.closeMediaViewer);
	useEffect(() => {
		if (!isOpen) return;
		LayerManager.addLayer('modal', 'media-viewer', MediaViewerCommands.closeMediaViewer);
		return () => {
			LayerManager.removeLayer('modal', 'media-viewer');
		};
	}, [isOpen]);
	useEffect(() => {
		if (!isOpen || items.length <= 1) return;
		const preloadIndices = [currentIndex - 1, currentIndex + 1].filter(
			(i) => i >= 0 && i < items.length && i !== currentIndex,
		);
		for (const idx of preloadIndices) {
			const item = items[idx];
			if (!item) continue;
			if (item.type === 'image' || item.type === 'gif') {
				const isItemBlob = item.src.startsWith('blob:');
				if (isItemBlob) continue;
				const baseProxyURL = getBaseProxyURL(item.src);
				const shouldRequestAnimated = item.animated || item.type === 'gif';
				let preloadSrc: string;
				if (shouldRequestAnimated) {
					preloadSrc = buildMediaProxyURL(baseProxyURL, {
						format: resolvePreferredImageFormat(item.contentType),
						animated: true,
					});
				} else {
					preloadSrc = buildViewerStaticImageURL(item);
				}
				if (!ImageCacheUtils.hasImage(preloadSrc)) {
					ImageCacheUtils.loadImage(preloadSrc, () => {});
				}
			} else if (item.type === 'gifv' || item.type === 'video') {
				const video = document.createElement('video');
				video.preload = 'metadata';
				video.src = item.src;
				video.load();
			}
		}
	}, [isOpen, currentIndex, items]);
	const defaultName = useMemo(() => {
		if (!currentItem) return '';
		return untracked(() =>
			deriveDefaultNameFromMessage({
				message,
				attachmentId: currentItem.attachmentId,
				embedIndex: currentItem.embedIndex,
				url: currentItem.originalSrc,
				proxyUrl: currentItem.src,
				i18nInstance: i18n,
			}),
		);
	}, [currentItem, i18n.locale, message]);
	const isCurrentGifFavoriteMedia = currentItem?.type === 'gif' || currentItem?.type === 'gifv';
	const {isFavorited, toggleFavorite: toggleCurrentFavorite} = useMediaFavorite({
		channelId,
		messageId,
		attachmentId: currentItem?.attachmentId,
		embedIndex: currentItem?.embedIndex,
		defaultName,
		contentHash: currentItem?.contentHash,
		isGifv: isCurrentGifFavoriteMedia,
		embedURL: isCurrentGifFavoriteMedia ? currentItem?.originalSrc : undefined,
		proxyURL: isCurrentGifFavoriteMedia ? currentItem?.src : undefined,
		naturalWidth: currentItem?.naturalWidth,
		naturalHeight: currentItem?.naturalHeight,
	});
	const handleFavoriteClick = useCallback(async () => {
		await toggleCurrentFavorite();
	}, [toggleCurrentFavorite]);
	const handleDownload = useCallback(() => {
		if (!currentItem) return;
		const mediaType = (() => {
			if (currentItem.type === 'audio') return 'audio';
			if (currentItem.type === 'video' || currentItem.type === 'gifv') return 'video';
			if (currentItem.type === 'gif') return 'gif';
			return 'image';
		})();
		const downloadSrc =
			currentItem.src !== currentItem.originalSrc ? getBaseProxyURL(currentItem.src) : currentItem.originalSrc;
		createDownloadHandler(downloadSrc, mediaType)();
	}, [currentItem]);
	const handleOpenInBrowser = useCallback(() => {
		if (!currentItem) return;
		openExternalUrlWithWarning(currentItem.originalSrc);
	}, [currentItem]);
	const handleCopyLink = useCallback(() => {
		if (!currentItem) return;
		void copyMediaLinkToClipboard({i18n, originalSrc: currentItem.originalSrc});
	}, [currentItem, i18n]);
	const handleCopyMedia = useCallback(() => {
		if (!currentItem) return;
		void copyMediaToClipboard({
			i18n,
			originalSrc: currentItem.originalSrc,
			proxyURL: currentItem.src,
			type: currentItem.type,
			defaultName,
		});
	}, [currentItem, defaultName, i18n]);
	const handleDelete = useCallback(
		(bypassConfirm?: boolean) => {
			if (!message) return;
			if (bypassConfirm) {
				MessageCommands.remove(message.channelId, message.id);
				return;
			}
			MessageCommands.showDeleteConfirmation(i18n, {message, showShiftBypassConfirmationTip: true});
		},
		[i18n, message],
	);
	const handlePrevious = useCallback(() => {
		MediaViewerCommands.navigateMediaViewer((currentIndex - 1 + items.length) % items.length);
	}, [currentIndex, items.length]);
	const handleNext = useCallback(() => {
		MediaViewerCommands.navigateMediaViewer((currentIndex + 1) % items.length);
	}, [currentIndex, items.length]);
	const handleThumbnailSelect = useCallback(
		(index: number) => {
			if (index === currentIndex) return;
			MediaViewerCommands.navigateMediaViewer(index);
		},
		[currentIndex],
	);
	const handleContextMenu = useCallback(
		(event: MouseEvent<HTMLDivElement>) => {
			if (!currentItem || !message) return;
			const renderMenu = ({onClose}: {onClose: () => void}) => (
				<MediaContextMenu
					message={message}
					sourceChannel={sourceChannel}
					originalSrc={currentItem.originalSrc}
					proxyURL={currentItem.src}
					type={currentItem.type}
					contentHash={currentItem.contentHash}
					attachmentId={currentItem.attachmentId}
					embedIndex={currentItem.embedIndex}
					defaultName={defaultName}
					naturalWidth={currentItem.naturalWidth}
					naturalHeight={currentItem.naturalHeight}
					onClose={onClose}
					onDelete={handleDelete}
					data-flx="messaging.media-viewer-modal.render-menu.media-context-menu"
				/>
			);
			ContextMenuCommands.openFromEvent(event, renderMenu);
		},
		[currentItem, defaultName, handleDelete, message, sourceChannel],
	);
	const handleMenuOpen = useCallback(() => {
		if (!currentItem || !message) return;
		if (isMobile) {
			setIsMediaMenuOpen(true);
		} else {
			ContextMenuCommands.openAtPoint({x: window.innerWidth / 2, y: window.innerHeight / 2}, ({onClose}) => (
				<MediaContextMenu
					message={message}
					sourceChannel={sourceChannel}
					originalSrc={currentItem.originalSrc}
					proxyURL={currentItem.src}
					type={currentItem.type}
					contentHash={currentItem.contentHash}
					attachmentId={currentItem.attachmentId}
					embedIndex={currentItem.embedIndex}
					defaultName={defaultName}
					naturalWidth={currentItem.naturalWidth}
					naturalHeight={currentItem.naturalHeight}
					onClose={onClose}
					onDelete={handleDelete}
					data-flx="messaging.media-viewer-modal.handle-menu-open.media-context-menu"
				/>
			));
		}
	}, [currentItem, defaultName, handleDelete, message, sourceChannel, isMobile]);
	const permissions = useMemo(
		() => (message ? untracked(() => getMessagePermissions(message, sourceChannel)) : null),
		[message, sourceChannel],
	);
	const forwardMediaSelection = useMemo<MessageCommands.ForwardMediaSelection | undefined>(() => {
		if (!currentItem) return undefined;
		if (currentItem.attachmentId) {
			return {attachmentIds: [currentItem.attachmentId]};
		}
		if (currentItem.embedIndex !== undefined) {
			return {embedIndices: [currentItem.embedIndex]};
		}
		return undefined;
	}, [currentItem]);
	const canForwardCurrentMedia = Boolean(
		message &&
			permissions?.canForwardMessage &&
			(forwardMediaSelection?.attachmentIds?.length || forwardMediaSelection?.embedIndices?.length),
	);
	const handleReply = useCallback(() => {
		if (!message) return;
		MediaViewerCommands.closeMediaViewer();
		requestMessageReply(message, {sourceChannel});
	}, [message, sourceChannel]);
	const handleForward = useCallback(() => {
		if (!message || !forwardMediaSelection) return;
		const handleForwardSuccess = ({shouldNavigate}: ForwardModalSuccess) => {
			if (shouldNavigate) {
				MediaViewerCommands.closeMediaViewer();
			}
		};
		requestMessageForward(message, sourceChannel, {
			mediaSelection: forwardMediaSelection,
			onForwardSuccess: handleForwardSuccess,
		});
	}, [forwardMediaSelection, message, sourceChannel]);
	const isBlob = currentItem?.src.startsWith('blob:');
	const imageSrc = useMemo(() => {
		if (!currentItem) return '';
		if (isBlob) {
			return currentItem.src;
		}
		const baseProxyURL = getBaseProxyURL(currentItem.src);
		const shouldRequestAnimated = currentItem.animated || currentItem.type === 'gif';
		const shouldStartAnimationFromViewerOpen =
			currentItem.type === 'gif' || currentGifvIsActualGif || (currentItem.type === 'image' && currentItem.animated);
		if (shouldRequestAnimated) {
			const animatedSrc = buildMediaProxyURL(baseProxyURL, {
				format: resolvePreferredImageFormat(currentItem.contentType),
				animated: true,
			});
			return shouldStartAnimationFromViewerOpen
				? appendMediaViewerAnimationSession(animatedSrc, sessionId)
				: animatedSrc;
		}
		if (currentItem.type === 'gifv' || currentItem.type === 'video' || currentItem.type === 'audio') {
			return baseProxyURL;
		}
		return buildViewerStaticImageURL(currentItem);
	}, [currentGifvIsActualGif, currentItem, isBlob, sessionId]);
	const thumbnails = useMemo(
		() =>
			items.map((item, index) => {
				const name =
					item.filename ||
					item.originalSrc.split('/').pop()?.split('?')[0] ||
					i18n._(ATTACHMENT_DESCRIPTOR, {index1: index + 1});
				if ((item.type === 'image' || item.type === 'gif' || item.animated) && !item.src.startsWith('blob:')) {
					const baseProxyURL = getBaseProxyURL(item.src);
					return {
						src: buildMediaProxyURL(baseProxyURL, {
							format: resolvePreferredImageFormat(item.contentType),
							width: 320,
							height: 320,
							animated: Boolean(item.animated || item.type === 'gif'),
						}),
						alt: name,
						type: item.type,
					};
				}
				return {
					src: item.src,
					alt: name,
					type: item.type,
				};
			}),
		[items, i18n.locale],
	);
	if (!isOpen || !currentItem) {
		return null;
	}
	const portalRoot = getMediaViewerPortalRoot();
	if (!portalRoot) {
		return null;
	}
	const dimensions =
		currentItem.naturalWidth && currentItem.naturalHeight
			? `${currentItem.naturalWidth}×${currentItem.naturalHeight}`
			: undefined;
	const fileName = currentItem.filename || currentItem.originalSrc.split('/').pop()?.split('?')[0] || 'media';
	const fileSize = currentItem.fileSize != null ? formatFileSize(currentItem.fileSize) : undefined;
	const expiryInfo =
		currentItem.expiresAt && currentItem.expiresAt.length > 0
			? {
					expiresAt: new Date(currentItem.expiresAt),
					isExpired: currentItem.expired ?? false,
					label: formatAttachmentDate(new Date(currentItem.expiresAt)),
				}
			: undefined;
	const getTitle = () => {
		if (currentItem.type === 'image') {
			return currentItem.animated ? i18n._(ANIMATED_IMAGE_PREVIEW_DESCRIPTOR) : i18n._(IMAGE_PREVIEW_DESCRIPTOR);
		}
		if (currentItem.type === 'gif' || currentItem.type === 'gifv') {
			return i18n._(GIF_PREVIEW_DESCRIPTOR);
		}
		if (currentItem.type === 'video') {
			return i18n._(VIDEO_PREVIEW_DESCRIPTOR);
		}
		if (currentItem.type === 'audio') {
			return i18n._(AUDIO_PREVIEW_DESCRIPTOR);
		}
		return i18n._(MEDIA_PREVIEW_DESCRIPTOR);
	};
	const modalTitle = getTitle();
	const renderMedia = () => {
		if (currentItem.type === 'gifv') {
			if (currentGifvIsActualGif) {
				return (
					<RenderReadyImage
						src={imageSrc}
						alt={i18n._(ANIMATED_GIF_DESCRIPTOR)}
						className={styles.gifvImage}
						style={{
							objectFit: 'contain',
						}}
						draggable={false}
						data-flx="messaging.media-viewer-modal.render-media.gifv-image"
					/>
				);
			}
			return (
				<video
					key={currentItem.src}
					src={currentItem.src}
					className={styles.gifvVideo}
					style={{
						objectFit: 'contain',
					}}
					autoPlay
					loop
					muted
					playsInline
					controls={false}
					aria-label={i18n._(ANIMATED_VIDEO_DESCRIPTOR)}
					data-flx="messaging.media-viewer-modal.render-media.gifv-video"
				>
					<track kind="captions" data-flx="messaging.media-viewer-modal.render-media.track" />
				</video>
			);
		}
		if (currentItem.type === 'video') {
			const hasNaturalVideoDimensions = currentItem.naturalWidth > 0 && currentItem.naturalHeight > 0;
			const videoAspectRatio = hasNaturalVideoDimensions
				? `${currentItem.naturalWidth} / ${currentItem.naturalHeight}`
				: '16 / 9';
			return (
				<div
					className={styles.videoPlayerContainer}
					style={
						{
							'--video-natural-width': hasNaturalVideoDimensions ? `${currentItem.naturalWidth}px` : '960px',
							'--video-aspect-ratio': hasNaturalVideoDimensions
								? currentItem.naturalWidth / currentItem.naturalHeight
								: 16 / 9,
							aspectRatio: videoAspectRatio,
						} as CSSProperties
					}
					data-flx="messaging.media-viewer-modal.render-media.video-player-container"
				>
					<VideoPlayer
						src={currentItem.src}
						width={currentItem.naturalWidth}
						height={currentItem.naturalHeight}
						duration={currentItem.duration}
						autoPlay
						fillContainer
						isMobile={isMobile}
						className={styles.videoPlayer}
						data-flx="messaging.media-viewer-modal.render-media.video-player"
					/>
				</div>
			);
		}
		if (currentItem.type === 'audio') {
			return (
				<div className={styles.mediaContainer} data-flx="messaging.media-viewer-modal.render-media.media-container">
					<div
						className={styles.audioPlayerContainer}
						data-flx="messaging.media-viewer-modal.render-media.audio-player-container"
					>
						<AudioPlayer
							src={currentItem.src}
							title={fileName}
							duration={currentItem.duration}
							autoPlay
							isMobile={isMobile}
							className={styles.audioPlayer}
							data-flx="messaging.media-viewer-modal.render-media.audio-player"
						/>
					</div>
				</div>
			);
		}
		const imageAlt = (() => {
			if (currentItem.type === 'gif') return i18n._(ANIMATED_GIF_DESCRIPTOR);
			if (currentItem.animated) return i18n._(ANIMATED_IMAGE_DESCRIPTOR);
			return i18n._(IMAGE_DESCRIPTOR);
		})();
		return (
			<RenderReadyImage
				src={imageSrc}
				alt={imageAlt}
				width={currentItem.naturalWidth}
				height={currentItem.naturalHeight}
				className={styles.image}
				sharpenWhenZoomed={shouldUseSharpCanvasForItem(currentItem, imageSrc)}
				style={{
					width: 'auto',
					height: 'auto',
					maxWidth: `min(var(--media-fit-max-width, 100%), ${currentItem.naturalWidth}px)`,
					maxHeight: `min(var(--media-fit-max-height, 100%), ${currentItem.naturalHeight}px)`,
					aspectRatio: `${currentItem.naturalWidth}/${currentItem.naturalHeight}`,
					objectFit: 'contain',
				}}
				draggable={false}
				data-flx="messaging.media-viewer-modal.render-media.image"
			/>
		);
	};
	const canFavoriteCurrentItem =
		Boolean(channelId) &&
		Boolean(messageId) &&
		(currentItem.type === 'image' ||
			currentItem.type === 'gif' ||
			currentItem.type === 'gifv' ||
			currentItem.type === 'video');
	return createPortal(
		<PortalHostContext.Provider value={portalRoot}>
			<MediaModal
				title={modalTitle}
				fileName={fileName}
				fileSize={fileSize}
				expiryInfo={
					expiryInfo
						? {
								expiresAt: expiryInfo.expiresAt,
								isExpired: expiryInfo.isExpired,
							}
						: undefined
				}
				dimensions={dimensions}
				isFavorited={canFavoriteCurrentItem ? isFavorited : undefined}
				onFavorite={canFavoriteCurrentItem ? handleFavoriteClick : undefined}
				onDownload={handleDownload}
				onOpenInBrowser={handleOpenInBrowser}
				onCopyLink={handleCopyLink}
				onCopyMedia={handleCopyMedia}
				onReply={permissions?.canSendMessages ? handleReply : undefined}
				onForward={canForwardCurrentMedia ? handleForward : undefined}
				enablePanZoom={currentItem.type === 'image' || currentItem.type === 'gif' || currentItem.type === 'gifv'}
				currentIndex={currentIndex}
				totalAttachments={items.length}
				onPrevious={items.length > 1 ? handlePrevious : undefined}
				onNext={items.length > 1 ? handleNext : undefined}
				thumbnails={thumbnails}
				onSelectThumbnail={handleThumbnailSelect}
				providerName={currentItem.providerName}
				videoSrc={currentItem.type === 'video' ? currentItem.src : undefined}
				initialTime={currentItem.initialTime}
				mediaType={currentItem.type === 'audio' ? 'audio' : currentItem.type === 'video' ? 'video' : 'image'}
				onMenuOpen={handleMenuOpen}
				data-flx="messaging.media-viewer-modal.media-viewer-modal-component.media-modal"
			>
				<div
					className={styles.mediaContextMenuWrapper}
					onContextMenu={handleContextMenu}
					role="region"
					aria-label={modalTitle}
					data-flx="messaging.media-viewer-modal.media-viewer-modal-component.media-context-menu-wrapper"
				>
					{renderMedia()}
				</div>
			</MediaModal>
			{isMobile && message && (
				<MobileMediaOptionsSheet
					currentItem={currentItem}
					defaultName={defaultName}
					isOpen={isMediaMenuOpen}
					message={message}
					onClose={() => setIsMediaMenuOpen(false)}
					onDelete={handleDelete}
					sourceChannel={sourceChannel}
					data-flx="messaging.media-viewer-modal.media-viewer-modal-component.mobile-media-options-sheet"
				/>
			)}
		</PortalHostContext.Provider>,
		portalRoot,
	);
});
export const MediaViewerModal: FC = MediaViewerModalComponent;
