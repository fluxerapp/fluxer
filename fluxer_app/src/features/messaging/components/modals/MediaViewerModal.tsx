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
import {buildStaticGifPreviewURL} from '@app/features/messaging/utils/MediaProxyUtils';
import {
	buildViewerMediaURL,
	getBaseProxyURL,
	isGifvRenderedAsImage,
	isViewerImageItem,
} from '@app/features/messaging/utils/MediaViewerItemUtils';
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
import {snapMediaProxyImageSize} from '@app/features/user/utils/AvatarUtils';
import {AudioPlayer} from '@app/features/voice/components/media_player/components/AudioPlayer';
import {VideoPlayer} from '@app/features/voice/components/media_player/components/VideoPlayer';
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

const MEDIA_VIEWER_THUMBNAIL_SIZE = 44;

type RenderReadyImageProps = ImgHTMLAttributes<HTMLImageElement>;

const isImagePainted = (image: HTMLImageElement | null): image is HTMLImageElement =>
	image?.complete === true && image.naturalWidth > 0;

const RenderReadyImage: FC<RenderReadyImageProps> = ({
	className,
	onError,
	onLoad,
	src,
	...imageProps
}: RenderReadyImageProps) => {
	const [isReady, setIsReady] = useState(false);
	const [renderedSource, setRenderedSource] = useState(src);
	const [decodedBeforeSwap, setDecodedBeforeSwap] = useState(() => ImageCacheUtils.hasImage(src));
	if (renderedSource !== src) {
		setRenderedSource(src);
		setDecodedBeforeSwap(ImageCacheUtils.hasImage(src));
	}
	const shouldMountPlaceholder = !decodedBeforeSwap && !isReady;
	const imageRef = useRef<HTMLImageElement | null>(null);
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
			const image = event.currentTarget;
			ImageCacheUtils.rememberImage(image.getAttribute('src'), image);
			onLoad?.(event);
			markReady(image);
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
	useLayoutEffect(() => {
		readyTokenRef.current += 1;
		const image = imageRef.current;
		if (!isImagePainted(image)) {
			setIsReady(false);
			return () => {
				readyTokenRef.current += 1;
			};
		}
		ImageCacheUtils.rememberImage(image.getAttribute('src'), image);
		setIsReady(true);
		markReady(image);
		return () => {
			readyTokenRef.current += 1;
		};
	}, [markReady, src]);
	const imageClassName = [className, isReady ? undefined : styles.imagePending].filter(Boolean).join(' ') || undefined;
	return (
		<>
			{shouldMountPlaceholder && (
				<span
					className={styles.placeholder}
					aria-hidden="true"
					data-flx="messaging.media-viewer-modal.render-ready-image.placeholder"
				>
					<Spinner size="large" data-flx="messaging.media-viewer-modal.render-ready-image.spinner" />
				</span>
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
	const {isOpen, items, currentIndex, channelId, messageId, message, sourceChannel} = MediaViewer;
	const {enabled: isMobile} = MobileLayout;
	const [isMediaMenuOpen, setIsMediaMenuOpen] = useState(false);
	const currentItem = items[currentIndex];
	const currentGifvIsActualGif = currentItem != null && isGifvRenderedAsImage(currentItem);
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
		const count = items.length;
		const adjacentIndices =
			count === 2 ? [(currentIndex + 1) % count] : [(currentIndex - 1 + count) % count, (currentIndex + 1) % count];
		const unpinCallbacks = adjacentIndices.map((index) => {
			const item = items[index];
			if (!item || !isViewerImageItem(item) || item.src.startsWith('blob:')) return () => {};
			return ImageCacheUtils.pinImage(buildViewerMediaURL(item));
		});
		return () => {
			for (const unpin of unpinCallbacks) unpin();
		};
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
	const imageSrc = useMemo(() => (currentItem ? buildViewerMediaURL(currentItem) : ''), [currentItem]);
	const thumbnails = useMemo(() => {
		const snappedThumbnailSize = snapMediaProxyImageSize(MEDIA_VIEWER_THUMBNAIL_SIZE);
		return items.map((item, index) => {
			const name =
				item.filename ||
				item.originalSrc.split('/').pop()?.split('?')[0] ||
				i18n._(ATTACHMENT_DESCRIPTOR, {index1: index + 1});
			if ((item.type === 'image' || item.type === 'gif' || item.animated) && !item.src.startsWith('blob:')) {
				return {
					src: buildStaticGifPreviewURL(item.src, snappedThumbnailSize, snappedThumbnailSize),
					alt: name,
					type: item.type,
				};
			}
			return {
				src: item.src,
				alt: name,
				type: item.type,
			};
		});
	}, [items, i18n.locale]);
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
						initialTime={currentItem.initialTime}
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
