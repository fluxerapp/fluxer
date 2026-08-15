// SPDX-License-Identifier: AGPL-3.0-or-later

import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import * as FavoriteGifCommands from '@app/features/expressions/commands/FavoriteGifCommands';
import * as FavoriteMemeCommands from '@app/features/expressions/commands/FavoriteMemeCommands';
import {AddFavoriteMemeModal} from '@app/features/expressions/components/modals/AddFavoriteMemeModal';
import FavoriteGif from '@app/features/expressions/state/FavoriteGif';
import FavoriteMemes from '@app/features/expressions/state/FavoriteMemes';
import * as FavoriteGifUtils from '@app/features/expressions/utils/FavoriteGifUtils';
import * as FavoriteMemeUtils from '@app/features/expressions/utils/FavoriteMemeUtils';
import {
	ADD_TO_FAVORITES_DESCRIPTOR,
	COPY_LINK_DESCRIPTOR,
	LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR,
	OPEN_LINK_DESCRIPTOR,
	REMOVE_FROM_FAVORITES_DESCRIPTOR,
	SOMETHING_WENT_WRONG_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {EditAltTextModal} from '@app/features/messaging/components/modals/EditAltTextModal';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {createDownloadHandler} from '@app/features/messaging/utils/FileDownloadUtils';
import {buildMediaProxyURL, stripMediaProxyParams} from '@app/features/messaging/utils/MediaProxyUtils';
import Permission from '@app/features/permissions/state/Permission';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {
	CopyLinkIcon,
	CopyMediaIcon,
	DownloadMediaIcon,
	FavoriteIcon,
	OpenMediaLinkIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {getElectronAPI, openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import Users from '@app/features/user/state/Users';
import type {ClipboardWriteFileMediaType} from '@app/types/electron.d';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {I18n, MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PencilSimpleIcon} from '@phosphor-icons/react';
import {autorun} from 'mobx';
import {useCallback, useMemo, useSyncExternalStore} from 'react';

const COPY_IMAGE_DESCRIPTOR = msg({
	message: 'Copy image',
	comment: 'Media context menu action that copies an image attachment to the clipboard.',
});
const COPY_GIF_DESCRIPTOR = msg({
	message: 'Copy GIF',
	comment: 'Media context menu action that copies a GIF attachment to the clipboard.',
});
const COPY_VIDEO_DESCRIPTOR = msg({
	message: 'Copy video',
	comment: 'Media context menu action that copies a video attachment to the clipboard.',
});
const COPY_AUDIO_DESCRIPTOR = msg({
	message: 'Copy audio',
	comment: 'Media context menu action that copies an audio attachment to the clipboard.',
});
const COPY_MEDIA_DESCRIPTOR = msg({
	message: 'Copy media',
	comment: 'Generic media context menu action label when the specific type is unknown.',
});
const COPY_IMAGE_LINK_DESCRIPTOR = msg({
	message: 'Copy image link',
	comment: 'Image context menu action that copies the image URL to the clipboard.',
});
const COPY_GIF_LINK_DESCRIPTOR = msg({
	message: 'Copy GIF link',
	comment: 'Media context menu action that copies the URL of the GIF attachment.',
});
const COPY_VIDEO_LINK_DESCRIPTOR = msg({
	message: 'Copy video link',
	comment: 'Media context menu action that copies the URL of the video attachment.',
});
const COPY_AUDIO_LINK_DESCRIPTOR = msg({
	message: 'Copy audio link',
	comment: 'Media context menu action that copies the URL of the audio attachment.',
});
const COPY_FILE_LINK_DESCRIPTOR = msg({
	message: 'Copy file link',
	comment: 'Media context menu action that copies the URL of the file attachment.',
});
const OPEN_IMAGE_LINK_DESCRIPTOR = msg({
	message: 'Open image link',
	comment: 'Image context menu action that opens the image URL in an external browser.',
});
const OPEN_GIF_LINK_DESCRIPTOR = msg({
	message: 'Open GIF link',
	comment: 'Media context menu action that opens the GIF URL in an external browser.',
});
const OPEN_VIDEO_LINK_DESCRIPTOR = msg({
	message: 'Open video link',
	comment: 'Media context menu action that opens the video URL in an external browser.',
});
const OPEN_AUDIO_LINK_DESCRIPTOR = msg({
	message: 'Open audio link',
	comment: 'Media context menu action that opens the audio URL in an external browser.',
});
const OPEN_FILE_LINK_DESCRIPTOR = msg({
	message: 'Open file link',
	comment: 'Media context menu action that opens the file URL in an external browser.',
});
const DOWNLOAD_IMAGE_DESCRIPTOR = msg({
	message: 'Download image',
	comment: 'Image context menu action that downloads the image to disk.',
});
const DOWNLOAD_GIF_DESCRIPTOR = msg({
	message: 'Download GIF',
	comment: 'Media context menu action that downloads a GIF attachment.',
});
const DOWNLOAD_VIDEO_DESCRIPTOR = msg({
	message: 'Download video',
	comment: 'Media context menu action that downloads a video attachment.',
});
const DOWNLOAD_AUDIO_DESCRIPTOR = msg({
	message: 'Download audio',
	comment: 'Media context menu action that downloads an audio attachment.',
});
const DOWNLOAD_FILE_DESCRIPTOR = msg({
	message: 'Download file',
	comment: 'Media context menu action that downloads a file attachment.',
});
const DOWNLOAD_MEDIA_DESCRIPTOR = msg({
	message: 'Download media',
	comment: 'Generic media context menu download action when the specific type is unknown.',
});
const ATTACHMENT_IS_EXPIRED_OR_UNAVAILABLE_DESCRIPTOR = msg({
	message: 'Attachment is expired or unavailable',
	comment: 'Toast shown when an attachment URL is no longer accessible.',
});
const URL_COPIED_TO_CLIPBOARD_DESCRIPTOR = msg({
	message: 'URL copied to clipboard',
	comment: 'Success toast confirming that a media URL was copied as text fallback.',
});
const LOADING_DESCRIPTOR = msg({
	message: 'Loading...',
	comment: 'Generic loading toast or label shown while an operation is in progress.',
});
const COPIED_TO_CLIPBOARD_DESCRIPTOR = msg({
	message: 'Copied to clipboard',
	comment: 'Generic success toast confirming that content was copied to the clipboard.',
});
const FAILED_TO_COPY_TO_CLIPBOARD_DESCRIPTOR = msg({
	message: 'Failed to copy to clipboard',
	comment: 'Error toast shown when copying to the clipboard fails.',
});
const COPYING_IMAGE_DESCRIPTOR = msg({
	message: 'Copying image...',
	comment: 'Progress toast shown while encoding and copying an image to the clipboard.',
});
const IMAGE_COPIED_TO_CLIPBOARD_DESCRIPTOR = msg({
	message: 'Image copied to clipboard',
	comment: 'Success toast confirming that an image was copied as a bitmap to the clipboard.',
});
const ATTACHMENT_ID_COPIED_TO_CLIPBOARD_DESCRIPTOR = msg({
	message: 'Attachment ID copied to clipboard',
	comment: 'Developer-mode toast confirming an attachment ID was copied.',
});
const EDIT_ALT_TEXT_DESCRIPTOR = msg({
	message: 'Edit alt text',
	comment: 'Action that opens the modal for editing an image or video accessibility description.',
});
const ADD_TO_URL_ONLY_GIF_FAVORITES_DESCRIPTOR = msg({
	message: 'Add to URL-only GIF favorites',
	comment: 'Media context menu action that saves a GIF favorite by URL without uploading it.',
});
const REMOVE_FROM_URL_ONLY_GIF_FAVORITES_DESCRIPTOR = msg({
	message: 'Remove from URL-only GIF favorites',
	comment: 'Media context menu action that removes a GIF favorite saved by URL.',
});
const ADD_TO_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Add to saved media',
	comment: 'Action that adds the selected media to saved media.',
});
const REMOVE_FROM_SAVED_MEDIA_DESCRIPTOR = msg({
	message: 'Remove from saved media',
	comment: 'Action that removes the selected media from saved media.',
});
const logger = new Logger('MediaMenuData');

function showMediaErrorModal(i18n: I18n, message: MessageDescriptor, dataFlx: string, defer = false): void {
	showGenericErrorModal({
		title: () => i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR),
		message: () => i18n._(message),
		dataFlx,
		defer,
	});
}

export type MediaType = 'image' | 'gif' | 'gifv' | 'video' | 'audio' | 'file';

async function canvasToPngBlob(canvas: HTMLCanvasElement): Promise<Blob> {
	return await new Promise<Blob>((resolve, reject) => {
		canvas.toBlob((blob) => {
			if (!blob) {
				reject(new Error('Failed to encode PNG'));
				return;
			}
			resolve(blob);
		}, 'image/png');
	});
}

async function createImageBitmapWithOrientation(blob: Blob): Promise<ImageBitmap> {
	try {
		return await createImageBitmap(blob, {imageOrientation: 'from-image'});
	} catch {
		return await createImageBitmap(blob);
	}
}

async function convertImageBlobToPng(blob: Blob): Promise<Blob> {
	if (blob.type === 'image/png') return blob;
	if (blob.size === 0) {
		throw new Error('Image blob is empty');
	}
	const imageBitmap = await createImageBitmapWithOrientation(blob);
	try {
		if (typeof OffscreenCanvas !== 'undefined') {
			const canvas = new OffscreenCanvas(imageBitmap.width, imageBitmap.height);
			const ctx = canvas.getContext('2d');
			if (!ctx) {
				throw new Error('OffscreenCanvas 2D context is unavailable');
			}
			ctx.drawImage(imageBitmap, 0, 0);
			if ('convertToBlob' in canvas) {
				return await canvas.convertToBlob({type: 'image/png'});
			}
		}
		const canvas = document.createElement('canvas');
		canvas.width = imageBitmap.width;
		canvas.height = imageBitmap.height;
		const ctx = canvas.getContext('2d');
		if (!ctx) {
			throw new Error('Canvas 2D context is unavailable');
		}
		ctx.drawImage(imageBitmap, 0, 0);
		return await canvasToPngBlob(canvas);
	} finally {
		imageBitmap.close?.();
	}
}

async function fetchFirstPngBlob(urls: Array<string>): Promise<Blob> {
	let lastError: unknown = null;
	for (const url of urls) {
		try {
			const response = await fetch(url);
			if (!response.ok) {
				throw new Error(`Unexpected response status ${response.status} for ${url}`);
			}
			const blob = await response.blob();
			return await convertImageBlobToPng(blob);
		} catch (error) {
			lastError = error;
		}
	}
	throw lastError ?? new Error('Failed to fetch and convert image');
}

function getClipboardFileMediaType(type: MediaType): ClipboardWriteFileMediaType | null {
	switch (type) {
		case 'gif':
			return 'gif';
		case 'gifv':
		case 'video':
			return 'video';
		case 'audio':
			return 'audio';
		default:
			return null;
	}
}

export interface MediaMenuDataOptions {
	onClose: () => void;
}

export interface MediaMenuDataProps {
	message: Message;
	originalSrc: string;
	proxyURL?: string;
	type: MediaType;
	contentHash?: string | null;
	attachmentId?: string;
	embedIndex?: number;
	defaultName?: string;
	defaultAltText?: string;
	naturalWidth?: number;
	naturalHeight?: number;
	snapshotIndex?: number;
}

export interface MediaMenuData {
	groups: Array<MenuGroupType>;
	handlers: MediaMenuHandlers;
	state: MediaMenuState;
}

export interface MediaMenuHandlers {
	handleAddToFavorites: () => void;
	handleRemoveFromFavorites: () => Promise<void>;
	handleEditAltText: () => void;
	handleCopyMedia: () => Promise<void>;
	handleDownloadMedia: () => void;
	handleCopyLink: () => Promise<void>;
	handleOpenLink: () => void;
	handleCopyAttachmentId: () => Promise<void>;
	canEditAltText: boolean;
	canCopyAttachmentId: boolean;
	copyLinkLabel: string;
	openLinkLabel: string;
}

export const mediaMenuItemIds = {
	copy: 'media-copy',
	download: 'media-download',
	copyLink: 'media-copy-link',
	openLink: 'media-open-link',
	favorite: 'media-favorite',
	editAltText: 'media-edit-alt-text',
	copyAttachmentId: 'media-copy-attachment-id',
} as const;

export interface MediaMenuState {
	isFavorited: boolean;
	copyLabel: string;
	downloadLabel: string;
}

function getCopyLabel(type: MediaType, i18n: I18n): string {
	switch (type) {
		case 'image':
			return i18n._(COPY_IMAGE_DESCRIPTOR);
		case 'gif':
		case 'gifv':
			return i18n._(COPY_GIF_DESCRIPTOR);
		case 'video':
			return i18n._(COPY_VIDEO_DESCRIPTOR);
		case 'audio':
			return i18n._(COPY_AUDIO_DESCRIPTOR);
		case 'file':
			return i18n._(COPY_LINK_DESCRIPTOR);
		default:
			return i18n._(COPY_MEDIA_DESCRIPTOR);
	}
}

function getCopyLinkLabel(type: MediaType, i18n: I18n): string {
	switch (type) {
		case 'image':
			return i18n._(COPY_IMAGE_LINK_DESCRIPTOR);
		case 'gif':
		case 'gifv':
			return i18n._(COPY_GIF_LINK_DESCRIPTOR);
		case 'video':
			return i18n._(COPY_VIDEO_LINK_DESCRIPTOR);
		case 'audio':
			return i18n._(COPY_AUDIO_LINK_DESCRIPTOR);
		case 'file':
			return i18n._(COPY_FILE_LINK_DESCRIPTOR);
		default:
			return i18n._(COPY_LINK_DESCRIPTOR);
	}
}

function getOpenLinkLabel(type: MediaType, i18n: I18n): string {
	switch (type) {
		case 'image':
			return i18n._(OPEN_IMAGE_LINK_DESCRIPTOR);
		case 'gif':
		case 'gifv':
			return i18n._(OPEN_GIF_LINK_DESCRIPTOR);
		case 'video':
			return i18n._(OPEN_VIDEO_LINK_DESCRIPTOR);
		case 'audio':
			return i18n._(OPEN_AUDIO_LINK_DESCRIPTOR);
		case 'file':
			return i18n._(OPEN_FILE_LINK_DESCRIPTOR);
		default:
			return i18n._(OPEN_LINK_DESCRIPTOR);
	}
}

function getDownloadLabel(type: MediaType, i18n: I18n): string {
	switch (type) {
		case 'image':
			return i18n._(DOWNLOAD_IMAGE_DESCRIPTOR);
		case 'gif':
		case 'gifv':
			return i18n._(DOWNLOAD_GIF_DESCRIPTOR);
		case 'video':
			return i18n._(DOWNLOAD_VIDEO_DESCRIPTOR);
		case 'audio':
			return i18n._(DOWNLOAD_AUDIO_DESCRIPTOR);
		case 'file':
			return i18n._(DOWNLOAD_FILE_DESCRIPTOR);
		default:
			return i18n._(DOWNLOAD_MEDIA_DESCRIPTOR);
	}
}

export interface CopyMediaToClipboardOptions {
	i18n: I18n;
	originalSrc: string;
	proxyURL?: string;
	type: MediaType;
	defaultName?: string;
}

export async function copyMediaToClipboard({
	i18n,
	originalSrc,
	proxyURL,
	type,
	defaultName,
}: CopyMediaToClipboardOptions): Promise<void> {
	if (!originalSrc) {
		showMediaErrorModal(
			i18n,
			ATTACHMENT_IS_EXPIRED_OR_UNAVAILABLE_DESCRIPTOR,
			'ui.media-menu-data.copy-media-expired-error-modal',
		);
		return;
	}
	if (type === 'file') {
		await TextCopyCommands.copy(i18n, originalSrc, true);
		ToastCommands.createToast({type: 'success', children: i18n._(LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR)});
		return;
	}
	const baseProxyURL = proxyURL ? stripMediaProxyParams(proxyURL) : null;
	const clipboardFileMediaType = getClipboardFileMediaType(type);
	if (clipboardFileMediaType) {
		const electronApi = getElectronAPI();
		if (!electronApi?.clipboardWriteFile) {
			await TextCopyCommands.copy(i18n, originalSrc, true);
			ToastCommands.createToast({type: 'success', children: i18n._(URL_COPIED_TO_CLIPBOARD_DESCRIPTOR)});
			return;
		}
		let toastId: string | null = null;
		try {
			toastId = ToastCommands.createToast({
				type: 'info',
				children: i18n._(LOADING_DESCRIPTOR),
				timeout: 0,
			});
			const result = await electronApi.clipboardWriteFile({
				url: baseProxyURL || originalSrc,
				suggestedName: defaultName,
				mediaType: clipboardFileMediaType,
			});
			if (!result.success) {
				throw new Error(result.error ?? 'Clipboard file write failed');
			}
			if (toastId) ToastCommands.destroyToast(toastId);
			ToastCommands.createToast({type: 'success', children: i18n._(COPIED_TO_CLIPBOARD_DESCRIPTOR)});
		} catch (error) {
			logger.error('Failed to copy media to clipboard:', error);
			if (toastId) ToastCommands.destroyToast(toastId);
			showMediaErrorModal(i18n, FAILED_TO_COPY_TO_CLIPBOARD_DESCRIPTOR, 'ui.media-menu-data.copy-media-error-modal');
		}
		return;
	}
	const urlsToTry: Array<string> = [];
	if (baseProxyURL) urlsToTry.push(buildMediaProxyURL(baseProxyURL, {format: 'png'}));
	if (baseProxyURL) urlsToTry.push(baseProxyURL);
	urlsToTry.push(originalSrc);
	let toastId: string | null = null;
	try {
		toastId = ToastCommands.createToast({
			type: 'info',
			children: i18n._(COPYING_IMAGE_DESCRIPTOR),
			timeout: 0,
		});
		if (!navigator.clipboard?.write || typeof ClipboardItem === 'undefined') {
			throw new Error('Clipboard image write is unavailable');
		}
		await navigator.clipboard.write([
			new ClipboardItem({
				'image/png': fetchFirstPngBlob(urlsToTry),
			}),
		]);
		if (toastId) ToastCommands.destroyToast(toastId);
		ToastCommands.createToast({
			type: 'success',
			children: i18n._(IMAGE_COPIED_TO_CLIPBOARD_DESCRIPTOR),
		});
	} catch (error) {
		logger.error('Failed to copy image to clipboard:', error);
		if (toastId) ToastCommands.destroyToast(toastId);
		await TextCopyCommands.copy(i18n, originalSrc, true);
		ToastCommands.createToast({
			type: 'success',
			children: i18n._(URL_COPIED_TO_CLIPBOARD_DESCRIPTOR),
		});
	}
}

export async function copyMediaLinkToClipboard({i18n, originalSrc}: {i18n: I18n; originalSrc: string}): Promise<void> {
	if (!originalSrc) {
		showMediaErrorModal(
			i18n,
			ATTACHMENT_IS_EXPIRED_OR_UNAVAILABLE_DESCRIPTOR,
			'ui.media-menu-data.copy-link-expired-error-modal',
		);
		return;
	}
	await TextCopyCommands.copy(i18n, originalSrc, true);
	ToastCommands.createToast({
		type: 'success',
		children: i18n._(LINK_COPIED_TO_CLIPBOARD_DESCRIPTOR),
	});
}

export function useMediaMenuData(props: MediaMenuDataProps, options: MediaMenuDataOptions): MediaMenuData {
	const {i18n} = useLingui();
	const {onClose} = options;
	const {
		message,
		originalSrc,
		proxyURL,
		type,
		contentHash,
		attachmentId,
		embedIndex,
		defaultName,
		defaultAltText,
		naturalWidth,
		naturalHeight,
		snapshotIndex,
	} = props;
	const snapshotAttachments =
		snapshotIndex !== undefined ? (message.messageSnapshots?.[snapshotIndex]?.attachments ?? []) : null;
	const memes = useSyncExternalStore(
		(listener) => {
			const dispose = autorun(listener);
			return () => dispose();
		},
		() => FavoriteMemes.memes,
	);
	const gifFavoriteState = useSyncExternalStore(
		(listener) => {
			const dispose = autorun(listener);
			return () => dispose();
		},
		() => {
			if (type !== 'gif' && type !== 'gifv') return 'none';
			const useSavedMedia = FavoriteGif.saveGifFavoritesAsSavedMedia;
			const hasUrl = FavoriteGif.hasUrl(originalSrc);
			return `${useSavedMedia ? 'sm' : 'url'}:${hasUrl ? '1' : '0'}`;
		},
	);
	const isGifFavoriteMedia = type === 'gif' || type === 'gifv';
	const hasUrlOnlyGifFavorite = isGifFavoriteMedia && gifFavoriteState.endsWith(':1');
	const hasSavedMediaFavorite = contentHash ? FavoriteMemeUtils.isFavorited(memes, {contentHash}) : false;
	const isFavorited = isGifFavoriteMedia
		? FavoriteGifUtils.isGifFavoriteActive({
				hasUrlOnlyFavorite: hasUrlOnlyGifFavorite,
				hasSavedMediaFavorite,
				saveAsSavedMedia: gifFavoriteState.startsWith('sm:'),
			})
		: hasSavedMediaFavorite;
	const currentUserId = Users.currentUserId;
	const canManageMessages = Permission.can(Permissions.MANAGE_MESSAGES, {channelId: message.channelId});
	const canEditAltText = useMemo(() => {
		if (!attachmentId) return false;
		const attachmentSource = snapshotAttachments ?? message.attachments;
		const attachment = attachmentSource.find((att) => att.id === attachmentId);
		const mimeType = attachment?.content_type?.toLowerCase() ?? '';
		const canEditMedia = mimeType.startsWith('image/') || mimeType.startsWith('video/');
		if (!canEditMedia) return false;
		const isMessageAuthor = currentUserId === message.author?.id;
		return isMessageAuthor || canManageMessages;
	}, [attachmentId, canManageMessages, currentUserId, message, snapshotAttachments]);
	const handleAddToFavorites = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<AddFavoriteMemeModal
					channelId={message.channelId}
					messageId={message.id}
					attachmentId={attachmentId}
					embedIndex={embedIndex}
					defaultName={
						defaultName ||
						FavoriteMemeUtils.deriveDefaultNameFromEmbedMedia(i18n, {
							url: originalSrc,
							proxy_url: originalSrc,
							flags: 0,
						})
					}
					defaultAltText={defaultAltText}
					data-flx="ui.action-menu.items.media-menu-data.handle-add-to-favorites.add-favorite-meme-modal"
				/>
			)),
		);
	}, [message, attachmentId, embedIndex, defaultName, defaultAltText, originalSrc, onClose, i18n]);
	const handleAddToUrlOnlyGifFavorites = useCallback(() => {
		if (!originalSrc) return;
		FavoriteGifCommands.addFavoriteGifFromMedia(i18n, {
			url: originalSrc,
			proxyUrl: proxyURL ?? originalSrc,
			width: naturalWidth ?? 0,
			height: naturalHeight ?? 0,
			media: {},
			placeholder: null,
		});
		onClose();
	}, [i18n, naturalHeight, naturalWidth, onClose, originalSrc, proxyURL]);
	const handleRemoveFromUrlOnlyGifFavorites = useCallback(() => {
		if (!originalSrc) return;
		FavoriteGifCommands.removeFavoriteGifByUrl(i18n, originalSrc);
		onClose();
	}, [i18n, onClose, originalSrc]);
	const handleRemoveFromFavorites = useCallback(async () => {
		if (!contentHash) return;
		const meme = memes.find((m) => m.contentHash === contentHash);
		if (!meme) return;
		await FavoriteMemeCommands.deleteFavoriteMeme(i18n, meme.id);
		onClose();
	}, [contentHash, memes, onClose, i18n]);
	const handleEditAltText = useCallback(() => {
		if (!canEditAltText || !attachmentId) return;
		const attachmentSource = snapshotAttachments ?? message.attachments;
		const attachment = attachmentSource.find((att) => att.id === attachmentId);
		const currentDescription = attachment?.description ?? null;
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<EditAltTextModal
					message={message}
					attachmentId={attachmentId}
					currentDescription={currentDescription}
					snapshotIndex={snapshotIndex}
					onClose={() => ModalCommands.pop()}
					data-flx="ui.action-menu.items.media-menu-data.handle-edit-alt-text.edit-alt-text-modal"
				/>
			)),
		);
	}, [canEditAltText, message, attachmentId, onClose, snapshotAttachments, snapshotIndex]);
	const handleCopyMedia = useCallback(async () => {
		await copyMediaToClipboard({i18n, originalSrc, proxyURL, type, defaultName});
		onClose();
	}, [originalSrc, proxyURL, type, onClose, i18n, defaultName]);
	const handleDownloadMedia = useCallback(() => {
		if (!originalSrc) {
			showMediaErrorModal(
				i18n,
				ATTACHMENT_IS_EXPIRED_OR_UNAVAILABLE_DESCRIPTOR,
				'ui.media-menu-data.download-media-expired-error-modal',
				true,
			);
			onClose();
			return;
		}
		const mediaType: 'image' | 'gif' | 'video' | 'audio' | 'file' = (() => {
			if (type === 'video' || type === 'gifv') return 'video';
			if (type === 'gif') return 'gif';
			if (type === 'audio') return 'audio';
			if (type === 'file') return 'file';
			return 'image';
		})();
		const baseProxyURL = proxyURL ? stripMediaProxyParams(proxyURL) : null;
		const urlToSave = baseProxyURL || originalSrc;
		createDownloadHandler(urlToSave, mediaType)();
		onClose();
	}, [originalSrc, proxyURL, type, onClose, i18n]);
	const handleCopyLink = useCallback(async () => {
		await copyMediaLinkToClipboard({i18n, originalSrc});
		onClose();
	}, [originalSrc, onClose, i18n]);
	const handleOpenLink = useCallback(() => {
		if (!originalSrc) {
			showMediaErrorModal(
				i18n,
				ATTACHMENT_IS_EXPIRED_OR_UNAVAILABLE_DESCRIPTOR,
				'ui.media-menu-data.open-link-expired-error-modal',
				true,
			);
			onClose();
			return;
		}
		void openExternalUrl(originalSrc);
		onClose();
	}, [originalSrc, onClose, i18n]);
	const handleCopyAttachmentId = useCallback(async () => {
		if (!attachmentId) return;
		await TextCopyCommands.copy(i18n, attachmentId, true);
		ToastCommands.createToast({
			type: 'success',
			children: i18n._(ATTACHMENT_ID_COPIED_TO_CLIPBOARD_DESCRIPTOR),
		});
		onClose();
	}, [attachmentId, onClose, i18n]);
	const copyLabel = useMemo(() => getCopyLabel(type, i18n), [type, i18n.locale]);
	const downloadLabel = useMemo(() => getDownloadLabel(type, i18n), [type, i18n.locale]);
	const copyLinkLabel = useMemo(() => getCopyLinkLabel(type, i18n), [type, i18n.locale]);
	const openLinkLabel = useMemo(() => getOpenLinkLabel(type, i18n), [type, i18n.locale]);
	const handlers: MediaMenuHandlers = useMemo(
		() => ({
			handleAddToFavorites,
			handleRemoveFromFavorites,
			handleEditAltText,
			handleCopyMedia,
			handleDownloadMedia,
			handleCopyLink,
			handleOpenLink,
			handleCopyAttachmentId,
			canEditAltText,
			canCopyAttachmentId: Boolean(attachmentId),
			copyLinkLabel,
			openLinkLabel,
		}),
		[
			handleAddToFavorites,
			handleRemoveFromFavorites,
			handleEditAltText,
			handleCopyMedia,
			handleDownloadMedia,
			handleCopyLink,
			handleOpenLink,
			handleCopyAttachmentId,
			canEditAltText,
			attachmentId,
			copyLinkLabel,
			openLinkLabel,
		],
	);
	const state: MediaMenuState = useMemo(
		() => ({
			isFavorited,
			copyLabel,
			downloadLabel,
		}),
		[isFavorited, copyLabel, downloadLabel],
	);
	const groups = useMemo(() => {
		const result: Array<MenuGroupType> = [];
		if (isGifFavoriteMedia) {
			result.push({
				items: [
					hasUrlOnlyGifFavorite
						? {
								id: `${mediaMenuItemIds.favorite}-url`,
								icon: (
									<FavoriteIcon
										filled
										size={20}
										data-flx="ui.action-menu.items.media-menu-data.groups.favorite-icon.url-remove"
									/>
								),
								label: i18n._(REMOVE_FROM_URL_ONLY_GIF_FAVORITES_DESCRIPTOR),
								onClick: handleRemoveFromUrlOnlyGifFavorites,
							}
						: {
								id: `${mediaMenuItemIds.favorite}-url`,
								icon: (
									<FavoriteIcon
										size={20}
										data-flx="ui.action-menu.items.media-menu-data.groups.favorite-icon.url-add"
									/>
								),
								label: i18n._(ADD_TO_URL_ONLY_GIF_FAVORITES_DESCRIPTOR),
								onClick: handleAddToUrlOnlyGifFavorites,
							},
					hasSavedMediaFavorite
						? {
								id: `${mediaMenuItemIds.favorite}-saved-media`,
								icon: (
									<FavoriteIcon
										filled
										size={20}
										data-flx="ui.action-menu.items.media-menu-data.groups.favorite-icon.saved-media-remove"
									/>
								),
								label: i18n._(REMOVE_FROM_SAVED_MEDIA_DESCRIPTOR),
								onClick: handleRemoveFromFavorites,
							}
						: {
								id: `${mediaMenuItemIds.favorite}-saved-media`,
								icon: (
									<FavoriteIcon
										size={20}
										data-flx="ui.action-menu.items.media-menu-data.groups.favorite-icon.saved-media-add"
									/>
								),
								label: i18n._(ADD_TO_SAVED_MEDIA_DESCRIPTOR),
								onClick: handleAddToFavorites,
							},
				],
			});
		} else {
			result.push({
				items: [
					isFavorited
						? {
								id: mediaMenuItemIds.favorite,
								icon: (
									<FavoriteIcon filled size={20} data-flx="ui.action-menu.items.media-menu-data.groups.favorite-icon" />
								),
								label: i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR),
								onClick: handleRemoveFromFavorites,
							}
						: {
								id: mediaMenuItemIds.favorite,
								icon: (
									<FavoriteIcon size={20} data-flx="ui.action-menu.items.media-menu-data.groups.favorite-icon--2" />
								),
								label: i18n._(ADD_TO_FAVORITES_DESCRIPTOR),
								onClick: handleAddToFavorites,
							},
				],
			});
		}
		if (canEditAltText) {
			result.push({
				items: [
					{
						id: mediaMenuItemIds.editAltText,
						icon: (
							<PencilSimpleIcon
								size={remFromPx(20)}
								data-flx="ui.action-menu.items.media-menu-data.groups.pencil-simple-icon"
							/>
						),
						label: i18n._(EDIT_ALT_TEXT_DESCRIPTOR),
						onClick: handleEditAltText,
					},
				],
			});
		}
		result.push({
			items: [
				{
					id: mediaMenuItemIds.copy,
					icon: <CopyMediaIcon size={20} data-flx="ui.action-menu.items.media-menu-data.groups.copy-media-icon" />,
					label: copyLabel,
					onClick: handleCopyMedia,
				},
				{
					id: mediaMenuItemIds.download,
					icon: (
						<DownloadMediaIcon size={20} data-flx="ui.action-menu.items.media-menu-data.groups.download-media-icon" />
					),
					label: downloadLabel,
					onClick: handleDownloadMedia,
				},
				...(type === 'file'
					? []
					: [
							{
								id: mediaMenuItemIds.copyLink,
								icon: <CopyLinkIcon size={20} data-flx="ui.action-menu.items.media-menu-data.groups.copy-link-icon" />,
								label: copyLinkLabel,
								onClick: handleCopyLink,
							},
						]),
				{
					id: mediaMenuItemIds.openLink,
					icon: (
						<OpenMediaLinkIcon size={20} data-flx="ui.action-menu.items.media-menu-data.groups.open-media-link-icon" />
					),
					label: openLinkLabel,
					onClick: handleOpenLink,
				},
			],
		});
		return result;
	}, [
		isFavorited,
		copyLabel,
		downloadLabel,
		copyLinkLabel,
		openLinkLabel,
		type,
		message,
		isGifFavoriteMedia,
		hasUrlOnlyGifFavorite,
		hasSavedMediaFavorite,
		handleAddToFavorites,
		handleAddToUrlOnlyGifFavorites,
		handleRemoveFromUrlOnlyGifFavorites,
		handleRemoveFromFavorites,
		handleEditAltText,
		handleCopyMedia,
		handleDownloadMedia,
		handleCopyLink,
		handleOpenLink,
		canEditAltText,
		i18n.locale,
	]);
	return {
		groups,
		handlers,
		state,
	};
}
