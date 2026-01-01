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

import type {I18n} from '@lingui/core';
import {t} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {autorun} from 'mobx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useSyncExternalStore} from 'react';
import {clearAllAttachmentMocks, setAttachmentMock} from '~/actions/DeveloperOptionsActionCreators';
import * as FavoriteMemeActionCreators from '~/actions/FavoriteMemeActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {AddFavoriteMemeModal} from '~/components/modals/AddFavoriteMemeModal';
import type {MessageRecord} from '~/records/MessageRecord';
import DeveloperModeStore from '~/stores/DeveloperModeStore';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import FavoriteMemeStore from '~/stores/FavoriteMemeStore';
import * as FavoriteMemeUtils from '~/utils/FavoriteMemeUtils';
import {createSaveHandler} from '~/utils/FileDownloadUtils';
import {buildMediaProxyURL, stripMediaProxyParams} from '~/utils/MediaProxyUtils';
import {openExternalUrl} from '~/utils/NativeUtils';
import {
	CopyIcon,
	CopyIdIcon,
	CopyLinkIcon,
	FavoriteIcon,
	OpenLinkIcon,
	SaveIcon,
	WrenchToolIcon,
} from './ContextMenuIcons';
import {MenuGroup} from './MenuGroup';
import {MenuItem} from './MenuItem';
import {MenuItemSubmenu} from './MenuItemSubmenu';
import {MessageContextMenu} from './MessageContextMenu';

type MediaType = 'image' | 'gif' | 'gifv' | 'video' | 'audio' | 'file';

interface MediaContextMenuProps {
	message: MessageRecord;
	originalSrc: string;
	proxyURL?: string;
	type: MediaType;
	contentHash?: string | null;
	attachmentId?: string;
	embedIndex?: number;
	defaultName?: string;
	defaultAltText?: string;
	onClose: () => void;
	onDelete: (bypassConfirm?: boolean) => void;
}

export const MediaContextMenu: React.FC<MediaContextMenuProps> = observer(
	({
		message,
		originalSrc,
		proxyURL,
		type,
		contentHash,
		attachmentId,
		embedIndex,
		defaultName,
		defaultAltText,
		onClose,
		onDelete,
	}) => {
		const {i18n} = useLingui();

		const memes = useSyncExternalStore(
			(listener) => {
				const dispose = autorun(listener);
				return () => dispose();
			},
			() => FavoriteMemeStore.memes,
		);

		const isFavorited = contentHash ? memes.some((meme) => meme.contentHash === contentHash) : false;

		const handleAddToFavorites = useCallback(() => {
			ModalActionCreators.push(
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
					/>
				)),
			);
			onClose();
		}, [message, attachmentId, embedIndex, defaultName, defaultAltText, originalSrc, onClose]);

		const handleRemoveFromFavorites = useCallback(async () => {
			if (!contentHash) return;

			const meme = memes.find((m) => m.contentHash === contentHash);
			if (!meme) return;

			await FavoriteMemeActionCreators.deleteFavoriteMeme(i18n, meme.id);
			onClose();
		}, [contentHash, memes, onClose, i18n]);

		const handleCopyMedia = useCallback(async () => {
			if (!originalSrc) {
				ToastActionCreators.createToast({
					type: 'error',
					children: t(i18n)`Attachment is expired or unavailable`,
				});
				onClose();
				return;
			}

			if (type === 'video' || type === 'gifv' || type === 'gif' || type === 'audio' || type === 'file') {
				await TextCopyActionCreators.copy(i18n, originalSrc, true);
				ToastActionCreators.createToast({
					type: 'success',
					children: type === 'file' ? t(i18n)`Link copied to clipboard` : t(i18n)`URL copied to clipboard`,
				});
				onClose();
				return;
			}

			const baseProxyURL = proxyURL ? stripMediaProxyParams(proxyURL) : null;
			const pngUrl = baseProxyURL ? buildMediaProxyURL(baseProxyURL, {format: 'png'}) : null;
			const urlToFetch = pngUrl || originalSrc;
			let toastId: string | null = null;

			try {
				toastId = ToastActionCreators.createToast({
					type: 'info',
					children: t(i18n)`Copying image...`,
					timeout: 0,
				});

				const response = await fetch(urlToFetch);
				const blob = await response.blob();

				if (blob.type !== 'image/png') {
					throw new Error('Image is not PNG format, falling back to URL copy');
				}

				await navigator.clipboard.write([
					new ClipboardItem({
						'image/png': blob,
					}),
				]);

				if (toastId) ToastActionCreators.destroyToast(toastId);
				ToastActionCreators.createToast({
					type: 'success',
					children: t(i18n)`Image copied to clipboard`,
				});
				onClose();
			} catch (error) {
				console.error('Failed to copy image to clipboard:', error);
				if (toastId) ToastActionCreators.destroyToast(toastId);

				await TextCopyActionCreators.copy(i18n, originalSrc, true);
				ToastActionCreators.createToast({
					type: 'success',
					children: t(i18n)`URL copied to clipboard`,
				});
				onClose();
			}
		}, [originalSrc, proxyURL, type, onClose, i18n]);

		const handleSaveMedia = useCallback(() => {
			if (!originalSrc) {
				ToastActionCreators.createToast({
					type: 'error',
					children: t(i18n)`Attachment is expired or unavailable`,
				});
				onClose();
				return;
			}

			const mediaType: 'image' | 'video' | 'audio' | 'file' =
				type === 'video' || type === 'gifv' ? 'video' : type === 'audio' ? 'audio' : type === 'file' ? 'file' : 'image';

			const baseProxyURL = proxyURL ? stripMediaProxyParams(proxyURL) : null;
			const urlToSave = baseProxyURL || originalSrc;

			createSaveHandler(urlToSave, mediaType)();
			onClose();
		}, [originalSrc, proxyURL, type, onClose, i18n]);

		const handleCopyLink = useCallback(async () => {
			if (!originalSrc) {
				ToastActionCreators.createToast({
					type: 'error',
					children: t(i18n)`Attachment is expired or unavailable`,
				});
				onClose();
				return;
			}

			await TextCopyActionCreators.copy(i18n, originalSrc, true);
			ToastActionCreators.createToast({
				type: 'success',
				children: t(i18n)`Link copied to clipboard`,
			});
			onClose();
		}, [originalSrc, onClose, i18n]);

		const handleOpenLink = useCallback(() => {
			if (!originalSrc) {
				ToastActionCreators.createToast({
					type: 'error',
					children: t(i18n)`Attachment is expired or unavailable`,
				});
				onClose();
				return;
			}

			void openExternalUrl(originalSrc);
			onClose();
		}, [originalSrc, onClose, i18n]);

		const handleCopyAttachmentId = useCallback(async () => {
			if (!attachmentId) return;

			await TextCopyActionCreators.copy(i18n, attachmentId, true);
			ToastActionCreators.createToast({
				type: 'success',
				children: t(i18n)`Attachment ID copied to clipboard`,
			});
			onClose();
		}, [attachmentId, onClose, i18n]);

		const copyLabel = getCopyLabel(type, i18n);
		const saveLabel = getSaveLabel(type, i18n);

		const isDev = DeveloperModeStore.isDeveloper;
		const currentMock = attachmentId ? DeveloperOptionsStore.mockAttachmentStates[attachmentId] : undefined;

		const mockExpiresSoon = () =>
			setAttachmentMock(attachmentId!, {
				expired: false,
				expiresAt: new Date(Date.now() + 86400000).toISOString(),
			});
		const mockExpiresWeek = () =>
			setAttachmentMock(attachmentId!, {
				expired: false,
				expiresAt: new Date(Date.now() + 7 * 86400000).toISOString(),
			});
		const mockExpired = () =>
			setAttachmentMock(attachmentId!, {
				expired: true,
				expiresAt: new Date(Date.now() - 3600000).toISOString(),
			});
		const clearMock = () => setAttachmentMock(attachmentId!, null);

		return (
			<>
				<MenuGroup>
					{isFavorited ? (
						<MenuItem icon={<FavoriteIcon filled />} onClick={handleRemoveFromFavorites}>
							{t(i18n)`Remove from Favorites`}
						</MenuItem>
					) : (
						<MenuItem icon={<FavoriteIcon />} onClick={handleAddToFavorites}>
							{t(i18n)`Add to Favorites`}
						</MenuItem>
					)}
				</MenuGroup>

				<MenuGroup>
					<MenuItem icon={<CopyIcon />} onClick={handleCopyMedia}>
						{copyLabel}
					</MenuItem>
					<MenuItem icon={<SaveIcon />} onClick={handleSaveMedia}>
						{saveLabel}
					</MenuItem>
				</MenuGroup>

				<MenuGroup>
					<MenuItem icon={<CopyLinkIcon />} onClick={handleCopyLink}>
						{t(i18n)`Copy Link`}
					</MenuItem>
					<MenuItem icon={<OpenLinkIcon />} onClick={handleOpenLink}>
						{t(i18n)`Open Link`}
					</MenuItem>
				</MenuGroup>

				{attachmentId && (
					<MenuGroup>
						<MenuItem icon={<CopyIdIcon />} onClick={handleCopyAttachmentId}>
							{t(i18n)`Copy Attachment ID`}
						</MenuItem>
					</MenuGroup>
				)}

				{isDev && attachmentId && (
					<MenuGroup>
						<MenuItemSubmenu
							label={t(i18n)`Attachment Mock`}
							icon={<WrenchToolIcon />}
							render={() => (
								<>
									<MenuItem onClick={mockExpiresSoon}>{t(i18n)`Mock expires in 1 day`}</MenuItem>
									<MenuItem onClick={mockExpiresWeek}>{t(i18n)`Mock expires in 7 days`}</MenuItem>
									<MenuItem onClick={mockExpired}>{t(i18n)`Mock expired`}</MenuItem>
									{currentMock && <MenuItem onClick={clearMock}>{t(i18n)`Clear mock for this attachment`}</MenuItem>}
									<MenuItem onClick={clearAllAttachmentMocks}>{t(i18n)`Clear all attachment mocks`}</MenuItem>
								</>
							)}
						/>
					</MenuGroup>
				)}

				<MessageContextMenu message={message} onClose={onClose} onDelete={onDelete} />
			</>
		);
	},
);

function getCopyLabel(type: MediaType, i18n: I18n): string {
	switch (type) {
		case 'image':
			return t(i18n)`Copy Image`;
		case 'gif':
		case 'gifv':
			return t(i18n)`Copy GIF`;
		case 'video':
			return t(i18n)`Copy Video`;
		case 'audio':
			return t(i18n)`Copy Audio`;
		case 'file':
			return t(i18n)`Copy File Link`;
		default:
			return t(i18n)`Copy Media`;
	}
}

function getSaveLabel(type: MediaType, i18n: I18n): string {
	switch (type) {
		case 'image':
			return t(i18n)`Save Image`;
		case 'gif':
		case 'gifv':
			return t(i18n)`Save GIF`;
		case 'video':
			return t(i18n)`Save Video`;
		case 'audio':
			return t(i18n)`Save Audio`;
		case 'file':
			return t(i18n)`Save File`;
		default:
			return t(i18n)`Save Media`;
	}
}
