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
import {FileAudioIcon, PlayIcon, TrashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type FC, useCallback, useEffect, useRef, useState} from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as MediaViewerActionCreators from '~/actions/MediaViewerActionCreators';
import {deriveDefaultNameFromMessage, splitFilename} from '~/components/channel/embeds/EmbedUtils';
import type {BaseMediaProps} from '~/components/channel/embeds/media/MediaTypes';
import {canDeleteAttachmentUtil} from '~/components/channel/messageActionUtils';
import {InlineAudioPlayer} from '~/components/media-player/components/InlineAudioPlayer';
import {MediaContextMenu} from '~/components/uikit/ContextMenu/MediaContextMenu';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useDeleteAttachment} from '~/hooks/useDeleteAttachment';
import {useMediaFavorite} from '~/hooks/useMediaFavorite';
import type {MessageAttachment} from '~/records/MessageRecord';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import messageStyles from '~/styles/Message.module.css';
import {createSaveHandler} from '~/utils/FileDownloadUtils';
import {formatFileSize} from '~/utils/FileUtils';
import {buildMediaProxyURL} from '~/utils/MediaProxyUtils';
import styles from './EmbedAudio.module.css';

type EmbedAudioProps = BaseMediaProps & {
	src: string;
	title?: string;
	duration?: number;
	embedUrl?: string;
	fileSize?: number;
	mediaAttachments?: ReadonlyArray<MessageAttachment>;
};

const EmbedAudio: FC<EmbedAudioProps> = observer(
	({
		src,
		title,
		duration: apiDuration,
		embedUrl,
		channelId,
		messageId,
		attachmentId,
		embedIndex,
		message,
		contentHash,
		onDelete,
		fileSize,
	}) => {
		const {t} = useLingui();
		const effectiveSrc = buildMediaProxyURL(src);
		const audioRef = useRef<HTMLAudioElement>(null);
		const [duration, setDuration] = useState(apiDuration || 0);
		const {enabled: isMobile} = MobileLayoutStore;

		const defaultName =
			title || deriveDefaultNameFromMessage({message, attachmentId, embedIndex, url: embedUrl || src, proxyUrl: src});

		const {
			isFavorited,
			toggleFavorite: handleFavoriteClick,
			canFavorite,
		} = useMediaFavorite({
			channelId,
			messageId,
			attachmentId,
			embedIndex,
			defaultName,
			contentHash,
		});

		const handleContextMenu = useCallback(
			(e: React.MouseEvent) => {
				if (!message) return;

				e.preventDefault();
				e.stopPropagation();

				ContextMenuActionCreators.openFromEvent(e, ({onClose}) => (
					<MediaContextMenu
						message={message}
						originalSrc={src}
						type="audio"
						contentHash={contentHash}
						attachmentId={attachmentId}
						defaultName={defaultName}
						onClose={onClose}
						onDelete={onDelete || (() => {})}
					/>
				));
			},
			[message, src, contentHash, attachmentId, defaultName, onDelete],
		);

		useEffect(() => {
			if (apiDuration) return;

			const audio = audioRef.current;
			if (!audio) return;

			const handleLoadedMetadata = () => {
				setDuration(audio.duration);
			};

			audio.addEventListener('loadedmetadata', handleLoadedMetadata);

			if (audio.readyState === 0) {
				audio.load();
			}

			return () => {
				audio.removeEventListener('loadedmetadata', handleLoadedMetadata);
			};
		}, [apiDuration]);

		const handlePlayClick = (e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();

			MediaViewerActionCreators.openMediaViewer(
				[
					{
						src: effectiveSrc,
						originalSrc: embedUrl || src,
						naturalWidth: 0,
						naturalHeight: 0,
						type: 'audio',
						contentHash,
						attachmentId,
						embedIndex,
						filename: title || defaultName,
						fileSize,
						duration,
					},
				],
				0,
				{
					channelId,
					messageId,
					message,
				},
			);
		};

		const handleDownload = (e: React.MouseEvent) => {
			e.preventDefault();
			e.stopPropagation();
			createSaveHandler(src, 'audio')();
		};

		const handleDeleteClick = useDeleteAttachment(message, attachmentId);

		const formatTime = (time: number) => {
			if (!Number.isFinite(time)) return '0:00';
			const minutes = Math.floor(time / 60);
			const seconds = Math.floor(time % 60);
			return `${minutes}:${seconds.toString().padStart(2, '0')}`;
		};

		const {name: fileName, extension: fileExtension} = splitFilename(defaultName);

		const containerStyles: React.CSSProperties = isMobile
			? {
					display: 'grid',
					width: '100%',
					maxWidth: '100%',
					minWidth: 0,
				}
			: {
					display: 'grid',
					width: '400px',
					maxWidth: '400px',
				};

		const canDelete = canDeleteAttachmentUtil(message) && !isMobile;

		if (isMobile) {
			return (
				<div style={containerStyles} className={styles.container}>
					{/* biome-ignore lint/a11y/useMediaCaption: Audio embed doesn't require captions */}
					<audio ref={audioRef} src={effectiveSrc} preload="none" aria-label={title || t`Embedded audio`} />
					<button
						type="button"
						onClick={handlePlayClick}
						onContextMenu={handleContextMenu}
						className={styles.mobilePlayButton}
						aria-label={t`Play audio`}
					>
						<div className={styles.mobileIconContainer}>
							<FileAudioIcon size={32} />
						</div>
						<div className={styles.fileInfoContainer}>
							<p className={styles.fileName}>
								<span className={styles.fileNameTruncate}>{fileName}</span>
								<span className={styles.fileExtension}>{fileExtension}</span>
							</p>
							<p className={styles.fileSize}>{fileSize ? formatFileSize(fileSize) : formatTime(duration)}</p>
						</div>
						<div className={styles.mobilePlayIconContainer}>
							<PlayIcon size={20} weight="fill" />
						</div>
					</button>
				</div>
			);
		}

		return (
			<div style={containerStyles} className={styles.container}>
				{canDelete && (
					<Tooltip text={t`Delete`} position="top">
						<button
							type="button"
							onClick={handleDeleteClick}
							className={clsx(messageStyles.hoverAction, styles.deleteButton)}
							aria-label={t`Delete attachment`}
						>
							<TrashIcon size={16} weight="bold" />
						</button>
					</Tooltip>
				)}
				<InlineAudioPlayer
					src={effectiveSrc}
					title={defaultName}
					fileSize={fileSize}
					duration={apiDuration || duration}
					isFavorited={isFavorited}
					canFavorite={canFavorite}
					onFavoriteClick={handleFavoriteClick}
					onDownloadClick={handleDownload}
					onContextMenu={handleContextMenu}
				/>
			</div>
		);
	},
);

export default EmbedAudio;
