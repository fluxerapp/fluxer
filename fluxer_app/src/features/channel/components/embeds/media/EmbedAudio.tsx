// SPDX-License-Identifier: AGPL-3.0-or-later

import {deriveDefaultNameFromMessage} from '@app/features/channel/components/embeds/EmbedUtils';
import styles from '@app/features/channel/components/embeds/media/EmbedAudio.module.css';
import {getMediaButtonVisibility} from '@app/features/channel/components/embeds/media/MediaButtonUtils';
import type {BaseMediaProps} from '@app/features/channel/components/embeds/media/MediaTypes';
import {useMaybeMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import {DELETE_ATTACHMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {useDeleteAttachment} from '@app/features/messaging/hooks/useDeleteAttachment';
import {useMediaFavorite} from '@app/features/messaging/hooks/useMediaFavorite';
import {createDownloadHandler} from '@app/features/messaging/utils/FileDownloadUtils';
import {buildMediaProxyURL} from '@app/features/messaging/utils/MediaProxyUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import messageStyles from '@app/features/theme/styles/Message.module.css';
import {MediaContextMenu} from '@app/features/ui/action_menu/MediaContextMenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {InlineAudioPlayer} from '@app/features/voice/components/media_player/components/InlineAudioPlayer';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {TrashIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type FC, useCallback} from 'react';

const DELETE_DESCRIPTOR = msg({
	message: 'Delete',
	comment:
		'Button or menu action label in the channel and chat embed audio. Keep it concise. Keep the tone plain and specific.',
});

type EmbedAudioProps = BaseMediaProps & {
	src: string;
	title?: string;
	duration?: number;
	embedUrl?: string;
	fileSize?: number;
	mediaAttachments?: ReadonlyArray<MessageAttachment>;
	isPreview?: boolean;
	snapshotIndex?: number;
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
		isPreview,
		snapshotIndex,
	}) => {
		const {i18n} = useLingui();
		const messageViewContext = useMaybeMessageViewContext();
		const effectiveSrc = buildMediaProxyURL(src);
		const {enabled: isMobile} = MobileLayout;
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
				if (isPreview && snapshotIndex === undefined) return;
				e.preventDefault();
				e.stopPropagation();
				ContextMenuCommands.openFromEvent(e, ({onClose}) => (
					<MediaContextMenu
						message={message}
						sourceChannel={messageViewContext?.channel}
						originalSrc={src}
						type="audio"
						contentHash={contentHash}
						attachmentId={attachmentId}
						defaultName={defaultName}
						snapshotIndex={snapshotIndex}
						onClose={onClose}
						onDelete={onDelete || (() => {})}
						data-flx="channel.embeds.media.embed-audio.handle-context-menu.media-context-menu.audio"
					/>
				));
			},
			[
				message,
				messageViewContext?.channel,
				src,
				contentHash,
				attachmentId,
				defaultName,
				onDelete,
				isPreview,
				snapshotIndex,
			],
		);
		const handleDownload = useCallback(
			(e: React.MouseEvent) => {
				e.preventDefault();
				e.stopPropagation();
				createDownloadHandler(src, 'audio')();
			},
			[src],
		);
		const handleDeleteClick = useDeleteAttachment(message, attachmentId);
		const containerStyles: React.CSSProperties = isMobile
			? {
					display: 'grid',
					width: '100%',
					maxWidth: '100%',
					minWidth: 0,
				}
			: {
					display: 'grid',
					width: '100%',
					maxWidth: 'min(100%, 400px)',
					minWidth: 'min(400px, 100%)',
				};
		const {showDeleteButton, showDownloadButton} = getMediaButtonVisibility(
			canFavorite,
			isPreview ? undefined : message,
			attachmentId,
			{disableDelete: !!isPreview || snapshotIndex !== undefined},
		);
		return (
			<div style={containerStyles} className={styles.container} data-flx="channel.embeds.media.embed-audio.container">
				{showDeleteButton && (
					<Tooltip text={i18n._(DELETE_DESCRIPTOR)} position="top" data-flx="channel.embeds.media.embed-audio.tooltip">
						<button
							type="button"
							onClick={handleDeleteClick}
							className={clsx(messageStyles.hoverAction, styles.deleteButton)}
							aria-label={i18n._(DELETE_ATTACHMENT_DESCRIPTOR)}
							data-flx="channel.embeds.media.embed-audio.delete-button.delete-click"
						>
							<TrashIcon size={remFromPx(16)} weight="bold" data-flx="channel.embeds.media.embed-audio.trash-icon" />
						</button>
					</Tooltip>
				)}
				<InlineAudioPlayer
					src={effectiveSrc}
					title={defaultName}
					fileSize={fileSize}
					duration={apiDuration}
					isMobile={isMobile}
					isFavorited={isFavorited}
					canFavorite={canFavorite}
					onFavoriteClick={handleFavoriteClick}
					onDownloadClick={showDownloadButton ? handleDownload : undefined}
					onContextMenu={handleContextMenu}
					data-flx="channel.embeds.media.embed-audio.inline-audio-player.context-menu"
				/>
			</div>
		);
	},
);

export default EmbedAudio;
