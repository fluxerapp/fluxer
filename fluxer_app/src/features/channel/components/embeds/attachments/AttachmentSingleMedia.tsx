// SPDX-License-Identifier: AGPL-3.0-or-later

import {SpoilerOverlay} from '@app/features/app/components/shared/SpoilerOverlay';
import spoilerStyles from '@app/features/app/components/shared/SpoilerOverlay.module.css';
import {EmbedGif, EmbedGifv} from '@app/features/channel/components/embeds/media/EmbedGifv';
import {EmbedImage} from '@app/features/channel/components/embeds/media/EmbedImage';
import EmbedVideo from '@app/features/channel/components/embeds/media/EmbedVideo';
import {getInlineVideoLayoutConstraints} from '@app/features/channel/components/embeds/media/VideoDimensionUtils';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {getAttachmentMediaDimensions} from '@app/features/messaging/utils/MediaDimensionConfig';
import {resolveProxyRequestSize} from '@app/features/messaging/utils/MediaProxyRequestSize';
import {
	buildAnimatedImageProxyURL,
	buildMediaProxyURL,
	resolvePreferredImageFormat,
} from '@app/features/messaging/utils/MediaProxyUtils';
import {determineMediaType} from '@app/features/messaging/utils/MediaViewerItemUtils';
import {useSpoilerState} from '@app/features/messaging/utils/SpoilerUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/theme/styles/AttachmentSingleMedia.module.css';
import {createCalculator} from '@app/features/ui/utils/DimensionUtils';
import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {observer} from 'mobx-react-lite';
import type {CSSProperties, FC, ReactElement} from 'react';

export interface AttachmentSingleMediaProps {
	attachment: MessageAttachment;
	message?: Message;
	mediaAttachments: ReadonlyArray<MessageAttachment>;
	isPreview?: boolean;
	snapshotIndex?: number;
	onDelete?: (bypassConfirm?: boolean) => void;
}

interface AttachmentMediaConstraintStyle extends CSSProperties {
	'--attachment-media-max-height': string;
	'--attachment-media-max-width': string;
}

export const AttachmentSingleMedia: FC<AttachmentSingleMediaProps> = observer(
	({attachment, message, mediaAttachments, isPreview, snapshotIndex, onDelete}) => {
		const mediaType = determineMediaType(attachment);
		const isGifv = mediaType === 'gifv';
		const isVideo = mediaType === 'video';
		const isAnimatedGif = mediaType === 'gif';
		const isSpoiler = (attachment.flags & MessageAttachmentFlags.IS_SPOILER) !== 0;
		const nsfw = attachment.nsfw || (attachment.flags & MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA) !== 0;
		const {hidden: spoilerHidden, reveal: revealSpoiler} = useSpoilerState(isSpoiler, message?.channelId);
		const wrapSpoiler = (node: ReactElement) =>
			isSpoiler ? (
				<SpoilerOverlay
					hidden={spoilerHidden}
					onReveal={revealSpoiler}
					className={spoilerStyles.media}
					data-flx="channel.embeds.attachments.attachment-single-media.wrap-spoiler.spoiler-overlay"
				>
					{node}
				</SpoilerOverlay>
			) : (
				node
			);
		const naturalWidth = attachment.width!;
		const naturalHeight = attachment.height!;
		const attachmentDimensions = getAttachmentMediaDimensions();
		const videoLayoutConstraints = getInlineVideoLayoutConstraints(attachmentDimensions);
		const standaloneMediaCalculator = createCalculator({
			maxWidth: attachmentDimensions.maxWidth,
			maxHeight: attachmentDimensions.maxHeight,
			responsive: true,
		});
		const mediaConstraintStyle: AttachmentMediaConstraintStyle = {
			'--attachment-media-max-height': remFromPx(attachmentDimensions.maxHeight),
			'--attachment-media-max-width': remFromPx(attachmentDimensions.maxWidth),
		};
		const {dimensions} = standaloneMediaCalculator.calculate({
			width: naturalWidth,
			height: naturalHeight,
		});
		const safeProxy = attachment.proxy_url ?? attachment.url ?? '';
		const safeUrl = attachment.url ?? '';
		const commonProps = {
			channelId: message?.channelId,
			messageId: message?.id,
			attachmentId: attachment.id,
			message,
			contentHash: attachment.content_hash,
			placeholder: attachment.placeholder,
			nsfw,
			onDelete,
			snapshotIndex,
		};
		if (isGifv) {
			return wrapSpoiler(
				<div
					className={styles.relativeWrapper}
					style={mediaConstraintStyle}
					data-flx="channel.embeds.attachments.attachment-single-media.relative-wrapper"
				>
					<div
						className={styles.singleMediaContainer}
						data-flx="channel.embeds.attachments.attachment-single-media.single-media-container"
					>
						<EmbedGifv
							data-flx="channel.embeds.attachments.attachment-single-media.embed-gifv"
							{...commonProps}
							embedURL={safeUrl}
							videoProxyURL={safeProxy}
							videoURL={safeUrl}
							naturalWidth={naturalWidth}
							naturalHeight={naturalHeight}
							isPreview={isPreview}
							alt={attachment.description ?? undefined}
						/>
					</div>
				</div>,
			);
		}
		if (isVideo) {
			return wrapSpoiler(
				<div
					className={styles.relativeWrapper}
					style={mediaConstraintStyle}
					data-flx="channel.embeds.attachments.attachment-single-media.relative-wrapper--2"
				>
					<div
						className={styles.singleMediaContainer}
						data-flx="channel.embeds.attachments.attachment-single-media.single-media-container--2"
					>
						<EmbedVideo
							data-flx="channel.embeds.attachments.attachment-single-media.embed-video"
							{...commonProps}
							src={safeProxy}
							width={naturalWidth}
							height={naturalHeight}
							maxWidth={videoLayoutConstraints.maxWidth}
							maxHeight={videoLayoutConstraints.maxHeight}
							title={attachment.title || attachment.filename}
							alt={attachment.description ?? undefined}
							mediaAttachments={mediaAttachments}
							isPreview={isPreview}
						/>
					</div>
				</div>,
			);
		}
		if (isAnimatedGif) {
			return wrapSpoiler(
				<div
					className={styles.relativeWrapper}
					style={mediaConstraintStyle}
					data-flx="channel.embeds.attachments.attachment-single-media.relative-wrapper--3"
				>
					<div
						className={styles.singleMediaContainer}
						data-flx="channel.embeds.attachments.attachment-single-media.single-media-container--3"
					>
						<EmbedGif
							data-flx="channel.embeds.attachments.attachment-single-media.embed-gif"
							{...commonProps}
							embedURL={safeUrl}
							proxyURL={buildAnimatedImageProxyURL(safeProxy)}
							naturalWidth={naturalWidth}
							naturalHeight={naturalHeight}
							isPreview={isPreview}
							alt={attachment.description ?? undefined}
						/>
					</div>
				</div>,
			);
		}
		const requestedSize = resolveProxyRequestSize(dimensions.width, dimensions.height, naturalWidth, naturalHeight);
		const optimizedSrc = buildMediaProxyURL(attachment.proxy_url ?? attachment.url ?? '', {
			format: resolvePreferredImageFormat(attachment.content_type),
			width: requestedSize?.width,
			height: requestedSize?.height,
		});
		return wrapSpoiler(
			<div
				className={styles.relativeWrapper}
				style={mediaConstraintStyle}
				data-flx="channel.embeds.attachments.attachment-single-media.relative-wrapper--4"
			>
				<div
					className={styles.singleMediaContainer}
					data-flx="channel.embeds.attachments.attachment-single-media.single-media-container--4"
				>
					<EmbedImage
						data-flx="channel.embeds.attachments.attachment-single-media.embed-image"
						{...commonProps}
						src={optimizedSrc}
						originalSrc={safeUrl}
						naturalWidth={naturalWidth}
						naturalHeight={naturalHeight}
						width={dimensions.width}
						height={dimensions.height}
						constrain={true}
						mediaAttachments={mediaAttachments}
						isPreview={isPreview}
						animated={isAnimatedGif}
						alt={attachment.description ?? undefined}
					/>
				</div>
			</div>,
		);
	},
);
