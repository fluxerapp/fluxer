// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	calculateMediaDimensions,
	type EmbedMediaRendererProps,
	getUrlHostname,
	isMediaMatureContent,
	isValidMedia,
	mediaPropsEqual,
	resolveEmbedImageSource,
	THUMBNAIL_SIZE,
} from '@app/features/channel/components/embeds/channel_embed/ChannelEmbedShared';
import {EmbedGif} from '@app/features/channel/components/embeds/media/EmbedGifv';
import {EmbedImage} from '@app/features/channel/components/embeds/media/EmbedImage';
import EmbedVideo from '@app/features/channel/components/embeds/media/EmbedVideo';
import {EmbedYouTube} from '@app/features/channel/components/embeds/media/EmbedYouTube';
import {getInlineVideoLayoutConstraints} from '@app/features/channel/components/embeds/media/VideoDimensionUtils';
import {getEmbedMediaDimensions} from '@app/features/messaging/utils/MediaDimensionConfig';
import {buildMediaProxyURL} from '@app/features/messaging/utils/MediaProxyUtils';
import messageStyles from '@app/features/theme/styles/Message.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {observer} from 'mobx-react-lite';
import {type FC, memo} from 'react';

const mediaFocusRingClass = messageStyles.mediaFocusRing;
const EmbedMediaRendererInner: FC<EmbedMediaRendererProps> = observer(
	({embed, message, embedIndex, onDelete, isPreview}) => {
		const {video, image, thumbnail} = embed;
		if (!isValidMedia(video) && !isValidMedia(image) && !isValidMedia(thumbnail)) {
			return null;
		}
		if (isValidMedia(video) && getUrlHostname(embed.provider?.url) === 'www.youtube.com') {
			return (
				<FocusRing
					within
					ringClassName={mediaFocusRingClass}
					data-flx="channel.embeds.embed.embed-media-renderer-inner.focus-ring"
				>
					<EmbedYouTube embed={embed} data-flx="channel.embeds.embed.embed-media-renderer-inner.embed-you-tube" />
				</FocusRing>
			);
		}
		if (isValidMedia(video)) {
			const videoLayoutConstraints = getInlineVideoLayoutConstraints(getEmbedMediaDimensions());
			return (
				<FocusRing
					within
					ringClassName={mediaFocusRingClass}
					data-flx="channel.embeds.embed.embed-media-renderer-inner.focus-ring--2"
				>
					<EmbedVideo
						src={buildMediaProxyURL(video.proxy_url)}
						width={video.width}
						height={video.height}
						maxWidth={videoLayoutConstraints.maxWidth}
						maxHeight={videoLayoutConstraints.maxHeight}
						placeholder={video.placeholder}
						title={embed.title}
						alt={video.description ?? undefined}
						duration={video.duration}
						nsfw={isMediaMatureContent(video)}
						channelId={message.channelId}
						messageId={message.id}
						embedUrl={embed.url}
						message={message}
						contentHash={video.content_hash}
						embedIndex={embedIndex}
						onDelete={onDelete}
						isPreview={isPreview}
						data-flx="channel.embeds.embed.embed-media-renderer-inner.embed-video"
					/>
				</FocusRing>
			);
		}
		if (isValidMedia(image)) {
			const {width, height} = calculateMediaDimensions(image);
			const isGif = image.content_type === 'image/gif' || image.url.toLowerCase().endsWith('.gif');
			if (isGif) {
				return (
					<FocusRing
						within
						ringClassName={mediaFocusRingClass}
						data-flx="channel.embeds.embed.embed-media-renderer-inner.focus-ring--3"
					>
						<EmbedGif
							embedURL={image.url}
							proxyURL={image.proxy_url}
							naturalWidth={image.width}
							naturalHeight={image.height}
							placeholder={image.placeholder}
							alt={image.description ?? embed.description ?? undefined}
							nsfw={isMediaMatureContent(image)}
							channelId={message.channelId}
							messageId={message.id}
							message={message}
							contentHash={image.content_hash}
							embedIndex={embedIndex}
							onDelete={onDelete}
							isPreview={isPreview}
							layoutConstraints={getEmbedMediaDimensions()}
							data-flx="channel.embeds.embed.embed-media-renderer-inner.embed-gif"
						/>
					</FocusRing>
				);
			}
			const resolvedImageSource = resolveEmbedImageSource(image, width, height);
			return (
				<FocusRing
					within
					ringClassName={mediaFocusRingClass}
					data-flx="channel.embeds.embed.embed-media-renderer-inner.focus-ring--4"
				>
					<EmbedImage
						src={resolvedImageSource.src}
						originalSrc={image.url}
						naturalWidth={image.width}
						naturalHeight={image.height}
						width={width}
						height={height}
						placeholder={image.placeholder}
						constrain={true}
						nsfw={isMediaMatureContent(image)}
						channelId={message.channelId}
						messageId={message.id}
						message={message}
						contentHash={image.content_hash}
						embedIndex={embedIndex}
						onDelete={onDelete}
						isPreview={isPreview}
						animated={resolvedImageSource.animated}
						alt={image.description ?? undefined}
						data-flx="channel.embeds.embed.embed-media-renderer-inner.embed-image"
					/>
				</FocusRing>
			);
		}
		if (isValidMedia(thumbnail)) {
			const {width, height} = calculateMediaDimensions(thumbnail);
			const isGif = thumbnail.content_type === 'image/gif' || thumbnail.url.toLowerCase().endsWith('.gif');
			if (isGif) {
				return (
					<FocusRing
						within
						ringClassName={mediaFocusRingClass}
						data-flx="channel.embeds.embed.embed-media-renderer-inner.focus-ring--5"
					>
						<EmbedGif
							embedURL={thumbnail.url}
							proxyURL={thumbnail.proxy_url}
							naturalWidth={thumbnail.width}
							naturalHeight={thumbnail.height}
							placeholder={thumbnail.placeholder}
							alt={thumbnail.description ?? embed.description ?? undefined}
							nsfw={isMediaMatureContent(thumbnail)}
							channelId={message.channelId}
							messageId={message.id}
							message={message}
							contentHash={thumbnail.content_hash}
							embedIndex={embedIndex}
							onDelete={onDelete}
							layoutConstraints={getEmbedMediaDimensions()}
							data-flx="channel.embeds.embed.embed-media-renderer-inner.embed-gif--2"
						/>
					</FocusRing>
				);
			}
			const thumbnailSource = resolveEmbedImageSource(thumbnail, width, height);
			return (
				<FocusRing
					within
					ringClassName={mediaFocusRingClass}
					data-flx="channel.embeds.embed.embed-media-renderer-inner.focus-ring--6"
				>
					<EmbedImage
						src={thumbnailSource.src}
						originalSrc={thumbnail.url}
						naturalWidth={thumbnail.width}
						naturalHeight={thumbnail.height}
						width={width}
						height={height}
						placeholder={thumbnail.placeholder}
						constrain={true}
						nsfw={isMediaMatureContent(thumbnail)}
						channelId={message.channelId}
						messageId={message.id}
						message={message}
						contentHash={thumbnail.content_hash}
						embedIndex={embedIndex}
						onDelete={onDelete}
						animated={thumbnailSource.animated}
						alt={thumbnail.description ?? undefined}
						data-flx="channel.embeds.embed.embed-media-renderer-inner.embed-image--2"
					/>
				</FocusRing>
			);
		}
		return null;
	},
);
export const EmbedMediaRenderer = memo(EmbedMediaRendererInner, mediaPropsEqual);
const InlineThumbnailRendererInner: FC<EmbedMediaRendererProps> = observer(
	({embed, message, embedIndex, onDelete, isPreview}) => {
		if (!embed.thumbnail || !isValidMedia(embed.thumbnail)) return null;
		const thumbnail = embed.thumbnail;
		const width = Math.min(THUMBNAIL_SIZE, Math.round((THUMBNAIL_SIZE * thumbnail.width) / thumbnail.height));
		const thumbnailSource = resolveEmbedImageSource(thumbnail, width, THUMBNAIL_SIZE);
		return (
			<FocusRing
				within
				ringClassName={mediaFocusRingClass}
				data-flx="channel.embeds.embed.inline-thumbnail-renderer-inner.focus-ring"
			>
				<EmbedImage
					src={thumbnailSource.src}
					originalSrc={thumbnail.url}
					naturalWidth={thumbnail.width}
					naturalHeight={thumbnail.height}
					width={width}
					height={THUMBNAIL_SIZE}
					placeholder={thumbnail.placeholder}
					constrain={true}
					isInline={true}
					nsfw={isMediaMatureContent(thumbnail)}
					channelId={message.channelId}
					messageId={message.id}
					message={message}
					contentHash={thumbnail.content_hash}
					embedIndex={embedIndex}
					onDelete={onDelete}
					isPreview={isPreview}
					animated={thumbnailSource.animated}
					alt={thumbnail.description ?? undefined}
					data-flx="channel.embeds.embed.inline-thumbnail-renderer-inner.embed-image"
				/>
			</FocusRing>
		);
	},
);
export const InlineThumbnailRenderer = memo(InlineThumbnailRendererInner, mediaPropsEqual);
