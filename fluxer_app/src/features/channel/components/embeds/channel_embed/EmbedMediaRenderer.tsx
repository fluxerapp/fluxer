// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	calculateMediaDimensions,
	type EmbedMediaRendererProps,
	getOptimizedMediaURL,
	getUrlHostname,
	isMediaMatureContent,
	isValidMedia,
	mediaPropsEqual,
} from '@app/features/channel/components/embeds/channel_embed/ChannelEmbedShared';
import {EmbedGif} from '@app/features/channel/components/embeds/media/EmbedGifv';
import {EmbedImage} from '@app/features/channel/components/embeds/media/EmbedImage';
import EmbedVideo from '@app/features/channel/components/embeds/media/EmbedVideo';
import {EmbedYouTube} from '@app/features/channel/components/embeds/media/EmbedYouTube';
import {getInlineVideoLayoutConstraints} from '@app/features/channel/components/embeds/media/VideoDimensionUtils';
import {getEmbedMediaDimensions} from '@app/features/messaging/utils/MediaDimensionConfig';
import {buildAnimatedImageProxyURL, buildMediaProxyURL} from '@app/features/messaging/utils/MediaProxyUtils';
import messageStyles from '@app/features/theme/styles/Message.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {MessageAttachmentFlags} from '@fluxer/constants/src/ChannelConstants';
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
			const imageIsAnimated = (image.flags & MessageAttachmentFlags.IS_ANIMATED) === MessageAttachmentFlags.IS_ANIMATED;
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
			return (
				<FocusRing
					within
					ringClassName={mediaFocusRingClass}
					data-flx="channel.embeds.embed.embed-media-renderer-inner.focus-ring--4"
				>
					<EmbedImage
						src={getOptimizedMediaURL(image.proxy_url, width, height, image.content_type)}
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
						animated={imageIsAnimated}
						alt={image.description ?? undefined}
						data-flx="channel.embeds.embed.embed-media-renderer-inner.embed-image"
					/>
				</FocusRing>
			);
		}
		if (isValidMedia(thumbnail)) {
			const {width, height} = calculateMediaDimensions(thumbnail);
			const isGif = thumbnail.content_type === 'image/gif' || thumbnail.url.toLowerCase().endsWith('.gif');
			const thumbnailIsAnimated =
				(thumbnail.flags & MessageAttachmentFlags.IS_ANIMATED) === MessageAttachmentFlags.IS_ANIMATED;
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
			return (
				<FocusRing
					within
					ringClassName={mediaFocusRingClass}
					data-flx="channel.embeds.embed.embed-media-renderer-inner.focus-ring--6"
				>
					<EmbedImage
						src={getOptimizedMediaURL(thumbnail.proxy_url, width, height, thumbnail.content_type)}
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
						animated={thumbnailIsAnimated}
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
		const width = Math.min(80, Math.round((80 * thumbnail.width) / thumbnail.height));
		const thumbnailIsAnimated =
			thumbnail.content_type === 'image/gif' ||
			(thumbnail.flags & MessageAttachmentFlags.IS_ANIMATED) === MessageAttachmentFlags.IS_ANIMATED;
		return (
			<FocusRing
				within
				ringClassName={mediaFocusRingClass}
				data-flx="channel.embeds.embed.inline-thumbnail-renderer-inner.focus-ring"
			>
				<EmbedImage
					src={
						thumbnail.content_type === 'image/gif'
							? buildAnimatedImageProxyURL(thumbnail.proxy_url, width * 2, 160)
							: getOptimizedMediaURL(thumbnail.proxy_url, width, 80, thumbnail.content_type)
					}
					originalSrc={thumbnail.url}
					naturalWidth={thumbnail.width}
					naturalHeight={thumbnail.height}
					width={width}
					height={80}
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
					animated={thumbnailIsAnimated}
					alt={thumbnail.description ?? undefined}
					data-flx="channel.embeds.embed.inline-thumbnail-renderer-inner.embed-image"
				/>
			</FocusRing>
		);
	},
);
export const InlineThumbnailRenderer = memo(InlineThumbnailRendererInner, mediaPropsEqual);
