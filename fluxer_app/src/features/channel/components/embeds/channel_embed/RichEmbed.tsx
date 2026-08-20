// SPDX-License-Identifier: AGPL-3.0-or-later

import {AttachmentMosaic} from '@app/features/channel/components/embeds/attachments/AttachmentMosaic';
import styles from '@app/features/channel/components/embeds/ChannelEmbed.module.css';
import {
	buildGalleryAttachments,
	collectGalleryImages,
	EMBED_MEDIA_MAX_WIDTH,
	type EmbedProps,
	getBorderColor,
	getUrlHostname,
	isValidMedia,
} from '@app/features/channel/components/embeds/channel_embed/ChannelEmbedShared';
import {
	EmbedMediaRenderer,
	InlineThumbnailRenderer,
} from '@app/features/channel/components/embeds/channel_embed/EmbedMediaRenderer';
import {
	EmbedAuthorComponent,
	EmbedDescription,
	EmbedFields,
	EmbedFooterComponent,
	EmbedProvider,
	EmbedTitle,
} from '@app/features/channel/components/embeds/channel_embed/EmbedParts';
import {
	EMBED_TEXT_OUTER_WIDTH,
	formatResponsiveEmbedWidth,
} from '@app/features/channel/components/embeds/EmbedRenderUtils';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {MessageEmbedTypes} from '@fluxer/constants/src/ChannelConstants';
import type {EmbedMedia} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type {FC} from 'react';
import {useMemo} from 'react';

export const RichEmbed: FC<EmbedProps> = observer(
	({embed, message, embedIndex, contextualEmbeds, onDelete, isPreview}) => {
		const embedList = contextualEmbeds ?? message.embeds;
		const hasVideo = isValidMedia(embed.video);
		const hasImage = isValidMedia(embed.image);
		const hasThumbnail = isValidMedia(embed.thumbnail);
		const hasAnyMedia = hasVideo || hasImage || hasThumbnail;
		const galleryImages = useMemo<Array<Required<EmbedMedia>>>(() => {
			return collectGalleryImages({embed, embedIndex, embedList});
		}, [embed, embedIndex, embedList]);
		const showGallery = galleryImages.length > 1 || (!hasAnyMedia && galleryImages.length > 0);
		const galleryAttachments = useMemo(
			() => (showGallery ? buildGalleryAttachments(galleryImages, embed, embedIndex) : undefined),
			[galleryImages, embed, embedIndex, showGallery],
		);
		const shouldRenderMedia = hasAnyMedia || showGallery;
		const promotesThumbnailToImage = embed.type === MessageEmbedTypes.ARTICLE || embed.type === MessageEmbedTypes.IMAGE;
		const isInlineThumbnail = !hasVideo && hasThumbnail && !hasImage && !promotesThumbnailToImage;
		const shouldRenderInlineThumbnail = isInlineThumbnail && !showGallery;
		const isYouTubeEmbed = getUrlHostname(embed.provider?.url) === 'www.youtube.com';
		const useNarrowWidth = shouldRenderMedia && !shouldRenderInlineThumbnail;
		return (
			<article
				className={clsx(styles.embed, styles.embedFull, markupStyles.markup)}
				data-search-highlight-scope="message"
				style={{
					borderLeft: `4px solid ${getBorderColor(embed.color)}`,
					maxWidth: formatResponsiveEmbedWidth(useNarrowWidth ? EMBED_MEDIA_MAX_WIDTH : EMBED_TEXT_OUTER_WIDTH),
				}}
				data-flx="channel.embeds.embed.rich-embed.embed"
			>
				<div className={styles.gridContainer} data-flx="channel.embeds.embed.rich-embed.grid-container">
					<div
						className={clsx(styles.grid, shouldRenderInlineThumbnail && styles.hasThumbnail)}
						data-flx="channel.embeds.embed.rich-embed.grid"
					>
						<div className={styles.embedContent} data-flx="channel.embeds.embed.rich-embed.embed-content">
							<EmbedProvider
								provider={embed.provider}
								data-flx="channel.embeds.channel-embed.rich-embed.embed-provider"
							/>
							<EmbedAuthorComponent
								author={embed.author}
								data-flx="channel.embeds.channel-embed.rich-embed.embed-author-component"
							/>
							<EmbedTitle
								title={embed.title}
								url={embed.url}
								messageId={message.id}
								channelId={message.channelId}
								data-flx="channel.embeds.channel-embed.rich-embed.embed-title"
							/>
							{!isYouTubeEmbed && (
								<EmbedDescription
									description={embed.description}
									messageId={message.id}
									channelId={message.channelId}
									data-flx="channel.embeds.channel-embed.rich-embed.embed-description"
								/>
							)}
							<EmbedFields
								fields={embed.fields}
								messageId={message.id}
								channelId={message.channelId}
								data-flx="channel.embeds.channel-embed.rich-embed.embed-fields"
							/>
							{!shouldRenderInlineThumbnail && shouldRenderMedia && (
								<div className={clsx(styles.embedMedia)} data-flx="channel.embeds.embed.rich-embed.embed-media">
									{showGallery && galleryAttachments ? (
										<AttachmentMosaic
											attachments={galleryAttachments}
											message={message}
											hideExpiryFootnote={true}
											isPreview={isPreview}
											data-flx="channel.embeds.embed.rich-embed.attachment-mosaic"
										/>
									) : (
										<EmbedMediaRenderer
											embed={embed}
											message={message}
											embedIndex={embedIndex}
											onDelete={onDelete}
											isPreview={isPreview}
											data-flx="channel.embeds.channel-embed.rich-embed.embed-media-renderer"
										/>
									)}
								</div>
							)}
							{(embed.footer || embed.timestamp) && (
								<EmbedFooterComponent
									footer={embed.footer}
									timestamp={embed.timestamp ? new Date(embed.timestamp) : undefined}
									messageId={message.id}
									channelId={message.channelId}
									data-flx="channel.embeds.channel-embed.rich-embed.embed-footer-component"
								/>
							)}
						</div>
						{shouldRenderInlineThumbnail && embed.thumbnail && isValidMedia(embed.thumbnail) && (
							<div className={styles.embedThumbnail} data-flx="channel.embeds.embed.rich-embed.embed-thumbnail">
								<InlineThumbnailRenderer
									embed={embed}
									message={message}
									embedIndex={embedIndex}
									onDelete={onDelete}
									isPreview={isPreview}
									data-flx="channel.embeds.channel-embed.rich-embed.inline-thumbnail-renderer"
								/>
							</div>
						)}
					</div>
				</div>
			</article>
		);
	},
);
