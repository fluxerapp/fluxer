// SPDX-License-Identifier: AGPL-3.0-or-later

import {AttachmentMosaic} from '@app/features/channel/components/embeds/attachments/AttachmentMosaic';
import styles from '@app/features/channel/components/embeds/ChannelEmbed.module.css';
import {
	buildGalleryAttachments,
	calculateMediaDimensions,
	collectGalleryImages,
	EMBED_MEDIA_CHROME_WIDTH,
	type EmbedProps,
	getBorderColor,
	isValidMedia,
} from '@app/features/channel/components/embeds/channel_embed/ChannelEmbedShared';
import {EmbedMediaRenderer} from '@app/features/channel/components/embeds/channel_embed/EmbedMediaRenderer';
import {
	EmbedAuthorComponent,
	EmbedDescription,
	EmbedFooterComponent,
	EmbedProvider,
	EmbedTitle,
} from '@app/features/channel/components/embeds/channel_embed/EmbedParts';
import {
	calculateBlueskyMediaContainerWidth,
	calculateBlueskyOuterMaxWidth,
	formatResponsiveEmbedWidth,
} from '@app/features/channel/components/embeds/EmbedRenderUtils';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {MessageEmbedTypes} from '@fluxer/constants/src/ChannelConstants';
import type {EmbedField, EmbedMedia} from '@fluxer/schema/src/domains/message/EmbedSchemas';
import {Trans} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type {FC, ReactNode} from 'react';
import {useMemo} from 'react';

interface BlueskyEngagementItem {
	metric: 'repost' | 'quote' | 'like' | 'save';
	countLabel: string;
}

function renderBlueskyEngagementMetric({metric, countLabel}: BlueskyEngagementItem): ReactNode {
	switch (metric) {
		case 'repost':
			return (
				<Trans>
					Reposts: <strong data-flx="channel.embeds.embed.bluesky-engagement-row.strong">{countLabel}</strong>
				</Trans>
			);
		case 'quote':
			return (
				<Trans>
					Quotes: <strong data-flx="channel.embeds.embed.bluesky-engagement-row.strong">{countLabel}</strong>
				</Trans>
			);
		case 'like':
			return (
				<Trans>
					Likes: <strong data-flx="channel.embeds.embed.bluesky-engagement-row.strong">{countLabel}</strong>
				</Trans>
			);
		case 'save':
			return (
				<Trans>
					Saves: <strong data-flx="channel.embeds.embed.bluesky-engagement-row.strong">{countLabel}</strong>
				</Trans>
			);
	}
}

const BlueskyEngagementRow: FC<{fields?: ReadonlyArray<EmbedField>}> = observer(({fields}) => {
	const engagementItems = useMemo(() => {
		const map = new Map<string, string>();
		for (const field of fields ?? []) {
			map.set(field.name, field.value);
		}
		const shouldRenderCount = (count?: string): count is string => {
			if (!count) return false;
			const trimmed = count.trim();
			if (trimmed.length === 0) return false;
			return !/^0(?:\.0+)?$/.test(trimmed);
		};
		const items: Array<BlueskyEngagementItem> = [];
		const repostCount = map.get('repostCount');
		const quoteCount = map.get('quoteCount');
		const likeCount = map.get('likeCount');
		const bookmarkCount = map.get('bookmarkCount') ?? map.get('saveCount');
		if (shouldRenderCount(repostCount)) items.push({metric: 'repost', countLabel: repostCount});
		if (shouldRenderCount(quoteCount)) items.push({metric: 'quote', countLabel: quoteCount});
		if (shouldRenderCount(likeCount)) items.push({metric: 'like', countLabel: likeCount});
		if (shouldRenderCount(bookmarkCount)) items.push({metric: 'save', countLabel: bookmarkCount});
		return items;
	}, [fields]);
	if (engagementItems.length === 0) return null;
	return (
		<div className={styles.blueskyEngagement} data-flx="channel.embeds.embed.bluesky-engagement-row.bluesky-engagement">
			{engagementItems.map((item) => (
				<div
					key={item.metric}
					className={styles.blueskyEngagementItem}
					data-flx="channel.embeds.embed.bluesky-engagement-row.bluesky-engagement-item"
				>
					{renderBlueskyEngagementMetric(item)}
				</div>
			))}
		</div>
	);
});

interface BlueskyEmbedProps extends EmbedProps {
	isNested?: boolean;
}

export const BlueskyEmbed: FC<BlueskyEmbedProps> = observer(
	({embed, message, embedIndex, contextualEmbeds, onDelete, isPreview, isNested = false}) => {
		const embedList = isNested ? [embed] : (contextualEmbeds ?? message.embeds);
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
		const blueskyMediaMaxWidth = useMemo(() => {
			if (showGallery || isNested) return undefined;
			const primaryMedia = hasVideo ? embed.video : hasImage ? embed.image : hasThumbnail ? embed.thumbnail : undefined;
			if (!primaryMedia || !isValidMedia(primaryMedia)) return undefined;
			return calculateMediaDimensions(primaryMedia).width;
		}, [embed.video, embed.image, embed.thumbnail, hasVideo, hasImage, hasThumbnail, isNested, showGallery]);
		const blueskyMediaContainerWidth = useMemo(
			() => calculateBlueskyMediaContainerWidth(blueskyMediaMaxWidth),
			[blueskyMediaMaxWidth],
		);
		const blueskyMediaContainerStyle = useMemo(() => {
			if (blueskyMediaContainerWidth === undefined) return undefined;
			const width = formatResponsiveEmbedWidth(blueskyMediaContainerWidth);
			return {width, maxWidth: width};
		}, [blueskyMediaContainerWidth]);
		const useNarrowWidth = shouldRenderMedia;
		const blueskyOuterMaxWidth = useMemo(
			() =>
				calculateBlueskyOuterMaxWidth({
					mediaWidth: blueskyMediaMaxWidth,
					hasMedia: useNarrowWidth,
					chromeWidth: EMBED_MEDIA_CHROME_WIDTH,
				}),
			[blueskyMediaMaxWidth, useNarrowWidth],
		);
		const nestedChildEmbed = useMemo(() => {
			if (isNested) return undefined;
			const candidate = embed.children?.[0];
			if (!candidate) return undefined;
			if (candidate.type !== MessageEmbedTypes.BLUESKY) return undefined;
			return candidate;
		}, [embed.children, isNested]);
		const renderNestedChild = () => {
			if (!nestedChildEmbed) return null;
			return (
				<div
					className={styles.blueskyNestedEmbedContainer}
					data-flx="channel.embeds.embed.render-nested-child.bluesky-nested-embed-container"
				>
					<BlueskyEmbed
						embed={nestedChildEmbed}
						message={message}
						onDelete={onDelete}
						isPreview={isPreview}
						isNested={true}
						data-flx="channel.embeds.channel-embed.bluesky-embed.render-nested-child.bluesky-embed"
					/>
				</div>
			);
		};
		return (
			<article
				className={clsx(styles.embed, markupStyles.markup, isNested ? styles.blueskyNestedEmbed : styles.embedFull)}
				data-search-highlight-scope="message"
				style={{
					...(isNested
						? {maxWidth: '100%', width: '100%'}
						: {
								borderLeft: `4px solid ${getBorderColor(embed.color)}`,
								maxWidth: formatResponsiveEmbedWidth(blueskyOuterMaxWidth),
							}),
				}}
				data-flx="channel.embeds.embed.bluesky-embed.embed"
			>
				<div className={styles.gridContainer} data-flx="channel.embeds.embed.bluesky-embed.grid-container">
					<div className={styles.grid} data-flx="channel.embeds.embed.bluesky-embed.grid">
						<div className={styles.embedContent} data-flx="channel.embeds.embed.bluesky-embed.embed-content">
							<EmbedProvider
								provider={embed.provider}
								data-flx="channel.embeds.channel-embed.bluesky-embed.embed-provider"
							/>
							<EmbedAuthorComponent
								author={embed.author}
								data-flx="channel.embeds.channel-embed.bluesky-embed.embed-author-component"
							/>
							{!isNested && (
								<EmbedTitle
									title={embed.title}
									url={embed.url}
									markdown
									messageId={message.id}
									channelId={message.channelId}
									data-flx="channel.embeds.channel-embed.bluesky-embed.embed-title"
								/>
							)}
							<EmbedDescription
								description={embed.description}
								markdown
								messageId={message.id}
								channelId={message.channelId}
								data-flx="channel.embeds.channel-embed.bluesky-embed.embed-description"
							/>
							{shouldRenderMedia ? (
								<div
									className={clsx(
										styles.blueskyMediaEngagement,
										isNested && styles.blueskyNestedMediaEngagement,
										showGallery && styles.blueskyGalleryMedia,
									)}
									style={blueskyMediaContainerStyle}
									data-flx="channel.embeds.embed.bluesky-embed.bluesky-media-engagement"
								>
									<div className={clsx(styles.embedMedia)} data-flx="channel.embeds.embed.bluesky-embed.embed-media">
										{showGallery && galleryAttachments ? (
											<AttachmentMosaic
												attachments={galleryAttachments}
												message={message}
												hideExpiryFootnote={true}
												isPreview={isPreview}
												data-flx="channel.embeds.embed.bluesky-embed.attachment-mosaic"
											/>
										) : (
											<EmbedMediaRenderer
												embed={embed}
												message={message}
												embedIndex={embedIndex}
												onDelete={onDelete}
												isPreview={isPreview}
												data-flx="channel.embeds.channel-embed.bluesky-embed.embed-media-renderer"
											/>
										)}
									</div>
									{renderNestedChild()}
								</div>
							) : (
								renderNestedChild()
							)}
							{!isNested && (
								<EmbedFooterComponent
									footer={embed.footer}
									timestamp={embed.timestamp ? new Date(embed.timestamp) : undefined}
									data-flx="channel.embeds.channel-embed.bluesky-embed.embed-footer-component"
								/>
							)}
							{!isNested && (
								<BlueskyEngagementRow
									fields={embed.fields}
									data-flx="channel.embeds.channel-embed.bluesky-embed.bluesky-engagement-row"
								/>
							)}
						</div>
					</div>
				</div>
			</article>
		);
	},
);
