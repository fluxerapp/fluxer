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

import {Trans, useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {type FC, useCallback, useMemo} from 'react';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {GuildOperations, MessageAttachmentFlags, MessageEmbedTypes, Permissions} from '~/Constants';
import {AttachmentMosaic} from '~/components/channel/embeds/attachments/AttachmentMosaic';
import EmbedAudio from '~/components/channel/embeds/media/EmbedAudio';
import {EmbedGif, EmbedGifv} from '~/components/channel/embeds/media/EmbedGifv';
import {EmbedImage} from '~/components/channel/embeds/media/EmbedImage';
import EmbedVideo from '~/components/channel/embeds/media/EmbedVideo';
import {EmbedYouTube} from '~/components/channel/embeds/media/EmbedYouTube';
import {SpoilerOverlay} from '~/components/common/SpoilerOverlay';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {ExternalLinkWarningModal} from '~/components/modals/ExternalLinkWarningModal';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {SafeMarkdown} from '~/lib/markdown';
import {MarkdownContext} from '~/lib/markdown/renderers';
import type {
	EmbedAuthor as EmbedAuthorData,
	EmbedField as EmbedFieldType,
	EmbedFooter as EmbedFooterData,
	EmbedMedia,
	MessageAttachment,
	MessageEmbed,
	MessageRecord,
} from '~/records/MessageRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import ChannelStore from '~/stores/ChannelStore';
import GuildStore from '~/stores/GuildStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import PermissionStore from '~/stores/PermissionStore';
import TrustedDomainStore from '~/stores/TrustedDomainStore';
import mosaicStyles from '~/styles/AttachmentMosaic.module.css';
import markupStyles from '~/styles/Markup.module.css';
import messageStyles from '~/styles/Message.module.css';
import * as ColorUtils from '~/utils/ColorUtils';
import * as DateUtils from '~/utils/DateUtils';
import {createCalculator} from '~/utils/DimensionUtils';
import {getEmbedMediaDimensions} from '~/utils/MediaDimensionConfig';
import {buildMediaProxyURL} from '~/utils/MediaProxyUtils';
import {extractSpoileredUrls, useSpoilerState} from '~/utils/SpoilerUtils';
import styles from './Embed.module.css';

const THUMBNAIL_SIZE = 80;
const MAX_GALLERY_MEDIA = 10;

interface EmbedProps {
	embed: MessageEmbed;
	message: MessageRecord;
	embedIndex?: number;
	onDelete?: (bypassConfirm?: boolean) => void;
}

interface LinkComponentProps {
	url: string;
	children: React.ReactNode;
	className?: string;
}

interface MediaDimensions {
	width: number;
	height: number;
}

const thumbnailCalculator = createCalculator({
	maxWidth: THUMBNAIL_SIZE,
	maxHeight: THUMBNAIL_SIZE,
	forceScale: true,
});

const isValidMedia = (media?: Partial<EmbedMedia>): media is Required<EmbedMedia> => {
	return !!(
		media &&
		typeof media.proxy_url === 'string' &&
		typeof media.url === 'string' &&
		typeof media.width === 'number' &&
		typeof media.height === 'number'
	);
};

const calculateMediaDimensions = (media: Required<EmbedMedia>): MediaDimensions => {
	const embedDimensions = getEmbedMediaDimensions();
	const mediaCalculator = createCalculator({
		maxWidth: embedDimensions.maxWidth,
		maxHeight: embedDimensions.maxHeight,
	});

	const {dimensions} = mediaCalculator.calculate({
		width: media.width,
		height: media.height,
	});
	return dimensions;
};

const getOptimizedMediaURL = (
	proxyURL: string,
	width: number,
	height: number,
	includeFormat = true,
	animated?: boolean,
): string => {
	const targetWidth = Math.round(width * 2);
	const targetHeight = Math.round(height * 2);
	return buildMediaProxyURL(proxyURL, {
		format: includeFormat ? 'webp' : undefined,
		width: targetWidth,
		height: targetHeight,
		animated,
	});
};

const SuppressEmbedsConfirmModal: FC<{message: MessageRecord}> = ({message}) => {
	const {t} = useLingui();

	return (
		<ConfirmModal
			title={t`Suppress Embeds`}
			description={
				<Trans>
					Are you sure you want to suppress all link embeds on this message? This action will hide all embeds from this
					message.
				</Trans>
			}
			primaryText={t`Suppress Embeds`}
			primaryVariant="danger-primary"
			onPrimary={async () => {
				await MessageActionCreators.toggleSuppressEmbeds(message.channelId, message.id, message.flags);
			}}
		/>
	);
};

const shouldRenderAsInlineThumbnail = (media?: EmbedMedia): boolean => {
	if (!isValidMedia(media)) return false;

	const {dimensions: thumbnailDimensions} = thumbnailCalculator.calculate({
		width: media.width,
		height: media.height,
	});
	const thumbnailWidth = thumbnailDimensions.width;

	const {width: normalWidth} = calculateMediaDimensions(media);

	return normalWidth < 300 && thumbnailWidth >= 40;
};

const isMediaNSFW = (media?: EmbedMedia): boolean => {
	if (!media) return false;
	return Boolean(media.nsfw || ((media.flags ?? 0) & MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA) !== 0);
};

const normalizeUrl = (url?: string): string | null => {
	if (!url) return null;
	try {
		return new URL(url).href.replace(/\/$/, '');
	} catch {
		return null;
	}
};

const deriveFilenameFromUrl = (url: string): string => {
	try {
		const parsed = new URL(url);
		const filename = parsed.pathname.split('/').pop()?.trim();
		return filename && filename.length > 0 ? filename : 'embed-media';
	} catch {
		return 'embed-media';
	}
};

const buildGalleryAttachments = (
	images: Array<Required<EmbedMedia>>,
	embed: MessageEmbed,
	embedIndex?: number,
): Array<MessageAttachment> => {
	return images.map((media, index) => ({
		id: `${embed.id ?? embedIndex ?? 'embed'}-gallery-${index}`,
		filename: deriveFilenameFromUrl(media.url),
		title: embed.title ?? undefined,
		description: media.description ?? undefined,
		content_type: media.content_type ?? undefined,
		size: 0,
		url: media.url,
		proxy_url: media.proxy_url ?? media.url,
		width: media.width ?? undefined,
		height: media.height ?? undefined,
		placeholder: media.placeholder ?? undefined,
		placeholder_version: undefined,
		flags: media.flags,
		duration_secs: media.duration ? Math.round(media.duration) : undefined,
		duration: media.duration,
		waveform: undefined,
		content_hash: media.content_hash ?? undefined,
		nsfw: media.nsfw,
	}));
};

const getBorderColor = (color: number | undefined) => {
	if (color === undefined || color === 0) {
		return 'var(--brand-primary)';
	}
	return ColorUtils.int2rgb(color);
};

const mediaFocusRingClass = messageStyles.mediaFocusRing;

const LinkComponent: FC<LinkComponentProps> = observer(({url, children, className}) => {
	const handleClick = (e: React.MouseEvent<HTMLAnchorElement>) => {
		e.stopPropagation();

		try {
			const parsed = new URL(url);
			const isTrusted = TrustedDomainStore.isTrustedDomain(parsed.hostname);
			if (!isTrusted) {
				e.preventDefault();
				ModalActionCreators.push(modal(() => <ExternalLinkWarningModal url={url} hostname={parsed.hostname} />));
			}
		} catch (_error) {
			console.warn('Invalid URL in embed link:', url);
		}
	};

	return (
		<FocusRing>
			<a
				className={clsx(styles.embedLink, className)}
				href={url}
				rel="noopener noreferrer"
				target="_blank"
				onClick={handleClick}
			>
				{children}
			</a>
		</FocusRing>
	);
});

const EmbedProvider: FC<{provider?: EmbedAuthorData}> = observer(({provider}) => {
	if (!provider) return null;

	return (
		<div className={styles.embedProvider}>
			{provider.url ? <LinkComponent url={provider.url}>{provider.name}</LinkComponent> : <span>{provider.name}</span>}
		</div>
	);
});

const EmbedAuthor: FC<{author?: EmbedAuthorData}> = observer(({author}) => {
	if (!author) return null;

	return (
		<div className={styles.embedAuthor}>
			{author.proxy_icon_url && (
				<img
					alt=""
					className={styles.embedAuthorIcon}
					src={buildMediaProxyURL(author.proxy_icon_url)}
					width={24}
					height={24}
				/>
			)}
			{author.url ? (
				<LinkComponent className={clsx(styles.embedAuthorName, styles.embedAuthorNameLink)} url={author.url}>
					{author.name}
				</LinkComponent>
			) : (
				<span className={styles.embedAuthorName}>{author.name}</span>
			)}
		</div>
	);
});

const EmbedTitle: FC<{title?: string; url?: string}> = observer(({title, url}) => {
	if (!title) return null;

	return (
		<div className={styles.embedTitle}>
			{url ? (
				<LinkComponent url={url}>
					<SafeMarkdown content={title} options={{context: MarkdownContext.RESTRICTED_INLINE_REPLY}} />
				</LinkComponent>
			) : (
				<span>
					<SafeMarkdown content={title} options={{context: MarkdownContext.RESTRICTED_INLINE_REPLY}} />
				</span>
			)}
		</div>
	);
});

const EmbedDescription: FC<{
	messageId?: string;
	channelId?: string;
	description?: string;
}> = observer(({messageId, channelId, description}) => {
	if (!description) return null;

	return (
		<div className={styles.embedDescription}>
			<SafeMarkdown
				content={description}
				options={{
					context: MarkdownContext.RESTRICTED_EMBED_DESCRIPTION,
					messageId,
					channelId,
				}}
			/>
		</div>
	);
});

const EmbedFields: FC<{fields: Array<EmbedFieldType>}> = observer(({fields}) => {
	if (!fields?.length) return null;

	const groupFields = (fields: Array<EmbedFieldType>): Array<Array<EmbedFieldType>> => {
		const groupedFields: Array<Array<EmbedFieldType>> = [];
		let currentGroup: Array<EmbedFieldType> = [];
		const MAX_INLINE_PER_ROW = 3;

		for (const field of fields) {
			if (field.inline) {
				currentGroup.push(field);
				if (currentGroup.length === MAX_INLINE_PER_ROW) {
					groupedFields.push(currentGroup);
					currentGroup = [];
				}
			} else {
				if (currentGroup.length > 0) {
					groupedFields.push(currentGroup);
					currentGroup = [];
				}
				groupedFields.push([field]);
			}
		}

		if (currentGroup.length > 0) {
			groupedFields.push(currentGroup);
		}

		return groupedFields;
	};

	const groupedFields = groupFields(fields);

	return (
		<div className={styles.embedFields}>
			{groupedFields.map((group, groupIndex) => {
				const groupLength = group.length;
				return group.map(({name, value}, index) => {
					const span = groupLength === 1 ? 12 : groupLength === 2 ? 6 : 4;
					const gridColumnStart = index * span + 1;
					const gridColumnEnd = gridColumnStart + span;
					return (
						<div
							className={styles.embedField}
							key={`${groupIndex}-${index}`}
							style={{gridColumn: `${gridColumnStart} / ${gridColumnEnd}`}}
						>
							<div className={styles.embedFieldName}>
								<SafeMarkdown content={name} options={{context: MarkdownContext.RESTRICTED_INLINE_REPLY}} />
							</div>
							<div className={styles.embedFieldValue}>
								<SafeMarkdown content={value} options={{context: MarkdownContext.RESTRICTED_EMBED_DESCRIPTION}} />
							</div>
						</div>
					);
				});
			})}
		</div>
	);
});

const EmbedFooter: FC<{
	timestamp?: Date;
	footer?: EmbedFooterData;
}> = observer(({timestamp, footer}) => {
	const {i18n} = useLingui();
	const formattedTimestamp = timestamp ? DateUtils.getRelativeDateString(timestamp, i18n) : undefined;
	if (!(footer || formattedTimestamp)) return null;

	return (
		<div className={clsx(styles.embedFooter, footer?.proxy_icon_url && styles.hasThumbnail)}>
			{footer?.proxy_icon_url && (
				<img
					alt=""
					className={styles.embedFooterIcon}
					src={buildMediaProxyURL(footer.proxy_icon_url)}
					width={20}
					height={20}
				/>
			)}
			<div className={styles.embedFooterText}>
				{footer?.text}
				{formattedTimestamp && (
					<>
						<div className={styles.embedFooterSeparator} />
						<span>{formattedTimestamp}</span>
					</>
				)}
			</div>
		</div>
	);
});

const EmbedMediaRenderer: FC<{
	embed: MessageEmbed;
	message: MessageRecord;
	embedIndex?: number;
	onDelete?: (bypassConfirm?: boolean) => void;
}> = observer(({embed, message, embedIndex, onDelete}) => {
	const {video, image, thumbnail} = embed;

	if (!isValidMedia(video) && !isValidMedia(image) && !isValidMedia(thumbnail)) {
		return null;
	}

	if (isValidMedia(video) && embed.provider?.url && new URL(embed.provider.url).hostname === 'www.youtube.com') {
		return (
			<FocusRing within ringClassName={mediaFocusRingClass}>
				<EmbedYouTube embed={embed} />
			</FocusRing>
		);
	}

	if (isValidMedia(video)) {
		const {width, height} = calculateMediaDimensions(video);
		return (
			<FocusRing within ringClassName={mediaFocusRingClass}>
				<EmbedVideo
					src={buildMediaProxyURL(video.proxy_url)}
					width={width}
					height={height}
					placeholder={video.placeholder}
					title={embed.title}
					duration={video.duration}
					nsfw={isMediaNSFW(video)}
					channelId={message.channelId}
					messageId={message.id}
					embedUrl={embed.url}
					message={message}
					contentHash={video.content_hash}
					embedIndex={embedIndex}
					onDelete={onDelete}
				/>
			</FocusRing>
		);
	}

	if (isValidMedia(image)) {
		const {width, height} = calculateMediaDimensions(image);
		const isGif = image.content_type === 'image/gif' || image.url.toLowerCase().endsWith('.gif');

		if (isGif) {
			return (
				<FocusRing within ringClassName={mediaFocusRingClass}>
					<EmbedGif
						embedURL={image.url}
						proxyURL={image.proxy_url}
						naturalWidth={image.width}
						naturalHeight={image.height}
						placeholder={image.placeholder}
						nsfw={isMediaNSFW(image)}
						channelId={message.channelId}
						messageId={message.id}
						message={message}
						contentHash={image.content_hash}
						embedIndex={embedIndex}
						onDelete={onDelete}
					/>
				</FocusRing>
			);
		}

		return (
			<FocusRing within ringClassName={mediaFocusRingClass}>
				<EmbedImage
					src={getOptimizedMediaURL(image.proxy_url, width, height)}
					originalSrc={image.url}
					naturalWidth={image.width}
					naturalHeight={image.height}
					width={width}
					height={height}
					placeholder={image.placeholder}
					constrain={true}
					nsfw={isMediaNSFW(image)}
					channelId={message.channelId}
					messageId={message.id}
					message={message}
					contentHash={image.content_hash}
					embedIndex={embedIndex}
					onDelete={onDelete}
				/>
			</FocusRing>
		);
	}

	if (isValidMedia(thumbnail)) {
		const {width, height} = calculateMediaDimensions(thumbnail);
		const isGif = thumbnail.content_type === 'image/gif' || thumbnail.url.toLowerCase().endsWith('.gif');

		if (isGif) {
			return (
				<FocusRing within ringClassName={mediaFocusRingClass}>
					<EmbedGif
						embedURL={thumbnail.url}
						proxyURL={thumbnail.proxy_url}
						naturalWidth={thumbnail.width}
						naturalHeight={thumbnail.height}
						placeholder={thumbnail.placeholder}
						nsfw={isMediaNSFW(thumbnail)}
						channelId={message.channelId}
						messageId={message.id}
						message={message}
						contentHash={thumbnail.content_hash}
						embedIndex={embedIndex}
						onDelete={onDelete}
					/>
				</FocusRing>
			);
		}

		return (
			<FocusRing within ringClassName={mediaFocusRingClass}>
				<EmbedImage
					src={getOptimizedMediaURL(thumbnail.proxy_url, width, height)}
					originalSrc={thumbnail.url}
					naturalWidth={thumbnail.width}
					naturalHeight={thumbnail.height}
					width={width}
					height={height}
					placeholder={thumbnail.placeholder}
					constrain={true}
					nsfw={isMediaNSFW(thumbnail)}
					channelId={message.channelId}
					messageId={message.id}
					message={message}
					contentHash={thumbnail.content_hash}
					embedIndex={embedIndex}
					onDelete={onDelete}
				/>
			</FocusRing>
		);
	}

	return null;
});

const RichEmbed: FC<EmbedProps> = observer(({embed, message, embedIndex, onDelete}) => {
	const hasVideo = isValidMedia(embed.video);
	const hasImage = isValidMedia(embed.image);
	const hasThumbnail = isValidMedia(embed.thumbnail);
	const hasAnyMedia = hasVideo || hasImage || hasThumbnail;
	const normalizedUrl = useMemo(() => normalizeUrl(embed.url), [embed.url]);
	const galleryImages = useMemo<Array<Required<EmbedMedia>>>(() => {
		if (embedIndex === undefined || !normalizedUrl) return [];

		const images: Array<Required<EmbedMedia>> = [];

		const tryAddMedia = (media?: Required<EmbedMedia>) => {
			if (!media || images.length >= MAX_GALLERY_MEDIA) return;
			images.push(media);
		};

		const collectMedia = (candidate?: MessageEmbed) => {
			if (!candidate) return;
			const candidateMedia = isValidMedia(candidate.image)
				? candidate.image
				: isValidMedia(candidate.thumbnail)
					? candidate.thumbnail
					: undefined;
			tryAddMedia(candidateMedia);
		};

		collectMedia(embed);

		for (let i = embedIndex + 1; i < message.embeds.length && images.length < MAX_GALLERY_MEDIA; i++) {
			const candidate = message.embeds[i];
			if (!candidate) continue;
			if (normalizeUrl(candidate.url) !== normalizedUrl) break;
			collectMedia(candidate);
		}

		return images;
	}, [embed, embedIndex, message.embeds, normalizedUrl]);
	const showGallery = galleryImages.length > 1 || (!hasAnyMedia && galleryImages.length > 0);
	const galleryAttachments = useMemo(
		() => (showGallery ? buildGalleryAttachments(galleryImages, embed, embedIndex) : undefined),
		[galleryImages, embed, embedIndex, showGallery],
	);
	const isInlineThumbnail = !hasVideo && hasThumbnail && shouldRenderAsInlineThumbnail(embed.thumbnail);
	const shouldRenderInlineThumbnail = isInlineThumbnail && !showGallery;
	const isYouTubeEmbed = embed.provider?.url && new URL(embed.provider.url).hostname === 'www.youtube.com';
	const useNarrowWidth = hasAnyMedia && !shouldRenderInlineThumbnail;

	return (
		<article
			className={clsx(styles.embed, styles.embedFull, markupStyles.markup)}
			style={{
				borderLeft: `4px solid ${getBorderColor(embed.color)}`,
				maxWidth: useNarrowWidth ? '432px' : '516px',
			}}
		>
			<div className={styles.gridContainer}>
				<div className={clsx(styles.grid, shouldRenderInlineThumbnail && styles.hasThumbnail)}>
					<div className={styles.embedContent}>
						<EmbedProvider provider={embed.provider} />
						<EmbedAuthor author={embed.author} />
						<EmbedTitle title={embed.title} url={embed.url} />
						{!isYouTubeEmbed && (
							<EmbedDescription description={embed.description} messageId={message.id} channelId={message.channelId} />
						)}
						<EmbedFields fields={embed.fields?.slice() ?? []} />

						{!shouldRenderInlineThumbnail && hasAnyMedia && (
							<div className={clsx(styles.embedMedia)}>
								{showGallery && galleryAttachments ? (
									<AttachmentMosaic attachments={galleryAttachments} message={message} hideExpiryFootnote={true} />
								) : (
									<EmbedMediaRenderer embed={embed} message={message} embedIndex={embedIndex} onDelete={onDelete} />
								)}
							</div>
						)}

						{embed.footer && (
							<div className={styles.embedFooter}>
								<EmbedFooter
									footer={embed.footer}
									timestamp={embed.timestamp ? new Date(embed.timestamp) : undefined}
								/>
							</div>
						)}
					</div>

					{shouldRenderInlineThumbnail && embed.thumbnail && isValidMedia(embed.thumbnail) && (
						<div className={styles.embedThumbnail}>
							<FocusRing within ringClassName={mediaFocusRingClass}>
								<EmbedImage
									src={getOptimizedMediaURL(
										embed.thumbnail.proxy_url,
										Math.min(80, Math.round((80 * embed.thumbnail.width) / embed.thumbnail.height)),
										80,
										embed.thumbnail.content_type !== 'image/gif',
										embed.thumbnail.content_type === 'image/gif' ? true : undefined,
									)}
									originalSrc={embed.thumbnail.url}
									naturalWidth={embed.thumbnail.width}
									naturalHeight={embed.thumbnail.height}
									width={Math.min(80, Math.round((80 * embed.thumbnail.width) / embed.thumbnail.height))}
									height={80}
									placeholder={embed.thumbnail.placeholder}
									constrain={true}
									isInline={true}
									nsfw={isMediaNSFW(embed.thumbnail)}
									channelId={message.channelId}
									messageId={message.id}
									message={message}
									contentHash={embed.thumbnail.content_hash}
									embedIndex={embedIndex}
									onDelete={onDelete}
								/>
							</FocusRing>
						</div>
					)}
				</div>
			</div>
		</article>
	);
});

export const Embed: FC<EmbedProps> = observer(({embed, message, embedIndex, onDelete}) => {
	const {t} = useLingui();
	const {enabled: isMobile} = MobileLayoutStore;
	const channel = ChannelStore.getChannel(message.channelId);

	const normalizedEmbedUrl = useMemo(() => normalizeUrl(embed.url), [embed.url]);
	const isDuplicateEmbed = useMemo(() => {
		if (embedIndex === undefined || !normalizedEmbedUrl) return false;
		for (let i = 0; i < embedIndex; i++) {
			if (normalizeUrl(message.embeds[i]?.url) === normalizedEmbedUrl) {
				return true;
			}
		}
		return false;
	}, [embedIndex, message.embeds, normalizedEmbedUrl]);

	const canSuppressEmbeds = useCallback(() => {
		const guild = channel?.guildId ? GuildStore.getGuild(channel.guildId) : null;
		const sendMessageDisabled = guild ? (guild.disabledOperations & GuildOperations.SEND_MESSAGE) !== 0 : false;

		if (sendMessageDisabled) return false;
		if (message.isCurrentUserAuthor()) return true;
		if (!channel || channel.isPrivate()) return false;
		return PermissionStore.can(Permissions.MANAGE_MESSAGES, {channelId: message.channelId});
	}, [message, channel]);

	const handleSuppressEmbeds = useCallback(() => {
		ModalActionCreators.push(modal(() => <SuppressEmbedsConfirmModal message={message} />));
	}, [message]);

	const showSuppressButton = !isMobile && canSuppressEmbeds() && AccessibilityStore.showSuppressEmbedsButton;

	const spoileredUrls = useMemo(() => extractSpoileredUrls(message.content), [message.content]);
	const {isSpoilerEmbed, matchingSpoilerUrls} = useMemo(() => {
		const normalize = (url?: string) => {
			if (!url) return null;
			try {
				return new URL(url).href.replace(/\/$/, '');
			} catch {
				return null;
			}
		};

		const urlsToCheck = [
			embed.url,
			embed.provider?.url,
			embed.image?.url,
			embed.image?.proxy_url,
			embed.thumbnail?.url,
			embed.thumbnail?.proxy_url,
			embed.video?.url,
			embed.video?.proxy_url,
			embed.audio?.url,
			embed.audio?.proxy_url,
		].filter(Boolean) as Array<string>;

		if (spoileredUrls.size === 0) return {isSpoilerEmbed: false, matchingSpoilerUrls: [] as Array<string>};

		const matches = urlsToCheck
			.map((url) => normalize(url))
			.filter((normalized): normalized is string => !!normalized && spoileredUrls.has(normalized));

		return {isSpoilerEmbed: matches.length > 0, matchingSpoilerUrls: matches};
	}, [embed, message.content, spoileredUrls]);

	const {hidden: spoilerHidden, reveal: revealSpoiler} = useSpoilerState(
		isSpoilerEmbed,
		message.channelId,
		matchingSpoilerUrls,
	);
	const wrapSpoiler = (node: React.ReactElement) =>
		isSpoilerEmbed ? (
			<SpoilerOverlay hidden={spoilerHidden} onReveal={revealSpoiler}>
				{node}
			</SpoilerOverlay>
		) : (
			node
		);

	if (isDuplicateEmbed) {
		return null;
	}

	const hasRichContent = !!(
		embed.title ||
		embed.description ||
		embed.author ||
		embed.footer ||
		embed.fields?.length ||
		(embed.provider &&
			!(
				embed.type === MessageEmbedTypes.GIFV &&
				embed.provider.url &&
				new URL(embed.provider.url).hostname === 'tenor.com'
			))
	);

	if (!hasRichContent) {
		if (embed.type === MessageEmbedTypes.AUDIO && embed.audio?.proxy_url) {
			return wrapSpoiler(
				<div className={styles.container}>
					{showSuppressButton && (
						<button
							type="button"
							onClick={handleSuppressEmbeds}
							className={clsx(messageStyles.hoverAction, styles.suppressButton)}
							aria-label={t`Suppress embeds`}
						>
							<XIcon size={16} weight="bold" />
						</button>
					)}
					<div className={mosaicStyles.mosaicContainer}>
						<div className={mosaicStyles.oneByOneGrid}>
							<FocusRing within ringClassName={mediaFocusRingClass}>
								<EmbedAudio
									src={embed.audio.proxy_url}
									title={embed.title}
									duration={embed.audio.duration}
									embedUrl={embed.url}
									channelId={message.channelId}
									messageId={message.id}
									message={message}
									contentHash={embed.audio.content_hash}
									embedIndex={embedIndex}
									onDelete={onDelete}
								/>
							</FocusRing>
						</div>
					</div>
				</div>,
			);
		}

		if (embed.type === MessageEmbedTypes.VIDEO && isValidMedia(embed.video)) {
			if (embed.provider?.url && new URL(embed.provider.url).hostname === 'www.youtube.com') {
				return wrapSpoiler(
					<div className={styles.container}>
						{showSuppressButton && (
							<button
								type="button"
								onClick={handleSuppressEmbeds}
								className={clsx(messageStyles.hoverAction, styles.suppressButton)}
								aria-label={t`Suppress embeds`}
							>
								<XIcon size={16} weight="bold" />
							</button>
						)}
						<div className={mosaicStyles.mosaicContainer}>
							<div className={mosaicStyles.oneByOneGrid}>
								<FocusRing within ringClassName={mediaFocusRingClass}>
									<EmbedYouTube embed={embed} />
								</FocusRing>
							</div>
						</div>
					</div>,
				);
			}

			const {width, height} = calculateMediaDimensions(embed.video);
			return wrapSpoiler(
				<div className={styles.container}>
					{showSuppressButton && (
						<button
							type="button"
							onClick={handleSuppressEmbeds}
							className={clsx(messageStyles.hoverAction, styles.suppressButton)}
							aria-label={t`Suppress embeds`}
						>
							<XIcon size={16} weight="bold" />
						</button>
					)}
					<div className={mosaicStyles.mosaicContainer}>
						<div className={mosaicStyles.oneByOneGrid}>
							<FocusRing within ringClassName={mediaFocusRingClass}>
								<EmbedVideo
									src={embed.video.proxy_url}
									width={width}
									height={height}
									placeholder={embed.video.placeholder}
									title={embed.title}
									duration={embed.video.duration}
									nsfw={isMediaNSFW(embed.video)}
									channelId={message.channelId}
									messageId={message.id}
									embedUrl={embed.url}
									message={message}
									contentHash={embed.video.content_hash}
									embedIndex={embedIndex}
									onDelete={onDelete}
								/>
							</FocusRing>
						</div>
					</div>
				</div>,
			);
		}

		const {thumbnail} = embed;
		if (
			embed.type === MessageEmbedTypes.IMAGE &&
			isValidMedia(thumbnail) &&
			(thumbnail.flags & MessageAttachmentFlags.IS_ANIMATED) === MessageAttachmentFlags.IS_ANIMATED
		) {
			const {width, height} = calculateMediaDimensions(thumbnail);
			return wrapSpoiler(
				<div className={styles.container}>
					{showSuppressButton && (
						<button
							type="button"
							onClick={handleSuppressEmbeds}
							className={clsx(messageStyles.hoverAction, styles.suppressButton)}
							aria-label={t`Suppress embeds`}
						>
							<XIcon size={16} weight="bold" />
						</button>
					)}
					<div className={mosaicStyles.mosaicContainer}>
						<div className={mosaicStyles.oneByOneGrid}>
							<FocusRing within ringClassName={mediaFocusRingClass}>
								<EmbedGif
									embedURL={thumbnail.url}
									proxyURL={getOptimizedMediaURL(thumbnail.proxy_url, width, height)}
									naturalWidth={thumbnail.width}
									naturalHeight={thumbnail.height}
									placeholder={thumbnail.placeholder}
									channelId={message.channelId}
									messageId={message.id}
									message={message}
									contentHash={thumbnail.content_hash}
									embedIndex={embedIndex}
									onDelete={onDelete}
								/>
							</FocusRing>
						</div>
					</div>
				</div>,
			);
		}

		if (embed.type === MessageEmbedTypes.GIFV && isValidMedia(embed.video) && isValidMedia(thumbnail) && embed.url) {
			return wrapSpoiler(
				<div className={styles.container}>
					{showSuppressButton && (
						<button
							type="button"
							onClick={handleSuppressEmbeds}
							className={clsx(messageStyles.hoverAction, styles.suppressButton)}
							aria-label={t`Suppress embeds`}
						>
							<XIcon size={16} weight="bold" />
						</button>
					)}
					<div className={mosaicStyles.mosaicContainer}>
						<div className={mosaicStyles.oneByOneGrid}>
							<FocusRing within ringClassName={mediaFocusRingClass}>
								<EmbedGifv
									embedURL={embed.url}
									videoProxyURL={embed.video.proxy_url}
									videoURL={embed.video.url}
									naturalWidth={thumbnail.width}
									naturalHeight={thumbnail.height}
									placeholder={thumbnail.placeholder}
									channelId={message.channelId}
									messageId={message.id}
									message={message}
									contentHash={embed.video.content_hash}
									embedIndex={embedIndex}
									onDelete={onDelete}
								/>
							</FocusRing>
						</div>
					</div>
				</div>,
			);
		}

		if (isValidMedia(thumbnail)) {
			const {width, height} = calculateMediaDimensions(thumbnail);
			const isGif = thumbnail.content_type === 'image/gif' || thumbnail.url.toLowerCase().endsWith('.gif');

			if (isGif) {
				return wrapSpoiler(
					<div className={styles.container}>
						{showSuppressButton && (
							<button
								type="button"
								onClick={handleSuppressEmbeds}
								className={clsx(messageStyles.hoverAction, styles.suppressButton)}
								aria-label={t`Suppress embeds`}
							>
								<XIcon size={16} weight="bold" />
							</button>
						)}
						<div className={mosaicStyles.mosaicContainer}>
							<div className={mosaicStyles.oneByOneGrid}>
								<FocusRing within ringClassName={mediaFocusRingClass}>
									<EmbedGif
										embedURL={thumbnail.url}
										proxyURL={thumbnail.proxy_url}
										naturalWidth={thumbnail.width}
										naturalHeight={thumbnail.height}
										placeholder={thumbnail.placeholder}
										nsfw={isMediaNSFW(thumbnail)}
										channelId={message.channelId}
										messageId={message.id}
										message={message}
										contentHash={thumbnail.content_hash}
										embedIndex={embedIndex}
										onDelete={onDelete}
									/>
								</FocusRing>
							</div>
						</div>
					</div>,
				);
			}

			return wrapSpoiler(
				<div className={styles.container}>
					{showSuppressButton && (
						<button
							type="button"
							onClick={handleSuppressEmbeds}
							className={clsx(messageStyles.hoverAction, styles.suppressButton)}
							aria-label={t`Suppress embeds`}
						>
							<XIcon size={16} weight="bold" />
						</button>
					)}
					<div className={mosaicStyles.mosaicContainer}>
						<div className={mosaicStyles.oneByOneGrid}>
							<FocusRing within ringClassName={mediaFocusRingClass}>
								<EmbedImage
									src={getOptimizedMediaURL(thumbnail.proxy_url, width, height)}
									originalSrc={thumbnail.url}
									naturalWidth={thumbnail.width}
									naturalHeight={thumbnail.height}
									width={width}
									height={height}
									placeholder={thumbnail.placeholder}
									constrain={true}
									nsfw={isMediaNSFW(thumbnail)}
									channelId={message.channelId}
									messageId={message.id}
									message={message}
									contentHash={thumbnail.content_hash}
									embedIndex={embedIndex}
									onDelete={onDelete}
								/>
							</FocusRing>
						</div>
					</div>
				</div>,
			);
		}

		return null;
	}

	return wrapSpoiler(
		<div className={styles.container}>
			{showSuppressButton && (
				<button
					type="button"
					onClick={handleSuppressEmbeds}
					className={clsx(messageStyles.hoverAction, styles.suppressButton)}
					aria-label={t`Suppress embeds`}
				>
					<XIcon size={16} weight="bold" />
				</button>
			)}
			<RichEmbed embed={embed} message={message} embedIndex={embedIndex} onDelete={onDelete} />
		</div>,
	);
});
