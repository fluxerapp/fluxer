// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {SpoilerOverlay} from '@app/features/app/components/shared/SpoilerOverlay';
import styles from '@app/features/channel/components/embeds/ChannelEmbed.module.css';
import {BlueskyEmbed} from '@app/features/channel/components/embeds/channel_embed/BlueskyEmbed';
import {
	calculateMediaDimensions,
	type EmbedProps,
	getOptimizedMediaURL,
	getUrlHostname,
	isDuplicateEmbedAtIndex,
	isMediaMatureContent,
	isValidMedia,
} from '@app/features/channel/components/embeds/channel_embed/ChannelEmbedShared';
import {SuppressEmbedsConfirmModal} from '@app/features/channel/components/embeds/channel_embed/EmbedLink';
import {RichEmbed} from '@app/features/channel/components/embeds/channel_embed/RichEmbed';
import {hasRichEmbedContent} from '@app/features/channel/components/embeds/EmbedRenderUtils';
import EmbedAudio from '@app/features/channel/components/embeds/media/EmbedAudio';
import {EmbedGif, EmbedGifv} from '@app/features/channel/components/embeds/media/EmbedGifv';
import {EmbedImage} from '@app/features/channel/components/embeds/media/EmbedImage';
import EmbedVideo from '@app/features/channel/components/embeds/media/EmbedVideo';
import {EmbedYouTube} from '@app/features/channel/components/embeds/media/EmbedYouTube';
import {getInlineVideoLayoutConstraints} from '@app/features/channel/components/embeds/media/VideoDimensionUtils';
import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import {SUPPRESS_EMBEDS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {getEmbedMediaDimensions} from '@app/features/messaging/utils/MediaDimensionConfig';
import {buildAnimatedImageProxyURL} from '@app/features/messaging/utils/MediaProxyUtils';
import {buildMessageEmbedCopyText} from '@app/features/messaging/utils/MessageCopyTextUtils';
import {canonicalizeMediaUrl, extractSpoileredUrls, useSpoilerState} from '@app/features/messaging/utils/SpoilerUtils';
import Permission from '@app/features/permissions/state/Permission';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import messageStyles from '@app/features/theme/styles/Message.module.css';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {MessageAttachmentFlags, MessageEmbedTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {useLingui} from '@lingui/react/macro';
import {XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {type FC, useCallback, useMemo} from 'react';

const mediaFocusRingClass = messageStyles.mediaFocusRing;
export const Embed: FC<EmbedProps> = observer(({embed, message, embedIndex, contextualEmbeds, onDelete, isPreview}) => {
	const {i18n} = useLingui();
	const {enabled: isMobile} = MobileLayout;
	const channel = Channels.getChannel(message.channelId);
	const embedList = contextualEmbeds ?? message.embeds;
	const isDuplicateEmbed = useMemo(() => isDuplicateEmbedAtIndex(embedIndex, embedList), [embedIndex, embedList]);
	const canSuppressEmbeds = useCallback(() => {
		const guild = channel?.guildId ? Guilds.getGuild(channel.guildId) : null;
		const sendMessageDisabled = guild ? (guild.disabledOperations & GuildOperations.SEND_MESSAGE) !== 0 : false;
		if (sendMessageDisabled) return false;
		if (message.isCurrentUserAuthor()) return true;
		if (!channel || channel.isPrivate()) return false;
		return Permission.can(Permissions.MANAGE_MESSAGES, {channelId: message.channelId});
	}, [message, channel]);
	const handleSuppressEmbeds = useCallback(
		(e: React.MouseEvent<HTMLButtonElement>) => {
			e.preventDefault();
			e.stopPropagation();
			if (e.shiftKey) {
				void MessageCommands.toggleSuppressEmbeds(message.channelId, message.id, message.flags);
				return;
			}
			ModalCommands.push(
				modal(() => (
					<SuppressEmbedsConfirmModal
						message={message}
						data-flx="channel.embeds.embed.handle-suppress-embeds.suppress-embeds-confirm-modal"
					/>
				)),
			);
		},
		[message],
	);
	const showSuppressButton = !isMobile && canSuppressEmbeds() && Accessibility.showSuppressEmbedsButton && !isPreview;
	const copyText = useMemo(
		() =>
			buildMessageEmbedCopyText(embed, {
				channelId: message.channelId,
				messageId: message.id,
				i18n,
			}),
		[embed, i18n.locale, message.channelId, message.id],
	);
	const copyBlockProps = copyText
		? ({
				'data-message-copy-block': 'true',
				'data-message-copy-text': copyText,
			} as const)
		: {};
	const spoileredUrls = useMemo(() => extractSpoileredUrls(message.content), [message.content]);
	const {isSpoilerEmbed, matchingSpoilerUrls} = useMemo(() => {
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
			.map((url) => canonicalizeMediaUrl(url))
			.filter((canonical): canonical is string => !!canonical && spoileredUrls.has(canonical));
		return {isSpoilerEmbed: matches.length > 0, matchingSpoilerUrls: matches};
	}, [embed, spoileredUrls]);
	const {hidden: spoilerHidden, reveal: revealSpoiler} = useSpoilerState(
		isSpoilerEmbed,
		message.channelId,
		matchingSpoilerUrls,
	);
	const wrapSpoiler = (node: React.ReactElement) =>
		isSpoilerEmbed ? (
			<SpoilerOverlay
				hidden={spoilerHidden}
				onReveal={revealSpoiler}
				data-flx="channel.embeds.embed.wrap-spoiler.spoiler-overlay"
			>
				{node}
			</SpoilerOverlay>
		) : (
			node
		);
	const renderSuppressButton = (extraClassName?: string) => {
		if (!showSuppressButton) return null;
		return (
			<button
				type="button"
				onClick={handleSuppressEmbeds}
				className={clsx(messageStyles.hoverAction, styles.suppressButton, extraClassName)}
				aria-label={i18n._(SUPPRESS_EMBEDS_DESCRIPTOR)}
				data-flx="channel.embeds.embed.render-suppress-button.suppress-button.suppress-embeds"
			>
				<XIcon size={remFromPx(16)} weight="bold" data-flx="channel.embeds.embed.render-suppress-button.x-icon" />
			</button>
		);
	};
	const wrapMediaOnlyEmbed = (mediaContent: React.ReactNode) =>
		wrapSpoiler(
			<div
				className={styles.container}
				{...copyBlockProps}
				data-search-highlight-scope="message"
				data-flx="channel.embeds.embed.wrap-media-only-embed.container"
			>
				<div className={styles.mediaFrame} data-flx="channel.embeds.embed.wrap-media-only-embed.media-frame">
					{renderSuppressButton(styles.mediaSuppressButton)}
					{mediaContent}
				</div>
			</div>,
		);
	if (isDuplicateEmbed) return null;
	const hasRichContent = hasRichEmbedContent(embed);
	if (!hasRichContent) {
		if (embed.type === MessageEmbedTypes.AUDIO && embed.audio?.proxy_url) {
			return wrapMediaOnlyEmbed(
				<FocusRing within ringClassName={mediaFocusRingClass} data-flx="channel.embeds.embed.focus-ring">
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
						isPreview={isPreview}
						data-flx="channel.embeds.embed.embed-audio"
					/>
				</FocusRing>,
			);
		}
		if (embed.type === MessageEmbedTypes.VIDEO && isValidMedia(embed.video)) {
			if (getUrlHostname(embed.provider?.url) === 'www.youtube.com') {
				return wrapMediaOnlyEmbed(
					<FocusRing within ringClassName={mediaFocusRingClass} data-flx="channel.embeds.embed.focus-ring--2">
						<EmbedYouTube embed={embed} data-flx="channel.embeds.embed.embed-you-tube" />
					</FocusRing>,
				);
			}
			const videoLayoutConstraints = getInlineVideoLayoutConstraints(getEmbedMediaDimensions());
			return wrapMediaOnlyEmbed(
				<FocusRing within ringClassName={mediaFocusRingClass} data-flx="channel.embeds.embed.focus-ring--3">
					<EmbedVideo
						src={embed.video.proxy_url}
						width={embed.video.width}
						height={embed.video.height}
						maxWidth={videoLayoutConstraints.maxWidth}
						maxHeight={videoLayoutConstraints.maxHeight}
						placeholder={embed.video.placeholder}
						title={embed.title}
						alt={embed.video.description ?? undefined}
						duration={embed.video.duration}
						nsfw={isMediaMatureContent(embed.video)}
						channelId={message.channelId}
						messageId={message.id}
						embedUrl={embed.url}
						message={message}
						contentHash={embed.video.content_hash}
						embedIndex={embedIndex}
						onDelete={onDelete}
						data-flx="channel.embeds.embed.embed-video"
					/>
				</FocusRing>,
			);
		}
		const {thumbnail} = embed;
		if (
			embed.type === MessageEmbedTypes.IMAGE &&
			isValidMedia(thumbnail) &&
			(thumbnail.flags & MessageAttachmentFlags.IS_ANIMATED) === MessageAttachmentFlags.IS_ANIMATED
		) {
			return wrapMediaOnlyEmbed(
				<FocusRing within ringClassName={mediaFocusRingClass} data-flx="channel.embeds.embed.focus-ring--4">
					<EmbedGif
						embedURL={thumbnail.url}
						proxyURL={buildAnimatedImageProxyURL(thumbnail.proxy_url)}
						naturalWidth={thumbnail.width}
						naturalHeight={thumbnail.height}
						placeholder={thumbnail.placeholder}
						alt={thumbnail.description ?? embed.description ?? undefined}
						channelId={message.channelId}
						messageId={message.id}
						message={message}
						contentHash={thumbnail.content_hash}
						embedIndex={embedIndex}
						onDelete={onDelete}
						data-flx="channel.embeds.embed.embed-gif"
					/>
				</FocusRing>,
			);
		}
		if (embed.type === MessageEmbedTypes.GIFV && isValidMedia(embed.video) && isValidMedia(thumbnail) && embed.url) {
			return wrapMediaOnlyEmbed(
				<FocusRing within ringClassName={mediaFocusRingClass} data-flx="channel.embeds.embed.focus-ring--5">
					<EmbedGifv
						embedURL={embed.url}
						videoProxyURL={embed.video.proxy_url}
						videoURL={embed.video.url}
						thumbnailProxyURL={thumbnail.proxy_url}
						naturalWidth={embed.video.width}
						naturalHeight={embed.video.height}
						placeholder={thumbnail.placeholder}
						alt={embed.video.description ?? thumbnail.description ?? embed.description ?? undefined}
						channelId={message.channelId}
						messageId={message.id}
						message={message}
						contentHash={embed.video.content_hash}
						embedIndex={embedIndex}
						onDelete={onDelete}
						providerName={embed.provider?.name}
						data-flx="channel.embeds.embed.embed-gifv"
					/>
				</FocusRing>,
			);
		}
		if (isValidMedia(thumbnail)) {
			const {width, height} = calculateMediaDimensions(thumbnail);
			const isGif = thumbnail.content_type === 'image/gif' || thumbnail.url.toLowerCase().endsWith('.gif');
			const thumbnailIsAnimated =
				(thumbnail.flags & MessageAttachmentFlags.IS_ANIMATED) === MessageAttachmentFlags.IS_ANIMATED;
			if (isGif) {
				return wrapMediaOnlyEmbed(
					<FocusRing within ringClassName={mediaFocusRingClass} data-flx="channel.embeds.embed.focus-ring--6">
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
							data-flx="channel.embeds.embed.embed-gif--2"
						/>
					</FocusRing>,
				);
			}
			return wrapMediaOnlyEmbed(
				<FocusRing within ringClassName={mediaFocusRingClass} data-flx="channel.embeds.embed.focus-ring--7">
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
						data-flx="channel.embeds.embed.embed-image"
					/>
				</FocusRing>,
			);
		}
		return null;
	}
	return wrapSpoiler(
		<div
			className={styles.container}
			{...copyBlockProps}
			data-search-highlight-scope="message"
			data-flx="channel.embeds.embed.container"
		>
			{showSuppressButton && (
				<button
					type="button"
					onClick={handleSuppressEmbeds}
					className={clsx(messageStyles.hoverAction, styles.suppressButton)}
					aria-label={i18n._(SUPPRESS_EMBEDS_DESCRIPTOR)}
					data-flx="channel.embeds.embed.suppress-button.suppress-embeds"
				>
					<XIcon size={remFromPx(16)} weight="bold" data-flx="channel.embeds.embed.x-icon" />
				</button>
			)}
			{embed.type === MessageEmbedTypes.BLUESKY ? (
				<BlueskyEmbed
					embed={embed}
					message={message}
					embedIndex={embedIndex}
					contextualEmbeds={contextualEmbeds}
					onDelete={onDelete}
					isPreview={isPreview}
					data-flx="channel.embeds.channel-embed.embed.bluesky-embed"
				/>
			) : (
				<RichEmbed
					embed={embed}
					message={message}
					embedIndex={embedIndex}
					contextualEmbeds={contextualEmbeds}
					onDelete={onDelete}
					isPreview={isPreview}
					data-flx="channel.embeds.channel-embed.embed.rich-embed"
				/>
			)}
		</div>,
	);
});
