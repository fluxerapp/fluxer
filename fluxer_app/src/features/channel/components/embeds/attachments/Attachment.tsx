// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {ExpiryFootnote} from '@app/features/app/components/shared/ExpiryFootnote';
import {SpoilerOverlay} from '@app/features/app/components/shared/SpoilerOverlay';
import spoilerStyles from '@app/features/app/components/shared/SpoilerOverlay.module.css';
import styles from '@app/features/channel/components/embeds/attachments/Attachment.module.css';
import {AttachmentFile} from '@app/features/channel/components/embeds/attachments/AttachmentFile';
import EmbedAudio from '@app/features/channel/components/embeds/media/EmbedAudio';
import {EmbedGif, EmbedGifv} from '@app/features/channel/components/embeds/media/EmbedGifv';
import {EmbedImage} from '@app/features/channel/components/embeds/media/EmbedImage';
import EmbedVideo from '@app/features/channel/components/embeds/media/EmbedVideo';
import {getInlineVideoLayoutConstraints} from '@app/features/channel/components/embeds/media/VideoDimensionUtils';
import VoiceMessagePlayer from '@app/features/channel/components/embeds/media/VoiceMessagePlayer';
import {isMediaAttachment} from '@app/features/channel/components/MessageAttachmentUtils';
import {MessageUploadProgress} from '@app/features/channel/components/MessageUploadProgress';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {UploadingAttachment} from '@app/features/messaging/models/UploadingAttachment';
import {getEffectiveAttachmentExpiry} from '@app/features/messaging/utils/AttachmentExpiryUtils';
import {
	getAttachmentMediaDimensions,
	isUsefulVisualMediaSize,
} from '@app/features/messaging/utils/MediaDimensionConfig';
import {resolveProxyRequestSize} from '@app/features/messaging/utils/MediaProxyRequestSize';
import {
	buildAnimatedImageProxyURL,
	buildMediaProxyURL,
	resolvePreferredImageFormat,
} from '@app/features/messaging/utils/MediaProxyUtils';
import {useSpoilerState} from '@app/features/messaging/utils/SpoilerUtils';
import messageStyles from '@app/features/theme/styles/Message.module.css';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {createCalculator} from '@app/features/ui/utils/DimensionUtils';
import UserSettings from '@app/features/user/state/UserSettings';
import {MessageAttachmentFlags, MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import type {MessageAttachment} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type {FC} from 'react';

const THIS_ATTACHMENT_HAS_EXPIRED_DESCRIPTOR = msg({
	message: 'This attachment has expired',
	comment: 'Error message in the channel and chat attachment.',
});

interface AttachmentProps {
	attachment: MessageAttachment;
	isPreview?: boolean;
	snapshotIndex?: number;
	message?: Message;
	renderInMosaic?: boolean;
	mediaAttachments?: ReadonlyArray<MessageAttachment>;
	onDelete?: (bypassConfirm?: boolean) => void;
}

interface AttachmentMediaProps {
	attachment: MessageAttachment;
	mediaAttachments?: ReadonlyArray<MessageAttachment>;
	onDelete?: (bypassConfirm?: boolean) => void;
}

const isImageType = (contentType?: string): boolean => contentType?.startsWith('image/') ?? false;
const isVideoType = (contentType?: string): boolean => contentType?.startsWith('video/') ?? false;
const isAudioType = (contentType?: string): boolean => contentType?.startsWith('audio/') ?? false;
const isGifType = (contentType?: string): boolean => contentType === 'image/gif';
const isAnimated = (flags: number): boolean => (flags & MessageAttachmentFlags.IS_ANIMATED) !== 0;
const isVoiceMessageAttachment = (message: Message | undefined, attachment: MessageAttachment): boolean =>
	Boolean(message?.hasFlag(MessageFlags.VOICE_MESSAGE) && isAudioType(attachment.content_type) && attachment.waveform);
const hasValidDimensions = (attachment: MessageAttachment): boolean =>
	typeof attachment.width === 'number' &&
	attachment.width > 0 &&
	typeof attachment.height === 'number' &&
	attachment.height > 0;
const AnimatedAttachment: FC<AttachmentMediaProps & {message?: Message; isPreview?: boolean; snapshotIndex?: number}> =
	observer(({attachment, message, isPreview, snapshotIndex, onDelete}) => {
		const embedUrl = attachment.url ?? '';
		const proxyUrl = attachment.proxy_url ?? embedUrl;
		const nsfw = attachment.nsfw || (attachment.flags & MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA) !== 0;
		return (
			<FocusRing
				within
				ringClassName={messageStyles.mediaFocusRing}
				data-flx="channel.embeds.attachments.attachment.animated-attachment.focus-ring"
			>
				<EmbedGif
					embedURL={embedUrl}
					proxyURL={buildAnimatedImageProxyURL(proxyUrl)}
					naturalWidth={attachment.width!}
					naturalHeight={attachment.height!}
					placeholder={attachment.placeholder}
					alt={attachment.description ?? undefined}
					nsfw={nsfw}
					channelId={message?.channelId}
					messageId={message?.id}
					attachmentId={attachment.id}
					message={message}
					contentHash={attachment.content_hash}
					isPreview={isPreview}
					snapshotIndex={snapshotIndex}
					onDelete={onDelete}
					data-flx="channel.embeds.attachments.attachment.animated-attachment.embed-gif"
				/>
			</FocusRing>
		);
	});
const VideoAttachment: FC<AttachmentMediaProps & {message?: Message; isPreview?: boolean; snapshotIndex?: number}> =
	observer(({attachment, message, mediaAttachments = [], isPreview, snapshotIndex, onDelete}) => {
		const embedUrl = attachment.url ?? '';
		const proxyUrl = attachment.proxy_url ?? embedUrl;
		const nsfw = attachment.nsfw || (attachment.flags & MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA) !== 0;
		const attachmentDimensions = getAttachmentMediaDimensions();
		const videoLayoutConstraints = getInlineVideoLayoutConstraints(attachmentDimensions);
		return (
			<FocusRing
				within
				ringClassName={messageStyles.mediaFocusRing}
				data-flx="channel.embeds.attachments.attachment.video-attachment.focus-ring"
			>
				<div
					className={styles.attachmentWrapper}
					data-flx="channel.embeds.attachments.attachment.video-attachment.attachment-wrapper"
				>
					<EmbedVideo
						src={proxyUrl}
						width={attachment.width!}
						height={attachment.height!}
						maxWidth={videoLayoutConstraints.maxWidth}
						maxHeight={videoLayoutConstraints.maxHeight}
						placeholder={attachment.placeholder}
						title={attachment.title}
						alt={attachment.description ?? undefined}
						duration={attachment.duration}
						nsfw={nsfw}
						channelId={message?.channelId}
						messageId={message?.id}
						attachmentId={attachment.id}
						embedUrl={embedUrl}
						message={message}
						contentHash={attachment.content_hash}
						mediaAttachments={mediaAttachments}
						isPreview={isPreview}
						snapshotIndex={snapshotIndex}
						onDelete={onDelete}
						data-flx="channel.embeds.attachments.attachment.video-attachment.embed-video"
					/>
				</div>
			</FocusRing>
		);
	});
const GifvAttachment: FC<AttachmentMediaProps & {message?: Message; isPreview?: boolean; snapshotIndex?: number}> =
	observer(({attachment, message, isPreview, snapshotIndex, onDelete}) => {
		const embedUrl = attachment.url ?? '';
		const proxyUrl = attachment.proxy_url ?? embedUrl;
		const nsfw = attachment.nsfw || (attachment.flags & MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA) !== 0;
		return (
			<FocusRing
				within
				ringClassName={messageStyles.mediaFocusRing}
				data-flx="channel.embeds.attachments.attachment.gifv-attachment.focus-ring"
			>
				<div
					className={styles.attachmentWrapper}
					data-flx="channel.embeds.attachments.attachment.gifv-attachment.attachment-wrapper"
				>
					<EmbedGifv
						embedURL={embedUrl}
						videoProxyURL={proxyUrl}
						videoURL={embedUrl}
						naturalWidth={attachment.width!}
						naturalHeight={attachment.height!}
						placeholder={attachment.placeholder}
						alt={attachment.description ?? undefined}
						nsfw={nsfw}
						channelId={message?.channelId}
						messageId={message?.id}
						attachmentId={attachment.id}
						message={message}
						contentHash={attachment.content_hash}
						isPreview={isPreview}
						snapshotIndex={snapshotIndex}
						onDelete={onDelete}
						data-flx="channel.embeds.attachments.attachment.gifv-attachment.embed-gifv"
					/>
				</div>
			</FocusRing>
		);
	});
const AudioAttachment: FC<AttachmentMediaProps & {message?: Message; isPreview?: boolean; snapshotIndex?: number}> =
	observer(({attachment, message, isPreview, snapshotIndex, onDelete}) => (
		<FocusRing
			within
			ringClassName={messageStyles.mediaFocusRing}
			data-flx="channel.embeds.attachments.attachment.audio-attachment.focus-ring"
		>
			<div
				className={styles.attachmentWrapper}
				data-flx="channel.embeds.attachments.attachment.audio-attachment.attachment-wrapper"
			>
				<EmbedAudio
					src={attachment.proxy_url ?? attachment.url ?? ''}
					title={attachment.title || attachment.filename}
					duration={attachment.duration}
					embedUrl={attachment.url ?? ''}
					channelId={message?.channelId}
					messageId={message?.id}
					attachmentId={attachment.id}
					message={message}
					contentHash={attachment.content_hash}
					isPreview={isPreview}
					snapshotIndex={snapshotIndex}
					onDelete={onDelete}
					data-flx="channel.embeds.attachments.attachment.audio-attachment.embed-audio"
				/>
			</div>
		</FocusRing>
	));
const AttachmentMedia: FC<AttachmentMediaProps & {message?: Message; isPreview?: boolean; snapshotIndex?: number}> =
	observer(({attachment, message, mediaAttachments = [], isPreview, snapshotIndex, onDelete}) => {
		const attachmentIsAnimated =
			isGifType(attachment.content_type) || (isAnimated(attachment.flags) && !isVideoType(attachment.content_type));
		const nsfw = attachment.nsfw || (attachment.flags & MessageAttachmentFlags.CONTAINS_EXPLICIT_MEDIA) !== 0;
		if (isVoiceMessageAttachment(message, attachment)) {
			const src = attachment.proxy_url ?? attachment.url ?? '';
			return (
				<VoiceMessagePlayer
					src={src}
					title={attachment.title ?? attachment.filename}
					duration={attachment.duration ?? undefined}
					waveform={attachment.waveform!}
					channelId={message?.channelId}
					messageId={message?.id}
					attachmentId={attachment.id}
					message={message}
					contentHash={attachment.content_hash ?? undefined}
					mediaAttachments={mediaAttachments}
					isPreview={isPreview}
					snapshotIndex={snapshotIndex}
					onDelete={onDelete}
					data-flx="channel.embeds.attachments.attachment.attachment-media.voice-message-player"
				/>
			);
		}
		if (attachmentIsAnimated) {
			return (
				<AnimatedAttachment
					attachment={attachment}
					message={message}
					isPreview={isPreview}
					snapshotIndex={snapshotIndex}
					onDelete={onDelete}
					data-flx="channel.embeds.attachments.attachment.attachment-media.animated-attachment"
				/>
			);
		}
		const attachmentDimensions = getAttachmentMediaDimensions();
		const mediaCalculator = createCalculator({
			maxWidth: attachmentDimensions.maxWidth,
			maxHeight: attachmentDimensions.maxHeight,
			responsive: true,
		});
		const {dimensions} = mediaCalculator.calculate({
			width: attachment.width!,
			height: attachment.height!,
		});
		const requestedSize = resolveProxyRequestSize(
			dimensions.width,
			dimensions.height,
			attachment.width!,
			attachment.height!,
		);
		const proxySrc = attachment.proxy_url ?? attachment.url ?? '';
		const optimizedSrc = buildMediaProxyURL(proxySrc, {
			format: resolvePreferredImageFormat(attachment.content_type),
			width: requestedSize?.width,
			height: requestedSize?.height,
			animated: attachmentIsAnimated,
		});
		return (
			<FocusRing
				within
				ringClassName={messageStyles.mediaFocusRing}
				data-flx="channel.embeds.attachments.attachment.attachment-media.focus-ring"
			>
				<div
					className={styles.attachmentWrapper}
					data-flx="channel.embeds.attachments.attachment.attachment-media.attachment-wrapper"
				>
					<EmbedImage
						src={optimizedSrc}
						originalSrc={attachment.url ?? ''}
						naturalWidth={attachment.width!}
						naturalHeight={attachment.height!}
						width={dimensions.width}
						height={dimensions.height}
						placeholder={attachment.placeholder}
						constrain={true}
						alt={attachment.description ?? undefined}
						nsfw={nsfw}
						channelId={message?.channelId}
						messageId={message?.id}
						attachmentId={attachment.id}
						message={message}
						contentHash={attachment.content_hash}
						mediaAttachments={mediaAttachments}
						isPreview={isPreview}
						snapshotIndex={snapshotIndex}
						animated={attachmentIsAnimated}
						onDelete={onDelete}
						data-flx="channel.embeds.attachments.attachment.attachment-media.embed-image"
					/>
				</div>
			</FocusRing>
		);
	});
export const Attachment: FC<AttachmentProps> = observer(
	({attachment, isPreview, snapshotIndex, message, renderInMosaic, onDelete}) => {
		const {i18n} = useLingui();
		const isSpoiler = (attachment.flags & MessageAttachmentFlags.IS_SPOILER) !== 0;
		const {hidden: spoilerHidden, reveal: revealSpoiler} = useSpoilerState(isSpoiler, message?.channelId);
		const wrapSpoiler = (node: React.ReactElement, className?: string) =>
			isSpoiler ? (
				<SpoilerOverlay
					hidden={spoilerHidden}
					onReveal={revealSpoiler}
					className={className}
					data-flx="channel.embeds.attachments.attachment.wrap-spoiler.spoiler-overlay"
				>
					{node}
				</SpoilerOverlay>
			) : (
				node
			);
		if (UploadingAttachment.is(attachment) && message) {
			return wrapSpoiler(
				<MessageUploadProgress
					attachment={attachment}
					message={message}
					data-flx="channel.embeds.attachments.attachment.message-upload-progress"
				/>,
			);
		}
		const {
			attachment: att,
			isExpired: effectiveExpired,
			expiresAt: effectiveExpiresAt,
		} = getEffectiveAttachmentExpiry(attachment, DeveloperOptions.mockAttachmentStates[attachment.id]);
		const enrichedAttachment = {
			...att,
			url: att.url ?? null,
			proxy_url: att.proxy_url ?? att.url ?? null,
		};
		const renderWithFootnote = (content: React.ReactElement) => (
			<div
				className={styles.attachmentWrapper}
				data-flx="channel.embeds.attachments.attachment.render-with-footnote.attachment-wrapper"
			>
				{content}
				{Accessibility.showAttachmentExpiryIndicator && (
					<ExpiryFootnote
						expiresAt={effectiveExpiresAt}
						isExpired={effectiveExpired}
						data-flx="channel.embeds.attachments.attachment.render-with-footnote.expiry-footnote"
					/>
				)}
			</div>
		);
		if (effectiveExpired || !att.url) {
			return renderWithFootnote(
				wrapSpoiler(
					<AttachmentFile
						attachment={enrichedAttachment}
						isPreview={isPreview}
						message={message}
						spoilerHidden={spoilerHidden}
						data-flx="channel.embeds.attachments.attachment.attachment-file"
					/>,
				),
			);
		}
		const inlineAttachmentMedia = UserSettings.getInlineAttachmentMedia();
		if (renderInMosaic && isMediaAttachment(att)) {
			return null;
		}
		const isVisualType = isImageType(att.content_type) || isVideoType(att.content_type);
		const tooSmallToShowInline =
			isVisualType && !isUsefulVisualMediaSize(att.width ?? 0, att.height ?? 0, getAttachmentMediaDimensions());
		if ((!inlineAttachmentMedia || tooSmallToShowInline) && isVisualType) {
			return renderWithFootnote(
				wrapSpoiler(
					<FocusRing
						within
						ringClassName={messageStyles.mediaFocusRing}
						data-flx="channel.embeds.attachments.attachment.focus-ring"
					>
						<AttachmentFile
							attachment={enrichedAttachment}
							isPreview={isPreview}
							message={message}
							spoilerHidden={spoilerHidden}
							data-flx="channel.embeds.attachments.attachment.attachment-file--2"
						/>
					</FocusRing>,
				),
			);
		}
		if (isAudioType(att.content_type)) {
			return renderWithFootnote(
				wrapSpoiler(
					<div
						className={effectiveExpired ? styles.expiredContent : undefined}
						data-flx="channel.embeds.attachments.attachment.expired-content"
					>
						{effectiveExpired && (
							<div className={styles.expiredOverlay} data-flx="channel.embeds.attachments.attachment.expired-overlay">
								{i18n._(THIS_ATTACHMENT_HAS_EXPIRED_DESCRIPTOR)}
							</div>
						)}
						{isVoiceMessageAttachment(message, enrichedAttachment) ? (
							<FocusRing
								within
								ringClassName={messageStyles.mediaFocusRing}
								data-flx="channel.embeds.attachments.attachment.focus-ring--2"
							>
								<VoiceMessagePlayer
									src={enrichedAttachment.proxy_url ?? enrichedAttachment.url ?? ''}
									title={enrichedAttachment.title ?? enrichedAttachment.filename}
									duration={enrichedAttachment.duration ?? undefined}
									waveform={enrichedAttachment.waveform!}
									channelId={message?.channelId}
									messageId={message?.id}
									attachmentId={enrichedAttachment.id}
									message={message}
									contentHash={enrichedAttachment.content_hash ?? undefined}
									isPreview={isPreview}
									snapshotIndex={snapshotIndex}
									onDelete={onDelete}
									data-flx="channel.embeds.attachments.attachment.voice-message-player"
								/>
							</FocusRing>
						) : (
							<AudioAttachment
								attachment={enrichedAttachment}
								message={message}
								isPreview={isPreview}
								snapshotIndex={snapshotIndex}
								onDelete={onDelete}
								data-flx="channel.embeds.attachments.attachment.audio-attachment"
							/>
						)}
					</div>,
				),
			);
		}
		if (!hasValidDimensions(att)) {
			return renderWithFootnote(
				wrapSpoiler(
					<FocusRing
						within
						ringClassName={messageStyles.mediaFocusRing}
						data-flx="channel.embeds.attachments.attachment.focus-ring--3"
					>
						<AttachmentFile
							attachment={enrichedAttachment}
							isPreview={isPreview}
							message={message}
							spoilerHidden={spoilerHidden}
							data-flx="channel.embeds.attachments.attachment.attachment-file--3"
						/>
					</FocusRing>,
				),
			);
		}
		if (isImageType(att.content_type)) {
			return renderWithFootnote(
				wrapSpoiler(
					<div
						className={effectiveExpired ? styles.expiredContent : undefined}
						data-flx="channel.embeds.attachments.attachment.expired-content--2"
					>
						{effectiveExpired && (
							<div
								className={styles.expiredOverlay}
								data-flx="channel.embeds.attachments.attachment.expired-overlay--2"
							>
								{i18n._(THIS_ATTACHMENT_HAS_EXPIRED_DESCRIPTOR)}
							</div>
						)}
						<AttachmentMedia
							attachment={enrichedAttachment}
							message={message}
							isPreview={isPreview}
							snapshotIndex={snapshotIndex}
							onDelete={onDelete}
							data-flx="channel.embeds.attachments.attachment.attachment-media"
						/>
					</div>,
					spoilerStyles.media,
				),
			);
		}
		if (isVideoType(att.content_type)) {
			return renderWithFootnote(
				wrapSpoiler(
					<div
						className={effectiveExpired ? styles.expiredContent : undefined}
						data-flx="channel.embeds.attachments.attachment.expired-content--3"
					>
						{effectiveExpired && (
							<div
								className={styles.expiredOverlay}
								data-flx="channel.embeds.attachments.attachment.expired-overlay--3"
							>
								{i18n._(THIS_ATTACHMENT_HAS_EXPIRED_DESCRIPTOR)}
							</div>
						)}
						{isAnimated(att.flags) ? (
							<GifvAttachment
								attachment={enrichedAttachment}
								message={message}
								isPreview={isPreview}
								snapshotIndex={snapshotIndex}
								onDelete={onDelete}
								data-flx="channel.embeds.attachments.attachment.gifv-attachment"
							/>
						) : (
							<VideoAttachment
								attachment={enrichedAttachment}
								message={message}
								isPreview={isPreview}
								snapshotIndex={snapshotIndex}
								onDelete={onDelete}
								data-flx="channel.embeds.attachments.attachment.video-attachment"
							/>
						)}
					</div>,
					spoilerStyles.media,
				),
			);
		}
		return renderWithFootnote(
			wrapSpoiler(
				<div
					className={effectiveExpired ? styles.expiredContent : undefined}
					data-flx="channel.embeds.attachments.attachment.expired-content--4"
				>
					{effectiveExpired && (
						<div className={styles.expiredOverlay} data-flx="channel.embeds.attachments.attachment.expired-overlay--4">
							{i18n._(THIS_ATTACHMENT_HAS_EXPIRED_DESCRIPTOR)}
						</div>
					)}
					<FocusRing
						within
						ringClassName={messageStyles.mediaFocusRing}
						data-flx="channel.embeds.attachments.attachment.focus-ring--4"
					>
						<AttachmentFile
							attachment={att}
							isPreview={isPreview}
							message={message}
							spoilerHidden={spoilerHidden}
							data-flx="channel.embeds.attachments.attachment.attachment-file--4"
						/>
					</FocusRing>
				</div>,
			),
		);
	},
);
