// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {ForwardMediaSelection} from '@app/features/messaging/commands/MessageCommands';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import styles from '@app/features/messaging/components/modals/ForwardMessagePreview.module.css';
import {
	buildForwardMessagePreviewThumbnailURL,
	type ForwardMessagePreviewAttachmentSummary,
	type ForwardMessagePreviewThumbnail,
	resolveForwardMessagePreviewContent,
} from '@app/features/messaging/components/modals/ForwardMessagePreviewContent';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import Permission from '@app/features/permissions/state/Permission';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {type Icon, ImageIcon, ImagesIcon, PaperclipIcon, PlayCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

const EDITED_DESCRIPTOR = msg({
	message: '(edited)',
	comment: 'Suffix after the message text in the forward modal preview when the forwarded message was edited.',
});
const IMAGE_COUNT_DESCRIPTOR = msg({
	message: '{count, plural, one {# image} other {# images}}',
	comment:
		'Attachment summary in the forward modal message preview when the message has images and no videos. count is the number of images.',
});
const VIDEO_COUNT_DESCRIPTOR = msg({
	message: '{count, plural, one {# video} other {# videos}}',
	comment:
		'Attachment summary in the forward modal message preview when the message has videos and no images. count is the number of videos.',
});
const FILE_COUNT_DESCRIPTOR = msg({
	message: '{count, plural, one {# file} other {# files}}',
	comment:
		'Attachment summary in the forward modal message preview when the message has no images or videos. count is the number of attached files.',
});
const IMAGE_AND_VIDEO_COUNT_DESCRIPTOR = msg({
	message: '{imageCount, plural, one {# image} other {# images}}, {videoCount, plural, one {# video} other {# videos}}',
	comment:
		'Attachment summary in the forward modal message preview when the message has both images and videos. imageCount and videoCount are the number of each.',
});
const MORE_ATTACHMENTS_COUNT_DESCRIPTOR = msg({
	message: '+{count}',
	comment:
		'Badge on the forward modal preview thumbnail. count is the number of other attachments that the thumbnail does not show.',
});

const ATTACHMENT_ICON_SIZE_WITH_TEXT = 18;
const ATTACHMENT_ICON_SIZE = 20;
const PLAY_ICON_SIZE = 24;
const PLACEHOLDER_ICON_SIZE = 24;

interface ForwardMessagePreviewProps {
	readonly message: Message;
	readonly mediaSelection?: ForwardMediaSelection;
	readonly embedChannel: Channel | null;
}

interface ForwardMessagePreviewAttachmentRowProps {
	readonly summary: ForwardMessagePreviewAttachmentSummary;
	readonly hasText: boolean;
}

interface ForwardMessagePreviewThumbnailFrameProps {
	readonly thumbnail: ForwardMessagePreviewThumbnail;
	readonly overflowCount: number;
}

function canEmbedLinksIn(channel: Channel | null): boolean {
	if (channel == null) return true;
	if (channel.isPrivate()) return true;
	return Permission.can(Permissions.EMBED_LINKS, channel);
}

function formatAttachmentSummary(i18n: I18n, summary: ForwardMessagePreviewAttachmentSummary): string {
	switch (summary.kind) {
		case 'images_and_videos':
			return i18n._(IMAGE_AND_VIDEO_COUNT_DESCRIPTOR, {
				imageCount: summary.imageCount,
				videoCount: summary.videoCount,
			});
		case 'videos':
			return i18n._(VIDEO_COUNT_DESCRIPTOR, {count: summary.count});
		case 'images':
			return i18n._(IMAGE_COUNT_DESCRIPTOR, {count: summary.count});
		case 'files':
			return i18n._(FILE_COUNT_DESCRIPTOR, {count: summary.count});
	}
}

function resolveAttachmentSummaryIcon(summary: ForwardMessagePreviewAttachmentSummary): Icon {
	switch (summary.kind) {
		case 'images_and_videos':
			return ImagesIcon;
		case 'videos':
			return PlayCircleIcon;
		case 'images':
			return summary.count === 1 ? ImageIcon : ImagesIcon;
		case 'files':
			return PaperclipIcon;
	}
}

function ForwardMessagePreviewAttachmentRow({summary, hasText}: ForwardMessagePreviewAttachmentRowProps) {
	const {i18n} = useLingui();
	const SummaryIcon = resolveAttachmentSummaryIcon(summary);
	return (
		<div
			className={styles.attachmentRow}
			data-flx="messaging.forward-message-preview.forward-message-preview-attachment-row.attachment-row"
		>
			<SummaryIcon
				className={styles.attachmentIcon}
				size={remFromPx(hasText ? ATTACHMENT_ICON_SIZE_WITH_TEXT : ATTACHMENT_ICON_SIZE)}
				weight="fill"
				aria-hidden={true}
				data-flx="messaging.forward-message-preview.forward-message-preview-attachment-row.attachment-icon"
			/>
			<span
				className={clsx(styles.attachmentSummary, hasText && styles.attachmentSummaryWithText)}
				data-flx="messaging.forward-message-preview.forward-message-preview-attachment-row.attachment-summary"
			>
				{formatAttachmentSummary(i18n, summary)}
			</span>
		</div>
	);
}

function ForwardMessagePreviewThumbnailFrame({thumbnail, overflowCount}: ForwardMessagePreviewThumbnailFrameProps) {
	const {i18n} = useLingui();
	const hasOverflow = overflowCount > 0;
	return (
		<div
			className={styles.thumbnailStack}
			data-flx="messaging.forward-message-preview.forward-message-preview-thumbnail-frame.thumbnail-stack"
		>
			<div
				className={clsx(
					styles.thumbnail,
					thumbnail.showsFilePlaceholder && styles.thumbnailPlaceholder,
					thumbnail.showsPlayIcon && styles.thumbnailVideo,
					thumbnail.isSensitive && styles.thumbnailSensitive,
					hasOverflow && styles.thumbnailWithOverflow,
				)}
				data-flx="messaging.forward-message-preview.forward-message-preview-thumbnail-frame.thumbnail"
			>
				{thumbnail.showsFilePlaceholder ? (
					<PaperclipIcon
						className={styles.thumbnailPlaceholderIcon}
						size={remFromPx(PLACEHOLDER_ICON_SIZE)}
						weight="fill"
						aria-hidden={true}
						data-flx="messaging.forward-message-preview.forward-message-preview-thumbnail-frame.thumbnail-placeholder-icon"
					/>
				) : (
					<img
						className={styles.thumbnailImage}
						src={buildForwardMessagePreviewThumbnailURL(thumbnail)}
						alt=""
						data-flx="messaging.forward-message-preview.forward-message-preview-thumbnail-frame.thumbnail-image"
					/>
				)}
				{thumbnail.showsPlayIcon && (
					<PlayCircleIcon
						className={styles.playIcon}
						size={remFromPx(PLAY_ICON_SIZE)}
						weight="fill"
						aria-hidden={true}
						data-flx="messaging.forward-message-preview.forward-message-preview-thumbnail-frame.play-icon"
					/>
				)}
			</div>
			{hasOverflow && (
				<span
					className={styles.overflowCount}
					aria-hidden={true}
					data-flx="messaging.forward-message-preview.forward-message-preview-thumbnail-frame.overflow-count"
				>
					{i18n._(MORE_ATTACHMENTS_COUNT_DESCRIPTOR, {count: overflowCount})}
				</span>
			)}
		</div>
	);
}

export const ForwardMessagePreview = observer(({message, mediaSelection, embedChannel}: ForwardMessagePreviewProps) => {
	const {i18n} = useLingui();
	const preview = resolveForwardMessagePreviewContent({
		message,
		mediaSelection,
		canEmbedLinks: canEmbedLinksIn(embedChannel),
	});
	const hasAttachments = preview.attachmentSummary != null;
	return (
		<div className={styles.preview} data-flx="messaging.forward-message-preview.preview">
			<div className={styles.content} data-flx="messaging.forward-message-preview.content">
				{preview.text != null && (
					<div
						className={clsx(markupStyles.markup, styles.message, hasAttachments && styles.messageWithAttachments)}
						data-flx="messaging.forward-message-preview.message"
					>
						<SafeMarkdown
							content={preview.text}
							options={{
								context: MarkdownContext.RESTRICTED_INLINE_PREVIEW,
								messageId: message.id,
								channelId: message.channelId,
								mentionChannels: preview.mentionChannels,
							}}
							data-flx="messaging.forward-message-preview.safe-markdown"
						/>
						{preview.isEdited && (
							<span className={styles.edited} data-flx="messaging.forward-message-preview.edited">
								{' '}
								{i18n._(EDITED_DESCRIPTOR)}
							</span>
						)}
					</div>
				)}
				{preview.attachmentSummary != null && (
					<ForwardMessagePreviewAttachmentRow
						summary={preview.attachmentSummary}
						hasText={preview.text != null}
						data-flx="messaging.forward-message-preview.forward-message-preview-attachment-row"
					/>
				)}
			</div>
			{preview.thumbnail != null && (
				<ForwardMessagePreviewThumbnailFrame
					thumbnail={preview.thumbnail}
					overflowCount={preview.overflowCount}
					data-flx="messaging.forward-message-preview.forward-message-preview-thumbnail-frame"
				/>
			)}
		</div>
	);
});
