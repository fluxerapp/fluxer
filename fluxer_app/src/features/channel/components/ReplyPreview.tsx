// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import {useCollapsedMessageVisibility} from '@app/features/channel/components/CollapsedMessageVisibilityContext';
import {PreloadableUserPopout} from '@app/features/channel/components/PreloadableUserPopout';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import MessageReferences, {MessageReferenceState} from '@app/features/messaging/state/MessageReferences';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import LocalUserSpamOverride from '@app/features/moderation/state/LocalUserSpamOverride';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import styles from '@app/features/theme/styles/Message.module.css';
import {Avatar} from '@app/features/ui/components/Avatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowBendUpLeftIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {type CSSProperties, useCallback} from 'react';

const ORIGINAL_MESSAGE_WAS_DELETED_DESCRIPTOR = msg({
	message: 'Original message was deleted',
	comment: 'Label in the channel and chat reply preview. Keep the tone plain and specific.',
});
const ORIGINAL_MESSAGE_FAILED_TO_LOAD_DESCRIPTOR = msg({
	message: 'Original message failed to load',
	comment: 'Error message in the channel and chat reply preview.',
});
const REPLY_HIDDEN_BECAUSE_THE_ORIGINAL_AUTHOR_IS_BLOCKED_DESCRIPTOR = msg({
	message: 'Reply hidden because the original author is blocked.',
	comment: 'Error message in the channel and chat reply preview. Keep the tone plain and specific.',
});
const REPLY_HIDDEN_BECAUSE_THE_ORIGINAL_AUTHOR_IS_MARKED_DESCRIPTOR = msg({
	message: 'Reply hidden because the original author is marked as a spammer.',
	comment: 'Description text in the channel and chat reply preview. Keep the tone plain and specific.',
});
const MESSAGE_CONTAINS_ATTACHED_MEDIA_DESCRIPTOR = msg({
	message: 'Message contains attached media',
	comment: 'Label in the channel and chat reply preview.',
});
export const ReplyPreview = observer(
	({
		message,
		channelId,
		guildId,
		animateEmoji,
		messageDisplayCompact,
	}: {
		message: Message;
		channelId: string;
		guildId?: string;
		animateEmoji: boolean;
		messageDisplayCompact: boolean;
	}) => {
		const {i18n} = useLingui();
		const {message: referencedMessage, state: messageState} = MessageReferences.getMessageReference(
			message.messageReference?.channel_id ?? '',
			message.messageReference?.message_id ?? '',
		);
		const {isMessageRevealed} = useCollapsedMessageVisibility();
		const resolvedGuildId = guildId ?? message.guildId ?? message.messageReference?.guild_id;
		const jumpToRepliedMessage = useCallback(() => {
			if (message.messageReference?.message_id) {
				goToMessage(message.channelId, message.messageReference.message_id, {
					returnTargetId: message.id,
					returnChannelId: message.channelId,
				});
			}
		}, [message.channelId, message.id, message.messageReference]);
		if (!message.messageReference) return null;
		if (messageState !== MessageReferenceState.LOADED || !referencedMessage) {
			return (
				<div
					className={clsx(styles.repliedMessage, messageDisplayCompact && styles.repliedMessageCompact)}
					data-message-copy-hidden="true"
					data-flx="channel.reply-preview.replied-message"
				>
					<div className={styles.repliedIconContainer} data-flx="channel.reply-preview.replied-icon-container">
						<ArrowBendUpLeftIcon
							weight="bold"
							className={styles.repliedIcon}
							data-flx="channel.reply-preview.replied-icon"
						/>
					</div>
					<button
						type="button"
						disabled
						className={clsx(styles.repliedTextPreview, styles.unstyled)}
						tabIndex={-1}
						data-flx="channel.reply-preview.replied-text-preview.button"
					>
						{messageState === MessageReferenceState.DELETED ? (
							<span className={styles.repliedItalic} data-flx="channel.reply-preview.replied-italic">
								{i18n._(ORIGINAL_MESSAGE_WAS_DELETED_DESCRIPTOR)}
							</span>
						) : (
							<span className={styles.repliedItalic} data-flx="channel.reply-preview.replied-italic--2">
								{i18n._(ORIGINAL_MESSAGE_FAILED_TO_LOAD_DESCRIPTOR)}
							</span>
						)}
					</button>
				</div>
			);
		}
		const isSpammerReply =
			referencedMessage.author.id !== Authentication.currentUserId &&
			LocalUserSpamOverride.isUserMarkedAsSpammer(referencedMessage.author.id, referencedMessage.author.flags);
		const isCollapsedReferenceRevealed = isMessageRevealed(referencedMessage);
		const hiddenReplyReason =
			referencedMessage.blocked && !isCollapsedReferenceRevealed
				? i18n._(REPLY_HIDDEN_BECAUSE_THE_ORIGINAL_AUTHOR_IS_BLOCKED_DESCRIPTOR)
				: isSpammerReply && !isCollapsedReferenceRevealed
					? i18n._(REPLY_HIDDEN_BECAUSE_THE_ORIGINAL_AUTHOR_IS_MARKED_DESCRIPTOR)
					: null;
		if (hiddenReplyReason) {
			return (
				<div
					className={clsx(styles.repliedMessage, messageDisplayCompact && styles.repliedMessageCompact)}
					data-message-copy-hidden="true"
					data-flx="channel.reply-preview.replied-message--2"
				>
					<div className={styles.repliedIconContainer} data-flx="channel.reply-preview.replied-icon-container--2">
						<ArrowBendUpLeftIcon
							weight="bold"
							className={styles.repliedIcon}
							data-flx="channel.reply-preview.replied-icon--2"
						/>
					</div>
					<button
						type="button"
						disabled
						className={clsx(styles.repliedTextPreview, styles.unstyled)}
						tabIndex={-1}
						data-flx="channel.reply-preview.replied-text-preview.button--2"
					>
						<span
							className={clsx(styles.repliedTextContent, styles.repliedItalic)}
							data-flx="channel.reply-preview.replied-text-content"
						>
							{hiddenReplyReason}
						</span>
					</button>
				</div>
			);
		}
		return (
			<div
				className={clsx(styles.repliedMessage, messageDisplayCompact && styles.repliedMessageCompact)}
				data-message-copy-hidden="true"
				data-flx="channel.reply-preview.replied-message--3"
			>
				{!messageDisplayCompact ? (
					<PreloadableUserPopout
						user={referencedMessage.author}
						isWebhook={referencedMessage.webhookId != null}
						webhookId={referencedMessage.webhookId ?? undefined}
						guildId={resolvedGuildId}
						channelId={channelId}
						message={referencedMessage}
						enableLongPressActions={true}
						data-flx="channel.reply-preview.preloadable-user-popout"
					>
						<Avatar
							user={referencedMessage.author}
							size={16}
							className={styles.repliedAvatar}
							guildId={resolvedGuildId}
							data-user-id={referencedMessage.author.id}
							data-guild-id={resolvedGuildId}
							data-flx="channel.reply-preview.replied-avatar"
						/>
					</PreloadableUserPopout>
				) : (
					<div className={styles.repliedIconContainer} data-flx="channel.reply-preview.replied-icon-container--3">
						<ArrowBendUpLeftIcon
							weight="bold"
							className={styles.repliedIcon}
							data-flx="channel.reply-preview.replied-icon--3"
						/>
					</div>
				)}
				<PreloadableUserPopout
					user={referencedMessage.author}
					isWebhook={referencedMessage.webhookId != null}
					webhookId={referencedMessage.webhookId ?? undefined}
					guildId={resolvedGuildId}
					channelId={channelId}
					message={referencedMessage}
					enableLongPressActions={true}
					longPressWrapperElement="span"
					data-flx="channel.reply-preview.preloadable-user-popout--2"
				>
					<span
						className={styles.repliedUsername}
						style={
							{
								'--replied-username-color': GuildMembers.getMember(
									resolvedGuildId ?? '',
									referencedMessage.author.id,
								)?.getColorString(),
							} as CSSProperties
						}
						data-user-id={referencedMessage.author.id}
						data-guild-id={resolvedGuildId}
						data-flx="channel.reply-preview.replied-username"
					>
						{message.mentions.some((mention) => mention.id === referencedMessage.author.id) && '@'}
						{NicknameUtils.getNickname(referencedMessage.author, resolvedGuildId)}
					</span>
				</PreloadableUserPopout>
				<FocusRing offset={-2} data-flx="channel.reply-preview.focus-ring">
					<button
						type="button"
						className={clsx(styles.repliedTextPreview, styles.unstyled)}
						onClick={jumpToRepliedMessage}
						tabIndex={0}
						onKeyDown={(e) => {
							if (e.key === 'Enter' || e.key === ' ') {
								e.preventDefault();
								jumpToRepliedMessage();
							}
						}}
						data-flx="channel.reply-preview.replied-text-preview.jump-to-replied-message.button"
					>
						{referencedMessage.content ? (
							<span
								className={clsx(styles.repliedTextContent, markupStyles.markup)}
								data-flx="channel.reply-preview.replied-text-content--2"
							>
								<SafeMarkdown
									content={referencedMessage.content}
									options={{
										context: MarkdownContext.RESTRICTED_INLINE_REPLY,
										messageId: referencedMessage.id,
										channelId,
										disableAnimatedEmoji: !animateEmoji,
										mentionChannels: referencedMessage.mentionChannels,
									}}
									data-flx="channel.reply-preview.safe-markdown"
								/>
							</span>
						) : (
							<span
								className={clsx(styles.repliedTextContent, styles.repliedItalic)}
								data-flx="channel.reply-preview.replied-text-content--3"
							>
								{i18n._(MESSAGE_CONTAINS_ATTACHED_MEDIA_DESCRIPTOR)}
							</span>
						)}
					</button>
				</FocusRing>
			</div>
		);
	},
);
