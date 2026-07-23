// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {SILENT_MENTION} from '@app/features/app/config/I18nDisplayConstants';
import {
	getAnimatedMediaPlaybackAllowed,
	subscribeAnimatedMediaPlaybackChange,
	useAnimatedMediaPlaybackAllowed,
} from '@app/features/app/hooks/useAnimatedMediaPlayback';
import {UserTag} from '@app/features/channel/components/ChannelUserTag';
import {CompactAuthorPrefix, CompactMessageLayout} from '@app/features/channel/components/CompactMessageLayout';
import {EditingMessageInput} from '@app/features/channel/components/EditingMessageInput';
import {isMediaOnlyEmbed} from '@app/features/channel/components/embeds/EmbedRenderUtils';
import {MessageAttachments} from '@app/features/channel/components/MessageAttachments';
import {ThreadPreviewCard} from '@app/features/channel/components/ThreadPreviewCard';
import {MessageAuthorInfo} from '@app/features/channel/components/MessageAuthorInfo';
import {MessageAvatar} from '@app/features/channel/components/MessageAvatar';
import {shouldAnimateMessageEmojiByDefault} from '@app/features/channel/components/MessageEmojiAnimationUtils';
import {MessageTimeoutIndicator} from '@app/features/channel/components/MessageTimeoutIndicator';
import {MessageUsername} from '@app/features/channel/components/MessageUsername';
import {useMessageViewContext} from '@app/features/channel/components/MessageViewContext';
import {ReplyPreview} from '@app/features/channel/components/ReplyPreview';
import {TimestampWithTooltip} from '@app/features/channel/components/TimestampWithTooltip';
import {createSystemMessage} from '@app/features/devtools/utils/CommandUtils';
import Emoji from '@app/features/emoji/state/Emoji';
import {checkEmojiAvailability} from '@app/features/expressions/utils/ExpressionPermissionUtils';
import Guilds from '@app/features/guild/state/Guilds';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GuildMembers from '@app/features/member/state/GuildMembers';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {parse} from '@app/features/messaging/components/markdown/renderers';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import MessageEdit from '@app/features/messaging/state/MessageEdit';
import {hasStyleableMessageText} from '@app/features/messaging/utils/FailedMessageDisplayUtils';
import {
	buildExistingAttachmentEditReferences,
	canSubmitEmptyMessageEdit,
} from '@app/features/messaging/utils/MessageEditContentUtils';
import {retryFailedMessage} from '@app/features/messaging/utils/MessageRetryUtils';
import {NodeType} from '@app/features/messaging/utils/markdown/parser/Enums';
import {SpoilerSyncProvider} from '@app/features/messaging/utils/SpoilerUtils';
import {resolveTypedEmojiShortcodes} from '@app/features/messaging/utils/TypedEmojiShortcodeUtils';
import {compactMarkdownProps} from '@app/features/theme/layout/MessageLayoutAttributes';
import markupStyles from '@app/features/theme/styles/Markup.module.css';
import styles from '@app/features/theme/styles/Message.module.css';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import UserSettings from '@app/features/user/state/UserSettings';
import Users from '@app/features/user/state/Users';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {FLUXERBOT_ID} from '@fluxer/constants/src/AppConstants';
import {MessageFlags, MessageStates, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowsClockwiseIcon, BellSlashIcon, EyeIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {autorun} from 'mobx';
import {observer} from 'mobx-react-lite';
import {type MouseEvent, useCallback, useEffect, useMemo, useState} from 'react';

const JUMP_TO_MESSAGE_FROM_SENT_DESCRIPTOR = msg({
	message: 'Jump to message from {displayName}, sent {formattedDate}',
	comment:
		'Label in the channel and chat user message. Preserve {displayName}, {formattedDate}; they are inserted by code.',
});
const EDITED_DESCRIPTOR = msg({
	message: '(edited)',
	comment: 'Button or menu action label in the channel and chat user message. Keep it concise.',
});
const FAILED_TO_SEND_MESSAGE_HOLD_FOR_OPTIONS_DESCRIPTOR = msg({
	message: 'Failed to send message. Hold for options.',
	comment: 'Error message in the channel and chat user message.',
});
const FAILED_TO_SEND_MESSAGE_DESCRIPTOR = msg({
	message: 'Failed to send message.',
	comment: 'Error message in the channel and chat user message.',
});
const THIS_WAS_A_MESSAGE_DESCRIPTOR = msg({
	message: 'This was a {silentMention} message.',
	comment: 'Description text in the channel and chat user message. Preserve {silentMention}; it is inserted by code.',
});
const MessageStateToClassName: Record<string, string> = {
	[MessageStates.SENT]: styles.messageSent,
	[MessageStates.SENDING]: styles.messageSending,
	[MessageStates.FAILED]: styles.messageFailed,
};
const CUSTOM_EMOJI_MARKDOWN_PATTERN = /<a?:[a-zA-Z0-9_+-]{2,}:([0-9]+)>/g;
const getDefaultMessageEmojiAnimationAllowed = (): boolean =>
	shouldAnimateMessageEmojiByDefault({
		animateEmojiSetting: UserSettings.getAnimateEmoji(),
		animatedMediaPlaybackAllowed: getAnimatedMediaPlaybackAllowed(),
	});
const setMessageEmojiImagesAnimated = (messageId: string, shouldAnimate: boolean): void => {
	const emojiImgs = document.querySelectorAll(
		`img[data-message-id="${messageId}"][data-animated="true"]`,
	) as NodeListOf<HTMLImageElement>;
	for (const img of emojiImgs) {
		const url = new URL(img.src, window.location.origin);
		url.searchParams.set('animated', shouldAnimate.toString());
		img.src = url.toString();
	}
};

function messageContentCopyBlockProps(content: string): {
	'data-message-copy-block'?: 'true';
	'data-message-copy-text'?: string;
} {
	return content ? {'data-message-copy-block': 'true', 'data-message-copy-text': content} : {};
}

export const UserMessage = observer(() => {
	const {i18n} = useLingui();
	const {
		message,
		channel,
		handleDelete,
		isHovering,
		shouldGroup,
		messageDisplayCompact,
		previewContext,
		previewOverrides,
		onHeadingActivate,
	} = useMessageViewContext();
	const [animateEmoji, setAnimateEmoji] = useState(getDefaultMessageEmojiAnimationAllowed);
	const animatedMediaPlaybackAllowed = useAnimatedMediaPlaybackAllowed();
	const isEditing = MessageEdit.isEditing(message.channelId, message.id);
	const userAuthor = Users.getUser(message.author.id);
	const author = message.webhookId != null ? message.author : (userAuthor ?? message.author);
	const formattedDate = DateUtils.getRelativeDateString(message.timestamp, i18n);
	const jumpHeadingLabel = useMemo(() => {
		const displayName = previewOverrides?.displayName ?? NicknameUtils.getDisplayName(message.author);
		return i18n._(JUMP_TO_MESSAGE_FROM_SENT_DESCRIPTOR, {displayName, formattedDate});
	}, [previewOverrides?.displayName, message.author, formattedDate, i18n.locale]);
	const jumpHeading = onHeadingActivate ? (
		<h3 className={styles.jumpHeading} data-flx="channel.user-message.jump-heading">
			<button
				type="button"
				tabIndex={-1}
				className={styles.jumpHeadingButton}
				onClick={onHeadingActivate}
				data-flx="channel.user-message.jump-heading-button.heading-activate"
			>
				{jumpHeadingLabel}
			</button>
		</h3>
	) : null;
	const AuthorHeading = (onHeadingActivate ? 'div' : 'h3') as 'div' | 'h3';
	const showUserAvatarsInCompactMode = Accessibility.showUserAvatarsInCompactMode;
	const {nodes: astNodes} = useMemo(
		() =>
			parse({
				content: message.content,
				context: MarkdownContext.STANDARD_WITH_JUMBO,
			}),
		[message.content],
	);
	const shouldHideContent =
		UserSettings.getRenderEmbeds() &&
		message.embeds.length > 0 &&
		message.embeds.every(isMediaOnlyEmbed) &&
		astNodes.length === 1 &&
		astNodes[0].type === NodeType.Link &&
		!message.suppressEmbeds;
	const hasStyleableTextContent = useMemo(() => hasStyleableMessageText(astNodes), [astNodes]);
	const guild = Guilds.getGuild(channel.guildId ?? '');
	const member = GuildMembers.getMember(guild?.id ?? '', author?.id ?? '');
	const shouldAppearAuthorless = false;
	const mobileLayout = MobileLayout;
	const shouldShowFailedFooter =
		message.state === MessageStates.FAILED && (mobileLayout.enabled || shouldHideContent || !hasStyleableTextContent);
	const shouldApplyFailedTextStyling =
		message.state === MessageStates.FAILED && !shouldHideContent && hasStyleableTextContent;
	const messageStateClassName =
		message.state === MessageStates.FAILED
			? shouldApplyFailedTextStyling
				? styles.messageFailed
				: undefined
			: MessageStateToClassName[message.state];
	const checkCustomEmojiAvailability = useCallback(
		(content: string): boolean => {
			CUSTOM_EMOJI_MARKDOWN_PATTERN.lastIndex = 0;
			let match: RegExpExecArray | null = null;
			while ((match = CUSTOM_EMOJI_MARKDOWN_PATTERN.exec(content))) {
				const emojiId = match[1];
				const emoji = Emoji.getEmojiById(emojiId);
				if (!emoji) {
					continue;
				}
				const availability = checkEmojiAvailability(i18n, emoji, channel);
				if (availability.canUse) {
					continue;
				}
				if (availability.lockReason) {
					const errorMessage = createSystemMessage(channel.id, availability.lockReason);
					MessageCommands.createOptimistic(channel.id, errorMessage.toJSON());
				}
				return true;
			}
			return false;
		},
		[channel, i18n],
	);
	const finishEditing = useCallback(() => {
		MessageEdit.clearDraftContent(message.id);
		MessageCommands.stopEdit(channel.id);
	}, [channel.id, message.id]);
	const onSubmit = useCallback(
		(actualContent?: string) => {
			if (message.messageSnapshots) {
				return;
			}
			const content = resolveTypedEmojiShortcodes({
				content: (actualContent ?? '').trim(),
				channel,
				i18n,
			});
			if (!content) {
				if (canSubmitEmptyMessageEdit(message)) {
					if (message.content.length === 0) {
						finishEditing();
						return;
					}
					MessageCommands.edit(
						channel.id,
						message.id,
						'',
						undefined,
						message._allowedMentions,
						buildExistingAttachmentEditReferences(message),
					).then((result) => {
						if (result) {
							finishEditing();
						}
					});
					return;
				}
				handleDelete();
				return;
			}
			if (checkCustomEmojiAvailability(content)) {
				return;
			}
			MessageCommands.edit(channel.id, message.id, content, undefined, message._allowedMentions).then((result) => {
				if (result) {
					finishEditing();
				}
			});
		},
		[
			channel,
			channel.id,
			handleDelete,
			i18n,
			message,
			message.id,
			message.messageSnapshots,
			message._allowedMentions,
			message.content,
			checkCustomEmojiAvailability,
			finishEditing,
		],
	);
	const cancelEditing = useCallback(() => {
		MessageCommands.stopEdit(message.channelId);
	}, [message.channelId]);
	const handleDismissSystemMessage = useCallback(() => {
		MessageCommands.deleteOptimistic(message.channelId, message.id);
	}, [message.channelId, message.id]);
	const handleRetryFailedMessage = useCallback(
		(event: MouseEvent<HTMLButtonElement>) => {
			event.preventDefault();
			event.stopPropagation();
			retryFailedMessage(message);
		},
		[message],
	);
	const shouldShowEditingInput = isEditing && !previewContext && !mobileLayout.enabled;
	const compactAuthorPrefix = (
		<CompactAuthorPrefix
			message={message}
			author={author}
			guild={guild}
			member={member ?? undefined}
			showAvatar={showUserAvatarsInCompactMode}
			showTimeoutIndicator={true}
			isHovering={isHovering}
			previewContext={previewContext}
			previewOverrides={previewOverrides}
			data-flx="channel.user-message.compact-author-prefix"
		/>
	);
	const renderMessageContent = useCallback(() => {
		if (shouldShowEditingInput) {
			return (
				<EditingMessageInput
					channel={channel}
					message={message}
					onCancel={cancelEditing}
					onSubmit={onSubmit}
					data-flx="channel.user-message.render-message-content.editing-message-input.submit"
				/>
			);
		}
		if (shouldHideContent) return null;
		return (
			<div
				className={clsx(markupStyles.markup)}
				data-search-highlight-scope="message"
				data-flx="channel.user-message.render-message-content.div"
				{...messageContentCopyBlockProps(message.content)}
			>
				<SafeMarkdown
					content={message.content}
					options={{
						context: MarkdownContext.STANDARD_WITH_JUMBO,
						messageId: message.id,
						channelId: message.channelId,
						mentionChannels: message.mentionChannels,
					}}
					data-flx="channel.user-message.render-message-content.safe-markdown"
				/>
				{(message.editedTimestamp || message.isEditing) &&
					(message.isEditing ? (
						<span className={styles.editedLabel} data-flx="channel.user-message.render-message-content.edited-label">
							{' '}
							{i18n._(EDITED_DESCRIPTOR)}
						</span>
					) : (
						<TimestampWithTooltip
							date={message.editedTimestamp!}
							className={styles.editedTimestamp}
							data-flx="channel.user-message.render-message-content.edited-timestamp"
						>
							<span
								className={styles.editedLabel}
								data-flx="channel.user-message.render-message-content.edited-label--2"
							>
								{' '}
								{i18n._(EDITED_DESCRIPTOR)}
							</span>
						</TimestampWithTooltip>
					))}
			</div>
		);
	}, [
		shouldShowEditingInput,
		shouldHideContent,
		message,
		message.content,
		message.id,
		message.channelId,
		message.editedTimestamp,
		message.isEditing,
		channel,
		cancelEditing,
		onSubmit,
		i18n,
	]);
	useEffect(() => {
		if (animateEmoji) return;
		setMessageEmojiImagesAnimated(message.id, animatedMediaPlaybackAllowed && isHovering);
	}, [animateEmoji, animatedMediaPlaybackAllowed, isHovering, message.id]);
	useEffect(() => {
		const updateDefaultEmojiAnimation = () => {
			const shouldAnimate = getDefaultMessageEmojiAnimationAllowed();
			setAnimateEmoji(shouldAnimate);
			setMessageEmojiImagesAnimated(message.id, shouldAnimate);
		};
		const disposer = autorun(() => {
			updateDefaultEmojiAnimation();
		});
		const unsubscribePlayback = subscribeAnimatedMediaPlaybackChange(updateDefaultEmojiAnimation);
		return () => {
			disposer();
			unsubscribePlayback();
		};
	}, [message.id]);
	const renderFailedFooter = useCallback(() => {
		if (!shouldShowFailedFooter) {
			return null;
		}
		const failedLabel = mobileLayout.enabled
			? i18n._(FAILED_TO_SEND_MESSAGE_HOLD_FOR_OPTIONS_DESCRIPTOR)
			: i18n._(FAILED_TO_SEND_MESSAGE_DESCRIPTOR);
		return (
			<div className={styles.failedFooter} data-flx="channel.user-message.render-failed-footer.failed-footer">
				<div
					className={styles.failedFooterMessage}
					data-flx="channel.user-message.render-failed-footer.failed-footer-message"
				>
					<WarningCircleIcon
						weight="fill"
						className={styles.failedFooterIcon}
						data-flx="channel.user-message.render-failed-footer.failed-footer-icon"
					/>
					<span
						className={styles.failedFooterLabel}
						data-flx="channel.user-message.render-failed-footer.failed-footer-label"
					>
						{failedLabel}
					</span>
				</div>
				{!mobileLayout.enabled && !previewContext && message.nonce && (
					<button
						type="button"
						className={styles.failedRetryButton}
						onClick={handleRetryFailedMessage}
						data-flx="channel.user-message.render-failed-footer.failed-retry-button.retry-failed-message"
					>
						<ArrowsClockwiseIcon
							weight="bold"
							className={styles.failedRetryIcon}
							data-flx="channel.user-message.render-failed-footer.failed-retry-icon"
						/>
						<span data-flx="channel.user-message.render-failed-footer.span">{i18n._(TRY_AGAIN_DESCRIPTOR)}</span>
					</button>
				)}
			</div>
		);
	}, [handleRetryFailedMessage, message.nonce, mobileLayout.enabled, previewContext, shouldShowFailedFooter, i18n]);
	if (message.type === MessageTypes.CLIENT_SYSTEM && message.author.id === FLUXERBOT_ID) {
		return (
			<SpoilerSyncProvider data-flx="channel.user-message.spoiler-sync-provider">
				<div className={styles.messageContent} data-flx="channel.user-message.message-content">
					{jumpHeading}
					<AuthorHeading className={styles.messageAuthorInfo} data-flx="channel.user-message.message-author-info">
						<span className={styles.messageAuthorRow} data-flx="channel.user-message.message-author-row">
							<span className={styles.messageAuthorPart} data-flx="channel.user-message.message-author-part">
								<MessageUsername
									user={author}
									message={message}
									guild={guild}
									member={member ?? undefined}
									className={styles.messageUsername}
									isPreview={!!previewContext}
									previewColor={previewOverrides?.usernameColor}
									previewName={previewOverrides?.displayName}
									data-flx="channel.user-message.message-username"
								/>
								<UserTag
									className={styles.userTagOffset}
									system={author.system}
									data-flx="channel.user-message.user-tag-offset"
								/>
							</span>
							<TimestampWithTooltip
								date={message.timestamp}
								className={styles.messageTimestamp}
								data-flx="channel.user-message.message-timestamp"
							>
								<span
									className={styles.authorDashSeparator}
									aria-hidden="true"
									data-flx="channel.user-message.author-dash-separator"
								>
									{' \u2014 '}
								</span>
								{formattedDate}
							</TimestampWithTooltip>
						</span>
					</AuthorHeading>
					<div className={styles.messageText} data-flx="channel.user-message.message-text">
						<div
							className={clsx(markupStyles.markup)}
							data-search-highlight-scope="message"
							data-flx="channel.user-message.div"
							{...messageContentCopyBlockProps(message.content)}
						>
							<SafeMarkdown
								content={message.content}
								options={{
									context: MarkdownContext.STANDARD_WITH_JUMBO,
									messageId: message.id,
									channelId: message.channelId,
									mentionChannels: message.mentionChannels,
								}}
								data-flx="channel.user-message.safe-markdown"
							/>
						</div>
						<div className={styles.systemMessageContainer} data-flx="channel.user-message.system-message-container">
							<EyeIcon className={styles.systemMessageIcon} data-flx="channel.user-message.system-message-icon" />
							<div data-flx="channel.user-message.div--2">
								<Trans>
									Only you can see this message.{' '}
									<button
										type="button"
										className={styles.systemMessageDismissButton}
										onClick={handleDismissSystemMessage}
										key="dismiss"
										data-flx="channel.user-message.system-message-dismiss-button.dismiss-system-message"
									>
										Dismiss
									</button>
								</Trans>
							</div>
						</div>
					</div>
				</div>
				<div className={styles.messageGutterLeft} data-flx="channel.user-message.message-gutter-left" />
				<MessageAvatar
					user={author}
					message={message}
					guildId={guild?.id}
					size={40}
					className={styles.messageAvatar}
					isHovering={isHovering}
					isPreview={!!previewContext}
					data-flx="channel.user-message.message-avatar"
				/>
				<div className={styles.messageGutterRight} data-flx="channel.user-message.message-gutter-right" />
				<div className={styles.container} data-flx="channel.user-message.container">
					<MessageAttachments data-flx="channel.user-message.message-attachments" />
				</div>
			</SpoilerSyncProvider>
		);
	}
	if (messageDisplayCompact) {
		return (
			<SpoilerSyncProvider data-flx="channel.user-message.spoiler-sync-provider--2">
				{message.messageReference && message.messageReference.type === 0 && (
					<ReplyPreview
						message={message}
						channelId={channel.id}
						guildId={channel.guildId}
						animateEmoji={animateEmoji}
						messageDisplayCompact={messageDisplayCompact}
						data-flx="channel.user-message.reply-preview"
					/>
				)}
				<CompactMessageLayout
					message={message}
					shouldGroup={shouldGroup}
					mobileLayoutEnabled={mobileLayout.enabled}
					data-flx="channel.user-message.compact-message-layout"
				>
					{(showMetadata) =>
						isEditing && !previewContext && !mobileLayout.enabled ? (
							<div
								className={clsx(markupStyles.markup, styles.compactInlineContent)}
								data-flx="channel.user-message.compact-inline-content"
								{...compactMarkdownProps()}
							>
								{showMetadata && compactAuthorPrefix}
								<EditingMessageInput
									channel={channel}
									message={message}
									onCancel={cancelEditing}
									onSubmit={onSubmit}
									data-flx="channel.user-message.editing-message-input.submit"
								/>
							</div>
						) : (
							<div
								className={clsx(markupStyles.markup, styles.compactInlineContent, messageStateClassName)}
								data-search-highlight-scope="message"
								data-flx="channel.user-message.compact-inline-content--2"
								{...compactMarkdownProps()}
							>
								{showMetadata && compactAuthorPrefix}
								{!shouldHideContent && (
									<>
										<SafeMarkdown
											content={message.content}
											options={{
												context: MarkdownContext.STANDARD_WITH_JUMBO,
												messageId: message.id,
												channelId: message.channelId,
												mentionChannels: message.mentionChannels,
											}}
											data-flx="channel.user-message.safe-markdown--2"
										/>
										{(message.editedTimestamp || message.isEditing) &&
											(message.isEditing ? (
												<span className={styles.editedLabel} data-flx="channel.user-message.edited-label">
													{' '}
													{i18n._(EDITED_DESCRIPTOR)}
												</span>
											) : (
												<TimestampWithTooltip
													date={message.editedTimestamp!}
													className={styles.editedTimestamp}
													data-flx="channel.user-message.edited-timestamp"
												>
													<span className={styles.editedLabel} data-flx="channel.user-message.edited-label--2">
														{' '}
														{i18n._(EDITED_DESCRIPTOR)}
													</span>
												</TimestampWithTooltip>
											))}
									</>
								)}
							</div>
						)
					}
				</CompactMessageLayout>
				<div className={styles.container} data-flx="channel.user-message.container--2">
					<MessageAttachments data-flx="channel.user-message.message-attachments--2" />
					{renderFailedFooter()}
				</div>
			</SpoilerSyncProvider>
		);
	}
	return (
		<SpoilerSyncProvider data-flx="channel.user-message.spoiler-sync-provider--3">
			{message.messageReference && message.messageReference.type === 0 && (
				<ReplyPreview
					message={message}
					channelId={channel.id}
					guildId={channel.guildId}
					animateEmoji={animateEmoji}
					messageDisplayCompact={messageDisplayCompact}
					data-flx="channel.user-message.reply-preview--2"
				/>
			)}
			{(message.content || isEditing) && (!shouldHideContent || isEditing) && (
				<div className={styles.messageContent} data-flx="channel.user-message.message-content--2">
					{!shouldGroup && jumpHeading}
					{!shouldGroup && (
						<AuthorHeading className={styles.messageAuthorInfo} data-flx="channel.user-message.message-author-info--2">
							<span className={styles.messageAuthorRow} data-flx="channel.user-message.message-author-row--2">
								<span className={styles.messageAuthorPart} data-flx="channel.user-message.message-author-part--2">
									<MessageTimeoutIndicator
										guildId={message.guildId}
										userId={author.id}
										data-flx="channel.user-message.message-timeout-indicator"
									/>
									<MessageUsername
										user={author}
										message={message}
										guild={guild}
										member={member ?? undefined}
										className={styles.messageUsername}
										isPreview={!!previewContext}
										previewColor={previewOverrides?.usernameColor}
										previewName={previewOverrides?.displayName}
										data-flx="channel.user-message.message-username--2"
									/>
									{author.bot && (
										<UserTag
											className={styles.userTagOffset}
											system={author.system}
											data-flx="channel.user-message.user-tag-offset--2"
										/>
									)}
								</span>
								<TimestampWithTooltip
									date={message.timestamp}
									className={styles.messageTimestamp}
									data-flx="channel.user-message.message-timestamp--2"
								>
									<span
										className={styles.authorDashSeparator}
										aria-hidden="true"
										data-flx="channel.user-message.author-dash-separator--2"
									>
										{' \u2014 '}
									</span>
									{formattedDate}
								</TimestampWithTooltip>
							</span>
							{(message.flags & MessageFlags.SUPPRESS_NOTIFICATIONS) !== 0 && (
								<Tooltip
									text={i18n._(THIS_WAS_A_MESSAGE_DESCRIPTOR, {silentMention: SILENT_MENTION})}
									data-flx="channel.user-message.tooltip"
								>
									<BellSlashIcon
										weight="fill"
										className={styles.silentMessageIcon}
										data-flx="channel.user-message.silent-message-icon"
									/>
								</Tooltip>
							)}
						</AuthorHeading>
					)}
					<div
						className={clsx(styles.messageText, messageStateClassName)}
						data-flx="channel.user-message.message-text--2"
					>
						{renderMessageContent()}
					</div>
				</div>
			)}
			{shouldGroup && (
				<MessageAuthorInfo
					message={message}
					author={author}
					guild={guild}
					member={member ?? undefined}
					shouldGroup={shouldGroup}
					shouldAppearAuthorless={shouldAppearAuthorless}
					mobileLayoutEnabled={mobileLayout.enabled}
					isHovering={isHovering}
					formattedDate={formattedDate}
					previewContext={previewContext}
					previewOverrides={previewOverrides}
					data-flx="channel.user-message.message-author-info--3"
				/>
			)}
			{!shouldGroup && (
				<>
					<div className={styles.messageGutterLeft} data-flx="channel.user-message.message-gutter-left--2" />
					<MessageAvatar
						user={author}
						message={message}
						guildId={guild?.id}
						size={40}
						className={styles.messageAvatar}
						isHovering={isHovering}
						isPreview={!!previewContext}
						data-flx="channel.user-message.message-avatar--2"
					/>
					<div className={styles.messageGutterRight} data-flx="channel.user-message.message-gutter-right--2" />
				</>
			)}
			<div className={styles.container} data-flx="channel.user-message.container--3">
				{((!message.content && !isEditing) || (shouldHideContent && !isEditing)) && !shouldGroup && jumpHeading}
				{((!message.content && !isEditing) || (shouldHideContent && !isEditing)) && !shouldGroup && (
					<AuthorHeading className={styles.messageAuthorInfo} data-flx="channel.user-message.message-author-info--4">
						<span className={styles.messageAuthorRow} data-flx="channel.user-message.message-author-row--3">
							<span className={styles.messageAuthorPart} data-flx="channel.user-message.message-author-part--3">
								<MessageTimeoutIndicator
									guildId={message.guildId}
									userId={author.id}
									data-flx="channel.user-message.message-timeout-indicator--2"
								/>
								<MessageUsername
									user={author}
									message={message}
									guild={guild}
									member={member ?? undefined}
									className={styles.messageUsername}
									isPreview={!!previewContext}
									previewColor={previewOverrides?.usernameColor}
									previewName={previewOverrides?.displayName}
									data-flx="channel.user-message.message-username--3"
								/>
								{author.bot && (
									<UserTag
										className={styles.userTagOffset}
										system={author.system}
										data-flx="channel.user-message.user-tag-offset--3"
									/>
								)}
							</span>
							<TimestampWithTooltip
								date={message.timestamp}
								className={styles.messageTimestamp}
								data-flx="channel.user-message.message-timestamp--3"
							>
								<span
									className={styles.authorDashSeparator}
									aria-hidden="true"
									data-flx="channel.user-message.author-dash-separator--3"
								>
									{' \u2014 '}
								</span>
								{formattedDate}
							</TimestampWithTooltip>
						</span>
						{(message.flags & MessageFlags.SUPPRESS_NOTIFICATIONS) !== 0 && (
							<Tooltip
								text={i18n._(THIS_WAS_A_MESSAGE_DESCRIPTOR, {silentMention: SILENT_MENTION})}
								data-flx="channel.user-message.tooltip--2"
							>
								<BellSlashIcon
									weight="fill"
									className={styles.silentMessageIcon}
									data-flx="channel.user-message.silent-message-icon--2"
								/>
							</Tooltip>
						)}
					</AuthorHeading>
				)}
				<MessageAttachments data-flx="channel.user-message.message-attachments--3" />
				{message.threadId && (
					<ThreadPreviewCard
						threadId={message.threadId}
						threadName={message.threadName ?? ''}
						guildId={channel.guildId}
						parentChannelId={channel.id}
						data-flx="channel.user-message.thread-preview-card"
					/>
				)}
				{renderFailedFooter()}
			</div>
		</SpoilerSyncProvider>
	);
});
