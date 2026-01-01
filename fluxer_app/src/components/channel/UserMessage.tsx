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
import {BellSlashIcon, EyeIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {autorun} from 'mobx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import {FLUXERBOT_ID, MessageEmbedTypes, MessageFlags, MessageStates, MessageTypes} from '~/Constants';
import {EditingMessageInput} from '~/components/channel/EditingMessageInput';
import {MessageAttachments} from '~/components/channel/MessageAttachments';
import {MessageAuthorInfo} from '~/components/channel/MessageAuthorInfo';
import {MessageAvatar} from '~/components/channel/MessageAvatar';
import {MessageUsername} from '~/components/channel/MessageUsername';
import {ReplyPreview} from '~/components/channel/ReplyPreview';
import {TimestampWithTooltip} from '~/components/channel/TimestampWithTooltip';
import {UserTag} from '~/components/channel/UserTag';
import {Tooltip} from '~/components/uikit/Tooltip';
import FocusManager from '~/lib/FocusManager';
import {SafeMarkdown} from '~/lib/markdown';
import {NodeType} from '~/lib/markdown/parser/types/enums';
import {MarkdownContext, parse} from '~/lib/markdown/renderers';
import AccessibilityStore from '~/stores/AccessibilityStore';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import MessageEditStore from '~/stores/MessageEditStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import UserStore from '~/stores/UserStore';
import markupStyles from '~/styles/Markup.module.css';
import styles from '~/styles/Message.module.css';
import * as DateUtils from '~/utils/DateUtils';
import {SpoilerSyncProvider} from '~/utils/SpoilerUtils';
import {useMessageViewContext} from './MessageViewContext';

const MessageStateToClassName: Record<string, string> = {
	[MessageStates.SENT]: styles.messageSent,
	[MessageStates.SENDING]: styles.messageSending,
	[MessageStates.FAILED]: styles.messageFailed,
};

export const UserMessage = observer(() => {
	const {t, i18n} = useLingui();
	const {message, channel, handleDelete, isHovering, shouldGroup, previewContext, previewOverrides} =
		useMessageViewContext();
	const [animateEmoji, setAnimateEmoji] = React.useState(
		UserSettingsStore.getAnimateEmoji() && FocusManager.isFocused(),
	);
	const [value, setValue] = React.useState('');
	const hasInitializedEditingRef = React.useRef(false);
	const isEditing = MessageEditStore.isEditing(message.channelId, message.id);
	const userAuthor = UserStore.getUser(message.author.id);
	const author = message.webhookId != null ? message.author : (userAuthor ?? message.author);
	const formattedDate = DateUtils.getRelativeDateString(message.timestamp, i18n);
	const messageDisplayCompact = UserSettingsStore.getMessageDisplayCompact();
	const showUserAvatarsInCompactMode = AccessibilityStore.showUserAvatarsInCompactMode;
	const textareaRef = React.useRef<HTMLTextAreaElement>(null);
	const {nodes: astNodes} = React.useMemo(
		() =>
			parse({
				content: message.content,
				context: MarkdownContext.STANDARD_WITH_JUMBO,
			}),
		[message.content],
	);

	const shouldHideContent =
		UserSettingsStore.getRenderEmbeds() &&
		message.embeds.length > 0 &&
		message.embeds.every((embed) => embed.type === MessageEmbedTypes.IMAGE || embed.type === MessageEmbedTypes.GIFV) &&
		astNodes.length === 1 &&
		astNodes[0].type === NodeType.Link &&
		!message.suppressEmbeds;

	const guild = GuildStore.getGuild(channel.guildId ?? '');
	const member = GuildMemberStore.getMember(guild?.id ?? '', author?.id ?? '');
	const shouldAppearAuthorless = false;

	const mobileLayout = MobileLayoutStore;

	React.useLayoutEffect(() => {
		if (isEditing) {
			if (!hasInitializedEditingRef.current) {
				hasInitializedEditingRef.current = true;
				const persistedDraft = MessageEditStore.getEditingContent(channel.id, message.id);
				const initialValue = persistedDraft ?? message.content;
				setValue(initialValue);
				textareaRef.current?.focus();
				textareaRef.current?.setSelectionRange(initialValue.length, initialValue.length);
			} else {
				textareaRef.current?.focus();
			}
		} else {
			hasInitializedEditingRef.current = false;
			setValue('');
		}
	}, [channel.id, isEditing, message.content, message.id]);

	React.useEffect(() => {
		if (!isEditing) {
			return;
		}
		MessageEditStore.setEditingContent(channel.id, message.id, value);
	}, [channel.id, isEditing, message.id, value]);

	React.useEffect(() => {
		if (animateEmoji) return;
		const emojiImgs = document.querySelectorAll(
			`img[data-message-id="${message.id}"][data-animated="true"]`,
		) as NodeListOf<HTMLImageElement>;

		for (const img of emojiImgs) {
			const src = img.src;
			img.src = isHovering ? src.replace('.webp', '.gif') : src.replace('.gif', '.webp');
		}
	}, [animateEmoji, isHovering, message.id]);

	React.useEffect(() => {
		const disposer = autorun(() => {
			const shouldAnimate = UserSettingsStore.animateEmoji && FocusManager.isFocused();
			setAnimateEmoji(shouldAnimate);
			if (shouldAnimate) {
				const emojiImgs = document.querySelectorAll(
					`img[data-message-id="${message.id}"][data-animated="true"]`,
				) as NodeListOf<HTMLImageElement>;

				for (const img of emojiImgs) {
					const src = img.src;
					img.src = src.replace('.webp', '.gif');
				}
			}
		});
		return () => disposer();
	}, [message.id]);

	React.useEffect(() => {
		const disposer = autorun(() => {
			const shouldAnimate = UserSettingsStore.animateEmoji && FocusManager.isFocused();
			setAnimateEmoji(shouldAnimate);

			const emojiImgs = document.querySelectorAll(
				`img[data-message-id="${message.id}"][data-animated="true"]`,
			) as NodeListOf<HTMLImageElement>;

			for (const img of emojiImgs) {
				const src = img.src;
				if (shouldAnimate) {
					img.src = src.replace('.webp', '.gif');
				} else {
					img.src = src.replace('.gif', '.webp');
				}
			}
		});

		return () => disposer();
	}, [message.id]);

	const onSubmit = React.useCallback(
		(actualContent?: string) => {
			if (message.messageSnapshots) {
				return;
			}
			const content = (actualContent ?? value).trim();
			if (!content) {
				handleDelete();
				return;
			}

			MessageActionCreators.stopEdit(channel.id);
			MessageActionCreators.edit(channel.id, message.id, content);
		},
		[channel.id, handleDelete, message.id, value, message.messageSnapshots],
	);

	const cancelEditing = React.useCallback(() => {
		MessageActionCreators.stopEdit(message.channelId);
	}, [message.channelId]);

	const handleDismissSystemMessage = React.useCallback(() => {
		MessageActionCreators.deleteOptimistic(message.channelId, message.id);
	}, [message.channelId, message.id]);

	if (message.type === MessageTypes.CLIENT_SYSTEM && message.author.id === FLUXERBOT_ID) {
		return (
			<SpoilerSyncProvider>
				<div className={styles.messageGutterLeft} />

				<MessageAvatar
					user={author}
					message={message}
					guildId={guild?.id}
					size={40}
					className={styles.messageAvatar}
					isHovering={isHovering}
					isPreview={!!previewContext}
				/>

				<div className={styles.messageGutterRight} />

				<div className={styles.messageContent}>
					<h3 className={styles.messageAuthorInfo}>
						<span className={styles.authorContainer}>
							<MessageUsername
								user={author}
								message={message}
								guild={guild}
								member={member ?? undefined}
								className={styles.messageUsername}
								isPreview={!!previewContext}
								previewColor={previewOverrides?.usernameColor}
								previewName={previewOverrides?.displayName}
							/>
							<UserTag className={styles.userTagOffset} system={author.system} />
						</span>
						<TimestampWithTooltip date={message.timestamp} className={styles.messageTimestamp}>
							{formattedDate}
						</TimestampWithTooltip>
					</h3>
					<div className={styles.messageText}>
						<div className={clsx(markupStyles.markup)}>
							<SafeMarkdown
								content={message.content}
								options={{
									context: MarkdownContext.STANDARD_WITH_JUMBO,
									messageId: message.id,
									channelId: message.channelId,
								}}
							/>
						</div>

						<div className={styles.systemMessageContainer}>
							<EyeIcon className={styles.systemMessageIcon} />
							<div>
								<Trans>
									only you can see this message.{' '}
									<button
										type="button"
										className={styles.systemMessageDismissButton}
										onClick={handleDismissSystemMessage}
										key="dismiss"
									>
										dismiss
									</button>
								</Trans>
							</div>
						</div>
					</div>
				</div>

				<div className={styles.container}>
					<MessageAttachments />
				</div>
			</SpoilerSyncProvider>
		);
	}

	const renderMessageContent = () => {
		if (isEditing && !previewContext && !mobileLayout.enabled) {
			return (
				<EditingMessageInput
					channel={channel}
					onCancel={cancelEditing}
					onSubmit={onSubmit}
					textareaRef={textareaRef}
					value={value}
					setValue={setValue}
				/>
			);
		}

		if (shouldHideContent) return null;

		return (
			<div className={clsx(markupStyles.markup)}>
				<SafeMarkdown
					content={message.content}
					options={{
						context: MarkdownContext.STANDARD_WITH_JUMBO,
						messageId: message.id,
						channelId: message.channelId,
					}}
				/>
				{(message.editedTimestamp || message.isEditing) &&
					(message.isEditing ? (
						<span className={styles.editedLabel}> {t`(edited)`}</span>
					) : (
						<TimestampWithTooltip date={message.editedTimestamp!} className={styles.editedTimestamp}>
							<span className={styles.editedLabel}> {t`(edited)`}</span>
						</TimestampWithTooltip>
					))}
			</div>
		);
	};

	if (messageDisplayCompact) {
		return (
			<SpoilerSyncProvider>
				{message.messageReference && message.messageReference.type === 0 && (
					<ReplyPreview message={message} channelId={channel.id} animateEmoji={animateEmoji} />
				)}

				<div className={styles.compactContentWrapper}>
					<MessageAuthorInfo
						message={message}
						author={author}
						guild={guild}
						member={member ?? undefined}
						shouldGroup={shouldGroup}
						shouldAppearAuthorless={shouldAppearAuthorless}
						messageDisplayCompact={messageDisplayCompact}
						showUserAvatarsInCompactMode={showUserAvatarsInCompactMode}
						mobileLayoutEnabled={mobileLayout.enabled}
						isHovering={isHovering}
						formattedDate={formattedDate}
						previewContext={previewContext}
						previewOverrides={previewOverrides}
					/>
					{!shouldHideContent && (
						<span className={clsx(styles.compactInlineContent, MessageStateToClassName[message.state])}>
							{isEditing && !previewContext && !mobileLayout.enabled ? (
								<EditingMessageInput
									channel={channel}
									onCancel={cancelEditing}
									onSubmit={onSubmit}
									textareaRef={textareaRef}
									value={value}
									setValue={setValue}
								/>
							) : (
								<span className={clsx(markupStyles.markup, 'inline')}>
									<SafeMarkdown
										content={message.content}
										options={{
											context: MarkdownContext.STANDARD_WITH_JUMBO,
											messageId: message.id,
											channelId: message.channelId,
										}}
									/>
									{(message.editedTimestamp || message.isEditing) &&
										(message.isEditing ? (
											<span className={styles.editedLabel}> {t`(edited)`}</span>
										) : (
											<TimestampWithTooltip date={message.editedTimestamp!} className={styles.editedTimestamp}>
												<span className={styles.editedLabel}> {t`(edited)`}</span>
											</TimestampWithTooltip>
										))}
								</span>
							)}
						</span>
					)}
				</div>

				<div className={styles.container}>
					<MessageAttachments />
				</div>

				{mobileLayout.enabled && message.state === MessageStates.FAILED && (
					<div className={styles.mobileFailedIndicator}>
						<WarningCircleIcon weight="fill" className={styles.mobileFailedIcon} />
						<span>{t`Failed to send message. Hold for options.`}</span>
					</div>
				)}
			</SpoilerSyncProvider>
		);
	}

	return (
		<SpoilerSyncProvider>
			{message.messageReference && message.messageReference.type === 0 && (
				<ReplyPreview message={message} channelId={channel.id} animateEmoji={animateEmoji} />
			)}

			{!shouldGroup && (
				<>
					<div className={styles.messageGutterLeft} />
					<MessageAvatar
						user={author}
						message={message}
						guildId={guild?.id}
						size={40}
						className={styles.messageAvatar}
						isHovering={isHovering}
						isPreview={!!previewContext}
					/>
					<div className={styles.messageGutterRight} />
				</>
			)}

			{shouldGroup && (
				<MessageAuthorInfo
					message={message}
					author={author}
					guild={guild}
					member={member ?? undefined}
					shouldGroup={shouldGroup}
					shouldAppearAuthorless={shouldAppearAuthorless}
					messageDisplayCompact={messageDisplayCompact}
					showUserAvatarsInCompactMode={showUserAvatarsInCompactMode}
					mobileLayoutEnabled={mobileLayout.enabled}
					isHovering={isHovering}
					formattedDate={formattedDate}
					previewContext={previewContext}
					previewOverrides={previewOverrides}
				/>
			)}

			{(message.content || isEditing) && (!shouldHideContent || isEditing) && (
				<div className={styles.messageContent}>
					{!shouldGroup && (
						<h3 className={styles.messageAuthorInfo}>
							<span className={styles.authorContainer}>
								<MessageUsername
									user={author}
									message={message}
									guild={guild}
									member={member ?? undefined}
									className={styles.messageUsername}
									isPreview={!!previewContext}
									previewColor={previewOverrides?.usernameColor}
									previewName={previewOverrides?.displayName}
								/>
								{author.bot && <UserTag className={styles.userTagOffset} system={author.system} />}
							</span>
							<TimestampWithTooltip date={message.timestamp} className={styles.messageTimestamp}>
								{formattedDate}
							</TimestampWithTooltip>
							{(message.flags & MessageFlags.SUPPRESS_NOTIFICATIONS) !== 0 && (
								<Tooltip text={t`This was a @silent message.`}>
									<BellSlashIcon weight="fill" className={styles.silentMessageIcon} />
								</Tooltip>
							)}
						</h3>
					)}
					<div className={clsx(styles.messageText, MessageStateToClassName[message.state])}>
						{renderMessageContent()}
					</div>
				</div>
			)}

			<div className={styles.container}>
				{((!message.content && !isEditing) || (shouldHideContent && !isEditing)) && !shouldGroup && (
					<h3 className={styles.messageAuthorInfo}>
						<span className={styles.authorContainer}>
							<MessageUsername
								user={author}
								message={message}
								guild={guild}
								member={member ?? undefined}
								className={styles.messageUsername}
								isPreview={!!previewContext}
								previewColor={previewOverrides?.usernameColor}
								previewName={previewOverrides?.displayName}
							/>
							{author.bot && <UserTag className={styles.userTagOffset} />}
						</span>
						<TimestampWithTooltip date={message.timestamp} className={styles.messageTimestamp}>
							{formattedDate}
						</TimestampWithTooltip>
						{(message.flags & MessageFlags.SUPPRESS_NOTIFICATIONS) !== 0 && (
							<Tooltip text={t`This was a @silent message.`}>
								<BellSlashIcon weight="fill" className={styles.silentMessageIcon} />
							</Tooltip>
						)}
					</h3>
				)}

				<MessageAttachments />
			</div>

			{mobileLayout.enabled && message.state === MessageStates.FAILED && (
				<div className={styles.mobileFailedIndicator}>
					<WarningCircleIcon weight="fill" className={styles.mobileFailedIcon} />
					<span>{t`Failed to send message. Hold for options.`}</span>
				</div>
			)}
		</SpoilerSyncProvider>
	);
});
