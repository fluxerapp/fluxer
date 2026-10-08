// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ChannelTextareaControls,
	LexicalChannelTextareaContent,
} from '@app/features/channel/components/LexicalChannelTextareaContent';
import type {Channel} from '@app/features/channel/models/Channel';
import {EmojiPickerPopout} from '@app/features/emoji/components/popouts/EmojiPickerPopout';
import * as ForumCommands from '@app/features/forum/commands/ForumCommands';
import styles from '@app/features/forum/components/Forum.module.css';
import {ForumComposerTags} from '@app/features/forum/components/ForumTagPicker';
import {isMediaChannel, isTagRequired} from '@app/features/forum/utils/ForumChannelUtils';
import {reportForumError} from '@app/features/forum/utils/ForumErrors';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import * as DraftCommands from '@app/features/messaging/commands/DraftCommands';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {useTextareaAttachments} from '@app/features/messaging/hooks/useCloudUpload';
import type {SendMessageFunction} from '@app/features/messaging/hooks/useMessageSubmission';
import Drafts from '@app/features/messaging/state/MessagingDrafts';
import {CloudUpload} from '@app/features/messaging/upload/CloudUpload';
import * as MessageSubmitUtils from '@app/features/messaging/utils/MessageSubmitUtils';
import {hasVisibleMessageContent} from '@app/features/messaging/utils/VisibleMessageContent';
import Permission from '@app/features/permissions/state/Permission';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {openThread} from '@app/features/threads/commands/ThreadNavigation';
import ThreadPanel from '@app/features/threads/state/ThreadPanel';
import * as ThreadD from '@app/features/threads/utils/ThreadMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import Users from '@app/features/user/state/Users';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {THREAD_NAME_MAX_LENGTH} from '@fluxer/constants/src/ThreadConstants';
import type {MessageStickerItem} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {useLingui} from '@lingui/react/macro';
import {EyeIcon, ImageSquareIcon, SmileyIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';

function useCooldownSeconds(until: number): number {
	const [now, setNow] = useState(() => Date.now());
	useEffect(() => {
		if (until <= Date.now()) return;
		const timer = window.setInterval(() => setNow(Date.now()), 1000);
		return () => window.clearInterval(timer);
	}, [until]);
	return Math.max(0, Math.ceil((until - now) / 1000));
}

function resolvePostArgs(
	stickersOrTts?: Array<MessageStickerItem> | boolean,
	favoriteMemeIdOrStickers?: string | Array<MessageStickerItem>,
	maybeFavoriteMemeId?: string,
): {stickers: Array<MessageStickerItem>; favoriteMemeId?: string} {
	if (typeof stickersOrTts === 'boolean') {
		return {
			stickers: Array.isArray(favoriteMemeIdOrStickers) ? favoriteMemeIdOrStickers : [],
			favoriteMemeId: maybeFavoriteMemeId,
		};
	}
	return {
		stickers: stickersOrTts ?? [],
		favoriteMemeId: typeof favoriteMemeIdOrStickers === 'string' ? favoriteMemeIdOrStickers : undefined,
	};
}

interface ForumPostComposerProps {
	forum: Channel;
	initialTitle: string;
	onClose: () => void;
}

export const ForumPostComposer = observer(({forum, initialTitle, onClose}: ForumPostComposerProps) => {
	const {i18n} = useLingui();
	const [title, setTitle] = useState(initialTitle.slice(0, THREAD_NAME_MAX_LENGTH));
	const [tags, setTags] = useState<Array<string>>([]);
	const [error, setError] = useState<string | undefined>(undefined);
	const [body, setBody] = useState('');
	const [preview, setPreview] = useState(false);
	const controlsRef = useRef<ChannelTextareaControls | null>(null);
	const titleRef = useRef<HTMLInputElement>(null);
	useEffect(() => {
		titleRef.current?.focus();
	}, []);
	const stateRef = useRef({title, tags});
	stateRef.current = {title, tags};
	const draftKey = `forum-post:${forum.id}`;
	const accountKey = Users.viewAccountKey;
	const media = isMediaChannel(forum);
	const attachments = useTextareaAttachments(draftKey);
	const bypassesSlowmode = Permission.can(Permissions.BYPASS_SLOWMODE, forum);
	const cooldownSeconds = useCooldownSeconds(bypassesSlowmode ? 0 : ThreadPanel.getCreateCooldownUntil(forum.id));
	const handleSubmit: SendMessageFunction = useCallback(
		(content, _hasAttachments, stickersOrTts, favoriteMemeIdOrStickers, maybeFavoriteMemeId) => {
			const {stickers, favoriteMemeId} = resolvePostArgs(stickersOrTts, favoriteMemeIdOrStickers, maybeFavoriteMemeId);
			if (favoriteMemeId) {
				setError(i18n._(D.POST_FAVORITE_MEDIA_UNSUPPORTED_DESCRIPTOR));
				return false;
			}
			const name = stateRef.current.title.trim();
			if (!name) {
				setError(i18n._(D.POST_TITLE_REQUIRED_DESCRIPTOR));
				return false;
			}
			if (isTagRequired(forum) && stateRef.current.tags.length === 0) {
				setError(i18n._(D.POST_TAG_REQUIRED_DESCRIPTOR));
				return false;
			}
			const hasAttachments = CloudUpload.getTextareaAttachments(draftKey).length > 0;
			if (media && !hasAttachments) {
				setError(i18n._(D.POST_MEDIA_REQUIRED_DESCRIPTOR));
				return false;
			}
			const nonce = SnowflakeUtils.fromTimestamp(Date.now());
			const claimed = hasAttachments ? MessageSubmitUtils.claimMessageAttachments(draftKey, nonce, content) : [];
			void ForumCommands.createForumPost(forum, {
				name,
				appliedTags: stateRef.current.tags,
				content,
				nonce,
				hasAttachments: claimed.length > 0,
				stickerIds: stickers.map((sticker) => sticker.id),
			})
				.then((post) => {
					if (!post) return;
					ThreadPanel.startCreateCooldown(forum.id, forum.rateLimitPerUser);
					onClose();
					openThread(post);
				})
				.catch((failure) => {
					if (content && !Drafts.getDraft(draftKey)) DraftCommands.createDraft(accountKey, draftKey, content);
					reportForumError(i18n, failure, D.POST_CREATE_FAILED_DESCRIPTOR);
				});
			return true;
		},
		[forum, i18n, media, onClose, draftKey],
	);
	const ready =
		title.trim().length > 0 &&
		cooldownSeconds === 0 &&
		(media ? attachments.length > 0 : hasVisibleMessageContent(body) || attachments.length > 0) &&
		(!isTagRequired(forum) || tags.length > 0);
	const mobile = MobileLayout.enabled;
	return (
		<div className={styles.composerCard} data-flx="forum.forum-post-composer.composer">
			<div className={styles.composerHead} data-flx="forum.forum-post-composer.composer-head">
				<input
					className={styles.composerTitle}
					value={title}
					onChange={(event) => {
						setTitle(event.target.value);
						setError(undefined);
					}}
					onKeyDown={(event) => {
						if (event.key !== 'Enter') return;
						event.preventDefault();
						controlsRef.current?.focus();
					}}
					placeholder={i18n._(D.POST_TITLE_PLACEHOLDER_DESCRIPTOR)}
					aria-label={i18n._(D.POST_TITLE_DESCRIPTOR)}
					maxLength={THREAD_NAME_MAX_LENGTH}
					ref={titleRef}
					autoComplete="off"
					data-flx="forum.forum-post-composer.input.set-title"
				/>
				<FocusRing offset={-2} data-flx="forum.forum-post-composer.focus-ring">
					<button
						type="button"
						className={styles.composerIconButton}
						onClick={onClose}
						aria-label={i18n._(D.CANCEL_POST_DESCRIPTOR)}
						data-flx="forum.forum-post-composer.icon-button.close"
					>
						<XIcon size={remFromPx(18)} weight="bold" data-flx="forum.forum-post-composer.x-icon" />
					</button>
				</FocusRing>
			</div>
			<div
				className={clsx(styles.composerBody, preview && styles.composerBodyHidden)}
				data-flx="forum.forum-post-composer.composer-body"
			>
				<LexicalChannelTextareaContent
					accountKey={accountKey}
					channel={forum}
					draft={Drafts.getDraft(draftKey)}
					draftSegments={Drafts.getDraftSegments(draftKey)}
					disabled={cooldownSeconds > 0}
					typingEnabled={false}
					onSubmit={handleSubmit}
					enableReply={false}
					enableEditLast={false}
					enableSlashCommands={false}
					draftChannelId={draftKey}
					placeholder={i18n._(media ? D.POST_MEDIA_PLACEHOLDER_DESCRIPTOR : D.POST_MESSAGE_PLACEHOLDER_DESCRIPTOR)}
					bare
					controlsRef={controlsRef}
					onValueChange={setBody}
					data-flx="forum.forum-post-composer.lexical-channel-textarea-content.submit"
				/>
			</div>
			{preview && (
				<div className={styles.composerPreview} data-flx="forum.forum-post-composer.composer-preview">
					<span className={styles.composerPreviewLabel} data-flx="forum.forum-post-composer.composer-preview-label">
						{i18n._(D.PREVIEW_DESCRIPTOR)}
					</span>
					{hasVisibleMessageContent(body) ? (
						<SafeMarkdown
							content={body}
							options={{context: MarkdownContext.STANDARD_WITHOUT_JUMBO, channelId: forum.id}}
							data-flx="forum.forum-post-composer.safe-markdown"
						/>
					) : (
						<span className={styles.hint} data-flx="forum.forum-post-composer.preview-empty">
							{i18n._(D.PREVIEW_EMPTY_DESCRIPTOR)}
						</span>
					)}
				</div>
			)}
			{cooldownSeconds > 0 && (
				<div className={styles.hint} role="status" data-flx="forum.forum-post-composer.hint">
					{i18n._(ThreadD.SLOWMODE_COOLDOWN_DESCRIPTOR, {seconds: cooldownSeconds})}
				</div>
			)}
			{error && (
				<div className={styles.composerError} role="alert" data-flx="forum.forum-post-composer.error">
					{error}
				</div>
			)}
			<div className={styles.composerBar} data-flx="forum.forum-post-composer.composer-bar">
				<div className={styles.composerBarStart} data-flx="forum.forum-post-composer.composer-bar-start">
					<Tooltip text={i18n._(D.ATTACH_MEDIA_DESCRIPTOR)} data-flx="forum.forum-post-composer.attach-tooltip">
						<FocusRing offset={-2} data-flx="forum.forum-post-composer.attach.focus-ring">
							<button
								type="button"
								className={styles.composerIconButton}
								onClick={() => controlsRef.current?.uploadFiles()}
								aria-label={i18n._(D.ATTACH_MEDIA_DESCRIPTOR)}
								data-flx="forum.forum-post-composer.attach-button"
							>
								<ImageSquareIcon size={remFromPx(20)} weight="bold" data-flx="forum.forum-post-composer.image-icon" />
							</button>
						</FocusRing>
					</Tooltip>
					{!mobile && (
						<Popout
							position="top-start"
							render={({onClose: closePicker}) => (
								<EmojiPickerPopout
									channelId={forum.id}
									handleSelect={(emoji) => controlsRef.current?.insertEmoji(emoji)}
									onClose={closePicker}
									data-flx="forum.forum-post-composer.emoji-picker-popout"
								/>
							)}
							data-flx="forum.forum-post-composer.emoji-popout"
						>
							<Tooltip text={i18n._(D.ADD_EMOJI_DESCRIPTOR)} data-flx="forum.forum-post-composer.emoji-tooltip">
								<FocusRing offset={-2} data-flx="forum.forum-post-composer.emoji.focus-ring">
									<button
										type="button"
										className={styles.composerIconButton}
										aria-label={i18n._(D.ADD_EMOJI_DESCRIPTOR)}
										data-flx="forum.forum-post-composer.emoji-button"
									>
										<SmileyIcon size={remFromPx(20)} weight="bold" data-flx="forum.forum-post-composer.smiley-icon" />
									</button>
								</FocusRing>
							</Tooltip>
						</Popout>
					)}
					{forum.availableTags.length > 0 && (
						<ForumComposerTags
							forum={forum}
							selected={tags}
							onChange={(next) => {
								setTags(next);
								setError(undefined);
							}}
							data-flx="forum.forum-post-composer.forum-tag-picker.set-tags"
						/>
					)}
				</div>
				<div className={styles.composerBarEnd} data-flx="forum.forum-post-composer.composer-bar-end">
					<Tooltip
						text={i18n._(preview ? D.EDIT_DESCRIPTOR : D.PREVIEW_DESCRIPTOR)}
						data-flx="forum.forum-post-composer.preview-tooltip"
					>
						<FocusRing offset={-2} data-flx="forum.forum-post-composer.preview.focus-ring">
							<button
								type="button"
								className={clsx(styles.composerIconButton, preview && styles.composerIconButtonActive)}
								onClick={() => setPreview((value) => !value)}
								aria-pressed={preview}
								aria-label={i18n._(D.PREVIEW_DESCRIPTOR)}
								data-flx="forum.forum-post-composer.preview-button"
							>
								<EyeIcon size={remFromPx(20)} weight="bold" data-flx="forum.forum-post-composer.eye-icon" />
							</button>
						</FocusRing>
					</Tooltip>
					<Button
						small
						fitContent
						disabled={!ready}
						onClick={() => controlsRef.current?.submit()}
						data-flx="forum.forum-post-composer.button.post"
					>
						{i18n._(D.POST_DESCRIPTOR)}
					</Button>
				</div>
			</div>
		</div>
	);
});
