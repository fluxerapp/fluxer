// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Emoji from '@app/features/emoji/state/Emoji';
import styles from '@app/features/forum/components/Forum.module.css';
import {ForumPostMenuSheet, openForumPostContextMenu} from '@app/features/forum/components/ForumPostContextMenu';
import {ForumEmoji, ForumTagPill} from '@app/features/forum/components/ForumTagPill';
import ForumPosts from '@app/features/forum/state/ForumPosts';
import ForumReadState from '@app/features/forum/state/ForumReadState';
import {getDefaultReaction, getForumTags} from '@app/features/forum/utils/ForumChannelUtils';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import {isPinnedPost} from '@app/features/forum/utils/ForumPostList';
import * as ReactionCommands from '@app/features/messaging/commands/ReactionCommands';
import {Message} from '@app/features/messaging/models/MessagingMessage';
import {buildFitInsideMediaProxyURL} from '@app/features/messaging/utils/MediaProxyUtils';
import Navigation from '@app/features/navigation/state/Navigation';
import {buildMessageNotificationBody} from '@app/features/notification/utils/MessageNotificationPreview';
import Permission from '@app/features/permissions/state/Permission';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {openThread} from '@app/features/threads/commands/ThreadNavigation';
import {lastActivityId} from '@app/features/threads/state/ChannelThreads';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import Users from '@app/features/user/state/Users';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {MessageAttachmentFlags, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {DefaultReactionEmojiResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import type {
	MessageAttachment,
	Message as WireMessage,
} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {useLingui} from '@lingui/react/macro';
import {ChatCircleIcon, PlayIcon, PushPinSimpleIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useMemo, useState} from 'react';

const MAX_CARD_TAGS = 3;
const LIST_THUMBNAIL_SIZE = 96;
const GRID_THUMBNAIL_SIZE = 480;

function isVisualAttachment(attachment: MessageAttachment): boolean {
	const type = attachment.content_type ?? '';
	return (
		(type.startsWith('image/') || type.startsWith('video/')) &&
		(attachment.flags & MessageAttachmentFlags.IS_SPOILER) === 0 &&
		!attachment.nsfw &&
		!attachment.expired
	);
}

function firstMedia(message: WireMessage | null | undefined): {src: string; video: boolean} | null {
	const attachment = message?.attachments?.find(isVisualAttachment);
	const source = attachment?.proxy_url ?? attachment?.url;
	if (!attachment || !source) return null;
	return {src: source, video: (attachment.content_type ?? '').startsWith('video/')};
}

function defaultReactionState(
	message: WireMessage | null | undefined,
	reaction: DefaultReactionEmojiResponse,
): {count: number; me: boolean} {
	const match = message?.reactions?.find((entry) =>
		reaction.emoji_id != null ? entry.emoji.id === reaction.emoji_id : entry.emoji.name === reaction.emoji_name,
	);
	return {count: match?.count ?? 0, me: match?.me ?? false};
}

const DefaultReactionButton = observer(
	({forum, post, message}: {forum: Channel; post: Channel; message: WireMessage | null | undefined}) => {
		const {i18n} = useLingui();
		const reaction = getDefaultReaction(forum);
		if (!reaction || !message) return null;
		const state = defaultReactionState(message, reaction);
		const canReact = Permission.can(Permissions.ADD_REACTIONS, forum) || state.me || state.count > 0;
		const handleClick = (event: React.MouseEvent) => {
			event.stopPropagation();
			if (!canReact) return;
			const emoji = {
				id: reaction.emoji_id,
				name:
					reaction.emoji_name ?? (reaction.emoji_id ? (Emoji.getEmojiById(reaction.emoji_id)?.name ?? 'emoji') : ''),
			};
			if (state.me) {
				ReactionCommands.removeReaction(i18n, post.id, message.id, emoji);
			} else {
				ReactionCommands.addReaction(i18n, post.id, message.id, emoji);
			}
		};
		return (
			<FocusRing offset={-2} data-flx="forum.forum-post-card.default-reaction-button.focus-ring">
				<button
					type="button"
					className={clsx(styles.reaction, state.me && styles.reactionActive)}
					onClick={handleClick}
					aria-pressed={state.me}
					disabled={!canReact || post.isLocked || post.isArchived}
					data-flx="forum.forum-post-card.default-reaction-button.reaction.click.button"
				>
					<ForumEmoji emoji={reaction} data-flx="forum.forum-post-card.default-reaction-button.forum-emoji" />
					{state.count > 0 && <span data-flx="forum.forum-post-card.default-reaction-button.span">{state.count}</span>}
				</button>
			</FocusRing>
		);
	},
);

interface ForumPostCardProps {
	forum: Channel;
	post: Channel;
	grid: boolean;
}

export const ForumPostCard = observer(({forum, post, grid}: ForumPostCardProps) => {
	const {i18n} = useLingui();
	const [sheetOpen, setSheetOpen] = useState(false);
	const firstMessage = ForumPosts.getFirstMessage(post.id);
	const preview = useMemo(() => {
		if (firstMessage === undefined) return null;
		if (firstMessage === null) return i18n._(D.ORIGINAL_MESSAGE_DELETED_DESCRIPTOR);
		const record = new Message(firstMessage, {skipReactionHydration: true});
		return buildMessageNotificationBody(record, i18n);
	}, [firstMessage, i18n]);
	const media = firstMedia(firstMessage);
	const authorId = firstMessage?.author.id ?? post.ownerId;
	const author = authorId ? Users.getUser(authorId) : undefined;
	const authorName = author ? NicknameUtils.getNickname(author, forum.guildId, forum.id) : null;
	const tags = getForumTags(forum, post.appliedTags);
	const joined = ThreadMemberships.isMember(post.id);
	const postUnreadCount = joined ? 0 : ForumReadState.getPostUnreadCount(post.id);
	const unread = (joined && ReadStates.hasUnread(post.id)) || postUnreadCount > 0;
	const unreadCount = joined && unread ? ReadStates.getUnreadCount(post.id) : postUnreadCount;
	const isNew = !unread && ForumReadState.isNewPost(post);
	const pinned = isPinnedPost(post);
	const open = Navigation.threadId === post.id;
	const lastActivity = new Date(SnowflakeUtils.extractTimestamp(lastActivityId(post)));
	const handleOpen = useCallback(() => openThread(post), [post]);
	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			if (MobileLayout.enabled) {
				event.preventDefault();
				setSheetOpen(true);
				return;
			}
			openForumPostContextMenu(event, post);
		},
		[post],
	);
	const thumbnail = media
		? buildFitInsideMediaProxyURL(media.src, {
				width: grid ? GRID_THUMBNAIL_SIZE : LIST_THUMBNAIL_SIZE,
				height: grid ? GRID_THUMBNAIL_SIZE : LIST_THUMBNAIL_SIZE,
			})
		: null;
	return (
		<>
			<FocusRing offset={-2} data-flx="forum.forum-post-card.focus-ring">
				<div
					role="button"
					tabIndex={0}
					className={clsx(styles.card, grid && styles.cardGrid, unread && styles.cardUnread, open && styles.cardOpen)}
					aria-current={open ? 'true' : undefined}
					onClick={handleOpen}
					onKeyDown={(event) => {
						if (event.key === 'Enter' || event.key === ' ') {
							event.preventDefault();
							handleOpen();
						}
					}}
					onContextMenu={handleContextMenu}
					data-flx="forum.forum-post-card.card.open"
				>
					{grid && (
						<div className={styles.cardMedia} data-flx="forum.forum-post-card.card-media">
							{thumbnail ? (
								<img
									src={thumbnail}
									alt=""
									className={styles.cardMediaImage}
									loading="lazy"
									draggable={false}
									data-flx="forum.forum-post-card.card-media-image"
								/>
							) : (
								<div className={styles.cardMediaPlaceholder} data-flx="forum.forum-post-card.card-media-placeholder">
									<ChatCircleIcon size={32} data-flx="forum.forum-post-card.chat-circle-icon" />
								</div>
							)}
							{media?.video && (
								<span className={styles.cardMediaPlay} data-flx="forum.forum-post-card.card-media-play">
									<PlayIcon size={20} weight="fill" data-flx="forum.forum-post-card.play-icon" />
								</span>
							)}
						</div>
					)}
					<div className={styles.cardBody} data-flx="forum.forum-post-card.card-body">
						<div className={styles.cardHeader} data-flx="forum.forum-post-card.card-header">
							{pinned && (
								<PushPinSimpleIcon
									size={16}
									weight="fill"
									aria-label={i18n._(D.PINNED_DESCRIPTOR)}
									data-flx="forum.forum-post-card.push-pin-simple-icon"
								/>
							)}
							{isNew && (
								<span className={styles.newBadge} data-flx="forum.forum-post-card.new-badge">
									{i18n._(D.NEW_BADGE_DESCRIPTOR)}
								</span>
							)}
							{tags.slice(0, MAX_CARD_TAGS).map((tag) => (
								<ForumTagPill key={tag.id} tag={tag} data-flx="forum.forum-post-card.forum-tag-pill" />
							))}
							{tags.length > MAX_CARD_TAGS && (
								<span
									className={styles.tag}
									data-flx="forum.forum-post-card.tag"
								>{`+${tags.length - MAX_CARD_TAGS}`}</span>
							)}
						</div>
						<div className={styles.cardTitle} data-flx="forum.forum-post-card.card-title">
							{post.name}
						</div>
						{preview != null && (
							<div className={styles.cardPreview} data-flx="forum.forum-post-card.card-preview">
								{authorName && (
									<span
										className={styles.cardAuthor}
										data-flx="forum.forum-post-card.card-author"
									>{`${authorName}: `}</span>
								)}
								<span data-flx="forum.forum-post-card.preview-text">{preview}</span>
							</div>
						)}
						<div className={styles.cardFooter} data-flx="forum.forum-post-card.card-footer">
							<DefaultReactionButton
								forum={forum}
								post={post}
								message={firstMessage}
								data-flx="forum.forum-post-card.default-reaction-button"
							/>
							<span className={styles.cardMeta} data-flx="forum.forum-post-card.card-meta">
								<ChatCircleIcon
									size={remFromPx(14)}
									weight="fill"
									aria-hidden
									data-flx="forum.forum-post-card.chat-circle-icon--2"
								/>
								{i18n._(D.POST_COUNT_DESCRIPTOR, {count: post.messageCount})}
							</span>
							{unread && (
								<span className={styles.cardUnreadLabel} data-flx="forum.forum-post-card.card-unread-label">
									{unreadCount > 0
										? i18n._(D.UNREAD_MESSAGES_DESCRIPTOR, {count: unreadCount})
										: i18n._(D.HAS_UNREAD_MESSAGES_DESCRIPTOR)}
								</span>
							)}
							<span className={styles.cardMeta} data-flx="forum.forum-post-card.card-meta--2">
								<span className={styles.cardDot} aria-hidden data-flx="forum.forum-post-card.card-dot" />
								{DateUtils.getRelativeDateString(lastActivity, i18n)}
							</span>
						</div>
					</div>
					{!grid && thumbnail && (
						<div className={styles.cardThumb} data-flx="forum.forum-post-card.card-thumb">
							<img
								src={thumbnail}
								alt=""
								className={styles.cardMediaImage}
								loading="lazy"
								draggable={false}
								data-flx="forum.forum-post-card.card-media-image--2"
							/>
							{media?.video && (
								<span className={styles.cardMediaPlay} data-flx="forum.forum-post-card.card-media-play--2">
									<PlayIcon size={16} weight="fill" data-flx="forum.forum-post-card.play-icon--2" />
								</span>
							)}
						</div>
					)}
				</div>
			</FocusRing>
			{sheetOpen && (
				<ForumPostMenuSheet
					post={post}
					onClose={() => setSheetOpen(false)}
					data-flx="forum.forum-post-card.forum-post-menu-sheet"
				/>
			)}
		</>
	);
});
