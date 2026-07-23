// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/popouts/ThreadsPopout.module.css';
import * as ThreadCommands from '@app/features/channel/commands/ThreadCommands';
import ThreadCreation from '@app/features/channel/state/ThreadCreation';
import Threads from '@app/features/channel/state/Threads';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {getFormattedShortDate} from '@app/features/user/utils/DateFormatting';

function relativeTime(date: Date): string {
	const ms = Date.now() - date.getTime();
	const mins = Math.floor(ms / 60_000);
	if (mins < 60) return `${mins}m ago`;
	const hours = Math.floor(mins / 60);
	if (hours < 24) return `${hours}h ago`;
	const days = Math.floor(hours / 24);
	if (days < 7) return `${days}d ago`;
	return getFormattedShortDate(date);
}
import {ThreadIcon} from '@app/features/ui/components/icons/ThreadIcon';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo, useState} from 'react';
import type {Thread, ThreadPreview} from '@app/features/channel/state/Threads';

const THREADS_DESCRIPTOR = msg({message: 'Threads', comment: 'Threads popout title.'});
const CREATE_DESCRIPTOR = msg({message: 'Create', comment: 'Create button in threads popout.'});
const JOINED_LABEL_DESCRIPTOR = msg({message: '{count} JOINED THREADS', comment: 'Section label for joined threads.'});
const OTHER_ACTIVE_LABEL_DESCRIPTOR = msg({message: '{count} OTHER ACTIVE THREADS', comment: 'Section label for other active threads.'});
const OLDER_LABEL_DESCRIPTOR = msg({message: 'OLDER THREADS', comment: 'Section label for older (archived/closed) threads.'});
const NO_THREADS_DESCRIPTOR = msg({message: 'No threads yet', comment: 'Empty state in threads popout.'});
const SEARCH_PLACEHOLDER_DESCRIPTOR = msg({message: 'Search for Thread Name', comment: 'Placeholder for thread search input.'});

interface ThreadsPopoutProps {
	channelId: string;
	guildId: string;
	onClose?: () => void;
}

const EMPTY_PREVIEW: ThreadPreview = {
	lastMessagePreview: null,
	lastMessageAt: null,
	lastMessageAuthorId: null,
	lastMessageAuthorUsername: null,
	lastMessageAuthorAvatar: null,
	recentMemberAvatars: null,
};

const MAX_VISIBLE = 3;

function ThreadAvatarStack({
	preview,
	memberCount,
}: {
	preview: ThreadPreview;
	memberCount: number;
}) {
	const avatars = preview.recentMemberAvatars && preview.recentMemberAvatars.length > 0
		? preview.recentMemberAvatars
		: preview.lastMessageAuthorId
			? [{id: preview.lastMessageAuthorId, avatar: preview.lastMessageAuthorAvatar}]
			: [];

	const visible = avatars.slice(0, MAX_VISIBLE);
	const overflow = Math.max(0, memberCount - visible.length);

	return (
		<div className={styles.avatarStack} data-flx="channel.threads-popout.avatar-stack">
			{visible.map(({id, avatar}, i) => {
				const url = AvatarUtils.getUserAvatarURL({id, avatar});
				return (
					<img
						key={id}
						src={url}
						alt=""
						className={styles.stackAvatar}
						style={{zIndex: MAX_VISIBLE - i}}
						data-flx="channel.threads-popout.avatar-stack.avatar"
					/>
				);
			})}
			{visible.length === 0 && (
				<div className={styles.stackAvatarPlaceholder} data-flx="channel.threads-popout.avatar-stack.placeholder" />
			)}
			{overflow > 0 && (
				<div className={styles.stackOverflow} data-flx="channel.threads-popout.avatar-stack.overflow">
					+{overflow > 99 ? '99' : overflow}
				</div>
			)}
		</div>
	);
}

function SmallAvatar({authorId, avatar}: {authorId: string | null; avatar: string | null}) {
	if (!authorId) return <div className={styles.rowMetaAvatarPlaceholder} />;
	const url = AvatarUtils.getUserAvatarURL({id: authorId, avatar});
	return <img src={url ?? undefined} alt="" className={styles.rowMetaAvatar} />;
}

function ActiveThreadRow({
	thread,
	onClickRow,
}: {
	thread: Thread;
	onClickRow: (id: string) => void;
}) {
	const preview = thread.preview;
	const timeLabel = preview?.lastMessageAt ? relativeTime(preview.lastMessageAt) : null;
	const hasPreview = !!preview?.lastMessagePreview;

	return (
		<button
			type="button"
			className={styles.row}
			onClick={() => onClickRow(thread.id)}
			data-flx="channel.threads-popout.row.click"
		>
			<div className={styles.rowInfo} data-flx="channel.threads-popout.row-info">
				<p className={styles.rowName} data-flx="channel.threads-popout.row-name">
					{thread.name}
				</p>
				<p className={styles.rowMeta} data-flx="channel.threads-popout.row-meta">
					{preview?.lastMessageAuthorId && (
						<SmallAvatar authorId={preview.lastMessageAuthorId} avatar={preview.lastMessageAuthorAvatar ?? null} />
					)}
					{preview?.lastMessageAuthorUsername && (
						<span className={styles.rowSender} data-flx="channel.threads-popout.row-sender">
							{preview.lastMessageAuthorUsername}:
						</span>
					)}
					{hasPreview ? (
						<span className={styles.rowPreviewText} data-flx="channel.threads-popout.row-preview">
							{preview.lastMessagePreview}
						</span>
					) : (
						<span data-flx="channel.threads-popout.row-no-messages">No recent messages</span>
					)}
					{timeLabel && (
						<>
							<span className={styles.rowDot}>·</span>
							<span className={styles.rowTime} data-flx="channel.threads-popout.row-time">{timeLabel}</span>
						</>
					)}
				</p>
			</div>
			<ThreadAvatarStack
				preview={preview ?? EMPTY_PREVIEW}
				memberCount={thread.messageCount}
			/>
		</button>
	);
}

function OlderThreadRow({
	thread,
	onClickRow,
}: {
	thread: Thread;
	onClickRow: (id: string) => void;
}) {
	const preview = thread.preview;
	const timeLabel = preview?.lastMessageAt ? relativeTime(preview.lastMessageAt) : null;
	const creatorId = thread.threadCreatorId;
	const creatorUsername = thread.threadCreatorUsername;

	return (
		<button
			type="button"
			className={styles.row}
			onClick={() => onClickRow(thread.id)}
			data-flx="channel.threads-popout.older-row.click"
		>
			<div className={styles.rowInfo} data-flx="channel.threads-popout.older-row-info">
				<p className={styles.rowName} data-flx="channel.threads-popout.older-row-name">
					{thread.name}
				</p>
				<p className={styles.rowMeta} data-flx="channel.threads-popout.older-row-meta">
					{creatorId && <SmallAvatar authorId={creatorId} avatar={null} />}
					<span className={styles.rowStartedBy} data-flx="channel.threads-popout.row-started-by">Started by</span>
					{creatorUsername && (
						<span className={styles.rowStartedByName} data-flx="channel.threads-popout.row-creator-name">
							{creatorUsername}
						</span>
					)}
					{timeLabel && (
						<>
							<span className={styles.rowDot}>·</span>
							<span className={styles.rowTime}>Last active {timeLabel}</span>
						</>
					)}
				</p>
			</div>
			<ThreadAvatarStack
				preview={preview ?? EMPTY_PREVIEW}
				memberCount={thread.messageCount}
			/>
		</button>
	);
}

export const ThreadsPopout = observer(({channelId, guildId, onClose}: ThreadsPopoutProps) => {
	const {i18n} = useLingui();
	const [query, setQuery] = useState('');

	useEffect(() => {
		void ThreadCommands.fetchList(channelId);
	}, [channelId]);

	const allThreads = Threads.getThreadsForChannel(channelId);

	const {joined, otherActive, older} = useMemo(() => {
		const q = query.trim().toLowerCase();
		const threads = q ? allThreads.filter((t) => (t.name ?? '').toLowerCase().includes(q)) : allThreads;
		const joinedSet = Threads.joinedThreadIds;
		return {
			joined: threads.filter((t) => joinedSet.has(t.id) && t.isOpen()),
			otherActive: threads.filter((t) => !joinedSet.has(t.id) && t.isOpen()),
			older: threads.filter((t) => !t.isOpen()),
		};
	}, [allThreads, query, Threads.joinedThreadIds.size]);

	const handleCreate = () => {
		ThreadCreation.open({channelId});
		onClose?.();
	};

	const handleRowClick = async (threadId: string) => {
		if (!Threads.isJoined(threadId)) {
			await ThreadCommands.join(channelId, threadId);
		}
		NavigationCommands.selectThread(guildId, channelId, threadId);
		onClose?.();
	};

	const isEmpty = joined.length === 0 && otherActive.length === 0 && older.length === 0;

	return (
		<div className={styles.container} data-flx="channel.threads-popout.container">
			<div className={styles.header} data-flx="channel.threads-popout.header">
				<ThreadIcon size={18} className={styles.headerIcon} aria-hidden="true" data-flx="channel.threads-popout.header-icon" />
				<span className={styles.title} data-flx="channel.threads-popout.title">
					{i18n._(THREADS_DESCRIPTOR)}
				</span>
				<div className={styles.searchWrap} data-flx="channel.threads-popout.search-wrap">
					<MagnifyingGlassIcon size={14} className={styles.searchIcon} aria-hidden="true" data-flx="channel.threads-popout.search-icon" />
					<input
						className={styles.searchInput}
						placeholder={i18n._(SEARCH_PLACEHOLDER_DESCRIPTOR)}
						value={query}
						onChange={(e) => setQuery(e.target.value)}
						data-flx="channel.threads-popout.search-input"
					/>
				</div>
				<button
					type="button"
					className={styles.createBtn}
					onClick={handleCreate}
					data-flx="channel.threads-popout.create-button.click"
				>
					{i18n._(CREATE_DESCRIPTOR)}
				</button>
			</div>

			<div className={styles.body} data-flx="channel.threads-popout.body">
				{isEmpty && (
					<div className={styles.empty} data-flx="channel.threads-popout.empty">
						{i18n._(NO_THREADS_DESCRIPTOR)}
					</div>
				)}

				{joined.length > 0 && (
					<>
						<p className={styles.sectionLabel} data-flx="channel.threads-popout.joined-label">
							{i18n._(JOINED_LABEL_DESCRIPTOR, {count: joined.length})}
						</p>
						<div className={styles.list} data-flx="channel.threads-popout.joined-list">
							{joined.map((t) => (
								<ActiveThreadRow key={t.id} thread={t} onClickRow={(id) => void handleRowClick(id)} />
							))}
						</div>
					</>
				)}

				{otherActive.length > 0 && (
					<>
						<p className={styles.sectionLabel} data-flx="channel.threads-popout.other-label">
							{i18n._(OTHER_ACTIVE_LABEL_DESCRIPTOR, {count: otherActive.length})}
						</p>
						<div className={styles.list} data-flx="channel.threads-popout.other-list">
							{otherActive.map((t) => (
								<ActiveThreadRow key={t.id} thread={t} onClickRow={(id) => void handleRowClick(id)} />
							))}
						</div>
					</>
				)}

				{older.length > 0 && (
					<>
						<p className={styles.sectionLabel} data-flx="channel.threads-popout.older-label">
							{i18n._(OLDER_LABEL_DESCRIPTOR)}
						</p>
						<div className={styles.list} data-flx="channel.threads-popout.older-list">
							{older.map((t) => (
								<OlderThreadRow key={t.id} thread={t} onClickRow={(id) => void handleRowClick(id)} />
							))}
						</div>
					</>
				)}
			</div>
		</div>
	);
});
