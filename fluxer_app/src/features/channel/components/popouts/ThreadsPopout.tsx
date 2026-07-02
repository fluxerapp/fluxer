// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/popouts/ThreadsPopout.module.css';
import * as ThreadCommands from '@app/features/channel/commands/ThreadCommands';
import ThreadCreation from '@app/features/channel/state/ThreadCreation';
import Threads from '@app/features/channel/state/Threads';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {getFormattedShortDate} from '@app/features/user/utils/DateFormatting';
import {ThreadIcon} from '@app/features/ui/components/icons/ThreadIcon';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useMemo, useState} from 'react';

const THREADS_DESCRIPTOR = msg({message: 'Threads', comment: 'Threads popout title.'});
const CREATE_DESCRIPTOR = msg({message: 'Create', comment: 'Create button in threads popout.'});
const JOINED_THREADS_DESCRIPTOR = msg({message: '{count} JOINED THREADS', comment: 'Section label in threads popout.'});
const NO_THREADS_DESCRIPTOR = msg({message: 'No threads yet', comment: 'Empty state in threads popout.'});
const SEARCH_PLACEHOLDER_DESCRIPTOR = msg({message: 'Search for Thread Name', comment: 'Placeholder for thread search input in threads popout.'});
const NO_RECENT_MESSAGES_DESCRIPTOR = msg({message: 'No recent messages', comment: 'Fallback when thread has no preview message.'});

interface ThreadsPopoutProps {
	channelId: string;
	guildId: string;
	onClose?: () => void;
}

export const ThreadsPopout = observer(({channelId, guildId, onClose}: ThreadsPopoutProps) => {
	const {i18n} = useLingui();
	const [query, setQuery] = useState('');

	const allThreads = Threads.getJoinedThreadsForChannel(channelId);
	const filtered = useMemo(() => {
		const q = query.trim().toLowerCase();
		if (!q) return allThreads;
		return allThreads.filter((t) => (t.name ?? '').toLowerCase().includes(q));
	}, [allThreads, query]);

	const handleCreate = () => {
		ThreadCreation.open({channelId});
		onClose?.();
	};

	const handleRowClick = async (threadId: string) => {
		await ThreadCommands.join(channelId, threadId);
		NavigationCommands.selectThread(guildId, channelId, threadId);
		onClose?.();
	};

	return (
		<div className={styles.container} data-flx="channel.threads-popout.container">
			<div className={styles.header} data-flx="channel.threads-popout.header">
				<ThreadIcon size={16} className={styles.headerIcon} aria-hidden="true" data-flx="channel.threads-popout.header-icon" />
				<span className={styles.title} data-flx="channel.threads-popout.title">
					{i18n._(THREADS_DESCRIPTOR)}
				</span>
			</div>

			<div className={styles.searchRow} data-flx="channel.threads-popout.search-row">
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
				{filtered.length > 0 && (
					<p className={styles.sectionLabel} data-flx="channel.threads-popout.section-label">
						{i18n._(JOINED_THREADS_DESCRIPTOR, {count: filtered.length})}
					</p>
				)}
				{filtered.length === 0 && (
					<div className={styles.empty} data-flx="channel.threads-popout.empty">
						{i18n._(NO_THREADS_DESCRIPTOR)}
					</div>
				)}
				<div className={styles.list} data-flx="channel.threads-popout.list">
					{filtered.map((thread) => {
						const preview = thread.preview;
						const avatarUrl = preview?.lastMessageAuthorId
							? AvatarUtils.getUserAvatarURL({
									id: preview.lastMessageAuthorId,
									avatar: preview.lastMessageAuthorAvatar ?? null,
								})
							: null;
						const timeLabel = preview?.lastMessageAt
							? getFormattedShortDate(preview.lastMessageAt)
							: null;
						const hasMeta = preview?.lastMessagePreview || timeLabel;

						return (
							<button
								key={thread.id}
								type="button"
								className={styles.row}
								onClick={() => void handleRowClick(thread.id)}
								data-flx="channel.threads-popout.row.click"
							>
								<div className={styles.rowInfo} data-flx="channel.threads-popout.row-info">
									<p className={styles.rowName} data-flx="channel.threads-popout.row-name">
										{thread.name}
									</p>
									<p className={styles.rowMeta} data-flx="channel.threads-popout.row-meta">
										{hasMeta ? (
											<>
												<ThreadIcon size={10} className={styles.rowMetaIcon} aria-hidden="true" data-flx="channel.threads-popout.row-meta-icon" />
												{preview?.lastMessageAuthorUsername && (
													<span className={styles.rowSender} data-flx="channel.threads-popout.row-sender">
														{preview.lastMessageAuthorUsername}:
													</span>
												)}
												{preview?.lastMessagePreview && (
													<span className={styles.rowPreviewText} data-flx="channel.threads-popout.row-preview-text">
														{' '}{preview.lastMessagePreview}
													</span>
												)}
												{timeLabel && (
													<span className={styles.rowTime} data-flx="channel.threads-popout.row-time">
														{' · '}{timeLabel}
													</span>
												)}
											</>
										) : (
											<span className={styles.rowNoMessages} data-flx="channel.threads-popout.row-no-messages">
												{i18n._(NO_RECENT_MESSAGES_DESCRIPTOR)}
											</span>
										)}
									</p>
								</div>
								{avatarUrl ? (
									<img
										src={avatarUrl}
										alt=""
										className={styles.rowAvatar}
										data-flx="channel.threads-popout.row-avatar"
									/>
								) : (
									<div className={styles.rowAvatarPlaceholder} data-flx="channel.threads-popout.row-avatar-placeholder" />
								)}
							</button>
						);
					})}
				</div>
			</div>
		</div>
	);
});
