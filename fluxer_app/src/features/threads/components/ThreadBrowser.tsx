// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import {CLEAR_SEARCH_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as ThreadCommands from '@app/features/threads/commands/ThreadCommands';
import {openCreateThread, openThread} from '@app/features/threads/commands/ThreadNavigation';
import styles from '@app/features/threads/components/ThreadBrowser.module.css';
import {ThreadContextMenu} from '@app/features/threads/components/ThreadContextMenu';
import {reportThreadActionError} from '@app/features/threads/hooks/useThreadMenuData';
import {useThreadSearch} from '@app/features/threads/hooks/useThreadSearch';
import ChannelThreads, {lastActivityId} from '@app/features/threads/state/ChannelThreads';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';
import {canCreateThreadIn, isModeratorOfParent} from '@app/features/threads/utils/ThreadActionRules';
import * as D from '@app/features/threads/utils/ThreadMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {type SegmentedTab, SegmentedTabs} from '@app/features/ui/segmented_tabs/SegmentedTabs';
import sheetStyles from '@app/features/ui/sheet/Sheet.module.css';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {THREAD_SEARCH_NAME_MAX_LENGTH} from '@fluxer/constants/src/ThreadConstants';
import * as SnowflakeUtils from '@fluxer/snowflake/src/SnowflakeUtils';
import {useLingui} from '@lingui/react/macro';
import {ChatsIcon, LockSimpleIcon, MagnifyingGlassIcon, PlusIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';

type BrowserTab = 'active' | 'archived' | 'archivedPrivate';

function byLastActivity(a: Channel, b: Channel): number {
	return SnowflakeUtils.compare(lastActivityId(b), lastActivityId(a));
}

const ThreadBrowserItem = observer(({thread, onOpen}: {thread: Channel; onOpen: (thread: Channel) => void}) => {
	const {i18n} = useLingui();
	const lastActivity = new Date(SnowflakeUtils.extractTimestamp(lastActivityId(thread)));
	return (
		<FocusRing offset={-2} data-flx="threads.thread-browser.thread-browser-item.focus-ring">
			<button
				type="button"
				className={styles.item}
				onClick={() => onOpen(thread)}
				onContextMenu={(event) =>
					ContextMenuCommands.openFromEvent(event, ({onClose}) => (
						<ThreadContextMenu
							thread={thread}
							onClose={onClose}
							data-flx="threads.thread-browser.thread-browser-item.thread-context-menu"
						/>
					))
				}
				data-flx="threads.thread-browser.thread-browser-item.item.open"
			>
				<span className={styles.itemName} data-flx="threads.thread-browser.thread-browser-item.item-name">
					{thread.name}
				</span>
				<span className={styles.itemMeta} data-flx="threads.thread-browser.thread-browser-item.item-meta">
					{thread.isPrivateThread() && (
						<LockSimpleIcon size={14} data-flx="threads.thread-browser.thread-browser-item.private-icon" />
					)}
					<span data-flx="threads.thread-browser.thread-browser-item.message-count">
						{i18n._(D.MESSAGE_COUNT_DESCRIPTOR, {count: thread.messageCount})}
					</span>
					<span data-flx="threads.thread-browser.thread-browser-item.last-activity">
						{DateUtils.getRelativeDateString(lastActivity, i18n)}
					</span>
				</span>
			</button>
		</FocusRing>
	);
});

const EmptyState = ({searching = false}: {searching?: boolean}) => {
	const {i18n} = useLingui();
	return (
		<div className={styles.empty} data-flx="threads.thread-browser.empty-state.empty">
			<ChatsIcon size={40} className={styles.emptyIcon} data-flx="threads.thread-browser.empty-state.icon" />
			<span className={styles.emptyTitle} data-flx="threads.thread-browser.empty-state.title">
				{i18n._(searching ? D.NO_MATCHING_THREADS_DESCRIPTOR : D.NO_THREADS_DESCRIPTOR)}
			</span>
			<span data-flx="threads.thread-browser.empty-state.hint">
				{i18n._(searching ? D.NO_MATCHING_THREADS_HINT_DESCRIPTOR : D.NO_THREADS_HINT_DESCRIPTOR)}
			</span>
		</div>
	);
};

function matchesTab(thread: Channel, tab: BrowserTab): boolean {
	if (tab === 'active') return !thread.isArchived;
	if (!thread.isArchived) return false;
	return tab === 'archivedPrivate' ? thread.isPrivateThread() : !thread.isPrivateThread();
}

const SearchResults = observer(
	({
		parent,
		query,
		tab,
		onOpen,
	}: {
		parent: Channel;
		query: string;
		tab: BrowserTab;
		onOpen: (thread: Channel) => void;
	}) => {
		const {i18n} = useLingui();
		const {state, retry} = useThreadSearch(parent.id, query, tab !== 'active');
		const threads =
			state.status === 'done'
				? state.threadIds
						.map((id) => ChannelThreads.getThread(id))
						.filter((thread): thread is Channel => thread?.parentId === parent.id && matchesTab(thread, tab))
				: [];
		return (
			<div className={styles.list} role="status" data-flx="threads.thread-browser.search-results.list">
				{(state.status === 'loading' || state.status === 'idle') && (
					<div className={styles.loadMore} data-flx="threads.thread-browser.search-results.loading">
						<Spinner data-flx="threads.thread-browser.search-results.spinner" />
					</div>
				)}
				{state.status === 'indexing' && (
					<div className={styles.notice} data-flx="threads.thread-browser.search-results.indexing">
						<Spinner size="small" data-flx="threads.thread-browser.search-results.spinner--2" />
						<span data-flx="threads.thread-browser.search-results.indexing-text">
							{i18n._(D.THREAD_SEARCH_INDEXING_DESCRIPTOR)}
						</span>
					</div>
				)}
				{state.status === 'failed' && (
					<div className={styles.notice} role="alert" data-flx="threads.thread-browser.search-results.failed">
						<span data-flx="threads.thread-browser.search-results.failed-text">
							{i18n._(D.THREAD_SEARCH_FAILED_DESCRIPTOR)}
						</span>
						<Button
							small
							fitContent
							variant="secondary"
							onClick={retry}
							data-flx="threads.thread-browser.search-results.button.retry"
						>
							{i18n._(D.RETRY_SEARCH_DESCRIPTOR)}
						</Button>
					</div>
				)}
				{state.status === 'done' && threads.length === 0 && (
					<EmptyState searching data-flx="threads.thread-browser.search-results.empty-state" />
				)}
				{threads.map((thread) => (
					<ThreadBrowserItem
						key={thread.id}
						thread={thread}
						onOpen={onOpen}
						data-flx="threads.thread-browser.search-results.thread-browser-item"
					/>
				))}
			</div>
		);
	},
);

const ThreadSearchInput = ({value, onChange}: {value: string; onChange: (value: string) => void}) => {
	const {i18n} = useLingui();
	return (
		<div className={styles.search} role="search" data-flx="threads.thread-browser.search">
			<Input
				type="text"
				value={value}
				onChange={(event) => onChange(event.target.value)}
				placeholder={i18n._(D.SEARCH_THREADS_DESCRIPTOR)}
				aria-label={i18n._(D.SEARCH_THREADS_DESCRIPTOR)}
				maxLength={THREAD_SEARCH_NAME_MAX_LENGTH}
				autoComplete="off"
				className={styles.searchInput}
				leftIcon={
					<MagnifyingGlassIcon
						size={remFromPx(16)}
						weight="bold"
						data-flx="threads.thread-browser.search.magnifying-glass-icon"
					/>
				}
				rightElement={
					value ? (
						<FocusRing offset={-2} data-flx="threads.thread-browser.search.clear.focus-ring">
							<button
								type="button"
								className={styles.searchClear}
								onClick={() => onChange('')}
								aria-label={i18n._(CLEAR_SEARCH_DESCRIPTOR)}
								data-flx="threads.thread-browser.search.clear"
							>
								<XIcon size={remFromPx(14)} weight="bold" data-flx="threads.thread-browser.search.x-icon" />
							</button>
						</FocusRing>
					) : undefined
				}
				data-flx="threads.thread-browser.search.input"
			/>
		</div>
	);
};

const ActiveThreads = observer(({parent, onOpen}: {parent: Channel; onOpen: (thread: Channel) => void}) => {
	const {i18n} = useLingui();
	const threads = [...ChannelThreads.getActiveThreadsForParent(parent.id)].sort(byLastActivity);
	if (threads.length === 0) return <EmptyState data-flx="threads.thread-browser.active-threads.empty-state" />;
	const joined = threads.filter((thread) => ThreadMemberships.isMember(thread.id));
	const others = threads.filter((thread) => !ThreadMemberships.isMember(thread.id));
	return (
		<>
			{joined.length > 0 && (
				<div className={styles.sectionTitle} data-flx="threads.thread-browser.active-threads.section-title.joined">
					{i18n._(D.JOINED_THREADS_DESCRIPTOR)}
				</div>
			)}
			{joined.map((thread) => (
				<ThreadBrowserItem
					key={thread.id}
					thread={thread}
					onOpen={onOpen}
					data-flx="threads.thread-browser.active-threads.thread-browser-item.joined"
				/>
			))}
			{others.length > 0 && (
				<div className={styles.sectionTitle} data-flx="threads.thread-browser.active-threads.section-title.others">
					{i18n._(D.OTHER_THREADS_DESCRIPTOR)}
				</div>
			)}
			{others.map((thread) => (
				<ThreadBrowserItem
					key={thread.id}
					thread={thread}
					onOpen={onOpen}
					data-flx="threads.thread-browser.active-threads.thread-browser-item.other"
				/>
			))}
		</>
	);
});

const ArchivedThreads = observer(
	({parent, privateOnly, onOpen}: {parent: Channel; privateOnly: boolean; onOpen: (thread: Channel) => void}) => {
		const {i18n} = useLingui();
		const moderator = isModeratorOfParent(parent);
		const [threadIds, setThreadIds] = useState<ReadonlyArray<string>>([]);
		const [hasMore, setHasMore] = useState(false);
		const [loading, setLoading] = useState(false);
		const kind: ThreadCommands.ArchivedThreadListKind = !privateOnly ? 'public' : moderator ? 'private' : 'joined';
		const parentId = parent.id;
		const load = useCallback(
			(before?: string) => {
				const current = Channels.getChannel(parentId);
				if (!current) return;
				setLoading(true);
				void ThreadCommands.fetchArchivedThreads(current, kind, before)
					.then((page) => {
						setThreadIds((previous) => (before ? [...previous, ...page.threadIds] : page.threadIds));
						setHasMore(page.hasMore);
					})
					.catch((error) => reportThreadActionError(i18n, error))
					.finally(() => setLoading(false));
			},
			[i18n, kind, parentId],
		);
		useEffect(() => {
			setThreadIds([]);
			load();
		}, [load]);
		const threads = threadIds
			.map((id) => ChannelThreads.getThread(id))
			.filter((thread): thread is Channel => thread?.isArchived === true);
		const lastThread = threads[threads.length - 1];
		const nextCursor =
			kind === 'joined' ? lastThread?.id : (lastThread?.threadMetadata?.archive_timestamp ?? undefined);
		return (
			<div className={styles.list} data-flx="threads.thread-browser.archived-threads.list">
				{threads.length === 0 && !loading && (
					<EmptyState data-flx="threads.thread-browser.archived-threads.empty-state" />
				)}
				{threads.map((thread) => (
					<ThreadBrowserItem
						key={thread.id}
						thread={thread}
						onOpen={onOpen}
						data-flx="threads.thread-browser.archived-threads.thread-browser-item"
					/>
				))}
				{loading && (
					<div className={styles.loadMore} data-flx="threads.thread-browser.archived-threads.loading">
						<Spinner data-flx="threads.thread-browser.archived-threads.spinner" />
					</div>
				)}
				{!loading && hasMore && nextCursor && (
					<div className={styles.loadMore} data-flx="threads.thread-browser.archived-threads.load-more">
						<Button
							small
							fitContent
							variant="secondary"
							onClick={() => load(nextCursor)}
							data-flx="threads.thread-browser.archived-threads.button.load-more"
						>
							{i18n._(D.LOAD_MORE_DESCRIPTOR)}
						</Button>
					</div>
				)}
			</div>
		);
	},
);

export const ThreadBrowserCreateButton = observer(
	({parent, onClose, sheet = false}: {parent: Channel; onClose?: () => void; sheet?: boolean}) => {
		const {i18n} = useLingui();
		const current = Channels.getChannel(parent.id) ?? parent;
		if (!canCreateThreadIn(current, 'public') && !canCreateThreadIn(current, 'private')) return null;
		const handleClick = () => {
			onClose?.();
			openCreateThread(current, null);
		};
		if (sheet) {
			return (
				<FocusRing offset={-2} data-flx="threads.thread-browser-create-button.focus-ring">
					<button
						type="button"
						className={sheetStyles.closeButton}
						onClick={handleClick}
						aria-label={i18n._(D.CREATE_THREAD_DESCRIPTOR)}
						data-flx="threads.thread-browser.button.create--sheet"
					>
						<PlusIcon weight="bold" data-flx="threads.thread-browser-create-button.plus-icon" />
					</button>
				</FocusRing>
			);
		}
		return (
			<Button small fitContent onClick={handleClick} data-flx="threads.thread-browser.button.create">
				{i18n._(D.CREATE_THREAD_DESCRIPTOR)}
			</Button>
		);
	},
);

export const ThreadBrowser = observer(
	({parent, onClose, sheet = false}: {parent: Channel; onClose?: () => void; sheet?: boolean}) => {
		const {i18n} = useLingui();
		const [tab, setTab] = useState<BrowserTab>('active');
		const [query, setQuery] = useState('');
		const searching = query.trim().length > 0;
		const current = Channels.getChannel(parent.id) ?? parent;
		const handleOpen = useCallback(
			(thread: Channel) => {
				onClose?.();
				openThread(thread);
			},
			[onClose],
		);
		const tabs: Array<SegmentedTab<BrowserTab>> = [
			{id: 'active', label: i18n._(D.ACTIVE_THREADS_DESCRIPTOR)},
			{id: 'archived', label: i18n._(D.ARCHIVED_THREADS_DESCRIPTOR)},
		];
		if (current.type === ChannelTypes.GUILD_TEXT) {
			tabs.push({id: 'archivedPrivate', label: i18n._(D.ARCHIVED_PRIVATE_THREADS_DESCRIPTOR)});
		}
		return (
			<div
				className={clsx(styles.container, sheet && styles.containerSheet)}
				data-flx="threads.thread-browser.container"
			>
				{!sheet && (
					<div className={styles.header} data-flx="threads.thread-browser.header">
						<ChatsIcon className={styles.headerIcon} data-flx="threads.thread-browser.header-icon" />
						<span className={styles.title} data-flx="threads.thread-browser.title">
							{i18n._(D.THREADS_DESCRIPTOR)}
						</span>
						<ThreadBrowserCreateButton
							parent={current}
							onClose={onClose}
							data-flx="threads.thread-browser.thread-browser-create-button"
						/>
					</div>
				)}
				<ThreadSearchInput value={query} onChange={setQuery} data-flx="threads.thread-browser.thread-search-input" />
				<SegmentedTabs<BrowserTab>
					tabs={tabs}
					selectedTab={tab}
					onTabChange={setTab}
					ariaLabel={i18n._(D.THREADS_DESCRIPTOR)}
					className={styles.tabs}
					data-flx="threads.thread-browser.segmented-tabs"
				/>
				{searching ? (
					<SearchResults
						parent={current}
						query={query}
						tab={tab}
						onOpen={handleOpen}
						data-flx="threads.thread-browser.search-results"
					/>
				) : tab === 'active' ? (
					<div className={styles.list} data-flx="threads.thread-browser.list.active">
						<ActiveThreads parent={current} onOpen={handleOpen} data-flx="threads.thread-browser.active-threads" />
					</div>
				) : (
					<ArchivedThreads
						key={tab}
						parent={current}
						privateOnly={tab === 'archivedPrivate'}
						onOpen={handleOpen}
						data-flx="threads.thread-browser.archived-threads"
					/>
				)}
			</div>
		);
	},
);
