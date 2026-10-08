// SPDX-License-Identifier: AGPL-3.0-or-later

import {SkeletonLine} from '@app/features/app/components/skeleton/SkeletonLine';
import {ChannelHeader} from '@app/features/channel/components/ChannelHeader';
import type {Channel} from '@app/features/channel/models/Channel';
import styles from '@app/features/forum/components/Forum.module.css';
import {ForumPostCard} from '@app/features/forum/components/ForumPostCard';
import {ForumPostComposer} from '@app/features/forum/components/ForumPostComposer';
import {ForumFilterRow, ForumSearchCard} from '@app/features/forum/components/ForumToolbar';
import {getForumPostListView, useForumViewing, usePostDataPrefetch} from '@app/features/forum/hooks/useForumPosts';
import ForumPosts from '@app/features/forum/state/ForumPosts';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import {SafeMarkdown} from '@app/features/messaging/components/markdown';
import {MarkdownContext} from '@app/features/messaging/components/markdown/renderers/RendererTypes';
import {Button} from '@app/features/ui/button/Button';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {ForumLayoutTypes, THREAD_SEARCH_MAX_LIMIT} from '@fluxer/constants/src/ThreadConstants';
import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, CaretUpIcon, ChatsTeardropIcon, InfoIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';

const ForumGuidelines = observer(({forum}: {forum: Channel}) => {
	const {i18n} = useLingui();
	const [expanded, setExpanded] = useState(false);
	if (!forum.topic) return null;
	return (
		<div className={styles.guidelines} data-flx="forum.forum-channel-view.forum-guidelines.guidelines">
			<FocusRing offset={-2} data-flx="forum.forum-channel-view.forum-guidelines.focus-ring">
				<button
					type="button"
					className={styles.guidelinesToggle}
					onClick={() => setExpanded((value) => !value)}
					aria-expanded={expanded}
					aria-label={i18n._(expanded ? D.HIDE_GUIDELINES_DESCRIPTOR : D.SHOW_GUIDELINES_DESCRIPTOR)}
					data-flx="forum.forum-channel-view.forum-guidelines.guidelines-toggle.set-expanded.button"
				>
					<InfoIcon size={18} data-flx="forum.forum-channel-view.forum-guidelines.info-icon" />
					<span
						className={styles.guidelinesTitle}
						data-flx="forum.forum-channel-view.forum-guidelines.guidelines-title"
					>
						{i18n._(D.GUIDELINES_DESCRIPTOR)}
					</span>
					{expanded ? (
						<CaretUpIcon size={16} weight="bold" data-flx="forum.forum-channel-view.forum-guidelines.caret-up-icon" />
					) : (
						<CaretDownIcon
							size={16}
							weight="bold"
							data-flx="forum.forum-channel-view.forum-guidelines.caret-down-icon"
						/>
					)}
				</button>
			</FocusRing>
			{expanded && (
				<div className={styles.guidelinesBody} data-flx="forum.forum-channel-view.forum-guidelines.guidelines-body">
					<SafeMarkdown
						content={forum.topic}
						options={{context: MarkdownContext.STANDARD_WITHOUT_JUMBO, channelId: forum.id}}
						data-flx="forum.forum-channel-view.forum-guidelines.safe-markdown"
					/>
				</div>
			)}
		</div>
	);
});

const ForumEmptyState = ({searching}: {searching: boolean}) => {
	const {i18n} = useLingui();
	return (
		<div className={styles.empty} data-flx="forum.forum-channel-view.forum-empty-state.empty">
			<ChatsTeardropIcon
				size={48}
				className={styles.emptyIcon}
				data-flx="forum.forum-channel-view.forum-empty-state.chats-teardrop-icon"
			/>
			<span className={styles.emptyTitle} data-flx="forum.forum-channel-view.forum-empty-state.empty-title">
				{i18n._(searching ? D.NO_MATCHING_POSTS_DESCRIPTOR : D.NO_POSTS_DESCRIPTOR)}
			</span>
			<span data-flx="forum.forum-channel-view.forum-empty-state.span">
				{i18n._(searching ? D.NO_MATCHING_POSTS_HINT_DESCRIPTOR : D.NO_POSTS_HINT_DESCRIPTOR)}
			</span>
		</div>
	);
};

const SKELETON_CARD_COUNT = 3;

const ForumPostSkeletons = ({grid}: {grid: boolean}) => (
	<div
		className={clsx(styles.postList, grid && styles.postGrid)}
		aria-hidden
		data-flx="forum.forum-channel-view.forum-post-skeletons.post-list"
	>
		{Array.from({length: SKELETON_CARD_COUNT}, (_, index) => (
			<div key={index} className={styles.skeletonCard} data-flx="forum.forum-channel-view.forum-post-skeletons.card">
				<SkeletonLine width="40%" height="1rem" data-flx="forum.forum-channel-view.forum-post-skeletons.title" />
				<SkeletonLine width="80%" height="0.75rem" data-flx="forum.forum-channel-view.forum-post-skeletons.preview" />
				<SkeletonLine width="25%" height="0.75rem" data-flx="forum.forum-channel-view.forum-post-skeletons.meta" />
			</div>
		))}
	</div>
);

const PostCards = observer(({forum, posts, grid}: {forum: Channel; posts: ReadonlyArray<Channel>; grid: boolean}) => (
	<div
		className={clsx(styles.postList, grid && styles.postGrid)}
		data-flx="forum.forum-channel-view.post-cards.post-list"
	>
		{posts.map((post) => (
			<ForumPostCard
				key={post.id}
				forum={forum}
				post={post}
				grid={grid}
				data-flx="forum.forum-channel-view.post-cards.forum-post-card"
			/>
		))}
	</div>
));

const ForumPostList = observer(({forum}: {forum: Channel}) => {
	const {i18n} = useLingui();
	const view = getForumPostListView(forum);
	const grid = ForumPosts.getLayout(forum) === ForumLayoutTypes.GRID;
	const current = view.pinned ? [view.pinned, ...view.posts] : view.posts;
	usePostDataPrefetch(forum, current.length > 0 ? [...current, ...view.archived] : view.archived);
	const kind = view.searching ? 'search' : 'archived';
	const {list} = view;
	const handleLoadMore = useCallback(() => ForumPosts.loadMore(forum, kind), [forum, kind]);
	const handleRetry = useCallback(() => ForumPosts.retry(forum, kind), [forum, kind]);
	const settled = !list.loading && !list.indexing;
	const needsFirstPage = !view.searching && !list.loaded && settled && !list.failed;
	useEffect(() => {
		if (needsFirstPage && current.length < THREAD_SEARCH_MAX_LIMIT) ForumPosts.loadMore(forum, 'archived');
	}, [forum, needsFirstPage, current.length]);
	const empty = current.length === 0 && view.archived.length === 0;
	const searchPending = view.searching && (!list.loaded || list.loading) && !list.indexing && !list.failed;
	if (searchPending && current.length === 0) {
		return <ForumPostSkeletons grid={grid} data-flx="forum.forum-channel-view.forum-post-list.forum-post-skeletons" />;
	}
	return (
		<>
			{current.length > 0 && (
				<PostCards
					forum={forum}
					posts={current}
					grid={grid}
					data-flx="forum.forum-channel-view.forum-post-list.post-cards"
				/>
			)}
			{view.archived.length > 0 && (
				<>
					<div className={styles.sectionTitle} data-flx="forum.forum-channel-view.forum-post-list.section-title">
						{i18n._(D.OLDER_POSTS_DESCRIPTOR)}
					</div>
					<PostCards
						forum={forum}
						posts={view.archived}
						grid={grid}
						data-flx="forum.forum-channel-view.forum-post-list.post-cards--2"
					/>
				</>
			)}
			{empty && settled && list.loaded && (
				<ForumEmptyState
					searching={view.searching}
					data-flx="forum.forum-channel-view.forum-post-list.forum-empty-state"
				/>
			)}
			{list.indexing && (
				<div className={styles.notice} role="status" data-flx="forum.forum-channel-view.forum-post-list.notice">
					<Spinner size="small" data-flx="forum.forum-channel-view.forum-post-list.spinner" />
					<span data-flx="forum.forum-channel-view.forum-post-list.span">{i18n._(D.SEARCH_INDEXING_DESCRIPTOR)}</span>
				</div>
			)}
			{list.loading && (
				<div className={styles.notice} data-flx="forum.forum-channel-view.forum-post-list.notice--2">
					<Spinner size="small" data-flx="forum.forum-channel-view.forum-post-list.spinner--2" />
				</div>
			)}
			{list.failed && (
				<div className={styles.notice} role="alert" data-flx="forum.forum-channel-view.forum-post-list.notice--3">
					<span data-flx="forum.forum-channel-view.forum-post-list.span--2">{i18n._(D.SEARCH_FAILED_DESCRIPTOR)}</span>
					<Button
						small
						variant="secondary"
						onClick={handleRetry}
						data-flx="forum.forum-channel-view.forum-post-list.button.retry"
					>
						{i18n._(D.RETRY_DESCRIPTOR)}
					</Button>
				</div>
			)}
			{settled &&
				!list.failed &&
				(list.loaded ? list.hasMore : needsFirstPage && current.length >= THREAD_SEARCH_MAX_LIMIT) && (
					<div className={styles.notice} data-flx="forum.forum-channel-view.forum-post-list.notice--4">
						<Button
							small
							variant="secondary"
							onClick={handleLoadMore}
							data-flx="forum.forum-channel-view.forum-post-list.button.load-more"
						>
							{i18n._(D.LOAD_OLDER_POSTS_DESCRIPTOR)}
						</Button>
					</div>
				)}
		</>
	);
});

export const ForumChannelView = observer(({forum}: {forum: Channel}) => {
	const [composerTitle, setComposerTitle] = useState<string | null>(null);
	useForumViewing(forum);
	const handleNewPost = useCallback((title: string) => setComposerTitle(title.trim()), []);
	const handleCloseComposer = useCallback(() => setComposerTitle(null), []);
	return (
		<div className={styles.view} data-flx="forum.forum-channel-view.view">
			<ChannelHeader
				channel={forum}
				showMembersToggle={false}
				showPins={false}
				data-flx="forum.forum-channel-view.channel-header"
			/>
			<div className={styles.scroller} data-flx="forum.forum-channel-view.scroller">
				<div className={styles.content} data-flx="forum.forum-channel-view.content">
					<ForumGuidelines forum={forum} data-flx="forum.forum-channel-view.forum-guidelines" />
					{composerTitle == null ? (
						<>
							<ForumSearchCard
								forum={forum}
								onNewPost={handleNewPost}
								data-flx="forum.forum-channel-view.forum-search-card"
							/>
							<ForumFilterRow forum={forum} data-flx="forum.forum-channel-view.forum-filter-row" />
						</>
					) : (
						<ForumPostComposer
							key={forum.id}
							forum={forum}
							initialTitle={composerTitle}
							onClose={handleCloseComposer}
							data-flx="forum.forum-channel-view.forum-post-composer"
						/>
					)}
					<ForumPostList forum={forum} data-flx="forum.forum-channel-view.forum-post-list" />
				</div>
			</div>
		</div>
	);
});
