// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import styles from '@app/features/forum/components/Forum.module.css';
import {ForumEmoji, ForumTagPill} from '@app/features/forum/components/ForumTagPill';
import {getForumPostListView} from '@app/features/forum/hooks/useForumPosts';
import ForumPosts from '@app/features/forum/state/ForumPosts';
import {isMediaChannel} from '@app/features/forum/utils/ForumChannelUtils';
import * as D from '@app/features/forum/utils/ForumMessageDescriptors';
import {canCreatePost} from '@app/features/forum/utils/ForumPermissions';
import {CLEAR_SEARCH_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Button} from '@app/features/ui/button/Button';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {KeybindHint} from '@app/features/ui/keybind_hint/KeybindHint';
import {MenuBottomSheet, type MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {
	ForumLayoutTypes,
	ForumSortOrderTypes,
	THREAD_SEARCH_NAME_MAX_LENGTH,
} from '@fluxer/constants/src/ThreadConstants';
import type {I18n} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, MagnifyingGlassIcon, PlusIcon, SortAscendingIcon, TagIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useState} from 'react';

const ForumSortViewPanel = observer(({forum, sheet = false}: {forum: Channel; sheet?: boolean}) => {
	const {i18n} = useLingui();
	return (
		<div className={clsx(styles.sortPanel, sheet && styles.sortPanelSheet)} data-flx="forum.forum-toolbar.sort-panel">
			<section className={styles.sortSection} data-flx="forum.forum-toolbar.sort-panel.sort">
				<h3 className={styles.sortHeading} data-flx="forum.forum-toolbar.sort-panel.sort-heading">
					{i18n._(D.SORT_BY_DESCRIPTOR)}
				</h3>
				<RadioGroup
					aria-label={i18n._(D.SORT_BY_DESCRIPTOR)}
					optionAlign="center"
					value={ForumPosts.getSortOrder(forum)}
					onChange={(value) => ForumPosts.setSortOrder(forum, value)}
					options={[
						{value: ForumSortOrderTypes.LATEST_ACTIVITY, name: i18n._(D.SORT_RECENT_ACTIVITY_DESCRIPTOR)},
						{value: ForumSortOrderTypes.CREATION_TIME, name: i18n._(D.SORT_CREATION_DATE_DESCRIPTOR)},
					]}
					data-flx="forum.forum-toolbar.sort-panel.sort-radio-group"
				/>
			</section>
			{!isMediaChannel(forum) && (
				<section className={styles.sortSection} data-flx="forum.forum-toolbar.sort-panel.view">
					<h3 className={styles.sortHeading} data-flx="forum.forum-toolbar.sort-panel.view-heading">
						{i18n._(D.VIEW_AS_DESCRIPTOR)}
					</h3>
					<RadioGroup
						aria-label={i18n._(D.VIEW_AS_DESCRIPTOR)}
						optionAlign="center"
						value={ForumPosts.getLayout(forum)}
						onChange={(value) => ForumPosts.setLayout(forum, value)}
						options={[
							{value: ForumLayoutTypes.LIST, name: i18n._(D.LAYOUT_LIST_DESCRIPTOR)},
							{value: ForumLayoutTypes.GRID, name: i18n._(D.LAYOUT_GALLERY_DESCRIPTOR)},
						]}
						data-flx="forum.forum-toolbar.sort-panel.view-radio-group"
					/>
				</section>
			)}
		</div>
	);
});

function buildTagFilterGroups(i18n: I18n, forum: Channel): Array<MenuGroupType> {
	const selected = ForumPosts.getTagFilter(forum.id);
	const groups: Array<MenuGroupType> = [
		{
			items: forum.availableTags.map((tag) => ({
				label: tag.name,
				icon:
					tag.emoji_id || tag.emoji_name ? (
						<ForumEmoji emoji={tag} data-flx="forum.forum-toolbar.build-tag-filter-groups.forum-emoji" />
					) : undefined,
				checked: selected.includes(tag.id),
				onChange: () => ForumPosts.toggleTag(forum, tag.id),
			})),
		},
	];
	if (selected.length > 0) {
		groups.push({items: [{label: i18n._(D.CLEAR_TAGS_DESCRIPTOR), onClick: () => ForumPosts.setTagFilter(forum, [])}]});
	}
	return groups;
}

const TagFilterRow = observer(({forum}: {forum: Channel}) => {
	const {i18n} = useLingui();
	const selected = ForumPosts.getTagFilter(forum.id);
	return (
		<div
			className={styles.tagRow}
			role="group"
			aria-label={i18n._(D.TAGS_DESCRIPTOR)}
			data-flx="forum.forum-toolbar.tag-filter-row.tag-row"
		>
			{forum.availableTags.map((tag) => (
				<ForumTagPill
					key={tag.id}
					tag={tag}
					selected={selected.includes(tag.id)}
					onClick={() => ForumPosts.toggleTag(forum, tag.id)}
					data-flx="forum.forum-toolbar.tag-filter-row.forum-tag-pill.toggle-tag"
				/>
			))}
			{selected.length > 0 && (
				<FocusRing offset={-2} data-flx="forum.forum-toolbar.tag-filter-row.clear.focus-ring">
					<button
						type="button"
						className={styles.tagClear}
						onClick={() => ForumPosts.setTagFilter(forum, [])}
						data-flx="forum.forum-toolbar.tag-filter-row.button.set-tag-filter"
					>
						<XIcon size={remFromPx(12)} weight="bold" data-flx="forum.forum-toolbar.tag-filter-row.clear-icon" />
						{i18n._(D.CLEAR_TAGS_DESCRIPTOR)}
					</button>
				</FocusRing>
			)}
		</div>
	);
});

const SEARCH_STATUS_ID_SUFFIX = 'forum-search-status';
const CREATE_POST_COMBO = {key: 'Enter', shift: true};

const SearchStatus = observer(({forum}: {forum: Channel}) => {
	const {i18n} = useLingui();
	const view = getForumPostListView(forum);
	const pending = !view.list.loaded || view.list.loading || view.list.indexing;
	if (pending) return <>{i18n._(D.SEARCHING_POSTS_DESCRIPTOR)}</>;
	if (view.posts.length === 0) return <>{i18n._(D.NO_MATCHING_POSTS_SHORT_DESCRIPTOR)}</>;
	return <>{i18n._(D.SEARCH_RESULT_COUNT_DESCRIPTOR, {count: view.posts.length})}</>;
});

export const ForumSearchCard = observer(({forum, onNewPost}: {forum: Channel; onNewPost: (title: string) => void}) => {
	const {i18n} = useLingui();
	const query = ForumPosts.getQuery(forum.id);
	const searching = query.trim().length > 0;
	const canPost = canCreatePost(forum);
	const statusId = `${forum.id}-${SEARCH_STATUS_ID_SUFFIX}`;
	const handleKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
		if (event.key !== 'Enter' || !event.shiftKey || !canPost) return;
		event.preventDefault();
		onNewPost(query);
	};
	return (
		<div className={styles.entryCard} data-flx="forum.forum-toolbar.entry-card">
			<div className={styles.entryRow} data-flx="forum.forum-toolbar.entry-row">
				{ForumPosts.searchUnavailable ? (
					<span className={styles.entrySpacer} data-flx="forum.forum-toolbar.entry-spacer" />
				) : (
					<>
						<MagnifyingGlassIcon
							size={remFromPx(18)}
							weight="bold"
							className={styles.entryIcon}
							aria-hidden
							data-flx="forum.forum-toolbar.magnifying-glass-icon"
						/>
						<input
							type="text"
							className={styles.entryInput}
							value={query}
							maxLength={THREAD_SEARCH_NAME_MAX_LENGTH}
							onChange={(event) => ForumPosts.setQuery(forum, event.target.value)}
							onKeyDown={handleKeyDown}
							placeholder={i18n._(canPost ? D.SEARCH_FOR_POSTS_DESCRIPTOR : D.SEARCH_POSTS_DESCRIPTOR)}
							aria-label={i18n._(D.SEARCH_POSTS_DESCRIPTOR)}
							aria-describedby={searching ? statusId : undefined}
							autoComplete="off"
							data-flx="forum.forum-toolbar.search-input.set-query"
						/>
					</>
				)}
				{searching && (
					<FocusRing offset={-2} data-flx="forum.forum-toolbar.focus-ring">
						<button
							type="button"
							className={styles.clearButton}
							onClick={() => ForumPosts.setQuery(forum, '')}
							aria-label={i18n._(CLEAR_SEARCH_DESCRIPTOR)}
							data-flx="forum.forum-toolbar.icon-button.set-query"
						>
							<XIcon size={remFromPx(14)} weight="bold" data-flx="forum.forum-toolbar.x-icon" />
						</button>
					</FocusRing>
				)}
				{canPost && (
					<Button
						small
						fitContent
						leftIcon={<PlusIcon size={remFromPx(16)} weight="bold" data-flx="forum.forum-toolbar.plus-icon" />}
						onClick={() => onNewPost(query)}
						data-flx="forum.forum-toolbar.button.new-post"
					>
						{i18n._(D.NEW_POST_DESCRIPTOR)}
					</Button>
				)}
			</div>
			{searching && (
				<div className={styles.entryFooter} data-flx="forum.forum-toolbar.entry-footer">
					<span className={styles.entryStatus} id={statusId} role="status" data-flx="forum.forum-toolbar.entry-status">
						<SearchStatus forum={forum} data-flx="forum.forum-toolbar.search-status" />
						<FocusRing offset={-2} data-flx="forum.forum-toolbar.entry-clear.focus-ring">
							<button
								type="button"
								className={styles.entryLink}
								onClick={() => ForumPosts.setQuery(forum, '')}
								data-flx="forum.forum-toolbar.entry-clear.set-query"
							>
								{i18n._(D.CLEAR_TAGS_DESCRIPTOR)}
							</button>
						</FocusRing>
					</span>
					{canPost && !MobileLayout.enabled && (
						<span className={styles.entryHint} data-flx="forum.forum-toolbar.entry-hint">
							<KeybindHint combo={CREATE_POST_COMBO} data-flx="forum.forum-toolbar.keybind-hint" />
							{i18n._(D.CREATE_POST_SHORTCUT_HINT_DESCRIPTOR)}
						</span>
					)}
				</div>
			)}
		</div>
	);
});

export const ForumFilterRow = observer(({forum}: {forum: Channel}) => {
	const {i18n} = useLingui();
	const [sheet, setSheet] = useState<'sort' | 'tags' | null>(null);
	const mobile = MobileLayout.enabled;
	const tagCount = ForumPosts.getTagFilter(forum.id).length;
	return (
		<div className={styles.filterRow} data-flx="forum.forum-toolbar.filter-row">
			{mobile ? (
				<Button
					variant="secondary"
					small
					fitContent
					leftIcon={
						<SortAscendingIcon size={remFromPx(16)} weight="bold" data-flx="forum.forum-toolbar.sort-ascending-icon" />
					}
					rightIcon={
						<CaretDownIcon size={remFromPx(12)} weight="bold" data-flx="forum.forum-toolbar.caret-down-icon" />
					}
					className={styles.filterPill}
					onClick={() => setSheet('sort')}
					aria-haspopup="menu"
					data-flx="forum.forum-toolbar.button.sort-menu"
				>
					{i18n._(D.SORT_AND_VIEW_DESCRIPTOR)}
				</Button>
			) : (
				<Popout
					position="bottom-start"
					render={() => <ForumSortViewPanel forum={forum} data-flx="forum.forum-toolbar.forum-sort-view-panel" />}
					data-flx="forum.forum-toolbar.sort-popout"
				>
					<Button
						variant="secondary"
						small
						fitContent
						leftIcon={
							<SortAscendingIcon
								size={remFromPx(16)}
								weight="bold"
								data-flx="forum.forum-toolbar.sort-ascending-icon"
							/>
						}
						rightIcon={
							<CaretDownIcon size={remFromPx(12)} weight="bold" data-flx="forum.forum-toolbar.caret-down-icon" />
						}
						className={styles.filterPill}
						aria-haspopup="menu"
						data-flx="forum.forum-toolbar.button.sort-menu"
					>
						{i18n._(D.SORT_AND_VIEW_DESCRIPTOR)}
					</Button>
				</Popout>
			)}
			{forum.availableTags.length > 0 &&
				(mobile ? (
					<Button
						variant="secondary"
						small
						fitContent
						leftIcon={<TagIcon size={remFromPx(16)} weight="bold" data-flx="forum.forum-toolbar.tag-icon" />}
						rightIcon={
							<CaretDownIcon size={remFromPx(12)} weight="bold" data-flx="forum.forum-toolbar.caret-down-icon--2" />
						}
						className={styles.filterPill}
						onClick={() => setSheet('tags')}
						data-flx="forum.forum-toolbar.button.set-sheet"
					>
						{tagCount > 0 ? `${i18n._(D.TAGS_DESCRIPTOR)} (${tagCount})` : i18n._(D.TAGS_DESCRIPTOR)}
					</Button>
				) : (
					<>
						<span className={styles.filterDivider} aria-hidden data-flx="forum.forum-toolbar.filter-divider" />
						<TagFilterRow forum={forum} data-flx="forum.forum-toolbar.tag-filter-row" />
					</>
				))}
			<BottomSheet
				isOpen={sheet === 'sort'}
				onClose={() => setSheet(null)}
				title={i18n._(D.SORT_AND_VIEW_DESCRIPTOR)}
				snapPoints={[0, 1]}
				initialSnap={1}
				data-flx="forum.forum-toolbar.sort-bottom-sheet"
			>
				<ForumSortViewPanel forum={forum} sheet data-flx="forum.forum-toolbar.forum-sort-view-panel--sheet" />
			</BottomSheet>
			{sheet === 'tags' && (
				<MenuBottomSheet
					isOpen={true}
					onClose={() => setSheet(null)}
					title={i18n._(D.TAGS_DESCRIPTOR)}
					groups={buildTagFilterGroups(i18n, forum)}
					data-flx="forum.forum-toolbar.menu-bottom-sheet"
				/>
			)}
		</div>
	);
});
