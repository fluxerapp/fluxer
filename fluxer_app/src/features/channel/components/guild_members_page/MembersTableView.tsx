// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/GuildMembersPage.module.css';
import {renderGuildMemberContextMenu} from '@app/features/channel/components/guild_members_page/GuildMembersPageContextMenu';
import {
	ALL_DESCRIPTOR,
	VANITY_URL_DESCRIPTOR,
} from '@app/features/channel/components/guild_members_page/GuildMembersPageDescriptors';
import {
	DAY_MS,
	type DateRangeFilter,
	DEFAULT_PAGE_SIZE,
	getInviteCodes,
	getMemberContextUser,
	HOUR_MS,
	INDEXING_POLL_INTERVAL_MS,
	isDateRangeFilterActive,
	isJoinMethodFilterActive,
	isPresetMatch,
	type JoinMethodFilter,
	type MemberDisplayData,
	SEARCH_DEBOUNCE_MS,
	type SortMode,
} from '@app/features/channel/components/guild_members_page/GuildMembersPageShared';
import {MembersTableBody} from '@app/features/channel/components/guild_members_page/MembersTableBody';
import {MembersTableFooter} from '@app/features/channel/components/guild_members_page/MembersTableFooter';
import {MembersTableHeaderRow} from '@app/features/channel/components/guild_members_page/MembersTableHeaderRow';
import {MembersTableProgressSlot} from '@app/features/channel/components/guild_members_page/MembersTableProgressSlot';
import {MembersTableToolbar} from '@app/features/channel/components/guild_members_page/MembersTableToolbar';

import {RolesFilterMenuContent} from '@app/features/channel/components/guild_members_page/RolesFilterMenuContent';
import {
	buildMemberSearchParams,
	getDisplayedMembers,
	getMembersTableState,
	logger,
	searchGuildMembers,
	toMemberDisplayData,
} from '@app/features/channel/components/guild_members_page/SearchApi';
import {GuildMembersDateRangeModal} from '@app/features/guild/components/modals/guild_tabs/GuildMembersDateRangeModal';
import Guilds from '@app/features/guild/state/Guilds';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {ContextMenuCloseProvider} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import debounce from 'lodash/debounce';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const NEWEST_FIRST_DESCRIPTOR = msg({
	message: 'Newest first',
	comment: 'Sort option label on the community members page. Sorts joined-at descending.',
});
const OLDEST_FIRST_DESCRIPTOR = msg({
	message: 'Oldest first',
	comment: 'Sort option label on the community members page. Sorts joined-at ascending.',
});
const PAST_1_HOUR_DESCRIPTOR = msg({
	message: 'Past 1 hour',
	comment: 'Date-range filter option on the community members page. Last 60 minutes.',
});
const PAST_24_HOURS_DESCRIPTOR = msg({
	message: 'Past 24 hours',
	comment: 'Date-range filter option on the community members page.',
});
const PAST_7_DAYS_DESCRIPTOR = msg({
	message: 'Past 7 days',
	comment: 'Date-range filter option on the community members page.',
});
const PAST_2_WEEKS_DESCRIPTOR = msg({
	message: 'Past 2 weeks',
	comment: 'Date-range filter option on the community members page.',
});
const PAST_3_WEEKS_DESCRIPTOR = msg({
	message: 'Past 3 weeks',
	comment: 'Date-range filter option on the community members page.',
});
const PAST_4_WEEKS_DESCRIPTOR = msg({
	message: 'Past 4 weeks',
	comment: 'Date-range filter option on the community members page.',
});
const PAST_3_MONTHS_DESCRIPTOR = msg({
	message: 'Past 3 months',
	comment: 'Date-range filter option on the community members page.',
});
const CUSTOM_RANGE_DESCRIPTOR = msg({
	message: 'Custom range...',
	comment:
		'Date-range filter option on the community members page. Opens a date picker modal. Keep the trailing ellipsis.',
});
export const MembersTableView: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const [searchQuery, setSearchQuery] = useState('');
	const [sortMode, setSortMode] = useState<SortMode>('newest');
	const [pageSize, setPageSize] = useState(DEFAULT_PAGE_SIZE);
	const [memberSinceFilter, setMemberSinceFilter] = useState<DateRangeFilter>({});
	const [joinedFluxerFilter, setJoinedFluxerFilter] = useState<DateRangeFilter>({});
	const [joinMethodFilter, setJoinMethodFilter] = useState<JoinMethodFilter>({});
	const [roleFilter, setRoleFilter] = useState<Array<string>>([]);
	const [activeMenuMemberId, setActiveMenuMemberId] = useState<string | null>(null);
	const [contextMenuMemberId, setContextMenuMemberId] = useState<string | null>(null);
	const [searchMembers, setSearchMembers] = useState<Array<MemberDisplayData>>([]);
	const [totalCount, setTotalCount] = useState(0);
	const [currentPage, setCurrentPage] = useState(1);
	const [isSearching, setIsSearching] = useState(false);
	const [indexing, setIndexing] = useState(false);
	const [initialLoadDone, setInitialLoadDone] = useState(false);
	const [searchError, setSearchError] = useState(false);

	const isLoadingRef = useRef(false);
	const abortControllerRef = useRef<AbortController | null>(null);
	const filterKeyRef = useRef('');

	const tableScrollerRef = useRef<ScrollerHandle | null>(null);
	const roles = Guilds.getGuildRoles(guildId);
	const filterKey = useMemo(
		() =>
			JSON.stringify({
				query: searchQuery.trim(),
				sortMode,
				pageSize,
				roleFilter,
				memberSinceFilter,
				joinedFluxerFilter,
				joinMethodFilter,
			}),
		[searchQuery, sortMode, pageSize, roleFilter, memberSinceFilter, joinedFluxerFilter, joinMethodFilter],
	);
	const totalPages = Math.max(1, Math.ceil(totalCount / pageSize));

	const resetTableScroll = useCallback(() => {
		tableScrollerRef.current?.scrollTo({to: 0, animate: false});
	}, []);
	const performSearch = useCallback(
		async (params: {query?: string; page: number}) => {
			const {query, page} = params;
			if (isLoadingRef.current) {
				abortControllerRef.current?.abort();
			}
			const controller = new AbortController();
			abortControllerRef.current = controller;
			setIsSearching(true);
			isLoadingRef.current = true;
			try {
				const searchParams = buildMemberSearchParams({
					query,
					page,
					pageSize,
					sortMode,
					roleFilter,
					memberSinceFilter,
					joinedFluxerFilter,
					joinMethodFilter,
				});
				const response = await searchGuildMembers(guildId, searchParams);
				if (controller.signal.aborted) {
					return;
				}
				setSearchError(false);
				setIndexing(response.indexing);
				setTotalCount(response.total_result_count);
				const displayMembers = response.members.map((m) => toMemberDisplayData(m, guildId));
				setSearchMembers(displayMembers);
				const totalPages = Math.max(1, Math.ceil(response.total_result_count / pageSize));
				if (page > totalPages) {
					setCurrentPage(totalPages);
				}
				setInitialLoadDone(true);
			} catch (error) {
				if (controller.signal.aborted) {
					return;
				}
				logger.error('Failed to search guild members:', error);
				setSearchError(true);
				setInitialLoadDone(true);
			} finally {
				if (!controller.signal.aborted) {
					setIsSearching(false);
					isLoadingRef.current = false;
				}
			}
		},
		[guildId, sortMode, pageSize, roleFilter, memberSinceFilter, joinedFluxerFilter, joinMethodFilter],
	);
	useEffect(() => {
		const filtersChanged = filterKeyRef.current !== filterKey;
		if (filtersChanged) {
			filterKeyRef.current = filterKey;
			if (currentPage !== 1) {
				setCurrentPage(1);
				return;
			}
		}
		setSearchError(false);
		performSearch({query: searchQuery || undefined, page: currentPage});
	}, [currentPage, filterKey, performSearch, searchQuery]);
	useEffect(() => {
		if (!indexing) return;
		const interval = setInterval(() => {
			performSearch({query: searchQuery || undefined, page: currentPage});
		}, INDEXING_POLL_INTERVAL_MS);
		return () => clearInterval(interval);
	}, [currentPage, indexing, performSearch, searchQuery]);
	const [membersVerified, setMembersVerified] = useState(false);
	useEffect(() => {
		setMembersVerified(false);
		if (searchMembers.length === 0) {
			setMembersVerified(true);
			return;
		}
		const allUserIds = searchMembers.map((m) => m.userId);
		const inviterIds = searchMembers.map((m) => m.inviterId).filter((id): id is string => id != null);
		const idsToLoad = [...new Set([...allUserIds, ...inviterIds])];
		GuildMembers.ensureMembersLoaded(guildId, idsToLoad)
			.catch((error) => {
				logger.error('Failed to fetch guild members:', error);
			})
			.finally(() => {
				setMembersVerified(true);
			});
	}, [searchMembers, guildId]);
	const debouncedSetQuery = useMemo(
		() =>
			debounce((query: string) => {
				setSearchQuery(query);
			}, SEARCH_DEBOUNCE_MS),
		[],
	);
	useEffect(() => {
		return () => {
			debouncedSetQuery.cancel();
			abortControllerRef.current?.abort();
		};
	}, [debouncedSetQuery]);

	useEffect(() => {
		resetTableScroll();
	}, [currentPage, filterKey, resetTableScroll]);
	const [inputValue, setInputValue] = useState('');
	const handleInputChange = useCallback(
		(e: React.ChangeEvent<HTMLInputElement>) => {
			const value = e.target.value;
			setInputValue(value);
			debouncedSetQuery(value);
		},
		[debouncedSetQuery],
	);
	const handlePageSizeChange = useCallback(
		(value: number) => {
			resetTableScroll();
			setPageSize(value);
			setCurrentPage(1);
		},
		[resetTableScroll],
	);
	const handlePageSelect = useCallback(
		(page: number) => {
			if (isSearching || page === currentPage || page < 1 || page > totalPages) {
				return;
			}
			resetTableScroll();
			setCurrentPage(page);
		},
		[currentPage, isSearching, resetTableScroll, totalPages],
	);
	const handleSortMenuOpen = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			ContextMenuCommands.openFromElementBottomRight(event, ({onClose}) => (
				<ContextMenuCloseProvider
					value={onClose}
					data-flx="channel.guild-members-page.handle-sort-menu-open.context-menu-close-provider"
				>
					<MenuGroup data-flx="channel.guild-members-page.handle-sort-menu-open.menu-group">
						<MenuItemRadio
							selected={sortMode === 'newest'}
							closeOnSelect
							onSelect={() => setSortMode('newest')}
							data-flx="channel.guild-members-page.handle-sort-menu-open.menu-item-radio.set-sort-mode"
						>
							{i18n._(NEWEST_FIRST_DESCRIPTOR)}
						</MenuItemRadio>
						<MenuItemRadio
							selected={sortMode === 'oldest'}
							closeOnSelect
							onSelect={() => setSortMode('oldest')}
							data-flx="channel.guild-members-page.handle-sort-menu-open.menu-item-radio.set-sort-mode--2"
						>
							{i18n._(OLDEST_FIRST_DESCRIPTOR)}
						</MenuItemRadio>
					</MenuGroup>
				</ContextMenuCloseProvider>
			));
		},
		[sortMode, i18n],
	);
	const openDateRangeFilter = useCallback(
		(
			event: React.MouseEvent<HTMLButtonElement>,
			currentFilter: DateRangeFilter,
			setFilter: (filter: DateRangeFilter) => void,
		) => {
			const presets = [
				{label: i18n._(ALL_DESCRIPTOR), duration: 0},
				{label: i18n._(PAST_1_HOUR_DESCRIPTOR), duration: HOUR_MS},
				{label: i18n._(PAST_24_HOURS_DESCRIPTOR), duration: 24 * HOUR_MS},
				{label: i18n._(PAST_7_DAYS_DESCRIPTOR), duration: 7 * DAY_MS},
				{label: i18n._(PAST_2_WEEKS_DESCRIPTOR), duration: 14 * DAY_MS},
				{label: i18n._(PAST_3_WEEKS_DESCRIPTOR), duration: 21 * DAY_MS},
				{label: i18n._(PAST_4_WEEKS_DESCRIPTOR), duration: 28 * DAY_MS},
				{label: i18n._(PAST_3_MONTHS_DESCRIPTOR), duration: 90 * DAY_MS},
			];
			const isAll = currentFilter.gte == null && currentFilter.lte == null;
			const isCustom = !isAll && !presets.some((p) => p.duration > 0 && isPresetMatch(currentFilter, p.duration));
			ContextMenuCommands.openFromElementBottomRight(event, ({onClose}) => (
				<ContextMenuCloseProvider
					value={onClose}
					data-flx="channel.guild-members-page.open-date-range-filter.context-menu-close-provider"
				>
					<MenuGroup data-flx="channel.guild-members-page.open-date-range-filter.menu-group">
						{presets.map((preset) => {
							const selected = preset.duration === 0 ? isAll : isPresetMatch(currentFilter, preset.duration);
							return (
								<MenuItemRadio
									key={preset.duration}
									selected={selected}
									closeOnSelect
									onSelect={() => {
										if (preset.duration === 0) {
											setFilter({});
										} else {
											setFilter({gte: Math.floor((Date.now() - preset.duration) / 1000)});
										}
									}}
									data-flx="channel.guild-members-page.open-date-range-filter.menu-item-radio"
								>
									{preset.label}
								</MenuItemRadio>
							);
						})}
						<MenuItemRadio
							selected={isCustom}
							closeOnSelect
							onSelect={() => {
								ModalCommands.push(
									ModalCommands.modal(() => (
										<GuildMembersDateRangeModal
											onApply={(gte, lte) => setFilter({gte, lte})}
											initialGte={currentFilter.gte}
											initialLte={currentFilter.lte}
											data-flx="channel.guild-members-page.open-date-range-filter.guild-members-date-range-modal"
										/>
									)),
								);
							}}
							data-flx="channel.guild-members-page.open-date-range-filter.menu-item-radio.push"
						>
							{i18n._(CUSTOM_RANGE_DESCRIPTOR)}
						</MenuItemRadio>
					</MenuGroup>
				</ContextMenuCloseProvider>
			));
		},
		[i18n],
	);
	const handleMemberSinceFilterOpen = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			openDateRangeFilter(event, memberSinceFilter, setMemberSinceFilter);
		},
		[openDateRangeFilter, memberSinceFilter],
	);
	const handleJoinedFluxerFilterOpen = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			openDateRangeFilter(event, joinedFluxerFilter, setJoinedFluxerFilter);
		},
		[openDateRangeFilter, joinedFluxerFilter],
	);
	const handleJoinMethodFilterOpen = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			const inviteCodes = getInviteCodes(searchMembers);
			const vanityCode = guild?.vanityURLCode;
			const isAll =
				(joinMethodFilter.inviteCode == null || joinMethodFilter.inviteCode.length === 0) &&
				(joinMethodFilter.sourceType == null || joinMethodFilter.sourceType.length === 0);
			ContextMenuCommands.openFromElementBottomRight(event, ({onClose}) => (
				<ContextMenuCloseProvider
					value={onClose}
					data-flx="channel.guild-members-page.handle-join-method-filter-open.context-menu-close-provider"
				>
					<MenuGroup data-flx="channel.guild-members-page.handle-join-method-filter-open.menu-group">
						<MenuItemRadio
							selected={isAll}
							closeOnSelect
							onSelect={() => setJoinMethodFilter({})}
							data-flx="channel.guild-members-page.handle-join-method-filter-open.menu-item-radio.set-join-method-filter"
						>
							{i18n._(ALL_DESCRIPTOR)}
						</MenuItemRadio>
						{vanityCode && (
							<MenuItemRadio
								selected={joinMethodFilter.inviteCode?.includes(vanityCode) ?? false}
								closeOnSelect
								onSelect={() => setJoinMethodFilter({inviteCode: [vanityCode]})}
								data-flx="channel.guild-members-page.handle-join-method-filter-open.menu-item-radio.set-join-method-filter--2"
							>
								{i18n._(VANITY_URL_DESCRIPTOR)}
							</MenuItemRadio>
						)}
						{inviteCodes.map((code) => (
							<MenuItemRadio
								key={code}
								selected={joinMethodFilter.inviteCode?.includes(code) ?? false}
								closeOnSelect
								onSelect={() => setJoinMethodFilter({inviteCode: [code]})}
								data-flx="channel.guild-members-page.handle-join-method-filter-open.menu-item-radio.set-join-method-filter--3"
							>
								{code}
							</MenuItemRadio>
						))}
					</MenuGroup>
				</ContextMenuCloseProvider>
			));
		},
		[searchMembers, guild?.vanityURLCode, joinMethodFilter, i18n],
	);
	const handleRolesFilterOpen = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			ContextMenuCommands.openFromElementBottomRight(event, ({onClose}) => (
				<RolesFilterMenuContent
					roles={roles}
					initialRoleFilter={roleFilter}
					setRoleFilter={setRoleFilter}
					onClose={onClose}
					data-flx="channel.guild-members-page.handle-roles-filter-open.roles-filter-menu-content"
				/>
			));
		},
		[roles, roleFilter],
	);
	const handleRowClick = useCallback(
		(data: MemberDisplayData) => {
			UserProfileCommands.openUserProfile(data.userId, guildId);
		},
		[guildId],
	);
	const clearContextMenuMember = useCallback(() => {
		setContextMenuMemberId(null);
	}, []);
	const clearActiveMenuMember = useCallback(() => {
		setActiveMenuMemberId(null);
	}, []);
	const handleRowContextMenu = useCallback(
		(data: MemberDisplayData, event: React.MouseEvent<HTMLElement>) => {
			const user = getMemberContextUser(data);
			if (!user) return;
			setContextMenuMemberId(data.userId);
			ContextMenuCommands.openFromEvent(
				event,
				({onClose}) => renderGuildMemberContextMenu(user, guildId, onClose, clearContextMenuMember),
				{onClose: clearContextMenuMember},
			);
		},
		[clearContextMenuMember, guildId],
	);
	const handleActionsClick = useCallback(
		(data: MemberDisplayData, event: React.MouseEvent<HTMLElement>) => {
			const user = getMemberContextUser(data);
			if (!user) return;
			setActiveMenuMemberId(data.userId);
			ContextMenuCommands.openFromElementBottomRight(
				event,
				({onClose}) => renderGuildMemberContextMenu(user, guildId, onClose, clearActiveMenuMember),
				{onClose: clearActiveMenuMember},
			);
		},
		[clearActiveMenuMember, guildId],
	);
	if (!guild) {
		return null;
	}
	const displayedMembers = getDisplayedMembers(searchMembers, guildId, membersVerified);
	const {showProgress, showEmptySearch, showError, showPagination, showFooter} = getMembersTableState({
		initialLoadDone,
		membersVerified,
		isSearching,
		indexing,
		searchError,
		displayedMemberCount: displayedMembers.length,
		totalPages,
		totalCount,
	});
	const memberSinceActive = isDateRangeFilterActive(memberSinceFilter);
	const joinedFluxerActive = isDateRangeFilterActive(joinedFluxerFilter);
	const joinMethodActive = isJoinMethodFilterActive(joinMethodFilter);
	const rolesActive = roleFilter.length > 0;
	return (
		<div className={styles.pageContainer} data-flx="channel.guild-members-page.members-table-view.page-container">
			<div className={styles.content} data-flx="channel.guild-members-page.members-table-view.content">
				<MembersTableToolbar
					displayedCount={displayedMembers.length}
					totalCount={totalCount}
					showFooter={showFooter}
					inputValue={inputValue}
					onInputChange={handleInputChange}
					onSortMenuOpen={handleSortMenuOpen}
					indexing={indexing}
					data-flx="channel.guild-members-page.members-table-view.members-table-toolbar"
				/>
				<div className={styles.tableWrapper} data-flx="channel.guild-members-page.members-table-view.table-wrapper">
					<div className={styles.tableViewport} data-flx="channel.guild-members-page.members-table-view.table-viewport">
						<div
							className={styles.tableSurface}
							role="table"
							aria-colcount={6}
							data-flx="channel.guild-members-page.members-table-view.table-surface"
						>
							<MembersTableHeaderRow
								memberSinceActive={memberSinceActive}
								joinedFluxerActive={joinedFluxerActive}
								joinMethodActive={joinMethodActive}
								rolesActive={rolesActive}
								onMemberSinceFilterOpen={handleMemberSinceFilterOpen}
								onJoinedFluxerFilterOpen={handleJoinedFluxerFilterOpen}
								onJoinMethodFilterOpen={handleJoinMethodFilterOpen}
								onRolesFilterOpen={handleRolesFilterOpen}
								data-flx="channel.guild-members-page.members-table-view.members-table-header-row"
							/>
							<MembersTableProgressSlot
								show={showProgress}
								data-flx="channel.guild-members-page.members-table-view.members-table-progress-slot"
							/>
							<Scroller
								ref={tableScrollerRef}
								className={styles.tableScroller}
								contentClassName={styles.tableScrollerContent}
								fade={false}
								scrollbar="thin"
								scrollbarTrackMode="reserve"
								data-flx="channel.guild-members-page.members-table-view.table-scroller"
							>
								<MembersTableBody
									guildId={guildId}
									members={displayedMembers}
									showProgress={showProgress}
									showError={showError}
									showEmptySearch={showEmptySearch}
									ownerId={guild.ownerId}
									hideOwnerCrown={guild.features.has(GuildFeatures.HIDE_OWNER_CROWN)}
									activeMenuMemberId={activeMenuMemberId}
									contextMenuMemberId={contextMenuMemberId}
									onActionsClick={handleActionsClick}
									onContextMenu={handleRowContextMenu}
									onRowClick={handleRowClick}
									data-flx="channel.guild-members-page.members-table-view.members-table-body.row-context-menu"
								/>
							</Scroller>
						</div>
					</div>
					{showFooter && (
						<MembersTableFooter
							pageSize={pageSize}
							onPageSizeChange={handlePageSizeChange}
							showPagination={showPagination}
							currentPage={currentPage}
							totalPages={totalPages}
							isSearching={isSearching}
							onPageSelect={handlePageSelect}
							data-flx="channel.guild-members-page.members-table-view.members-table-footer"
						/>
					)}
				</div>
			</div>
		</div>
	);
});
