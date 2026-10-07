// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildMember} from '@app/features/member/models/GuildMember';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import type React from 'react';

export const SEARCH_DEBOUNCE_MS = 300;
export const DEFAULT_PAGE_SIZE = 25;
export const INDEXING_POLL_INTERVAL_MS = 5000;
export const HOUR_MS = 3_600_000;
export const DAY_MS = 86_400_000;
export const MAX_VISIBLE_PAGES = 7;
export const PAGE_SIZE_OPTIONS: Array<ComboboxOption<number>> = [
	{value: 12, label: '12'},
	{value: 25, label: '25'},
	{value: 50, label: '50'},
	{value: 100, label: '100'},
];

interface SearchableGuildMemberSupplemental {
	join_source_type: number | null;
	source_invite_code: string | null;
	inviter_id: string | null;
}

export interface SearchableGuildMember {
	id: string;
	guild_id: string;
	user_id: string;
	username: string;
	discriminator: string;
	global_name: string | null;
	nickname: string | null;
	role_ids: Array<string>;
	joined_at: number;
	supplemental: SearchableGuildMemberSupplemental;
	is_bot: boolean;
}

export interface GuildMemberSearchResponse {
	guild_id: string;
	members: Array<SearchableGuildMember>;
	page_result_count: number;
	total_result_count: number;
	indexing: boolean;
}

export interface SearchParams {
	query?: string;
	limit?: number;
	offset?: number;
	role_ids?: Array<string>;
	sort_by?: string;
	sort_order?: string;
	joined_at_gte?: number;
	joined_at_lte?: number;
	join_source_type?: Array<number>;
	source_invite_code?: Array<string>;
	user_created_at_gte?: number;
	user_created_at_lte?: number;
}

export interface MemberDisplayData {
	userId: string;
	displayName: string;
	tag: string;
	username: string;
	discriminator: string;
	nickname: string | null;
	roleIds: Array<string>;
	joinedAt: Date;
	isBot: boolean;
	user: User | null;
	member: GuildMember | null;
	joinSourceType: number | null;
	sourceInviteCode: string | null;
	inviterId: string | null;
	userCreatedAt: Date;
}

export type SortMode = 'newest' | 'oldest';

export interface DateRangeFilter {
	gte?: number;
	lte?: number;
}

export interface JoinMethodFilter {
	sourceType?: Array<number>;
	inviteCode?: Array<string>;
}

export interface MemberSearchRequestOptions {
	query?: string;
	page: number;
	pageSize: number;
	sortMode: SortMode;
	roleFilter: Array<string>;
	memberSinceFilter: DateRangeFilter;
	joinedFluxerFilter: DateRangeFilter;
	joinMethodFilter: JoinMethodFilter;
}

export interface MembersTableStateParams {
	initialLoadDone: boolean;
	membersVerified: boolean;
	isSearching: boolean;
	indexing: boolean;
	searchError: boolean;
	displayedMemberCount: number;
	totalPages: number;
	totalCount: number;
}

export interface MemberTableRowProps {
	data: MemberDisplayData;
	guildId: string;
	isOwner: boolean;
	activeMenuMemberId: string | null;
	contextMenuMemberId: string | null;
	onActionsClick: (data: MemberDisplayData, event: React.MouseEvent<HTMLElement>) => void;
	onContextMenu: (data: MemberDisplayData, event: React.MouseEvent<HTMLElement>) => void;
	onRowClick: (data: MemberDisplayData) => void;
}

export interface MembersTableBodyProps {
	guildId: string;
	members: ReadonlyArray<MemberDisplayData>;
	showProgress: boolean;
	showError: boolean;
	showEmptySearch: boolean;
	ownerId: string | null | undefined;
	hideOwnerCrown: boolean;
	activeMenuMemberId: string | null;
	contextMenuMemberId: string | null;
	onActionsClick: (data: MemberDisplayData, event: React.MouseEvent<HTMLElement>) => void;
	onContextMenu: (data: MemberDisplayData, event: React.MouseEvent<HTMLElement>) => void;
	onRowClick: (data: MemberDisplayData) => void;
}

export type PaginationItem = number | 'ellipsis-left' | 'ellipsis-right';
export type PaginationEllipsisSide = 'left' | 'right';

function hasSelectedValues<T>(values?: ReadonlyArray<T>): boolean {
	return values != null && values.length > 0;
}

export function isDateRangeFilterActive(filter: DateRangeFilter): boolean {
	return filter.gte != null || filter.lte != null;
}

export function isJoinMethodFilterActive(filter: JoinMethodFilter): boolean {
	return hasSelectedValues(filter.sourceType) || hasSelectedValues(filter.inviteCode);
}

export function isPresetMatch(filter: DateRangeFilter, durationMs: number): boolean {
	if (filter.gte == null || filter.lte != null) return false;
	const expected = Math.floor((Date.now() - durationMs) / 1000);
	return Math.abs(filter.gte - expected) < 60;
}

export function getInviteCodes(members: ReadonlyArray<MemberDisplayData>): Array<string> {
	const inviteCodes = new Set<string>();
	for (const member of members) {
		if (member.sourceInviteCode) {
			inviteCodes.add(member.sourceInviteCode);
		}
	}
	return Array.from(inviteCodes);
}

export function getMemberContextUser(data: MemberDisplayData): User | null {
	return data.user ?? Users.getUser(data.userId) ?? null;
}
