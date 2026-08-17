// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import type {
	GuildMemberSearchResponse,
	MemberDisplayData,
	MemberSearchRequestOptions,
	MembersTableStateParams,
	SearchableGuildMember,
	SearchParams,
} from '@app/features/channel/components/guild_members_page/GuildMembersPageShared';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {extractTimestampFromSnowflakeAsDate} from '@fluxer/snowflake/src/SnowflakeUtils';

export const logger = new Logger('GuildMembersPage');

export async function searchGuildMembers(guildId: string, params: SearchParams): Promise<GuildMemberSearchResponse> {
	const response = await http.post<GuildMemberSearchResponse>(Endpoints.GUILD_MEMBERS_SEARCH(guildId), {
		body: params,
	});
	return response.body;
}

export function toMemberDisplayData(searchMember: SearchableGuildMember, guildId: string): MemberDisplayData {
	const user = Users.getUser(searchMember.user_id);
	const member = GuildMembers.getMember(guildId, searchMember.user_id);
	const resolvedName = member?.nick || searchMember.nickname || searchMember.global_name || searchMember.username;
	const displayName = NicknameUtils.formatNicknameForStreamerMode(resolvedName);
	const tag = user ? user.tag : `${searchMember.username}#${searchMember.discriminator}`;
	return {
		userId: searchMember.user_id,
		displayName,
		tag,
		username: searchMember.username,
		discriminator: searchMember.discriminator,
		nickname: searchMember.nickname,
		roleIds: searchMember.role_ids,
		joinedAt: new Date(searchMember.joined_at * 1000),
		isBot: searchMember.is_bot,
		user: user ?? null,
		member: member ?? null,
		joinSourceType: searchMember.supplemental.join_source_type,
		sourceInviteCode: searchMember.supplemental.source_invite_code,
		inviterId: searchMember.supplemental.inviter_id,
		userCreatedAt: extractTimestampFromSnowflakeAsDate(searchMember.user_id),
	};
}

export function buildMemberSearchParams({
	query,
	page,
	pageSize,
	sortMode,
	roleFilter,
	memberSinceFilter,
	joinedFluxerFilter,
	joinMethodFilter,
}: MemberSearchRequestOptions): SearchParams {
	const searchParams: SearchParams = {
		limit: pageSize,
		offset: Math.max(0, (page - 1) * pageSize),
		sort_by: 'joinedAt',
		sort_order: sortMode === 'newest' ? 'desc' : 'asc',
	};
	if (query) {
		searchParams.query = query;
	}
	if (roleFilter.length > 0) {
		searchParams.role_ids = roleFilter;
	}
	if (memberSinceFilter.gte != null) {
		searchParams.joined_at_gte = memberSinceFilter.gte;
	}
	if (memberSinceFilter.lte != null) {
		searchParams.joined_at_lte = memberSinceFilter.lte;
	}
	if (joinedFluxerFilter.gte != null) {
		searchParams.user_created_at_gte = joinedFluxerFilter.gte;
	}
	if (joinedFluxerFilter.lte != null) {
		searchParams.user_created_at_lte = joinedFluxerFilter.lte;
	}
	if (joinMethodFilter.sourceType && joinMethodFilter.sourceType.length > 0) {
		searchParams.join_source_type = joinMethodFilter.sourceType;
	}
	if (joinMethodFilter.inviteCode && joinMethodFilter.inviteCode.length > 0) {
		searchParams.source_invite_code = joinMethodFilter.inviteCode;
	}
	return searchParams;
}

export function getDisplayedMembers(
	members: ReadonlyArray<MemberDisplayData>,
	guildId: string,
	membersVerified: boolean,
): ReadonlyArray<MemberDisplayData> {
	if (!membersVerified) {
		return members;
	}
	return members.filter((m) => GuildMembers.getMember(guildId, m.userId) != null);
}

export function getMembersTableState({
	initialLoadDone,
	membersVerified,
	isSearching,
	indexing,
	searchError,
	displayedMemberCount,
	totalPages,
	totalCount,
}: MembersTableStateParams) {
	const dataReady = initialLoadDone && membersVerified && !isSearching && !indexing;
	const showProgress = !dataReady;
	const showEmptySearch = dataReady && displayedMemberCount === 0 && !searchError;
	const showError = dataReady && searchError;
	return {
		showProgress,
		showEmptySearch,
		showError,
		showPagination: dataReady && !showError && !showEmptySearch && totalPages > 1,
		showFooter: dataReady && !showError && !showEmptySearch && totalCount > 0,
	};
}
