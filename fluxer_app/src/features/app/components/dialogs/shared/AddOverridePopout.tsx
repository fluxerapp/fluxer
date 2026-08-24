// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/dialogs/shared/AddOverridePopout.module.css';
import {DEFAULT_ROLE_COLOR_HEX, getRoleColor} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import Guilds from '@app/features/guild/state/Guilds';
import {ROLES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {GuildMember} from '@app/features/member/models/GuildMember';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch, {type SearchContext} from '@app/features/member/state/MemberSearch';
import {openRoleContextMenu} from '@app/features/ui/action_menu/RoleContextMenu';
import {Avatar} from '@app/features/ui/components/Avatar';
import {
	SearchableListPopout,
	type SearchableListPopoutItem,
	type SearchableListPopoutSection,
} from '@app/features/ui/popover/searchable_list_popout/SearchableListPopout';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo, useRef, useState} from 'react';

const SEARCH_ROLES_OR_MEMBERS_DESCRIPTOR = msg({
	message: 'Search roles or members…',
	comment: 'Short label in the settings dialog add override popout.',
});
const SEARCH_ROLES_OR_MEMBERS_2_DESCRIPTOR = msg({
	message: 'Search roles or members',
	comment: 'Short label in the settings dialog add override popout.',
});
const ROLES_AND_MEMBERS_DESCRIPTOR = msg({
	message: 'Roles and members',
	comment: 'Short label in the settings dialog add override popout.',
});

interface AddOverridePopoutProps {
	guildId: string;
	existingOverwriteIds: Set<string>;
	onSelect: (id: string, type: 0 | 1, name: string) => void;
	onClose: () => void;
}

const MEMBERS_LIMIT = 10;
const WORKER_RESULT_LIMIT = 25;
const SERVER_DEBOUNCE_MS = 300;

interface ParsedMemberQuery {
	usernameQuery: string;
	tagQuery: string | null;
	hasTagSeparator: boolean;
}

function parseMemberQuery(query: string): ParsedMemberQuery {
	const hashIndex = query.indexOf('#');
	if (hashIndex === -1) {
		return {usernameQuery: query, tagQuery: null, hasTagSeparator: false};
	}
	return {
		usernameQuery: query.slice(0, hashIndex),
		tagQuery: query.slice(hashIndex + 1),
		hasTagSeparator: true,
	};
}

function getMemberDisplayName(member: GuildMember, guildId: string): string {
	return NicknameUtils.getNickname(member.user, guildId);
}

function compareByDisplayName(a: GuildMember, b: GuildMember, guildId: string): number {
	const aName = getMemberDisplayName(a, guildId).toLowerCase();
	const bName = getMemberDisplayName(b, guildId).toLowerCase();
	return aName.localeCompare(bName);
}

function filterMembers(
	members: Array<GuildMember>,
	guildId: string,
	parsed: ParsedMemberQuery,
	stableOrder?: Map<string, number>,
): Array<GuildMember> {
	let matched: Array<GuildMember>;
	if (parsed.hasTagSeparator) {
		const usernameLower = parsed.usernameQuery.toLowerCase();
		const tagLower = parsed.tagQuery?.toLowerCase() ?? '';
		matched = members.filter((member) => {
			const nick = member.nick?.toLowerCase() ?? '';
			const username = member.user.username.toLowerCase();
			const matchesUsername =
				usernameLower.length === 0 || username.startsWith(usernameLower) || nick.startsWith(usernameLower);
			const matchesTag = tagLower.length === 0 || member.user.discriminator.startsWith(tagLower);
			return matchesUsername && matchesTag;
		});
	} else {
		const trimmed = parsed.usernameQuery.trim();
		if (trimmed.length === 0) {
			matched = [...members];
		} else {
			matched = matchSorter(members, trimmed, {
				keys: [(member) => getMemberDisplayName(member, guildId), 'nick', 'user.username', 'user.tag'],
			});
		}
	}
	if (stableOrder && stableOrder.size > 0) {
		const NEW_RANK = Number.MAX_SAFE_INTEGER;
		return [...matched].sort((a, b) => {
			const ra = stableOrder.get(a.user.id) ?? NEW_RANK;
			const rb = stableOrder.get(b.user.id) ?? NEW_RANK;
			if (ra !== rb) return ra - rb;
			return compareByDisplayName(a, b, guildId);
		});
	}
	return [...matched].sort((a, b) => compareByDisplayName(a, b, guildId));
}

export const AddOverridePopout: React.FC<AddOverridePopoutProps> = observer(function AddOverridePopout({
	guildId,
	existingOverwriteIds,
	onSelect,
	onClose,
}) {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const [searchQuery, setSearchQuery] = useState('');
	const [serverMemberIds, setServerMemberIds] = useState<Array<string>>([]);
	const searchContextRef = useRef<SearchContext | null>(null);
	const debounceTimerRef = useRef<NodeJS.Timeout | null>(null);
	const sessionRef = useRef<{key: string; order: Map<string, number>; nextRank: number}>({
		key: '',
		order: new Map(),
		nextRank: 0,
	});
	useEffect(() => {
		const context = MemberSearch.getSearchContext((results) => {
			setServerMemberIds(results.map((result) => result.id));
		}, WORKER_RESULT_LIMIT);
		searchContextRef.current = context;
		return () => {
			context.destroy();
			searchContextRef.current = null;
			if (debounceTimerRef.current) {
				clearTimeout(debounceTimerRef.current);
				debounceTimerRef.current = null;
			}
		};
	}, []);
	useEffect(() => {
		const trimmed = searchQuery.trim();
		const parsed = parseMemberQuery(trimmed);
		const queryForServer = parsed.usernameQuery.trim();
		if (debounceTimerRef.current) {
			clearTimeout(debounceTimerRef.current);
			debounceTimerRef.current = null;
		}
		const context = searchContextRef.current;
		if (queryForServer.length === 0) {
			context?.cancelSearch();
			setServerMemberIds([]);
			return;
		}
		context?.beginSearch(queryForServer, {guild: guildId});
		if (GuildMembers.isGuildFullyLoaded(guildId)) {
			return;
		}
		debounceTimerRef.current = setTimeout(() => {
			debounceTimerRef.current = null;
			void MemberSearch.fetchMembersInBackground(queryForServer, [guildId], guildId);
		}, SERVER_DEBOUNCE_MS);
	}, [searchQuery, guildId]);
	const roles = useMemo(() => {
		if (!guild) return [];
		return Object.values(guild.roles)
			.filter((role) => !existingOverwriteIds.has(role.id))
			.sort((a, b) => b.position - a.position);
	}, [guild, existingOverwriteIds]);
	const members = useMemo(() => {
		if (!guild) return [];
		const cached = GuildMembers.getMembers(guildId).filter((member) => !existingOverwriteIds.has(member.user.id));
		const trimmed = searchQuery.trim();
		const parsed = parseMemberQuery(trimmed);
		const hasQuery = trimmed.length > 0;
		if (!hasQuery) {
			sessionRef.current = {key: `${guildId}:`, order: new Map(), nextRank: 0};
			return [...cached].sort((a, b) => compareByDisplayName(a, b, guildId)).slice(0, MEMBERS_LIMIT);
		}
		const sessionKey = `${guildId}:${trimmed}`;
		if (sessionRef.current.key !== sessionKey) {
			sessionRef.current = {key: sessionKey, order: new Map(), nextRank: 0};
		}
		const merged = new Map<string, GuildMember>();
		for (const member of cached) {
			merged.set(member.user.id, member);
		}
		for (const id of serverMemberIds) {
			if (existingOverwriteIds.has(id) || merged.has(id)) continue;
			const member = GuildMembers.getMember(guildId, id);
			if (member) {
				merged.set(id, member);
			}
		}
		const result = filterMembers(Array.from(merged.values()), guildId, parsed, sessionRef.current.order).slice(
			0,
			MEMBERS_LIMIT,
		);
		const session = sessionRef.current;
		for (const member of result) {
			if (!session.order.has(member.user.id)) {
				session.order.set(member.user.id, session.nextRank++);
			}
		}
		return result;
	}, [guild, guildId, existingOverwriteIds, searchQuery, serverMemberIds]);
	const filteredRoles = useMemo(() => {
		const trimmed = searchQuery.trim();
		if (trimmed.length === 0) return roles;
		return matchSorter(roles, trimmed, {keys: ['name', 'id']});
	}, [roles, searchQuery]);
	const roleItems = useMemo<Array<SearchableListPopoutItem>>(() => {
		return filteredRoles.map((role) => ({
			id: `role-${role.id}`,
			ariaLabel: role.name,
			searchValues: [role.name, role.id],
			onSelect: () => {
				onSelect(role.id, 0, role.name);
				onClose();
			},
			onContextMenu: (event) => openRoleContextMenu(event, role.id),
			render: () => (
				<>
					<div
						className={styles.roleIndicator}
						style={{
							backgroundColor: role.color === 0 ? DEFAULT_ROLE_COLOR_HEX : getRoleColor(role.color),
						}}
						data-flx="app.add-override-popout.role-items.role-indicator"
					/>
					<span className={styles.itemLabel} data-flx="app.add-override-popout.role-items.item-label">
						{role.name}
					</span>
				</>
			),
		}));
	}, [filteredRoles, onClose, onSelect]);
	const memberItems = useMemo<Array<SearchableListPopoutItem>>(() => {
		return members.map((member) => {
			const displayName = getMemberDisplayName(member, guildId);
			return {
				id: `member-${member.user.id}`,
				ariaLabel: displayName,
				searchValues: [displayName, member.user.username, member.user.tag, member.user.id],
				onSelect: () => {
					onSelect(member.user.id, 1, displayName);
					onClose();
				},
				render: () => (
					<>
						<Avatar
							user={member.user}
							size={12}
							className={styles.avatar}
							guildId={guildId}
							data-flx="app.add-override-popout.member-items.avatar"
						/>
						<span className={styles.itemLabel} data-flx="app.add-override-popout.member-items.item-label">
							{displayName}
						</span>
					</>
				),
			};
		});
	}, [guildId, members, onClose, onSelect]);
	const sections = useMemo<Array<SearchableListPopoutSection>>(() => {
		const nextSections: Array<SearchableListPopoutSection> = [];
		if (roleItems.length > 0) {
			nextSections.push({
				id: 'roles',
				heading: i18n._(ROLES_DESCRIPTOR),
				items: roleItems,
			});
		}
		if (memberItems.length > 0) {
			nextSections.push({
				id: 'members',
				heading: <Trans>Members</Trans>,
				items: memberItems,
			});
		}
		return nextSections;
	}, [i18n.locale, memberItems, roleItems]);
	return (
		<SearchableListPopout
			className={styles.popoutContainer}
			searchClassName={styles.searchContainer}
			scrollerClassName={styles.scroller}
			sectionClassName={styles.section}
			sectionHeadingClassName={styles.sectionHeader}
			optionClassName={styles.itemButton}
			emptyStateClassName={styles.emptyState}
			placeholder={i18n._(SEARCH_ROLES_OR_MEMBERS_DESCRIPTOR)}
			searchInputAriaLabel={i18n._(SEARCH_ROLES_OR_MEMBERS_2_DESCRIPTOR)}
			listAriaLabel={i18n._(ROLES_AND_MEMBERS_DESCRIPTOR)}
			noResultsLabel={<Trans>No matches</Trans>}
			sections={sections}
			onRequestClose={onClose}
			onSearchQueryChange={setSearchQuery}
			disableInternalFiltering={true}
			data-flx="app.add-override-popout.popout-container"
		/>
	);
});
