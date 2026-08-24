// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getOverrideMemberLabel,
	parseOverrideMemberQuery,
	selectOverrideMembers,
} from '@app/features/app/components/dialogs/shared/AddOverrideMemberSearch';
import styles from '@app/features/app/components/dialogs/shared/AddOverridePopout.module.css';
import {DEFAULT_ROLE_COLOR_HEX, getRoleColor} from '@app/features/app/components/dialogs/shared/PermissionComponents';
import Guilds from '@app/features/guild/state/Guilds';
import {ROLES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import GuildMembers from '@app/features/member/state/GuildMembers';
import MemberSearch, {type SearchContext} from '@app/features/member/state/MemberSearch';
import {openRoleContextMenu} from '@app/features/ui/action_menu/RoleContextMenu';
import {Avatar} from '@app/features/ui/components/Avatar';
import {
	SearchableListPopout,
	type SearchableListPopoutItem,
	type SearchableListPopoutSection,
} from '@app/features/ui/popover/searchable_list_popout/SearchableListPopout';
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
		const parsed = parseOverrideMemberQuery(trimmed);
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
		return selectOverrideMembers({
			cachedMembers: GuildMembers.getMembers(guildId),
			workerMemberIds: serverMemberIds,
			resolveMember: (userId) => GuildMembers.getMember(guildId, userId),
			excludedIds: existingOverwriteIds,
			guildId,
			query: searchQuery,
			limit: MEMBERS_LIMIT,
		});
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
			const displayName = getOverrideMemberLabel(member, guildId);
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
