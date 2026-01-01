/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useListNavigation} from '@floating-ui/react';
import {Trans, useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {Input} from '~/components/form/Input';
import {Avatar} from '~/components/uikit/Avatar';
import {Scroller} from '~/components/uikit/Scroller';
import GuildMemberStore from '~/stores/GuildMemberStore';
import GuildStore from '~/stores/GuildStore';
import styles from './AddOverridePopout.module.css';
import {DEFAULT_ROLE_COLOR_HEX, getRoleColor} from './PermissionComponents';

interface AddOverridePopoutProps {
	guildId: string;
	existingOverwriteIds: Set<string>;
	onSelect: (id: string, type: 0 | 1, name: string) => void;
	onClose: () => void;
	context: any;
}

export const AddOverridePopout: React.FC<AddOverridePopoutProps> = observer(function AddOverridePopout({
	guildId,
	existingOverwriteIds,
	onSelect,
	onClose,
	context,
}) {
	const {t} = useLingui();
	const [searchQuery, setSearchQuery] = React.useState('');
	const [activeIndex, setActiveIndex] = React.useState<number | null>(null);
	const guild = GuildStore.getGuild(guildId);

	const roles = React.useMemo(() => {
		if (!guild) return [];
		return Object.values(guild.roles)
			.filter((role) => !existingOverwriteIds.has(role.id))
			.sort((a, b) => b.position - a.position);
	}, [guild, existingOverwriteIds]);

	const members = React.useMemo(() => {
		if (!guild) return [];
		const allMembers = GuildMemberStore.getMembers(guildId);
		return Array.from(allMembers.values())
			.filter((member) => !existingOverwriteIds.has(member.user.id))
			.slice(0, 15);
	}, [guild, guildId, existingOverwriteIds]);

	const filteredRoles = React.useMemo(() => {
		if (!searchQuery) return roles;
		return matchSorter(roles, searchQuery, {keys: ['name']});
	}, [roles, searchQuery]);

	const filteredMembers = React.useMemo(() => {
		if (!searchQuery) return members;
		return matchSorter(members, searchQuery, {
			keys: [(item) => item.user.username],
		});
	}, [members, searchQuery]);

	const listItems = React.useMemo(() => {
		const items: Array<{type: 'role' | 'member'; id: string; name: string; data: any}> = [];

		filteredRoles.forEach((role) => {
			items.push({type: 'role', id: role.id, name: role.name, data: role});
		});

		filteredMembers.forEach((member) => {
			items.push({type: 'member', id: member.user.id, name: member.user.username, data: member});
		});

		return items;
	}, [filteredRoles, filteredMembers]);

	const listRef = React.useRef<Array<HTMLElement | null>>([]);

	useListNavigation(context, {
		listRef,
		activeIndex,
		onNavigate: setActiveIndex,
		loop: true,
	});

	const handleSelect = React.useCallback(
		(item: (typeof listItems)[number]) => {
			if (item.type === 'role') {
				onSelect(item.id, 0, item.name);
			} else {
				onSelect(item.id, 1, item.name);
			}
			onClose();
		},
		[onSelect, onClose],
	);

	React.useEffect(() => {
		const handleKeyDown = (e: KeyboardEvent) => {
			if (e.key === 'Enter' && activeIndex !== null && listItems[activeIndex]) {
				e.preventDefault();
				handleSelect(listItems[activeIndex]);
			}
		};

		window.addEventListener('keydown', handleKeyDown);
		return () => window.removeEventListener('keydown', handleKeyDown);
	}, [activeIndex, listItems, handleSelect]);

	return (
		<div className={styles.popoutContainer}>
			<div className={styles.searchContainer}>
				<Input
					type="text"
					placeholder={t`Search roles or members...`}
					value={searchQuery}
					onChange={(e) => setSearchQuery(e.target.value)}
					leftIcon={<MagnifyingGlassIcon size={16} weight="bold" />}
				/>
			</div>

			<Scroller className={styles.scroller} key="add-override-popout-scroller">
				{filteredRoles.length > 0 && (
					<div className={styles.section}>
						<div className={styles.sectionHeader}>
							<Trans>Roles</Trans>
						</div>
						{filteredRoles.map((role, index) => {
							const itemIndex = index;
							const isActive = activeIndex === itemIndex;

							return (
								<button
									key={role.id}
									ref={(node) => {
										listRef.current[itemIndex] = node;
									}}
									type="button"
									className={`${styles.itemButton} ${isActive ? styles.itemButtonActive : styles.itemButtonInactive}`}
									onClick={() => handleSelect(listItems[itemIndex])}
								>
									<div
										className={styles.roleIndicator}
										style={{
											backgroundColor: role.color === 0 ? DEFAULT_ROLE_COLOR_HEX : getRoleColor(role.color),
										}}
									/>
									<span className={styles.itemLabel}>{role.name}</span>
								</button>
							);
						})}
					</div>
				)}

				{filteredMembers.length > 0 && (
					<div>
						<div className={styles.sectionHeader}>
							<Trans>Members</Trans> {members.length >= 15 && <Trans>(max 15 shown)</Trans>}
						</div>
						{filteredMembers.map((member, index) => {
							const itemIndex = filteredRoles.length + index;
							const isActive = activeIndex === itemIndex;

							return (
								<button
									key={member.user.id}
									ref={(node) => {
										listRef.current[itemIndex] = node;
									}}
									type="button"
									className={`${styles.itemButton} ${isActive ? styles.itemButtonActive : styles.itemButtonInactive}`}
									onClick={() => handleSelect(listItems[itemIndex])}
								>
									<Avatar user={member.user} size={12} className={styles.avatar} guildId={guildId} />
									<span className={styles.itemLabel}>{member.user.username}</span>
								</button>
							);
						})}
					</div>
				)}

				{filteredRoles.length === 0 && filteredMembers.length === 0 && (
					<div className={styles.emptyState}>
						<Trans>No results found</Trans>
					</div>
				)}
			</Scroller>
		</div>
	);
});
