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

import styles from '@app/components/search/UserFilterSheet.module.css';
import {Avatar} from '@app/components/uikit/Avatar';
import {BottomSheet} from '@app/components/uikit/bottom_sheet/BottomSheet';
import {Button} from '@app/components/uikit/button/Button';
import {Scroller} from '@app/components/uikit/Scroller';
import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/lib/PasswordManagerAutocomplete';
import type {ChannelRecord} from '@app/records/ChannelRecord';
import type {UserRecord} from '@app/records/UserRecord';
import GuildMemberStore from '@app/stores/GuildMemberStore';
import GuildStore from '@app/stores/GuildStore';
import UserStore from '@app/stores/UserStore';
import * as NicknameUtils from '@app/utils/NicknameUtils';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckIcon, MagnifyingGlassIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo, useState} from 'react';

interface UserFilterSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channel: ChannelRecord;
	selectedUserIds: Array<string>;
	onUsersChange: (userIds: Array<string>) => void;
	title?: string;
}

export const UserFilterSheet: React.FC<UserFilterSheetProps> = observer(
	({isOpen, onClose, channel, selectedUserIds, onUsersChange, title}) => {
		const {t} = useLingui();
		const [searchTerm, setSearchTerm] = useState('');

		useEffect(() => {
			if (isOpen) {
				setSearchTerm('');
			}
		}, [isOpen]);

		const availableUsers = useMemo((): Array<UserRecord> => {
			if (channel.guildId) {
				const members = GuildMemberStore.getMembers(channel.guildId);
				return members.map((m) => m.user);
			}
			return channel.recipientIds.map((id) => UserStore.getUser(id)).filter((u): u is UserRecord => u != null);
		}, [channel.guildId, channel.recipientIds]);

		const filteredUsers = useMemo(() => {
			if (!searchTerm.trim()) {
				return availableUsers.slice(0, 50);
			}
			const guild = channel.guildId ? GuildStore.getGuild(channel.guildId) : null;
			return matchSorter(availableUsers, searchTerm, {
				keys: [(user) => NicknameUtils.getNickname(user, guild?.id), 'username'],
			}).slice(0, 50);
		}, [availableUsers, searchTerm, channel.guildId]);

		const toggleUser = (userId: string) => {
			if (selectedUserIds.includes(userId)) {
				onUsersChange(selectedUserIds.filter((id) => id !== userId));
			} else {
				onUsersChange([...selectedUserIds, userId]);
			}
		};

		return (
			<BottomSheet
				isOpen={isOpen}
				onClose={onClose}
				snapPoints={[0, 1]}
				initialSnap={1}
				title={title ?? t`Filter by user`}
				disablePadding
			>
				<div className={styles.container}>
					<div className={styles.searchContainer}>
						<div className={styles.searchInputWrapper}>
							<MagnifyingGlassIcon size={20} className={styles.searchIcon} weight="regular" />
							<input
								type="text"
								className={styles.searchInput}
								placeholder={t`Search users`}
								value={searchTerm}
								onChange={(e) => setSearchTerm(e.target.value)}
								{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
								autoComplete="off"
								autoCorrect="off"
								autoCapitalize="off"
							/>
							{searchTerm.length > 0 && (
								<button type="button" className={styles.clearButton} onClick={() => setSearchTerm('')}>
									<XIcon size={18} weight="bold" />
								</button>
							)}
						</div>
					</div>

					<Scroller key="user-filter-scroller" className={styles.scroller} fade={false}>
						<div className={styles.listContent}>
							{filteredUsers.length === 0 ? (
								<div className={styles.emptyState}>
									{searchTerm ? <Trans>No users found</Trans> : <Trans>No users available</Trans>}
								</div>
							) : (
								filteredUsers.map((user) => {
									const isSelected = selectedUserIds.includes(user.id);
									const guild = channel.guildId ? GuildStore.getGuild(channel.guildId) : null;
									const displayName = NicknameUtils.getNickname(user, guild?.id);

									return (
										<button
											key={user.id}
											type="button"
											className={clsx(styles.userItem, isSelected && styles.userItemSelected)}
											onClick={() => toggleUser(user.id)}
										>
											<Avatar user={user} size={36} status={null} className={styles.avatar} />
											<div className={styles.userInfo}>
												<span className={styles.displayName}>{displayName}</span>
												<span className={styles.username}>{user.tag}</span>
											</div>
											{isSelected && <CheckIcon size={20} className={styles.checkIcon} weight="bold" />}
										</button>
									);
								})
							)}
						</div>
					</Scroller>

					<div className={styles.footer}>
						<Button variant="primary" onClick={onClose}>
							<Trans>Done</Trans>
						</Button>
					</div>
				</div>
			</BottomSheet>
		);
	},
);
