// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import Guilds from '@app/features/guild/state/Guilds';
import GuildMembers from '@app/features/member/state/GuildMembers';
import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import styles from '@app/features/search/components/search/UserFilterSheet.module.css';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Button} from '@app/features/ui/button/Button';
import {Avatar} from '@app/features/ui/components/Avatar';
import {Scroller} from '@app/features/ui/components/Scroller';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckIcon, MagnifyingGlassIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {matchSorter} from 'match-sorter';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo, useState} from 'react';

const FILTER_BY_USER_DESCRIPTOR = msg({
	message: 'Filter by user',
	comment: 'Short label in the search user filter sheet. Keep it concise.',
});
const SEARCH_USERS_DESCRIPTOR = msg({
	message: 'Search users',
	comment: 'Button or menu action label in the search user filter sheet. Keep it concise.',
});

interface UserFilterSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channel: Channel;
	selectedUserIds: Array<string>;
	onUsersChange: (userIds: Array<string>) => void;
	title?: string;
}

export const UserFilterSheet: React.FC<UserFilterSheetProps> = observer(
	({isOpen, onClose, channel, selectedUserIds, onUsersChange, title}) => {
		const {i18n} = useLingui();
		const [searchTerm, setSearchTerm] = useState('');
		useEffect(() => {
			if (isOpen) {
				setSearchTerm('');
			}
		}, [isOpen]);
		const availableUsers = useMemo((): Array<User> => {
			if (channel.guildId) {
				const members = GuildMembers.getMembers(channel.guildId);
				return members.map((m) => m.user);
			}
			return channel.recipientIds.map((id) => Users.getUser(id)).filter((u): u is User => u != null);
		}, [channel.guildId, channel.recipientIds]);
		const filteredUsers = useMemo(() => {
			if (!searchTerm.trim()) {
				return availableUsers.slice(0, 50);
			}
			const guild = channel.guildId ? Guilds.getGuild(channel.guildId) : null;
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
				title={title ?? i18n._(FILTER_BY_USER_DESCRIPTOR)}
				disablePadding
				data-flx="search.search.user-filter-sheet.bottom-sheet"
			>
				<div className={styles.container} data-flx="search.search.user-filter-sheet.container">
					<div className={styles.searchContainer} data-flx="search.search.user-filter-sheet.search-container">
						<div className={styles.searchInputWrapper} data-flx="search.search.user-filter-sheet.search-input-wrapper">
							<MagnifyingGlassIcon
								size={remFromPx(20)}
								className={styles.searchIcon}
								weight="regular"
								data-flx="search.search.user-filter-sheet.search-icon"
							/>
							<input
								type="text"
								className={styles.searchInput}
								placeholder={i18n._(SEARCH_USERS_DESCRIPTOR)}
								value={searchTerm}
								onChange={(e) => setSearchTerm(e.target.value)}
								data-flx="search.search.user-filter-sheet.search-input.set-search-term.text"
								{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
								autoComplete="off"
								autoCorrect="off"
								autoCapitalize="off"
							/>
							{searchTerm.length > 0 && (
								<button
									type="button"
									className={styles.clearButton}
									onClick={() => setSearchTerm('')}
									data-flx="search.search.user-filter-sheet.clear-button.set-search-term"
								>
									<XIcon size={remFromPx(18)} weight="bold" data-flx="search.search.user-filter-sheet.x-icon" />
								</button>
							)}
						</div>
					</div>
					<Scroller
						key="user-filter-scroller"
						className={styles.scroller}
						fade={false}
						data-flx="search.search.user-filter-sheet.scroller"
					>
						<div className={styles.listContent} data-flx="search.search.user-filter-sheet.list-content">
							{filteredUsers.length === 0 ? (
								<div className={styles.emptyState} data-flx="search.search.user-filter-sheet.empty-state">
									{searchTerm ? <Trans>No users found</Trans> : <Trans>No users available</Trans>}
								</div>
							) : (
								filteredUsers.map((user) => {
									const isSelected = selectedUserIds.includes(user.id);
									const guild = channel.guildId ? Guilds.getGuild(channel.guildId) : null;
									const displayName = NicknameUtils.getNickname(user, guild?.id);
									return (
										<button
											key={user.id}
											type="button"
											aria-pressed={isSelected}
											className={clsx(styles.userItem, isSelected && styles.userItemSelected)}
											onClick={() => toggleUser(user.id)}
											data-flx="search.search.user-filter-sheet.user-item.toggle-user.button"
										>
											<Avatar
												user={user}
												size={36}
												status={null}
												className={styles.avatar}
												data-flx="search.search.user-filter-sheet.avatar"
											/>
											<div className={styles.userInfo} data-flx="search.search.user-filter-sheet.user-info">
												<span className={styles.displayName} data-flx="search.search.user-filter-sheet.display-name">
													{displayName}
												</span>
												<span className={styles.username} data-flx="search.search.user-filter-sheet.username">
													{NicknameUtils.formatUserTagForStreamerMode(user)}
												</span>
											</div>
											{isSelected && (
												<CheckIcon
													size={remFromPx(20)}
													className={styles.checkIcon}
													weight="bold"
													data-flx="search.search.user-filter-sheet.check-icon"
												/>
											)}
										</button>
									);
								})
							)}
						</div>
					</Scroller>
					<div className={styles.footer} data-flx="search.search.user-filter-sheet.footer">
						<Button variant="primary" onClick={onClose} data-flx="search.search.user-filter-sheet.button.close">
							<Trans>Done</Trans>
						</Button>
					</div>
				</div>
			</BottomSheet>
		);
	},
);
