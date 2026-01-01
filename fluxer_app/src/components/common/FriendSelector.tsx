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

import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {RelationshipTypes} from '~/Constants';
import {Input, type RenderInputArgs} from '~/components/form/Input';
import {Avatar} from '~/components/uikit/Avatar';
import {Checkbox} from '~/components/uikit/Checkbox/Checkbox';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Scroller} from '~/components/uikit/Scroller';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import type {UserRecord} from '~/records/UserRecord';
import RelationshipStore from '~/stores/RelationshipStore';
import UserStore from '~/stores/UserStore';
import * as NicknameUtils from '~/utils/NicknameUtils';
import styles from './FriendSelector.module.css';

interface FriendSelectorProps {
	selectedUserIds: Array<string>;
	onToggle: (userId: string) => void;
	maxSelections?: number;
	excludeUserIds?: Array<string>;
	searchQuery?: string;
	onSearchQueryChange?: (value: string) => void;
	showSearchInput?: boolean;
	stickyUserIds?: Array<string>;
}

interface FriendGroup {
	letter: string;
	friendIds: Array<string>;
}

export const FriendSelector: React.FC<FriendSelectorProps> = observer(
	({
		selectedUserIds,
		onToggle,
		maxSelections,
		excludeUserIds = [],
		searchQuery: externalSearchQuery,
		onSearchQueryChange,
		showSearchInput = true,
		stickyUserIds = [],
	}) => {
		const {t} = useLingui();
		const [internalSearchQuery, setInternalSearchQuery] = React.useState('');
		const searchQuery = externalSearchQuery ?? internalSearchQuery;
		const [inputFocused, setInputFocused] = React.useState(false);
		const inputRef = React.useRef<HTMLInputElement | null>(null);

		const handleSearchChange = (value: string) => {
			if (onSearchQueryChange) {
				onSearchQueryChange(value);
			} else {
				setInternalSearchQuery(value);
			}
		};

		const relationships = RelationshipStore.getRelationships();
		const friendUsers = React.useMemo(() => {
			const friends = relationships.filter(
				(relationship) => relationship.type === RelationshipTypes.FRIEND && !excludeUserIds.includes(relationship.id),
			);

			return friends
				.map((relationship) => UserStore.getUser(relationship.id))
				.filter((user): user is UserRecord => Boolean(user))
				.sort((a, b) => NicknameUtils.getNickname(a).localeCompare(NicknameUtils.getNickname(b)));
		}, [relationships, excludeUserIds]);

		const activeStickyUserIds = React.useMemo(() => {
			return stickyUserIds.filter((id) => selectedUserIds.includes(id));
		}, [stickyUserIds, selectedUserIds]);

		const groupedFriends = React.useMemo(() => {
			const filtered = friendUsers.filter((user) => {
				if (!searchQuery) return true;
				return NicknameUtils.getNickname(user).toLowerCase().includes(searchQuery.toLowerCase());
			});

			const stickySet = new Set(activeStickyUserIds);
			const groups: Record<string, Array<UserRecord>> = {};

			filtered.forEach((user) => {
				if (stickySet.has(user.id)) return;
				const firstLetter = NicknameUtils.getNickname(user)[0].toUpperCase();
				if (!groups[firstLetter]) {
					groups[firstLetter] = [];
				}
				groups[firstLetter].push(user);
			});

			const groupArray: Array<FriendGroup> = Object.keys(groups)
				.sort()
				.map((letter) => ({
					letter,
					friendIds: groups[letter].map((user) => user.id),
				}));

			return groupArray;
		}, [friendUsers, searchQuery, activeStickyUserIds]);

		const handleRemovePill = (userId: string) => {
			onToggle(userId);
			if (inputRef.current) {
				inputRef.current.focus();
			}
		};

		const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
			if (e.key === 'Backspace' && searchQuery === '' && selectedUserIds.length > 0) {
				onToggle(selectedUserIds[selectedUserIds.length - 1]);
			}
		};

		const handleToggle = (userId: string) => {
			handleSearchChange('');
			onToggle(userId);
		};

		const isMaxed = maxSelections !== undefined && selectedUserIds.length >= maxSelections;

		const isMutableRefObject = (
			ref: React.Ref<HTMLInputElement> | undefined,
		): ref is React.MutableRefObject<HTMLInputElement | null> =>
			typeof ref === 'object' && ref !== null && 'current' in ref;

		const renderSearchInput = ({inputProps, inputClassName, ref: forwardedRef}: RenderInputArgs) => {
			const handleRef = (node: HTMLInputElement | null) => {
				inputRef.current = node;
				if (typeof forwardedRef === 'function') {
					forwardedRef(node);
				} else if (isMutableRefObject(forwardedRef)) {
					forwardedRef.current = node;
				}
			};

			return (
				<div className={clsx(inputClassName, styles.searchField)}>
					{selectedUserIds.map((userId) => {
						const user = UserStore.getUser(userId);
						if (!user) return null;

						return (
							<div key={userId} className={styles.selectedPill}>
								<Avatar user={user} size={16} />
								<span>{NicknameUtils.getNickname(user)}</span>
								<FocusRing offset={-2}>
									<button type="button" onClick={() => handleRemovePill(userId)} className={styles.removeButton}>
										<XIcon className={styles.removeIcon} weight="bold" />
									</button>
								</FocusRing>
							</div>
						);
					})}
					<div className={styles.searchFieldInner}>
						<MagnifyingGlassIcon
							className={clsx(styles.searchIcon, inputFocused && styles.searchIconFocused)}
							weight="bold"
						/>
						<input {...inputProps} ref={handleRef} className={styles.searchInput} spellCheck={false} />
					</div>
				</div>
			);
		};

		return (
			<div className={styles.container}>
				{showSearchInput && (
					<Input
						value={searchQuery}
						onChange={(e) => handleSearchChange(e.target.value)}
						onKeyDown={handleKeyDown}
						onFocus={() => setInputFocused(true)}
						onBlur={() => setInputFocused(false)}
						placeholder={selectedUserIds.length > 0 ? '' : t`Search friends`}
						renderInput={({inputProps, inputClassName, ref, defaultInput}) =>
							renderSearchInput({inputProps, inputClassName, ref, defaultInput})
						}
					/>
				)}

				<Scroller
					className={clsx(styles.scroller, !showSearchInput && styles.scrollerNoSearch)}
					key="friend-selector-scroller"
					fade={false}
					reserveScrollbarTrack={false}
				>
					{groupedFriends.length === 0 && activeStickyUserIds.length === 0 ? (
						<div className={styles.emptyState}>
							<p className={styles.emptyStateText}>{searchQuery ? t`No friends found` : t`You have no friends yet`}</p>
						</div>
					) : (
						<div className={styles.groupsContainer}>
							{activeStickyUserIds.length > 0 && (
								<div className={styles.friendsList}>
									{activeStickyUserIds.map((userId) => {
										const user = UserStore.getUser(userId);
										if (!user) return null;

										const isSelected = selectedUserIds.includes(userId);
										const canSelect = !isMaxed || isSelected;

										return (
											<FocusRing key={userId} offset={-2} enabled={canSelect}>
												<button
													type="button"
													onClick={() => canSelect && handleToggle(userId)}
													disabled={!canSelect}
													className={clsx(
														styles.friendButton,
														isSelected && styles.friendButtonSelected,
														!canSelect && styles.friendButtonDisabled,
													)}
												>
													<div className={styles.friendInfo}>
														<StatusAwareAvatar user={user} size={32} />
														<span className={styles.friendName}>{NicknameUtils.getNickname(user)}</span>
													</div>
													<div className={styles.checkboxContainer}>
														<Checkbox checked={isSelected} readOnly aria-hidden={true} />
													</div>
												</button>
											</FocusRing>
										);
									})}
								</div>
							)}
							{groupedFriends.map((group) => (
								<div key={group.letter}>
									<div className={styles.groupLetter}>{group.letter}</div>
									<div className={styles.friendsList}>
										{group.friendIds.map((userId) => {
											const user = UserStore.getUser(userId);
											if (!user) return null;

											const isSelected = selectedUserIds.includes(userId);
											const canSelect = !isMaxed || isSelected;

											return (
												<FocusRing key={userId} offset={-2} enabled={canSelect}>
													<button
														type="button"
														onClick={() => canSelect && handleToggle(userId)}
														disabled={!canSelect}
														className={clsx(
															styles.friendButton,
															isSelected && styles.friendButtonSelected,
															!canSelect && styles.friendButtonDisabled,
														)}
													>
														<div className={styles.friendInfo}>
															<StatusAwareAvatar user={user} size={32} />
															<span className={styles.friendName}>{NicknameUtils.getNickname(user)}</span>
														</div>
														<div className={styles.checkboxContainer}>
															<Checkbox checked={isSelected} readOnly aria-hidden={true} />
														</div>
													</button>
												</FocusRing>
											);
										})}
									</div>
								</div>
							))}
						</div>
					)}
				</Scroller>
			</div>
		);
	},
);
