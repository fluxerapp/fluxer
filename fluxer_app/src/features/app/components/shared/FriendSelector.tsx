// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/shared/FriendSelector.module.css';
import {SEARCH_FRIENDS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Relationships from '@app/features/relationship/state/Relationships';
import {Checkbox} from '@app/features/ui/checkbox/Checkbox';
import {Avatar} from '@app/features/ui/components/Avatar';
import {Input, type RenderInputArgs} from '@app/features/ui/components/form/FormInput';
import {Scroller} from '@app/features/ui/components/Scroller';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import type {User} from '@app/features/user/models/User';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {RelationshipTypes} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useMemo, useRef, useState} from 'react';

const REMOVE_DESCRIPTOR = msg({
	message: 'Remove {displayName}',
	comment: 'ARIA label for removing a selected friend from a recipient picker. {displayName} is the friend name.',
});
const NO_FRIENDS_FOUND_DESCRIPTOR = msg({
	message: 'No friends found',
	comment: 'Short label in the shared app friend selector.',
});
const YOU_HAVE_NO_FRIENDS_YET_DESCRIPTOR = msg({
	message: 'You have no friends yet',
	comment: 'Short label in the shared app friend selector.',
});

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
		const {i18n} = useLingui();
		const [internalSearchQuery, setInternalSearchQuery] = useState('');
		const searchQuery = externalSearchQuery ?? internalSearchQuery;
		const [inputFocused, setInputFocused] = useState(false);
		const inputRef = useRef<HTMLInputElement | null>(null);
		const handleSearchChange = (value: string) => {
			if (onSearchQueryChange) {
				onSearchQueryChange(value);
			} else {
				setInternalSearchQuery(value);
			}
		};
		const relationships = Relationships.getRelationships();
		const friendUsers = useMemo(() => {
			const friends = relationships.filter(
				(relationship) => relationship.type === RelationshipTypes.FRIEND && !excludeUserIds.includes(relationship.id),
			);
			return friends
				.map((relationship) => Users.getUser(relationship.id))
				.filter((user): user is User => Boolean(user))
				.sort((a, b) => NicknameUtils.getNickname(a, null).localeCompare(NicknameUtils.getNickname(b, null)));
		}, [relationships, excludeUserIds]);
		const activeStickyUserIds = useMemo(() => {
			return stickyUserIds.filter((id) => selectedUserIds.includes(id));
		}, [stickyUserIds, selectedUserIds]);
		const groupedFriends = useMemo(() => {
			const filtered = friendUsers.filter((user) => {
				if (!searchQuery) return true;
				return NicknameUtils.getNickname(user, null).toLowerCase().includes(searchQuery.toLowerCase());
			});
			const stickySet = new Set(activeStickyUserIds);
			const groups: Record<string, Array<User>> = {};
			filtered.forEach((user) => {
				if (stickySet.has(user.id)) return;
				const firstLetter = NicknameUtils.getNickname(user, null)[0].toUpperCase();
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
				<div
					className={clsx(inputClassName, styles.searchField)}
					data-flx="app.friend-selector.render-search-input.search-field"
				>
					{selectedUserIds.map((userId) => {
						const user = Users.getUser(userId);
						if (!user) return null;
						return (
							<div
								key={userId}
								className={styles.selectedPill}
								data-flx="app.friend-selector.render-search-input.selected-pill"
							>
								<Avatar user={user} size={16} data-flx="app.friend-selector.render-search-input.avatar" />
								<span data-flx="app.friend-selector.render-search-input.span">
									{NicknameUtils.getNickname(user, null)}
								</span>
								<FocusRing offset={-2} data-flx="app.friend-selector.render-search-input.focus-ring">
									<button
										type="button"
										onClick={() => handleRemovePill(userId)}
										className={styles.removeButton}
										aria-label={i18n._(REMOVE_DESCRIPTOR, {
											displayName: NicknameUtils.getNickname(user, null),
										})}
										data-flx="app.friend-selector.render-search-input.remove-button.remove-pill"
									>
										<XIcon
											className={styles.removeIcon}
											weight="bold"
											data-flx="app.friend-selector.render-search-input.remove-icon"
										/>
									</button>
								</FocusRing>
							</div>
						);
					})}
					<div
						className={styles.searchFieldInner}
						data-flx="app.friend-selector.render-search-input.search-field-inner"
					>
						<MagnifyingGlassIcon
							className={clsx(styles.searchIcon, inputFocused && styles.searchIconFocused)}
							weight="bold"
							data-flx="app.friend-selector.render-search-input.search-icon"
						/>
						<input
							data-flx="app.friend-selector.render-search-input.search-input"
							{...inputProps}
							ref={handleRef}
							className={styles.searchInput}
							spellCheck={false}
						/>
					</div>
				</div>
			);
		};
		return (
			<div className={styles.container} data-flx="app.friend-selector.container">
				{showSearchInput && (
					<Input
						value={searchQuery}
						onChange={(e) => handleSearchChange(e.target.value)}
						onKeyDown={handleKeyDown}
						onFocus={() => setInputFocused(true)}
						onBlur={() => setInputFocused(false)}
						placeholder={selectedUserIds.length > 0 ? '' : i18n._(SEARCH_FRIENDS_DESCRIPTOR)}
						renderInput={({inputProps, inputClassName, ref, defaultInput}) =>
							renderSearchInput({inputProps, inputClassName, ref, defaultInput})
						}
						data-flx="app.friend-selector.input.search-change"
					/>
				)}
				<Scroller
					className={clsx(styles.scroller, !showSearchInput && styles.scrollerNoSearch)}
					key="friend-selector-scroller"
					fade={false}
					data-flx="app.friend-selector.scroller"
				>
					{groupedFriends.length === 0 && activeStickyUserIds.length === 0 ? (
						<div className={styles.emptyState} data-flx="app.friend-selector.empty-state">
							<p className={styles.emptyStateText} data-flx="app.friend-selector.empty-state-text">
								{searchQuery ? i18n._(NO_FRIENDS_FOUND_DESCRIPTOR) : i18n._(YOU_HAVE_NO_FRIENDS_YET_DESCRIPTOR)}
							</p>
						</div>
					) : (
						<div className={styles.groupsContainer} data-flx="app.friend-selector.groups-container">
							{activeStickyUserIds.length > 0 && (
								<div className={styles.friendsList} data-flx="app.friend-selector.friends-list">
									{activeStickyUserIds.map((userId) => {
										const user = Users.getUser(userId);
										if (!user) return null;
										const isSelected = selectedUserIds.includes(userId);
										const canSelect = !isMaxed || isSelected;
										return (
											<FocusRing key={userId} offset={-2} enabled={canSelect} data-flx="app.friend-selector.focus-ring">
												<button
													type="button"
													onClick={() => canSelect && handleToggle(userId)}
													disabled={!canSelect}
													aria-pressed={isSelected}
													className={clsx(
														styles.friendButton,
														isSelected && styles.friendButtonSelected,
														!canSelect && styles.friendButtonDisabled,
													)}
													data-flx="app.friend-selector.friend-button"
												>
													<div className={styles.friendInfo} data-flx="app.friend-selector.friend-info">
														<StatusAwareAvatar
															user={user}
															size={32}
															data-flx="app.friend-selector.status-aware-avatar"
														/>
														<span className={styles.friendName} data-flx="app.friend-selector.friend-name">
															{NicknameUtils.getNickname(user, null)}
														</span>
													</div>
													<div className={styles.checkboxContainer} data-flx="app.friend-selector.checkbox-container">
														<Checkbox
															checked={isSelected}
															readOnly
															aria-hidden={true}
															data-flx="app.friend-selector.checkbox"
														/>
													</div>
												</button>
											</FocusRing>
										);
									})}
								</div>
							)}
							{groupedFriends.map((group) => (
								<div key={group.letter} data-flx="app.friend-selector.div">
									<div className={styles.groupLetter} data-flx="app.friend-selector.group-letter">
										{group.letter}
									</div>
									<div className={styles.friendsList} data-flx="app.friend-selector.friends-list--2">
										{group.friendIds.map((userId) => {
											const user = Users.getUser(userId);
											if (!user) return null;
											const isSelected = selectedUserIds.includes(userId);
											const canSelect = !isMaxed || isSelected;
											return (
												<FocusRing
													key={userId}
													offset={-2}
													enabled={canSelect}
													data-flx="app.friend-selector.focus-ring--2"
												>
													<button
														type="button"
														onClick={() => canSelect && handleToggle(userId)}
														disabled={!canSelect}
														aria-pressed={isSelected}
														className={clsx(
															styles.friendButton,
															isSelected && styles.friendButtonSelected,
															!canSelect && styles.friendButtonDisabled,
														)}
														data-flx="app.friend-selector.friend-button--2"
													>
														<div className={styles.friendInfo} data-flx="app.friend-selector.friend-info--2">
															<StatusAwareAvatar
																user={user}
																size={32}
																data-flx="app.friend-selector.status-aware-avatar--2"
															/>
															<span className={styles.friendName} data-flx="app.friend-selector.friend-name--2">
																{NicknameUtils.getNickname(user, null)}
															</span>
														</div>
														<div
															className={styles.checkboxContainer}
															data-flx="app.friend-selector.checkbox-container--2"
														>
															<Checkbox
																checked={isSelected}
																readOnly
																aria-hidden={true}
																data-flx="app.friend-selector.checkbox--2"
															/>
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
