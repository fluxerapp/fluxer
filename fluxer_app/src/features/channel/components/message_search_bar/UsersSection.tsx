// SPDX-License-Identifier: AGPL-3.0-or-later

import {AutocompleteOption} from '@app/features/channel/components/message_search_bar/AutocompleteOption';
import styles from '@app/features/channel/components/message_search_bar/MessageSearchBar.module.css';
import Guilds from '@app/features/guild/state/Guilds';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {StatusAwareAvatar} from '@app/features/ui/components/StatusAwareAvatar';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const USERS_DESCRIPTOR = msg({
	message: 'Users',
	comment:
		'Section header in the message search popout listing matching users for from: / mentions: filters. Title Case.',
});

interface UsersSectionProps {
	options: Array<User>;
	selectedIndex: number;
	hoverIndex: number;
	onSelect: (user: User) => void;
	onMouseEnter: (index: number) => void;
	onMouseLeave?: () => void;
	listboxId: string;
	guildId?: string;
	isInGuild: boolean;
}

export const UsersSection: React.FC<UsersSectionProps> = observer(
	({options, selectedIndex, hoverIndex, onSelect, onMouseEnter, onMouseLeave, listboxId, guildId, isInGuild}) => {
		const {i18n} = useLingui();
		if (options.length === 0) return null;
		return (
			<div className={styles.popoutSection} data-flx="channel.message-search-bar.users-section.popout-section">
				<div
					className={styles.popoutSectionHeader}
					data-flx="channel.message-search-bar.users-section.popout-section-header"
				>
					<span
						className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}
						data-flx="channel.message-search-bar.users-section.flex"
					>
						<MagnifyingGlassIcon
							weight="regular"
							size={remFromPx(14)}
							data-flx="channel.message-search-bar.users-section.magnifying-glass-icon"
						/>
						{i18n._(USERS_DESCRIPTOR)}
					</span>
				</div>
				{options.map((user: User, index) => {
					const guild = isInGuild && guildId ? Guilds.getGuild(guildId) : null;
					const nickname = NicknameUtils.getNickname(user, guild?.id);
					return (
						<AutocompleteOption
							key={user.id}
							index={index}
							isSelected={index === selectedIndex}
							isHovered={index === hoverIndex}
							onSelect={() => onSelect(user)}
							onMouseEnter={() => onMouseEnter(index)}
							onMouseLeave={onMouseLeave}
							listboxId={listboxId}
							data-flx="channel.message-search-bar.users-section.autocomplete-option.select"
						>
							<div className={styles.optionLabel} data-flx="channel.message-search-bar.users-section.option-label">
								<div
									className={styles.optionContent}
									data-flx="channel.message-search-bar.users-section.option-content"
								>
									<div className={styles.optionText} data-flx="channel.message-search-bar.users-section.option-text">
										<div
											className={styles.optionTitle}
											data-flx="channel.message-search-bar.users-section.option-title"
										>
											<span
												className={`${styles.userRow} ${styles.gap1}`}
												data-flx="channel.message-search-bar.users-section.user-row"
											>
												<span
													className={`${styles.userRow} ${styles.gap2}`}
													data-flx="channel.message-search-bar.users-section.user-row--2"
												>
													<StatusAwareAvatar
														user={user}
														size={16}
														data-flx="channel.message-search-bar.users-section.status-aware-avatar"
													/>
													<span
														className={`${styles.minW0} ${styles.overflowHidden}`}
														data-flx="channel.message-search-bar.users-section.min-w0"
													>
														{nickname}
													</span>
												</span>
												<span className={styles.userTag} data-flx="channel.message-search-bar.users-section.user-tag">
													{NicknameUtils.formatUserTagForStreamerMode(user)}
												</span>
											</span>
										</div>
									</div>
								</div>
							</div>
						</AutocompleteOption>
					);
				})}
			</div>
		);
	},
);
