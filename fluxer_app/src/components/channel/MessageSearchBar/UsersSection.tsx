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
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {StatusAwareAvatar} from '~/components/uikit/StatusAwareAvatar';
import type {UserRecord} from '~/records/UserRecord';
import GuildStore from '~/stores/GuildStore';
import * as NicknameUtils from '~/utils/NicknameUtils';
import {AutocompleteOption} from './AutocompleteOption';
import styles from './MessageSearchBar.module.css';

interface UsersSectionProps {
	options: Array<UserRecord>;
	selectedIndex: number;
	hoverIndex: number;
	onSelect: (user: UserRecord) => void;
	onMouseEnter: (index: number) => void;
	onMouseLeave?: () => void;
	listboxId: string;
	guildId?: string;
	isInGuild: boolean;
}

export const UsersSection: React.FC<UsersSectionProps> = observer(
	({options, selectedIndex, hoverIndex, onSelect, onMouseEnter, onMouseLeave, listboxId, guildId, isInGuild}) => {
		const {t} = useLingui();

		if (options.length === 0) return null;

		return (
			<div className={styles.popoutSection}>
				<div className={styles.popoutSectionHeader}>
					<span className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}>
						<MagnifyingGlassIcon weight="regular" size={14} />
						{t`Users`}
					</span>
				</div>
				{options.map((user: UserRecord, index) => {
					const guild = isInGuild && guildId ? GuildStore.getGuild(guildId) : null;
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
						>
							<div className={styles.optionLabel}>
								<div className={styles.optionContent}>
									<div className={styles.optionText}>
										<div className={styles.optionTitle}>
											<span className={`${styles.userRow} ${styles.gap1}`}>
												<span className={`${styles.userRow} ${styles.gap2}`}>
													<StatusAwareAvatar user={user} size={16} />
													<span className={`${styles.minW0} ${styles.overflowHidden}`}>{nickname}</span>
												</span>
												<span className={styles.userTag}>{user.tag}</span>
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
