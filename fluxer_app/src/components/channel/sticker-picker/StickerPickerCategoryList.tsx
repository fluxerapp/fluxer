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

import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import styles from '~/components/channel/EmojiPicker.module.css';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';
import GuildStore from '~/stores/GuildStore';

interface StickerPickerCategoryListProps {
	stickersByGuildId: ReadonlyMap<string, ReadonlyArray<GuildStickerRecord>>;
	handleCategoryClick: (category: string) => void;
	horizontal?: boolean;
}

export const StickerPickerCategoryList = observer(
	({stickersByGuildId, handleCategoryClick, horizontal = false}: StickerPickerCategoryListProps) => {
		if (horizontal) {
			return (
				<div className={styles.horizontalCategories}>
					{Array.from(stickersByGuildId.keys()).map((guildId) => {
						const guild = GuildStore.getGuild(guildId)!;
						return (
							<button
								key={guild.id}
								type="button"
								onClick={() => handleCategoryClick(guild.id)}
								className={clsx(styles.categoryListIcon, styles.textPrimaryMuted)}
								aria-label={guild.name}
							>
								<GuildIcon id={guild.id} name={guild.name} icon={guild.icon} className={styles.iconSize} sizePx={24} />
							</button>
						);
					})}
				</div>
			);
		}

		return (
			<div className={styles.categoryList}>
				<div className={styles.categoryListScroll}>
					<div className={styles.listItems}>
						{Array.from(stickersByGuildId.keys()).map((guildId) => {
							const guild = GuildStore.getGuild(guildId)!;
							return (
								<Tooltip key={guild.id} text={guild.name} position="left">
									<button
										type="button"
										onClick={() => handleCategoryClick(guild.id)}
										className={clsx(styles.categoryListIcon, styles.textPrimaryMuted)}
									>
										<GuildIcon
											id={guild.id}
											name={guild.name}
											icon={guild.icon}
											className={styles.iconSize}
											sizePx={24}
										/>
									</button>
								</Tooltip>
							);
						})}
					</div>
				</div>
			</div>
		);
	},
);
