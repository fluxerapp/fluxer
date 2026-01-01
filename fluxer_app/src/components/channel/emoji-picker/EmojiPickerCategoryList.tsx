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
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import styles from '~/components/channel/EmojiPicker.module.css';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import UnicodeEmojis from '~/lib/UnicodeEmojis';
import type {Emoji} from '~/stores/EmojiStore';
import GuildStore from '~/stores/GuildStore';

interface EmojiPickerCategoryListProps {
	customEmojisByGuildId: Map<string, Array<Emoji>>;
	unicodeEmojisByCategory: Map<string, Array<Emoji>>;
	handleCategoryClick: (category: string) => void;
	horizontal?: boolean;
}

export const EmojiPickerCategoryList = observer(
	({
		customEmojisByGuildId,
		unicodeEmojisByCategory,
		handleCategoryClick,
		horizontal = false,
	}: EmojiPickerCategoryListProps) => {
		const {i18n} = useLingui();
		if (horizontal) {
			return (
				<div className={styles.horizontalCategories}>
					{Array.from(customEmojisByGuildId.keys()).map((guildId) => {
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
					{Array.from(unicodeEmojisByCategory.keys()).map((category) => {
						const Icon = UnicodeEmojis.getCategoryIcon(category);
						return (
							<button
								key={category}
								type="button"
								onClick={() => handleCategoryClick(category)}
								className={clsx(styles.categoryListIcon, styles.textPrimaryMuted)}
								aria-label={UnicodeEmojis.getCategoryLabel(category, i18n)}
							>
								<Icon className={styles.iconSize} />
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
						{Array.from(customEmojisByGuildId.keys()).map((guildId) => {
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
						{Array.from(unicodeEmojisByCategory.keys()).map((category) => {
							const Icon = UnicodeEmojis.getCategoryIcon(category);
							return (
								<Tooltip key={category} text={UnicodeEmojis.getCategoryLabel(category, i18n)} position="left">
									<button
										type="button"
										onClick={() => handleCategoryClick(category)}
										className={clsx(styles.categoryListIcon, styles.textPrimaryMuted)}
									>
										<Icon className={styles.iconSize} />
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
