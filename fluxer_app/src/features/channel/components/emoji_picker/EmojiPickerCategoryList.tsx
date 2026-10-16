// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/EmojiPicker.module.css';

import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import {Scroller} from '@app/features/ui/components/Scroller';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ClockIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

const FREQUENTLY_USED_DESCRIPTOR = msg({
	message: 'Frequently used',
	comment: 'Short label in the channel and chat emoji picker category list. Keep it concise.',
});

interface EmojiPickerCategoryListProps {
	customEmojisByGuildId: ReadonlyMap<string, unknown>;
	unicodeEmojisByCategory: ReadonlyMap<string, unknown>;
	handleCategoryClick: (category: string) => void;
	showFrequentlyUsedButton: boolean;
	horizontal?: boolean;
}

export const EmojiPickerCategoryList = observer(
	({
		customEmojisByGuildId,
		unicodeEmojisByCategory,
		handleCategoryClick,
		showFrequentlyUsedButton = false,
		horizontal = false,
	}: EmojiPickerCategoryListProps) => {
		const {i18n} = useLingui();
		if (horizontal) {
			return (
				<div
					className={styles.horizontalCategories}
					data-flx="channel.emoji-picker.emoji-picker-category-list.horizontal-categories"
				>
					{showFrequentlyUsedButton && (
						<button
							type="button"
							onClick={() => handleCategoryClick('frequently-used')}
							className={clsx(styles.categoryListIcon, styles.textPrimaryMuted)}
							aria-label={i18n._(FREQUENTLY_USED_DESCRIPTOR)}
							data-flx="channel.emoji-picker.emoji-picker-category-list.category-list-icon.category-click.button"
						>
							<ClockIcon
								className={styles.iconSize}
								data-flx="channel.emoji-picker.emoji-picker-category-list.icon-size"
							/>
						</button>
					)}
					{Array.from(customEmojisByGuildId.keys()).map((guildId) => {
						const guild = Guilds.getGuild(guildId)!;
						return (
							<button
								key={guild.id}
								type="button"
								onClick={() => handleCategoryClick(guild.id)}
								className={clsx(styles.categoryListIcon, styles.textPrimaryMuted)}
								aria-label={guild.name}
								data-flx="channel.emoji-picker.emoji-picker-category-list.category-list-icon.category-click.button--2"
							>
								<GuildIcon
									id={guild.id}
									name={guild.name}
									icon={guild.icon}
									className={styles.iconSize}
									sizePx={24}
									data-flx="channel.emoji-picker.emoji-picker-category-list.icon-size--2"
								/>
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
								data-flx="channel.emoji-picker.emoji-picker-category-list.category-list-icon.category-click.button--3"
							>
								<Icon
									className={styles.iconSize}
									data-flx="channel.emoji-picker.emoji-picker-category-list.icon-size--3"
								/>
							</button>
						);
					})}
				</div>
			);
		}
		return (
			<div className={styles.categoryList} data-flx="channel.emoji-picker.emoji-picker-category-list.category-list">
				<Scroller
					className={styles.categoryListScroll}
					key="emoji-picker-category-list-scroller"
					fade={false}
					showTrack={false}
					data-flx="channel.emoji-picker.emoji-picker-category-list.category-list-scroll"
				>
					<div className={styles.listItems} data-flx="channel.emoji-picker.emoji-picker-category-list.list-items">
						{showFrequentlyUsedButton && (
							<Tooltip
								text={i18n._(FREQUENTLY_USED_DESCRIPTOR)}
								position="left"
								data-flx="channel.emoji-picker.emoji-picker-category-list.tooltip"
							>
								<button
									type="button"
									onClick={() => handleCategoryClick('frequently-used')}
									className={clsx(styles.categoryListIcon, styles.textPrimaryMuted)}
									data-flx="channel.emoji-picker.emoji-picker-category-list.category-list-icon.category-click.button--4"
								>
									<ClockIcon
										className={styles.iconSize}
										data-flx="channel.emoji-picker.emoji-picker-category-list.icon-size--4"
									/>
								</button>
							</Tooltip>
						)}
						{Array.from(customEmojisByGuildId.keys()).map((guildId) => {
							const guild = Guilds.getGuild(guildId)!;
							return (
								<Tooltip
									key={guild.id}
									text={guild.name}
									position="left"
									data-flx="channel.emoji-picker.emoji-picker-category-list.tooltip--2"
								>
									<button
										type="button"
										onClick={() => handleCategoryClick(guild.id)}
										className={clsx(styles.categoryListIcon, styles.textPrimaryMuted)}
										data-flx="channel.emoji-picker.emoji-picker-category-list.category-list-icon.category-click.button--5"
									>
										<GuildIcon
											id={guild.id}
											name={guild.name}
											icon={guild.icon}
											className={styles.iconSize}
											sizePx={24}
											data-flx="channel.emoji-picker.emoji-picker-category-list.icon-size--5"
										/>
									</button>
								</Tooltip>
							);
						})}
						{Array.from(unicodeEmojisByCategory.keys()).map((category) => {
							const Icon = UnicodeEmojis.getCategoryIcon(category);
							return (
								<Tooltip
									key={category}
									text={UnicodeEmojis.getCategoryLabel(category, i18n)}
									position="left"
									data-flx="channel.emoji-picker.emoji-picker-category-list.tooltip--3"
								>
									<button
										type="button"
										onClick={() => handleCategoryClick(category)}
										className={clsx(styles.categoryListIcon, styles.textPrimaryMuted)}
										data-flx="channel.emoji-picker.emoji-picker-category-list.category-list-icon.category-click.button--6"
									>
										<Icon
											className={styles.iconSize}
											data-flx="channel.emoji-picker.emoji-picker-category-list.icon-size--6"
										/>
									</button>
								</Tooltip>
							);
						})}
					</div>
				</Scroller>
			</div>
		);
	},
);
