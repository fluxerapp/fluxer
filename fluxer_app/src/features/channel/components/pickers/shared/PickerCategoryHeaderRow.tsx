// SPDX-License-Identifier: AGPL-3.0-or-later
import styles from '@app/features/channel/components/EmojiPicker.module.css';
import {PICKER_CATEGORY_HEADER_HEIGHT} from '@app/features/channel/components/pickers/shared/PickerRowLayout';
import type {PickerHeaderRow} from '@app/features/channel/components/pickers/shared/PickerRows';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {CaretDownIcon, ClockIcon, StarIcon} from '@phosphor-icons/react';
import type React from 'react';
export function PickerCategoryHeaderRow({
	row,
	isCollapsed,
	onToggle: handleToggleCategory,
	guildIconClassName,
}: {
	row: PickerHeaderRow;
	isCollapsed: boolean;
	onToggle: () => void;
	guildIconClassName?: string;
}) {
	let leadingIcon: React.ReactNode = null;
	if (row.category === 'favorites') {
		leadingIcon = (
			<StarIcon
				weight="fill"
				className={styles.headerIcon}
				data-flx="channel.emoji-picker.virtual-row.virtual-row-renderer.header-icon"
			/>
		);
	} else if (row.category === 'frequently-used') {
		leadingIcon = (
			<ClockIcon
				weight="fill"
				className={styles.headerIcon}
				data-flx="channel.emoji-picker.virtual-row.virtual-row-renderer.header-icon--2"
			/>
		);
	} else if (row.guildId) {
		leadingIcon = guildIconClassName ? (
			<GuildIcon
				id={row.guildId}
				name={row.name}
				icon={Guilds.getGuild(row.guildId)?.icon ?? null}
				className={guildIconClassName}
				sizePx={16}
				data-flx="channel.pickers.shared.picker-category-header-row.guild-icon--2"
			/>
		) : (
			<div className={styles.headerIcon} data-flx="channel.pickers.shared.picker-category-header-row.header-icon">
				<GuildIcon
					id={row.guildId}
					name={row.name}
					icon={Guilds.getGuild(row.guildId)?.icon ?? null}
					sizePx={16}
					data-flx="channel.pickers.shared.picker-category-header-row.guild-icon"
				/>
			</div>
		);
	}
	return (
		<button
			type="button"
			onClick={handleToggleCategory}
			style={{
				height: remFromPx(PICKER_CATEGORY_HEADER_HEIGHT),
				display: 'flex',
				alignItems: 'center',
				paddingLeft: '0.75rem',
				paddingRight: '0.75rem',
				marginBottom: '0.5rem',
				position: 'sticky',
				top: 0,
				zIndex: 1,
				cursor: 'pointer',
				border: 'none',
				width: '100%',
				textAlign: 'left',
				gap: '0.5rem',
				minWidth: 0,
			}}
			data-flx="channel.emoji-picker.virtual-row.virtual-row-renderer.category-title.toggle-category.button"
		>
			{leadingIcon}
			<div
				style={{display: 'flex', alignItems: 'center', flex: '1 1 auto', minWidth: 0}}
				data-flx="channel.emoji-picker.virtual-row.virtual-row-renderer.div"
			>
				<span
					className={styles.categoryTitle}
					style={{
						minWidth: 0,
						overflow: 'hidden',
						textOverflow: 'ellipsis',
						whiteSpace: 'nowrap',
						flex: '0 1 auto',
					}}
					data-flx="channel.emoji-picker.virtual-row.virtual-row-renderer.category-title"
				>
					{row.name}
				</span>
				<CaretDownIcon
					weight="bold"
					className={styles.caretIcon}
					style={{transform: `rotate(${isCollapsed ? -90 : 0}deg)`, marginLeft: '0.5rem'}}
					data-flx="channel.emoji-picker.virtual-row.virtual-row-renderer.caret-icon"
				/>
			</div>
		</button>
	);
}
