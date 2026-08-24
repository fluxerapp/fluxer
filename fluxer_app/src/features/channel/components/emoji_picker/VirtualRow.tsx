// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/EmojiPicker.module.css';
import {
	CATEGORY_HEADER_HEIGHT,
	EMOJI_ROW_HEIGHT,
} from '@app/features/channel/components/emoji_picker/EmojiPickerConstants';
import {EmojiRenderer} from '@app/features/channel/components/emoji_picker/EmojiRenderer';
import type {Channel} from '@app/features/channel/models/Channel';
import * as EmojiPickerCommands from '@app/features/emoji/commands/EmojiPickerCommands';
import EmojiPicker from '@app/features/emoji/state/EmojiPicker';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {CaretDownIcon, ClockIcon, StarIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';

export type VirtualRow =
	| {type: 'header'; category: string; name: string; guildId?: string; index: number}
	| {type: 'emoji-row'; emojis: Array<FlatEmoji>; index: number; isCustomEmoji?: boolean; guildId?: string};

interface VirtualRowRendererProps {
	row: VirtualRow;
	handleHover: (emoji: FlatEmoji | null, row?: number, column?: number) => void;
	handleSelect: (emoji: FlatEmoji, shiftKey?: boolean) => void;
	skinTone: string;
	channel: Channel | null;
	shouldAnimate: boolean;
	gridColumns?: number;
	gridWidth?: number;
	cellTrackWidth?: number;
	selectedColumnInRow: number;
	emojiRowIndex: number;
	shouldScrollOnSelection?: boolean;
	emojiRefs: React.MutableRefObject<Map<string, HTMLButtonElement>>;
}

const VirtualRowRenderer: React.FC<VirtualRowRendererProps> = React.memo(
	({
		row,
		handleHover,
		handleSelect,
		skinTone,
		channel,
		shouldAnimate,
		gridColumns = 9,
		gridWidth,
		cellTrackWidth,
		selectedColumnInRow,
		emojiRowIndex,
		shouldScrollOnSelection = false,
		emojiRefs,
	}) => {
		if (row.type === 'header') {
			const isCollapsed = EmojiPicker.isCategoryCollapsed(row.category);
			const handleToggleCategory = () => {
				EmojiPickerCommands.toggleCategory(row.category);
			};
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
				leadingIcon = (
					<div
						className={styles.headerIcon}
						data-flx="channel.emoji-picker.virtual-row.virtual-row-renderer.header-icon--3"
					>
						<GuildIcon
							id={row.guildId}
							name={row.name}
							icon={Guilds.getGuild(row.guildId)?.icon ?? null}
							sizePx={16}
							data-flx="channel.emoji-picker.virtual-row.virtual-row-renderer.guild-icon"
						/>
					</div>
				);
			}
			return (
				<button
					type="button"
					onClick={handleToggleCategory}
					style={{
						height: remFromPx(CATEGORY_HEADER_HEIGHT),
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
		const hasFixedTrack = cellTrackWidth != null && cellTrackWidth > 0;
		return (
			<div
				style={{
					height: remFromPx(EMOJI_ROW_HEIGHT),
					position: 'relative',
				}}
				data-flx="channel.emoji-picker.virtual-row.virtual-row-renderer.div--2"
			>
				<div
					className={hasFixedTrack ? `${styles.emojiGrid} ${styles.emojiGridFixedTrack}` : styles.emojiGrid}
					style={{
						gridTemplateColumns: hasFixedTrack
							? `repeat(auto-fill, ${remFromPx(cellTrackWidth)})`
							: `repeat(${gridColumns}, minmax(0, 1fr))`,
						marginInline: gridWidth ? 'auto' : undefined,
						width: gridWidth ? `${gridWidth}px` : undefined,
					}}
					data-flx="channel.emoji-picker.virtual-row.virtual-row-renderer.emoji-grid"
				>
					{row.emojis.map((emoji, colIndex) => {
						const isSelected = colIndex === selectedColumnInRow;
						const shouldHighlight = isSelected;
						return (
							<EmojiRenderer
								key={emoji.id ?? emoji.uniqueName}
								emoji={emoji}
								handleHover={(e) => handleHover(e, emojiRowIndex, colIndex)}
								handleSelect={handleSelect}
								skinTone={skinTone}
								channel={channel}
								shouldAnimate={shouldAnimate}
								isHighlighted={shouldHighlight}
								shouldScrollIntoView={isSelected && shouldScrollOnSelection}
								ref={(node) => {
									const key = `${emojiRowIndex}-${colIndex}`;
									if (node) {
										emojiRefs.current.set(key, node);
									} else {
										emojiRefs.current.delete(key);
									}
								}}
								data-flx="channel.emoji-picker.virtual-row.virtual-row-renderer.emoji-renderer"
							/>
						);
					})}
				</div>
			</div>
		);
	},
);

interface VirtualizedRowProps {
	row: VirtualRow;
	handleHover: (emoji: FlatEmoji | null, row?: number, column?: number) => void;
	handleSelect: (emoji: FlatEmoji, shiftKey?: boolean) => void;
	skinTone: string;
	channel: Channel | null;
	allowAnimation: boolean;
	gridColumns?: number;
	gridWidth?: number;
	cellTrackWidth?: number;
	selectedRow: number;
	selectedColumn: number;
	emojiRowIndex: number;
	shouldScrollOnSelection?: boolean;
	emojiRefs: React.MutableRefObject<Map<string, HTMLButtonElement>>;
}

export const VirtualizedRow: React.FC<VirtualizedRowProps> = observer(
	({
		row,
		handleHover,
		handleSelect,
		skinTone,
		channel,
		allowAnimation,
		gridColumns,
		gridWidth,
		cellTrackWidth,
		selectedRow,
		selectedColumn,
		emojiRowIndex,
		shouldScrollOnSelection = false,
		emojiRefs,
	}) => {
		const selectedColumnInRow = emojiRowIndex === selectedRow ? selectedColumn : -1;
		return (
			<VirtualRowRenderer
				row={row}
				handleHover={handleHover}
				handleSelect={handleSelect}
				skinTone={skinTone}
				channel={channel}
				shouldAnimate={allowAnimation}
				gridColumns={gridColumns}
				gridWidth={gridWidth}
				cellTrackWidth={cellTrackWidth}
				selectedColumnInRow={selectedColumnInRow}
				emojiRowIndex={emojiRowIndex}
				shouldScrollOnSelection={shouldScrollOnSelection}
				emojiRefs={emojiRefs}
				data-flx="channel.emoji-picker.virtual-row.virtualized-row.virtual-row-renderer"
			/>
		);
	},
);
