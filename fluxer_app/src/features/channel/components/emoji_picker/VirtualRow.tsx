// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/EmojiPicker.module.css';
import {EMOJI_ROW_HEIGHT} from '@app/features/channel/components/emoji_picker/EmojiPickerConstants';
import {EmojiRenderer} from '@app/features/channel/components/emoji_picker/EmojiRenderer';
import {PickerCategoryHeaderRow} from '@app/features/channel/components/pickers/shared/PickerCategoryHeaderRow';
import type {Channel} from '@app/features/channel/models/Channel';

import EmojiPicker from '@app/features/emoji/state/EmojiPicker';
import type {FlatEmoji} from '@app/features/emoji/types/EmojiTypes';

import {remFromPx} from '@app/features/theme/layout/RemFromPx';

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
	emojiRefs: React.RefObject<Map<string, HTMLButtonElement>>;
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
		if (row.type === 'header')
			return (
				<PickerCategoryHeaderRow
					row={row}
					isCollapsed={EmojiPicker.isCategoryCollapsed(row.category)}
					onToggle={() => EmojiPicker.toggleCategory(row.category)}
					data-flx="channel.emoji-picker.virtual-row.picker-category-header-row"
				/>
			);
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
	emojiRefs: React.RefObject<Map<string, HTMLButtonElement>>;
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
