// SPDX-License-Identifier: AGPL-3.0-or-later

import {PickerCategoryHeaderRow} from '@app/features/channel/components/pickers/shared/PickerCategoryHeaderRow';
import type {VirtualRow} from '@app/features/channel/components/sticker_picker/hooks/useVirtualRows';

import styles from '@app/features/channel/components/sticker_picker/VirtualRow.module.css';
import type {Channel} from '@app/features/channel/models/Channel';

import {useStickerAnimation} from '@app/features/emoji/hooks/useStickerAnimation';
import StickerPicker from '@app/features/emoji/state/StickerPicker';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import {checkStickerAvailability} from '@app/features/expressions/utils/ExpressionPermissionUtils';

import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {StickerContextMenuItems} from '@app/features/ui/action_menu/items/StickerContextMenuItems';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {useLingui} from '@lingui/react/macro';

import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';

interface VirtualRowRendererProps {
	row: VirtualRow;
	handleHover: (sticker: GuildSticker | null, row?: number, column?: number) => void;
	handleSelect: (sticker: GuildSticker, shiftKey?: boolean) => void;
	gridColumns?: number;
	cellTrackWidth?: number;
	selectedRow: number;
	selectedColumn: number;
	stickerRowIndex: number;
	shouldScrollOnSelection?: boolean;
	stickerRefs: React.RefObject<Map<string, HTMLButtonElement>>;
	channel?: Channel | null;
}

interface StickerButtonProps {
	sticker: GuildSticker;
	stickerKey: string;
	isSelected: boolean;
	stickerRefs: React.RefObject<Map<string, HTMLButtonElement>>;
	shouldScrollOnSelection?: boolean;
	handleStickerClick: (event: React.MouseEvent<HTMLButtonElement>) => void;
	handleContextMenu: (event: React.MouseEvent<HTMLButtonElement>) => void;
	handleHover: (sticker: GuildSticker | null, row?: number, column?: number) => void;
	stickerRowIndex: number;
	columnIndex: number;
}

const StickerButton: React.FC<StickerButtonProps> = React.memo(
	({
		sticker,
		stickerKey,
		isSelected,
		stickerRefs,
		shouldScrollOnSelection,
		handleStickerClick,
		handleContextMenu,
		handleHover,
		stickerRowIndex,
		columnIndex,
	}) => {
		const {shouldAnimate, interactionHandlers} = useStickerAnimation({
			isAnimated: sticker.animated,
		});
		return (
			<FocusRing offset={-2} data-flx="channel.sticker-picker.virtual-row.sticker-button.focus-ring">
				<button
					key={stickerKey}
					type="button"
					tabIndex={-1}
					ref={(el) => {
						if (el) {
							stickerRefs.current.set(stickerKey, el);
						} else {
							stickerRefs.current.delete(stickerKey);
						}
						if (isSelected && shouldScrollOnSelection && el) {
							el.scrollIntoView({block: 'nearest', inline: 'nearest'});
						}
					}}
					onClick={handleStickerClick}
					onContextMenu={handleContextMenu}
					onMouseEnter={() => {
						interactionHandlers.onMouseEnter();
						handleHover(sticker, stickerRowIndex, columnIndex);
					}}
					onMouseLeave={() => {
						interactionHandlers.onMouseLeave();
						handleHover(null);
					}}
					onFocus={() => {
						interactionHandlers.onFocus();
					}}
					onBlur={() => {
						interactionHandlers.onBlur();
					}}
					className={clsx(styles.stickerButton, isSelected && styles.selected)}
					aria-selected={isSelected}
					role="option"
					data-flx="channel.sticker-picker.virtual-row.sticker-button.sticker-button.sticker-click"
				>
					<img
						src={AvatarUtils.getStickerURL({
							id: sticker.id,
							animated: shouldAnimate,
							isAnimatable: sticker.animated,
							size: 320,
						})}
						alt={sticker.name}
						className={styles.stickerImage}
						data-flx="channel.sticker-picker.virtual-row.sticker-button.sticker-image"
					/>
				</button>
			</FocusRing>
		);
	},
);

StickerButton.displayName = 'StickerButton';

const VirtualRowRendererBase: React.FC<VirtualRowRendererProps> = React.memo(
	({
		row,
		handleHover,
		handleSelect,
		gridColumns = 4,
		cellTrackWidth,
		selectedRow,
		selectedColumn,
		stickerRowIndex,
		shouldScrollOnSelection = false,
		stickerRefs,
		channel,
	}) => {
		const {i18n} = useLingui();
		if (row.type === 'header')
			return (
				<PickerCategoryHeaderRow
					row={row}
					isCollapsed={StickerPicker.isCategoryCollapsed(row.category)}
					onToggle={() => StickerPicker.toggleCategory(row.category)}
					guildIconClassName={styles.guildIconSmall}
					data-flx="channel.sticker-picker.virtual-row.picker-category-header-row"
				/>
			);
		if (row.type === 'sticker-row') {
			const hasFixedTrack = cellTrackWidth != null && cellTrackWidth > 0;
			return (
				<div
					className={hasFixedTrack ? `${styles.stickerGrid} ${styles.stickerGridFixedTrack}` : styles.stickerGrid}
					style={{
						gridTemplateColumns: hasFixedTrack
							? `repeat(auto-fill, ${remFromPx(cellTrackWidth)})`
							: `repeat(${gridColumns}, minmax(0, 1fr))`,
					}}
					data-flx="channel.sticker-picker.virtual-row.virtual-row-renderer-base.sticker-grid"
				>
					{row.stickers.map((sticker, columnIndex) => {
						const isSelected = stickerRowIndex === selectedRow && columnIndex === selectedColumn;
						const stickerKey = `${sticker.guildId}:${sticker.id}`;
						const availability = checkStickerAvailability(i18n, sticker, channel ?? null);
						const handleStickerClick = (e: React.MouseEvent) => {
							if (!availability.canUse) {
								e.preventDefault();
								e.stopPropagation();
							} else {
								handleSelect(sticker, e.shiftKey);
							}
						};
						const handleContextMenu = (e: React.MouseEvent<HTMLButtonElement>) => {
							e.preventDefault();
							e.stopPropagation();
							ContextMenuCommands.openFromEvent(e, ({onClose}) => (
								<StickerContextMenuItems
									sticker={sticker}
									onClose={onClose}
									data-flx="channel.sticker-picker.virtual-row.handle-context-menu.sticker-context-menu-items"
								/>
							));
						};
						return (
							<StickerButton
								key={stickerKey}
								sticker={sticker}
								stickerKey={stickerKey}
								isSelected={isSelected}
								stickerRefs={stickerRefs}
								shouldScrollOnSelection={shouldScrollOnSelection}
								handleStickerClick={handleStickerClick}
								handleContextMenu={handleContextMenu}
								handleHover={handleHover}
								stickerRowIndex={stickerRowIndex}
								columnIndex={columnIndex}
								data-flx="channel.sticker-picker.virtual-row.virtual-row-renderer-base.sticker-button"
							/>
						);
					})}
				</div>
			);
		}
		return null;
	},
);

VirtualRowRendererBase.displayName = 'VirtualRowRendererBase';

export const VirtualRowRenderer = VirtualRowRendererBase;

VirtualRowRenderer.displayName = 'VirtualRowRenderer';

interface VirtualRowWrapperProps {
	row: VirtualRow;
	handleHover: (sticker: GuildSticker | null, row?: number, column?: number) => void;
	handleSelect: (sticker: GuildSticker, shiftKey?: boolean) => void;
	gridColumns?: number;
	cellTrackWidth?: number;
	selectedRow: number;
	selectedColumn: number;
	stickerRowIndex: number;
	shouldScrollOnSelection?: boolean;
	stickerRefs: React.RefObject<Map<string, HTMLButtonElement>>;
	channel?: Channel | null;
}

export const VirtualRowWrapper: React.FC<VirtualRowWrapperProps> = observer(
	({
		row,
		handleHover,
		handleSelect,
		gridColumns,
		cellTrackWidth,
		selectedRow,
		selectedColumn,
		stickerRowIndex,
		shouldScrollOnSelection = false,
		stickerRefs,
		channel,
	}) => {
		return (
			<VirtualRowRenderer
				row={row}
				handleHover={handleHover}
				handleSelect={handleSelect}
				gridColumns={gridColumns}
				cellTrackWidth={cellTrackWidth}
				selectedRow={selectedRow}
				selectedColumn={selectedColumn}
				stickerRowIndex={stickerRowIndex}
				shouldScrollOnSelection={shouldScrollOnSelection}
				stickerRefs={stickerRefs}
				channel={channel}
				data-flx="channel.sticker-picker.virtual-row.virtual-row-wrapper.virtual-row-renderer"
			/>
		);
	},
);

VirtualRowWrapper.displayName = 'VirtualRowWrapper';
