// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VirtualRow} from '@app/features/channel/components/sticker_picker/hooks/useVirtualRows';
import styles from '@app/features/channel/components/sticker_picker/VirtualRow.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import * as StickerPickerCommands from '@app/features/emoji/commands/StickerPickerCommands';
import {useStickerAnimation} from '@app/features/emoji/hooks/useStickerAnimation';
import StickerPicker from '@app/features/emoji/state/StickerPicker';
import type {GuildSticker} from '@app/features/expressions/models/GuildSticker';
import {checkStickerAvailability} from '@app/features/expressions/utils/ExpressionPermissionUtils';
import {GuildIcon} from '@app/features/guild/components/popouts/GuildIcon';
import Guilds from '@app/features/guild/state/Guilds';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {StickerContextMenuItems} from '@app/features/ui/action_menu/items/StickerContextMenuItems';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {useLingui} from '@lingui/react/macro';
import {CaretDownIcon, ClockIcon, StarIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useEffect, useRef, useState} from 'react';

const STICKER_ROW_HEIGHT = 92;
const CATEGORY_HEADER_HEIGHT = 32;
const OVERSCAN_ROWS = 5;

interface VirtualRowRendererProps {
	row: VirtualRow;
	handleHover: (sticker: GuildSticker | null, row?: number, column?: number) => void;
	handleSelect: (sticker: GuildSticker, shiftKey?: boolean) => void;
	gridColumns?: number;
	hoveredSticker: GuildSticker | null;
	selectedRow: number;
	selectedColumn: number;
	stickerRowIndex: number;
	shouldScrollOnSelection?: boolean;
	stickerRefs: React.MutableRefObject<Map<string, HTMLButtonElement>>;
	channel?: Channel | null;
}

interface StickerButtonProps {
	sticker: GuildSticker;
	stickerKey: string;
	isSelected: boolean;
	stickerRefs: React.MutableRefObject<Map<string, HTMLButtonElement>>;
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
		const {shouldAnimate, interactionHandlers} = useStickerAnimation({respectUserSettings: false});
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
							size: 320,
						})}
						alt={sticker.name}
						className={styles.stickerImage}
						loading="lazy"
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
		selectedRow,
		selectedColumn,
		stickerRowIndex,
		shouldScrollOnSelection = false,
		stickerRefs,
		channel,
	}) => {
		const {i18n} = useLingui();
		if (row.type === 'header') {
			const isCollapsed = StickerPicker.isCategoryCollapsed(row.category);
			const handleToggleCategory = () => {
				StickerPickerCommands.toggleCategory(row.category);
			};
			let leadingIcon: React.ReactNode = null;
			if (row.category === 'favorites') {
				leadingIcon = (
					<StarIcon
						weight="fill"
						className={styles.headerIcon}
						data-flx="channel.sticker-picker.virtual-row.virtual-row-renderer-base.header-icon"
					/>
				);
			} else if (row.category === 'frequently-used') {
				leadingIcon = (
					<ClockIcon
						weight="fill"
						className={styles.headerIcon}
						data-flx="channel.sticker-picker.virtual-row.virtual-row-renderer-base.header-icon--2"
					/>
				);
			} else if (row.guildId) {
				leadingIcon = (
					<GuildIcon
						id={row.guildId}
						name={row.name}
						icon={Guilds.getGuild(row.guildId)?.icon ?? null}
						className={styles.guildIconSmall}
						sizePx={16}
						data-flx="channel.sticker-picker.virtual-row.virtual-row-renderer-base.guild-icon-small"
					/>
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
					data-flx="channel.sticker-picker.virtual-row.virtual-row-renderer-base.button.toggle-category"
				>
					{leadingIcon}
					<div
						style={{display: 'flex', alignItems: 'center', flex: '1 1 auto', minWidth: 0}}
						data-flx="channel.sticker-picker.virtual-row.virtual-row-renderer-base.div"
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
							data-flx="channel.sticker-picker.virtual-row.virtual-row-renderer-base.category-title"
						>
							{row.name}
						</span>
						<CaretDownIcon
							weight="bold"
							className={styles.caretIcon}
							style={{transform: `rotate(${isCollapsed ? -90 : 0}deg)`, marginLeft: '0.5rem'}}
							data-flx="channel.sticker-picker.virtual-row.virtual-row-renderer-base.caret-icon"
						/>
					</div>
				</button>
			);
		}
		if (row.type === 'sticker-row') {
			return (
				<div
					className={styles.stickerGrid}
					style={{
						gridTemplateColumns: `repeat(${gridColumns}, minmax(0, 1fr))`,
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
	hoveredSticker: GuildSticker | null;
	selectedRow: number;
	selectedColumn: number;
	stickerRowIndex: number;
	shouldScrollOnSelection?: boolean;
	stickerRefs: React.MutableRefObject<Map<string, HTMLButtonElement>>;
	channel?: Channel | null;
}

export const VirtualRowWrapper: React.FC<VirtualRowWrapperProps> = observer(
	({
		row,
		handleHover,
		handleSelect,
		gridColumns,
		hoveredSticker,
		selectedRow,
		selectedColumn,
		stickerRowIndex,
		shouldScrollOnSelection = false,
		stickerRefs,
		channel,
	}) => {
		const [isVisible, setIsVisible] = useState(false);
		const placeholderRef = useRef<HTMLDivElement>(null);
		useEffect(() => {
			const placeholder = placeholderRef.current;
			if (!placeholder) return;
			const observer = new IntersectionObserver(
				(entries) => {
					entries.forEach((entry) => {
						if (entry.isIntersecting) {
							setIsVisible(true);
						} else {
							const rect = entry.boundingClientRect;
							const viewportHeight = window.innerHeight;
							const overscanDistance = OVERSCAN_ROWS * STICKER_ROW_HEIGHT;
							if (rect.bottom < -overscanDistance || rect.top > viewportHeight + overscanDistance) {
								setIsVisible(false);
							}
						}
					});
				},
				{
					rootMargin: `${OVERSCAN_ROWS * STICKER_ROW_HEIGHT}px 0px`,
					threshold: 0,
				},
			);
			observer.observe(placeholder);
			return () => {
				observer.disconnect();
			};
		}, []);
		const height = row.type === 'header' ? CATEGORY_HEADER_HEIGHT : STICKER_ROW_HEIGHT;
		if (!isVisible) {
			return (
				<div
					ref={placeholderRef}
					style={{height: remFromPx(height)}}
					data-flx="channel.sticker-picker.virtual-row.virtual-row-wrapper.div"
				/>
			);
		}
		return (
			<div ref={placeholderRef} data-flx="channel.sticker-picker.virtual-row.virtual-row-wrapper.div--2">
				<VirtualRowRenderer
					row={row}
					handleHover={handleHover}
					handleSelect={handleSelect}
					gridColumns={gridColumns}
					hoveredSticker={hoveredSticker}
					selectedRow={selectedRow}
					selectedColumn={selectedColumn}
					stickerRowIndex={stickerRowIndex}
					shouldScrollOnSelection={shouldScrollOnSelection}
					stickerRefs={stickerRefs}
					channel={channel}
					data-flx="channel.sticker-picker.virtual-row.virtual-row-wrapper.virtual-row-renderer"
				/>
			</div>
		);
	},
);

VirtualRowWrapper.displayName = 'VirtualRowWrapper';
