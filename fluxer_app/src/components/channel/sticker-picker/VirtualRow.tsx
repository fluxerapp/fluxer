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

import {CaretDownIcon, ClipboardIcon, ClockIcon, CrownIcon, StarIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as PremiumModalActionCreators from '~/actions/PremiumModalActionCreators';
import * as StickerPickerActionCreators from '~/actions/StickerPickerActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import styles from '~/components/channel/sticker-picker/VirtualRow.module.css';
import {GuildIcon} from '~/components/popouts/GuildIcon';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';
import GuildStore from '~/stores/GuildStore';
import StickerPickerStore from '~/stores/StickerPickerStore';
import {checkStickerAvailability} from '~/utils/ExpressionPermissionUtils';
import {shouldShowPremiumFeatures} from '~/utils/PremiumUtils';
import type {VirtualRow} from './hooks/useVirtualRows';

const STICKER_ROW_HEIGHT = 92;
const CATEGORY_HEADER_HEIGHT = 32;
const OVERSCAN_ROWS = 5;

interface VirtualRowRendererProps {
	row: VirtualRow;
	handleHover: (sticker: GuildStickerRecord | null, row?: number, column?: number) => void;
	handleSelect: (sticker: GuildStickerRecord, shiftKey?: boolean) => void;
	gridColumns?: number;
	hoveredSticker: GuildStickerRecord | null;
	selectedRow: number;
	selectedColumn: number;
	stickerRowIndex: number;
	shouldScrollOnSelection?: boolean;
	stickerRefs: React.MutableRefObject<Map<string, HTMLButtonElement>>;
	channel?: ChannelRecord | null;
}

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
		const {t, i18n} = useLingui();
		if (row.type === 'header') {
			const isCollapsed = StickerPickerStore.isCategoryCollapsed(row.category);

			const handleToggleCategory = () => {
				StickerPickerActionCreators.toggleCategory(row.category);
			};

			let leadingIcon: React.ReactNode = null;
			if (row.category === 'favorites') {
				leadingIcon = <StarIcon weight="fill" className={styles.headerIcon} />;
			} else if (row.category === 'frequently-used') {
				leadingIcon = <ClockIcon weight="fill" className={styles.headerIcon} />;
			} else if (row.guildId) {
				leadingIcon = (
					<GuildIcon
						id={row.guildId}
						name={row.name}
						icon={GuildStore.getGuild(row.guildId)?.icon ?? null}
						className={styles.guildIconSmall}
						sizePx={16}
					/>
				);
			}

			return (
				<button
					type="button"
					onClick={handleToggleCategory}
					style={{
						height: '32px',
						display: 'flex',
						alignItems: 'center',
						paddingLeft: '12px',
						paddingRight: '12px',
						marginBottom: '8px',
						position: 'sticky',
						top: 0,
						zIndex: 1,
						cursor: 'pointer',
						border: 'none',
						width: '100%',
						textAlign: 'left',
						gap: '8px',
						minWidth: 0,
					}}
				>
					{leadingIcon}
					<div style={{display: 'flex', alignItems: 'center', flex: '1 1 auto', minWidth: 0}}>
						<span
							className={styles.categoryTitle}
							style={{
								minWidth: 0,
								overflow: 'hidden',
								textOverflow: 'ellipsis',
								whiteSpace: 'nowrap',
								flex: '0 1 auto',
							}}
						>
							{row.name}
						</span>
						<CaretDownIcon
							weight="bold"
							className={styles.caretIcon}
							style={{transform: `rotate(${isCollapsed ? -90 : 0}deg)`, marginLeft: '8px'}}
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
				>
					{row.stickers.map((sticker, columnIndex) => {
						const isSelected = stickerRowIndex === selectedRow && columnIndex === selectedColumn;
						const isFavorite = StickerPickerStore.isFavorite(sticker);

						const stickerKey = `${sticker.guildId}:${sticker.id}`;

						const availability = checkStickerAvailability(i18n, sticker, channel ?? null);
						const isLocked = availability.isLockedByPremium;
						const isDisabled = !availability.canUse;

						const handleStickerClick = (e: React.MouseEvent) => {
							if (isLocked && shouldShowPremiumFeatures()) {
								e.preventDefault();
								e.stopPropagation();
								PremiumModalActionCreators.open();
							} else if (!availability.canUse) {
								e.preventDefault();
								e.stopPropagation();
							} else {
								handleSelect(sticker, e.shiftKey);
							}
						};

						const handleContextMenu = (e: React.MouseEvent<HTMLButtonElement>) => {
							e.preventDefault();
							e.stopPropagation();

							ContextMenuActionCreators.openFromEvent(e, (_props) => (
								<>
									<MenuGroup>
										<MenuItem
											icon={
												<StarIcon
													className={isFavorite ? styles.starIconFilled : styles.starIcon}
													weight={isFavorite ? 'fill' : 'bold'}
												/>
											}
											onClick={() => {
												StickerPickerActionCreators.toggleFavorite(sticker);
											}}
										>
											{isFavorite ? t`Unfavorite Sticker` : t`Favorite Sticker`}
										</MenuItem>
										<MenuItem
											icon={<ClipboardIcon className={styles.clipboardIcon} />}
											onClick={() => {
												TextCopyActionCreators.copy(i18n, sticker.id);
											}}
										>
											{t`Copy Sticker ID`}
										</MenuItem>
									</MenuGroup>
								</>
							));
						};

						return (
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
								onMouseEnter={() => handleHover(sticker, stickerRowIndex, columnIndex)}
								onMouseLeave={() => handleHover(null)}
								className={clsx(styles.stickerButton, isSelected && styles.selected, isLocked && styles.locked)}
								aria-disabled={isDisabled}
								aria-selected={isSelected}
								role="option"
							>
								<img src={sticker.url} alt={sticker.name} className={styles.stickerImage} />
								{sticker.isAnimated() && <div className={styles.gifBadge}>GIF</div>}
								{isLocked && (
									<div className={styles.premiumBadge}>
										<CrownIcon weight="fill" className={styles.premiumIcon} />
									</div>
								)}
							</button>
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
	handleHover: (sticker: GuildStickerRecord | null, row?: number, column?: number) => void;
	handleSelect: (sticker: GuildStickerRecord, shiftKey?: boolean) => void;
	gridColumns?: number;
	hoveredSticker: GuildStickerRecord | null;
	selectedRow: number;
	selectedColumn: number;
	stickerRowIndex: number;
	shouldScrollOnSelection?: boolean;
	stickerRefs: React.MutableRefObject<Map<string, HTMLButtonElement>>;
	channel?: ChannelRecord | null;
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
		const [isVisible, setIsVisible] = React.useState(false);
		const placeholderRef = React.useRef<HTMLDivElement>(null);

		React.useEffect(() => {
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
			return <div ref={placeholderRef} style={{height: `${height}px`}} />;
		}

		return (
			<div ref={placeholderRef}>
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
				/>
			</div>
		);
	},
);

VirtualRowWrapper.displayName = 'VirtualRowWrapper';
