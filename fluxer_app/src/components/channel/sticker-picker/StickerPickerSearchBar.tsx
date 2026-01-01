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
import {observer} from 'mobx-react-lite';
import React from 'react';
import {PickerSearchInput} from '~/components/channel/shared/PickerSearchInput';
import type {GuildStickerRecord} from '~/records/GuildStickerRecord';

interface StickerPickerSearchBarProps {
	searchTerm: string;
	setSearchTerm: (term: string) => void;
	hoveredSticker: GuildStickerRecord | null;
	inputRef?: React.RefObject<HTMLInputElement | null> | React.RefObject<HTMLInputElement>;
	selectedRow?: number;
	selectedColumn?: number;
	sections?: Array<number>;
	onSelect?: (row: number | null, column: number | null, event?: React.KeyboardEvent) => void;
	onSelectionChange?: (row: number, column: number, shouldScroll?: boolean) => void;
}

export const StickerPickerSearchBar = observer(
	({
		searchTerm,
		setSearchTerm,
		hoveredSticker,
		inputRef,
		selectedRow = -1,
		selectedColumn = -1,
		sections = [],
		onSelect,
		onSelectionChange,
	}: StickerPickerSearchBarProps) => {
		const {t} = useLingui();
		const handleKeyDown = React.useCallback(
			(event: React.KeyboardEvent<HTMLInputElement> | React.KeyboardEvent) => {
				if (sections.length === 0) {
					return;
				}

				let newRow = selectedRow;
				let newColumn = selectedColumn;

				switch (event.key) {
					case 'ArrowDown':
						event.preventDefault();
						event.stopPropagation();
						if (newRow === -1) {
							newRow = 0;
							newColumn = 0;
						} else {
							newRow += 1;
							if (newRow >= sections.length) {
								newRow = sections.length - 1;
							}
							if (newColumn >= sections[newRow]) {
								newColumn = sections[newRow] - 1;
							}
						}
						break;

					case 'ArrowUp':
						event.preventDefault();
						event.stopPropagation();
						newRow -= 1;
						if (newRow < 0) {
							newRow = 0;
							newColumn = 0;
						} else if (newColumn >= sections[newRow]) {
							newColumn = sections[newRow] - 1;
						}
						break;

					case 'ArrowLeft':
						event.preventDefault();
						event.stopPropagation();
						if (newRow === -1) {
							newRow = 0;
						}
						newColumn -= 1;
						if (newColumn < 0) {
							newRow -= 1;
							if (newRow >= 0) {
								newColumn = sections[newRow] - 1;
							} else {
								newRow = 0;
								newColumn = 0;
							}
						}
						break;

					case 'ArrowRight':
						event.preventDefault();
						event.stopPropagation();
						if (newRow === -1) {
							newRow = 0;
						}
						newColumn += 1;
						if (newColumn >= sections[newRow]) {
							newRow += 1;
							newColumn = 0;
							if (newRow >= sections.length) {
								newRow = sections.length - 1;
								newColumn = sections[newRow] - 1;
							}
						}
						break;

					case 'Enter':
						event.preventDefault();
						event.stopPropagation();
						if (newRow === -1) {
							newRow = 0;
						}
						if (newColumn === -1) {
							newColumn = 0;
						}
						onSelect?.(newRow, newColumn, event);
						return;

					case 'Escape':
						onSelect?.(null, null);
						return;

					default:
						return;
				}

				onSelectionChange?.(newRow, newColumn, true);
			},
			[sections, selectedRow, selectedColumn, onSelect, onSelectionChange],
		);

		React.useEffect(() => {
			const handleGlobalKeyDown = (event: KeyboardEvent) => {
				if (sections.length === 0) {
					return;
				}

				if (!['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight'].includes(event.key)) {
					return;
				}

				const syntheticEvent = {
					key: event.key,
					shiftKey: event.shiftKey,
					preventDefault: () => event.preventDefault(),
					stopPropagation: () => event.stopPropagation(),
				} as React.KeyboardEvent;

				handleKeyDown(syntheticEvent);
			};

			document.addEventListener('keydown', handleGlobalKeyDown as any);
			return () => document.removeEventListener('keydown', handleGlobalKeyDown as any);
		}, [sections, handleKeyDown]);

		const placeholder = hoveredSticker ? hoveredSticker.name : t`Find the perfect sticker`;

		return (
			<PickerSearchInput
				value={searchTerm}
				onChange={setSearchTerm}
				placeholder={placeholder}
				inputRef={inputRef}
				onKeyDown={handleKeyDown}
			/>
		);
	},
);
