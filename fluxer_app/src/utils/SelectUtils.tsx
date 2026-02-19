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

import type {GroupBase, StylesConfig} from 'react-select';

export function getSelectStyles<
	Option,
	IsMulti extends boolean = false,
	Group extends GroupBase<Option> = GroupBase<Option>,
>(
	error?: boolean,
	density: 'default' | 'compact' | 'compactOverlay' = 'default',
): StylesConfig<Option, IsMulti, Group> {
	const isCompact = density === 'compact' || density === 'compactOverlay';
	const isCompactOverlay = density === 'compactOverlay';
	const controlMinHeight = isCompactOverlay ? '18px' : isCompact ? '34px' : '44px';
	const controlPaddingLeft = isCompactOverlay ? '0px' : isCompact ? '10px' : '16px';
	const optionPaddingY = isCompactOverlay ? '6px' : isCompact ? '6px' : '8px';
	const optionPaddingX = isCompactOverlay ? '10px' : isCompact ? '10px' : '12px';
	const fontSize = isCompactOverlay ? '0.625rem' : isCompact ? '0.8125rem' : '0.875rem';
	const lineHeight = isCompactOverlay ? '0.75rem' : isCompact ? '1.125rem' : '1.25rem';
	const indicatorPadding = isCompactOverlay ? '0px' : isCompact ? '6px' : '8px';
	const controlBaseBorder = error ? 'var(--status-danger)' : 'var(--background-modifier-accent)';
	const controlFocusBorder = error ? 'var(--status-danger)' : 'var(--background-modifier-accent-focus)';
	const controlBorderColor = isCompactOverlay ? 'transparent' : controlBaseBorder;
	const controlFocusBorderColor = isCompactOverlay ? 'transparent' : controlFocusBorder;
	const controlBackgroundColor = isCompactOverlay ? 'transparent' : 'var(--form-surface-background)';
	const valueColor = isCompactOverlay ? 'var(--white)' : 'var(--text-primary)';
	const placeholderColor = isCompactOverlay ? 'rgb(255 255 255 / 0.92)' : 'var(--text-tertiary)';
	const indicatorColor = isCompactOverlay ? 'rgb(255 255 255 / 0.85)' : 'var(--text-tertiary)';
	const indicatorHoverColor = isCompactOverlay ? 'var(--white)' : 'var(--text-primary)';

	return {
		control: (provided, state) => {
			return {
				...provided,
				backgroundColor: controlBackgroundColor,
				borderColor: state.isFocused ? controlFocusBorderColor : controlBorderColor,
				borderWidth: isCompactOverlay ? '0px' : '1px',
				borderRadius: isCompactOverlay ? '0px' : '8px',
				minHeight: controlMinHeight,
				paddingLeft: controlPaddingLeft,
				paddingRight: '0px',
				boxShadow: 'none',
				outline: 'none',
				cursor: isCompactOverlay ? 'pointer' : provided.cursor,
				transition: 'color 0.15s ease, background-color 0.15s ease, border-color 0.15s ease',
				'&:hover': {
					borderColor: state.isFocused ? controlFocusBorderColor : controlBorderColor,
				},
			};
		},
		valueContainer: (provided) => ({
			...provided,
			padding: 0,
			gap: 0,
			minWidth: 0,
			overflow: isCompactOverlay ? 'visible' : provided.overflow,
		}),
		menu: (provided) => ({
			...provided,
			backgroundColor: 'var(--form-surface-background)',
			border: '1px solid var(--background-modifier-accent)',
			borderRadius: '8px',
			boxShadow: '0 8px 16px rgba(0, 0, 0, 0.24)',
			zIndex: 99999,
			overflow: 'hidden',
			overflowX: 'hidden',
			fontSize,
			lineHeight,
		}),
		menuList: (provided) => ({
			...provided,
			overflowY: 'auto',
			overflowX: 'hidden',
			paddingTop: '4px',
			paddingBottom: '4px',
			scrollbarWidth: 'thin',
			scrollbarColor: 'var(--scrollbar-thumb-bg) var(--scrollbar-track-bg)',
			'&::-webkit-scrollbar': {
				width: '8px',
			},
			'&::-webkit-scrollbar-track': {
				backgroundColor: 'var(--scrollbar-track-bg)',
			},
			'&::-webkit-scrollbar-thumb': {
				backgroundColor: 'var(--scrollbar-thumb-bg)',
				backgroundClip: 'padding-box',
				border: '2px solid transparent',
				borderRadius: '4px',
			},
			'&::-webkit-scrollbar-thumb:hover': {
				backgroundColor: 'var(--scrollbar-thumb-bg-hover)',
			},
		}),
		menuPortal: (provided) => ({
			...provided,
			zIndex: 99999,
		}),
		option: (provided, state) => ({
			...provided,
			backgroundColor: state.isSelected
				? 'var(--brand-primary)'
				: state.isFocused
					? 'var(--background-modifier-hover)'
					: 'transparent',
			color: state.isSelected ? 'white' : 'var(--text-primary)',
			cursor: 'pointer',
			paddingTop: optionPaddingY,
			paddingBottom: optionPaddingY,
			paddingLeft: optionPaddingX,
			paddingRight: optionPaddingX,
			transition: 'background-color 0.1s ease',
			fontSize,
			lineHeight,
			overflow: 'hidden',
			textOverflow: 'ellipsis',
			whiteSpace: 'nowrap',
			maxWidth: '100%',
			'&:hover': {
				backgroundColor: state.isSelected ? 'var(--brand-primary)' : 'var(--background-modifier-hover)',
			},
		}),
		singleValue: (provided) => ({
			...provided,
			color: valueColor,
			overflow: isCompactOverlay ? 'visible' : 'hidden',
			textOverflow: isCompactOverlay ? 'clip' : 'ellipsis',
			whiteSpace: 'nowrap',
			maxWidth: isCompactOverlay ? 'none' : '100%',
			fontSize,
			lineHeight,
			fontWeight: isCompactOverlay ? 500 : provided.fontWeight,
		}),
		placeholder: (provided) => ({
			...provided,
			color: placeholderColor,
			fontSize,
			lineHeight,
		}),
		input: (provided) => ({
			...provided,
			color: valueColor,
			fontSize,
			lineHeight,
			overflow: 'hidden',
			textOverflow: 'ellipsis',
		}),
		dropdownIndicator: (provided, state) => ({
			...provided,
			display: isCompactOverlay ? 'none' : provided.display,
			color: indicatorColor,
			padding: indicatorPadding,
			transform: state.selectProps.menuIsOpen ? 'rotate(180deg)' : 'rotate(0deg)',
			transition: 'transform 0.2s ease, color 0.15s ease',
			'&:hover': {
				color: indicatorHoverColor,
			},
		}),
		indicatorSeparator: () => ({
			display: 'none',
		}),
	};
}
