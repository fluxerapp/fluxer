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

export const getSelectStyles = <
	Option,
	IsMulti extends boolean = false,
	Group extends GroupBase<Option> = GroupBase<Option>,
>(
	error?: boolean,
): StylesConfig<Option, IsMulti, Group> => ({
	control: (provided, state) => {
		const baseBorder = error ? 'var(--status-danger)' : 'var(--background-modifier-accent)';
		const focusBorder = error ? 'var(--status-danger)' : 'var(--background-modifier-accent-focus)';
		const surfaceBackground = 'var(--form-surface-background)';
		return {
			...provided,
			backgroundColor: surfaceBackground,
			borderColor: state.isFocused ? focusBorder : baseBorder,
			borderWidth: '1px',
			borderRadius: '8px',
			minHeight: '44px',
			paddingLeft: '16px',
			paddingRight: '0px',
			boxShadow: 'none',
			outline: 'none',
			transition: 'border-color 0.15s ease',
			'&:hover': {
				backgroundColor: surfaceBackground,
				borderColor: state.isFocused ? focusBorder : baseBorder,
			},
		};
	},
	valueContainer: (provided) => ({
		...provided,
		padding: 0,
		gap: 0,
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
		fontSize: '0.875rem',
		lineHeight: '1.25rem',
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
		paddingTop: '8px',
		paddingBottom: '8px',
		paddingLeft: '12px',
		paddingRight: '12px',
		transition: 'background-color 0.1s ease',
		fontSize: '0.875rem',
		lineHeight: '1.25rem',
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
		color: 'var(--text-primary)',
		overflow: 'hidden',
		textOverflow: 'ellipsis',
		whiteSpace: 'nowrap',
		maxWidth: '100%',
		fontSize: '0.875rem',
		lineHeight: '1.25rem',
	}),
	placeholder: (provided) => ({
		...provided,
		color: 'var(--text-primary-muted)',
		fontSize: '0.875rem',
		lineHeight: '1.25rem',
	}),
	input: (provided) => ({
		...provided,
		color: 'var(--text-primary)',
		fontSize: '0.875rem',
		lineHeight: '1.25rem',
		overflow: 'hidden',
		textOverflow: 'ellipsis',
	}),
	dropdownIndicator: (provided, state) => ({
		...provided,
		color: 'var(--text-tertiary)',
		padding: '8px',
		transform: state.selectProps.menuIsOpen ? 'rotate(180deg)' : 'rotate(0deg)',
		transition: 'transform 0.2s ease, color 0.15s ease',
		'&:hover': {
			color: 'var(--text-primary)',
		},
	}),
	indicatorSeparator: () => ({
		display: 'none',
	}),
});
