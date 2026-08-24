// SPDX-License-Identifier: AGPL-3.0-or-later

import {getInitialsFromName, truncateInitials} from '@app/features/guild/utils/GuildInitialsUtils';
import type {ComboboxOption} from '@app/features/ui/components/form/FormCombobox';
import {clsx} from 'clsx';
import type React from 'react';

export interface GuildComboboxOption extends ComboboxOption {
	icon?: string | null;
	iconUrl?: string | null;
}

export interface GuildComboboxStyles {
	optionRow: string;
	valueRow?: string;
	rowGlobal?: string;
	rowDisabled?: string;
	avatar: string;
	avatarPlaceholder: string;
	label: string;
	notice?: string;
}

export interface GuildComboboxRenderersConfig<T extends GuildComboboxOption> {
	styles: GuildComboboxStyles;
	getNotice?: (option: T, disabled: boolean) => React.ReactNode;
}

const ROW_INITIALS_MAX_LENGTH = 1;

const renderRow = <T extends GuildComboboxOption>(
	option: T,
	disabled: boolean,
	rowClass: string,
	styles: GuildComboboxStyles,
	getNotice?: (option: T, disabled: boolean) => React.ReactNode,
) => {
	const isGlobal = !option.value;
	const iconUrl = option.iconUrl ?? null;
	const initial = truncateInitials(getInitialsFromName(option.label), ROW_INITIALS_MAX_LENGTH).toUpperCase();
	const notice = getNotice?.(option, disabled);
	return (
		<div
			className={clsx(rowClass, isGlobal && styles.rowGlobal, disabled && styles.rowDisabled)}
			data-flx="app.guild-combobox-renderers.render-row.row-global"
		>
			{!isGlobal &&
				(iconUrl ? (
					<div
						className={styles.avatar}
						style={{backgroundImage: `url(${iconUrl})`}}
						aria-hidden
						data-flx="app.guild-combobox-renderers.render-row.avatar"
					/>
				) : (
					<div
						className={styles.avatarPlaceholder}
						aria-hidden
						data-flx="app.guild-combobox-renderers.render-row.avatar-placeholder"
					>
						{initial}
					</div>
				))}
			<span className={styles.label} data-flx="app.guild-combobox-renderers.render-row.label">
				{option.label}
			</span>
			{notice ? (
				styles.notice ? (
					<span className={styles.notice} data-flx="app.guild-combobox-renderers.render-row.notice">
						{notice}
					</span>
				) : (
					notice
				)
			) : null}
		</div>
	);
};
export const createGuildComboboxRenderers = <T extends GuildComboboxOption>({
	styles,
	getNotice,
}: GuildComboboxRenderersConfig<T>) => {
	const renderOption = (option: T) =>
		renderRow(option, Boolean(option.isDisabled), styles.optionRow, styles, getNotice);
	const renderValue = (option: T | null) =>
		option
			? renderRow(option, Boolean(option.isDisabled), styles.valueRow ?? styles.optionRow, styles, getNotice)
			: null;
	return {renderOption, renderValue};
};
