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

import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {type OptionProps, components as reactSelectComponents, type SingleValueProps} from 'react-select';
import type {SelectOption} from '~/components/form/Select';
import * as AvatarUtils from '~/utils/AvatarUtils';

export interface GuildSelectOption extends SelectOption<string> {
	icon?: string | null;
}

export interface GuildSelectStyles {
	optionRow: string;
	valueRow?: string;
	rowGlobal?: string;
	rowDisabled?: string;
	avatar: string;
	avatarPlaceholder: string;
	label: string;
	notice?: string;
}

export interface GuildSelectComponentsConfig<T extends GuildSelectOption> {
	styles: GuildSelectStyles;
	getNotice?: (option: T, disabled: boolean) => React.ReactNode;
}

const renderRow = <T extends GuildSelectOption>(
	option: T,
	disabled: boolean,
	rowClass: string,
	styles: GuildSelectStyles,
	getNotice?: (option: T, disabled: boolean) => React.ReactNode,
) => {
	const isGlobal = !option.value;
	const iconUrl = option.icon ? AvatarUtils.getGuildIconURL({id: option.value, icon: option.icon}) : null;
	const initial = option.label.charAt(0).toUpperCase();
	const notice = getNotice?.(option, disabled);

	return (
		<div className={clsx(rowClass, isGlobal && styles.rowGlobal, disabled && styles.rowDisabled)}>
			{!isGlobal &&
				(iconUrl ? (
					<div className={styles.avatar} style={{backgroundImage: `url(${iconUrl})`}} aria-hidden />
				) : (
					<div className={styles.avatarPlaceholder} aria-hidden>
						{initial}
					</div>
				))}
			<span className={styles.label}>{option.label}</span>
			{notice ? styles.notice ? <span className={styles.notice}>{notice}</span> : notice : null}
		</div>
	);
};

export const createGuildSelectComponents = <T extends GuildSelectOption>({
	styles,
	getNotice,
}: GuildSelectComponentsConfig<T>) => {
	const Option = observer((props: OptionProps<T, false>) => (
		<reactSelectComponents.Option {...props}>
			{renderRow(props.data, Boolean(props.isDisabled), styles.optionRow, styles, getNotice)}
		</reactSelectComponents.Option>
	));

	const SingleValue = observer((props: SingleValueProps<T, false>) => (
		<reactSelectComponents.SingleValue {...props}>
			{renderRow(props.data, Boolean(props.isDisabled), styles.valueRow ?? styles.optionRow, styles, getNotice)}
		</reactSelectComponents.SingleValue>
	));

	return {Option, SingleValue};
};
