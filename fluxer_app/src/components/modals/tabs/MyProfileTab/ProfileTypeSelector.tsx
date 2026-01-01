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

import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {Select} from '~/components/form/Select';
import {createGuildSelectComponents, type GuildSelectOption} from '~/components/modals/shared/GuildSelectComponents';
import GuildStore from '~/stores/GuildStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import styles from './ProfileTypeSelector.module.css';

interface ProfileTypeSelectorProps {
	selectedGuildId: string | null;
	onChange: (guildId: string | null) => void;
	disabled: boolean;
}

export const ProfileTypeSelector = observer(({selectedGuildId, onChange, disabled}: ProfileTypeSelectorProps) => {
	const {t} = useLingui();
	const guilds = GuildStore.getGuilds();
	const guildOptions: Array<GuildSelectOption> = [
		{value: '', label: t`Global Profile`},
		...guilds.map((guild) => ({
			value: guild.id,
			label: guild.name || '',
			icon: guild.icon ?? null,
		})),
	];

	const guildSelectComponents = React.useMemo(
		() =>
			createGuildSelectComponents({
				styles: {
					optionRow: styles.guildOption,
					valueRow: styles.guildValue,
					rowGlobal: styles.guildOptionGlobal,
					avatar: styles.guildAvatar,
					avatarPlaceholder: styles.guildAvatarPlaceholder,
					label: styles.guildOptionLabel,
				},
			}),
		[],
	);

	const renderGuildRow = React.useCallback((option: GuildSelectOption) => {
		const isGlobal = !option.value;
		const iconUrl = option.icon ? AvatarUtils.getGuildIconURL({id: option.value, icon: option.icon}) : null;
		const initial = option.label.charAt(0).toUpperCase();

		return (
			<div className={clsx(styles.guildOption, isGlobal && styles.guildOptionGlobal)}>
				{!isGlobal &&
					(iconUrl ? (
						<div className={styles.guildAvatar} style={{backgroundImage: `url(${iconUrl})`}} aria-hidden />
					) : (
						<div className={styles.guildAvatarPlaceholder} aria-hidden>
							{initial}
						</div>
					))}
				<span className={styles.guildOptionLabel}>{option.label}</span>
			</div>
		);
	}, []);

	const renderOption = React.useCallback((option: GuildSelectOption) => renderGuildRow(option), [renderGuildRow]);

	const renderValue = React.useCallback(
		(option: GuildSelectOption | null) => {
			if (!option) return null;
			return renderGuildRow(option);
		},
		[renderGuildRow],
	);

	return (
		<div className={styles.container}>
			<Select
				label={t`Profile Type`}
				value={selectedGuildId || ''}
				options={guildOptions}
				onChange={(value) => onChange(value === '' ? null : value)}
				disabled={disabled}
				className={clsx(disabled && styles.disabled)}
				components={guildSelectComponents}
				renderOption={renderOption}
				renderValue={renderValue}
			/>
			{selectedGuildId && (
				<p className={styles.description}>
					<Trans>
						You are editing your per-community profile. This profile will only be visible in this community and will
						override your global profile.
					</Trans>
				</p>
			)}
		</div>
	);
});
