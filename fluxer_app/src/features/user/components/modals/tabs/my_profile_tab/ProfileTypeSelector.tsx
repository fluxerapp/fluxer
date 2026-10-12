// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildComboboxOption} from '@app/features/app/components/dialogs/shared/GuildComboboxRenderers';
import Guilds from '@app/features/guild/state/Guilds';
import {Combobox} from '@app/features/ui/components/form/FormCombobox';
import styles from '@app/features/user/components/modals/tabs/my_profile_tab/ProfileTypeSelector.module.css';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {GLOBAL_PROFILE_DESCRIPTOR, PROFILE_TYPE_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

interface ProfileTypeSelectorProps {
	selectedGuildId: string | null;
	onChange: (guildId: string | null) => void;
	disabled: boolean;
}

export const ProfileTypeSelector = observer(({selectedGuildId, onChange, disabled}: ProfileTypeSelectorProps) => {
	const {i18n} = useLingui();
	const guilds = Guilds.getGuilds();
	const guildOptions: Array<GuildComboboxOption> = [
		{value: '', label: i18n._(GLOBAL_PROFILE_DESCRIPTOR)},
		...guilds.map((guild) => ({
			value: guild.id,
			label: guild.name || '',
			icon: guild.icon ?? null,
		})),
	];
	const renderGuildRow = useCallback((option: GuildComboboxOption) => {
		const isGlobal = !option.value;
		const iconUrl = option.icon ? AvatarUtils.getGuildIconURL({id: option.value, icon: option.icon}) : null;
		const initial = option.label.charAt(0).toUpperCase();
		return (
			<div
				className={clsx(styles.guildOption, isGlobal && styles.guildOptionGlobal)}
				data-flx="user.my-profile-tab.profile-type-selector.render-guild-row.guild-option"
			>
				{!isGlobal &&
					(iconUrl ? (
						<div
							className={styles.guildAvatar}
							style={{backgroundImage: `url(${iconUrl})`}}
							aria-hidden
							data-flx="user.my-profile-tab.profile-type-selector.render-guild-row.guild-avatar"
						/>
					) : (
						<div
							className={styles.guildAvatarPlaceholder}
							aria-hidden
							data-flx="user.my-profile-tab.profile-type-selector.render-guild-row.guild-avatar-placeholder"
						>
							{initial}
						</div>
					))}
				<span
					className={styles.guildOptionLabel}
					data-flx="user.my-profile-tab.profile-type-selector.render-guild-row.guild-option-label"
				>
					{option.label}
				</span>
			</div>
		);
	}, []);
	const renderOption = useCallback((option: GuildComboboxOption) => renderGuildRow(option), [renderGuildRow]);
	const renderValue = useCallback(
		(option: GuildComboboxOption | null) => {
			if (!option) return null;
			return renderGuildRow(option);
		},
		[renderGuildRow],
	);
	return (
		<div className={styles.container} data-flx="user.my-profile-tab.profile-type-selector.container">
			<Combobox
				label={i18n._(PROFILE_TYPE_DESCRIPTOR)}
				value={selectedGuildId || ''}
				options={guildOptions}
				onChange={(value) => onChange(value === '' ? null : value)}
				disabled={disabled}
				className={clsx(disabled && styles.disabled)}
				renderOption={renderOption}
				renderValue={renderValue}
				data-flx="user.my-profile-tab.profile-type-selector.disabled.change"
			/>
			{selectedGuildId && (
				<p className={styles.description} data-flx="user.my-profile-tab.profile-type-selector.description">
					<Trans>
						You are editing your per-community profile. This profile will only be visible in this community and will
						override your global profile.
					</Trans>
				</p>
			)}
		</div>
	);
});
