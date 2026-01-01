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
import React from 'react';
import type {UseFormReturn} from 'react-hook-form';
import {Controller} from 'react-hook-form';
import {Select as FormSelect} from '~/components/form/Select';
import type {FormInputs, SelectOption} from '~/utils/modals/guildTabs/GuildOverviewTabUtils';
import {afkTimeoutOptionsRaw} from '~/utils/modals/guildTabs/GuildOverviewTabUtils';

import {SettingsSection} from '../components/SettingsSection';
import styles from '../GuildOverviewTab.module.css';
import type {ChannelLike} from '../types';

export const IdleSettingsSection: React.FC<{
	form: UseFormReturn<FormInputs>;
	canManageGuild: boolean;
	voiceChannels: Array<ChannelLike>;
}> = ({form, canManageGuild, voiceChannels}) => {
	const {t} = useLingui();
	const afkTimeoutOptions = React.useMemo<Array<{value: number; label: string}>>(
		() => afkTimeoutOptionsRaw.map(({value, label}) => ({value, label: t(label)})),
		[t],
	);
	const afkChannelOptions = React.useMemo<Array<SelectOption>>(
		() => [
			{value: null, label: t`No AFK Channel`},
			...voiceChannels.map((channel) => ({value: channel.id, label: channel.name ?? t`Unnamed channel`})),
		],
		[voiceChannels],
	);

	return (
		<SettingsSection
			title={<Trans>Idle Settings</Trans>}
			description={<Trans>Configure AFK channel and timeout</Trans>}
		>
			<div className={styles.settingsContent}>
				<div>
					<Controller
						name="afk_channel_id"
						control={form.control}
						render={({field}) => (
							<FormSelect<string | null>
								label={t`AFK / Idle Channel`}
								description={t`Move members to this channel when they're AFK.`}
								value={field.value ?? null}
								onChange={(v) => field.onChange(v)}
								options={afkChannelOptions}
								placeholder={t`Select a channel`}
								disabled={!canManageGuild}
							/>
						)}
					/>
				</div>

				<div>
					<Controller
						name="afk_timeout"
						control={form.control}
						render={({field}) => (
							<FormSelect<number>
								label={t`AFK Timeout`}
								value={field.value ?? 300}
								onChange={(v) => field.onChange(v)}
								options={afkTimeoutOptions}
								disabled={!canManageGuild}
							/>
						)}
					/>
				</div>
			</div>
		</SettingsSection>
	);
};
