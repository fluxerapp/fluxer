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
import {Switch} from '~/components/form/Switch';
import type {FormInputs, SelectOption} from '~/utils/modals/guildTabs/GuildOverviewTabUtils';

import {SettingsSection} from '../components/SettingsSection';
import styles from '../GuildOverviewTab.module.css';
import type {ChannelLike} from '../types';

export const SystemWelcomeSection: React.FC<{
	form: UseFormReturn<FormInputs>;
	canManageGuild: boolean;
	textChannels: Array<ChannelLike>;
}> = ({form, canManageGuild, textChannels}) => {
	const {t} = useLingui();
	const systemChannelOptions = React.useMemo<Array<SelectOption>>(
		() => [
			{value: null, label: t`No System Channel`},
			...textChannels.map((channel) => ({value: channel.id, label: channel.name ?? t`Unnamed channel`})),
		],
		[textChannels],
	);

	return (
		<SettingsSection
			title={<Trans>System &amp; Welcome</Trans>}
			description={<Trans>Choose destination for system and welcome messages</Trans>}
		>
			<div className={styles.settingsContent}>
				<div>
					<Controller
						name="system_channel_id"
						control={form.control}
						render={({field}) => (
							<FormSelect<string | null>
								label={t`Destination Channel`}
								description={t`Welcome and system messages will appear here.`}
								value={field.value ?? null}
								onChange={(v) => field.onChange(v)}
								options={systemChannelOptions}
								placeholder={t`Select a channel`}
								disabled={!canManageGuild}
							/>
						)}
					/>
				</div>

				<Controller
					name="suppress_join_notifications"
					control={form.control}
					render={({field}) => (
						<Switch
							label={t`Hide Join Messages`}
							description={t`When enabled, new member joins won't post to the destination channel.`}
							value={field.value ?? false}
							onChange={field.onChange}
							disabled={!canManageGuild}
						/>
					)}
				/>
			</div>
		</SettingsSection>
	);
};
