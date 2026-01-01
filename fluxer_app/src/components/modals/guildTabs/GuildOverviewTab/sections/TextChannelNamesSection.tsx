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
import type React from 'react';
import type {UseFormReturn} from 'react-hook-form';
import {Controller} from 'react-hook-form';
import {Switch} from '~/components/form/Switch';
import type {FormInputs} from '~/utils/modals/guildTabs/GuildOverviewTabUtils';

import {SettingsSection} from '../components/SettingsSection';

export const TextChannelNamesSection: React.FC<{
	form: UseFormReturn<FormInputs>;
	canManageGuild: boolean;
}> = ({form, canManageGuild}) => {
	const {t} = useLingui();
	return (
		<SettingsSection
			title={<Trans>Text Channel Names</Trans>}
			description={<Trans>Configure how text channel names can be formatted</Trans>}
		>
			<Controller
				name="text_channel_flexible_names"
				control={form.control}
				render={({field}) => (
					<Switch
						label={t`Allow Flexible Text Channel Names`}
						description={t`When enabled, text channels can have capitalized letters and spaces in their names (like voice channels). When disabled, names are restricted to lowercase with hyphens and underscores only.`}
						value={field.value ?? false}
						onChange={field.onChange}
						disabled={!canManageGuild}
					/>
				)}
			/>
		</SettingsSection>
	);
};
