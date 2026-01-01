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

export const DisallowUnclaimedAccountsSection: React.FC<{
	form: UseFormReturn<FormInputs>;
	canManageGuild: boolean;
}> = ({form, canManageGuild}) => {
	const {t} = useLingui();
	return (
		<SettingsSection
			title={<Trans>Unclaimed Account Access</Trans>}
			description={<Trans>Control whether unclaimed accounts can access this community</Trans>}
		>
			<Controller
				name="disallow_unclaimed_accounts"
				control={form.control}
				render={({field}) => (
					<Switch
						label={t`Disallow Unclaimed Accounts`}
						description={t`When enabled, unclaimed accounts will not be able to access or interact with this community.`}
						value={field.value ?? false}
						onChange={field.onChange}
						disabled={!canManageGuild}
					/>
				)}
			/>
		</SettingsSection>
	);
};
