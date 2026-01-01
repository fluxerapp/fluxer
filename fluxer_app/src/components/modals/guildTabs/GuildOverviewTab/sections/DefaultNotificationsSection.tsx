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

import {MessageNotifications} from '~/Constants';
import {RadioGroup} from '~/components/uikit/RadioGroup/RadioGroup';
import {WarningAlert} from '~/components/uikit/WarningAlert/WarningAlert';
import GuildStore from '~/stores/GuildStore';
import type {FormInputs} from '~/utils/modals/guildTabs/GuildOverviewTabUtils';

import {SettingsSection} from '../components/SettingsSection';
import styles from '../GuildOverviewTab.module.css';

export const DefaultNotificationsSection: React.FC<{
	form: UseFormReturn<FormInputs>;
	canManageGuild: boolean;
	guildId: string;
}> = ({form, canManageGuild, guildId}) => {
	const {t} = useLingui();
	const guild = GuildStore.getGuild(guildId)!;

	return (
		<SettingsSection
			title={<Trans>Default Notifications</Trans>}
			description={
				<Trans>
					Choose what new members get by default before they customize notifications. For larger communities,
					&quot;Mentions only&quot; is enforced.
				</Trans>
			}
		>
			{guild.isLargeGuild && (
				<WarningAlert>
					<Trans>
						Communities with over 250 people are forced onto the &quot;mentions only&quot; setting. Your original
						setting is preserved and will be restored if the community drops below 250 members.
					</Trans>
				</WarningAlert>
			)}
			<Controller
				name="default_message_notifications"
				control={form.control}
				render={({field}) => (
					<RadioGroup
						className={styles.notificationOptions}
						value={guild.isLargeGuild ? MessageNotifications.ONLY_MENTIONS : field.value}
						onChange={field.onChange}
						disabled={!canManageGuild || guild.isLargeGuild}
						aria-label={t`Default notification settings`}
						options={[
							{
								value: MessageNotifications.ALL_MESSAGES,
								name: t`All messages`,
								desc: t`Members receive notifications for every message`,
							},
							{
								value: MessageNotifications.ONLY_MENTIONS,
								name: t`Mentions only`,
								desc: t`Members are notified only when mentioned`,
							},
						]}
					/>
				)}
			/>
		</SettingsSection>
	);
};
