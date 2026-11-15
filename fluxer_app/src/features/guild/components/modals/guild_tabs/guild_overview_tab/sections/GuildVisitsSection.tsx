// SPDX-License-Identifier: AGPL-3.0-or-later

import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {SettingsSection} from '@app/features/guild/components/modals/guild_tabs/guild_overview_tab/components/GuildOverviewTabSettingsSection';
import Guilds from '@app/features/guild/state/Guilds';
import GuildVisitsRollout from '@app/features/guild/state/GuildVisitsRollout';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const logger = new Logger('GuildVisitsSection');

const VISITORS_TITLE_DESCRIPTOR = msg({
	message: 'Previewing',
	context: 'community-previews',
	comment:
		'Heading of the community setting about people previewing the community before they join. Previewing means reading a community before joining it.',
});
const LET_PEOPLE_LOOK_AROUND_DESCRIPTOR = msg({
	message: 'Allow people to preview this community before joining',
	comment: 'Switch label in community settings. On by default for new communities, owners can turn it off.',
});
const LET_PEOPLE_LOOK_AROUND_HELP_DESCRIPTOR = msg({
	message:
		"People with an invite link can read the channels @everyone can see before they join. They can't post, react or see the member list.",
	comment: 'Help text under the switch that lets people preview a community before joining.',
});
const UPDATE_FAILED_DESCRIPTOR = msg({
	message: "Couldn't update this setting. Try again.",
	comment: 'Error toast when the previews setting could not be saved.',
});

export const GuildVisitsSection = observer(function GuildVisitsSection({
	guildId,
	canManageGuild,
}: {
	guildId: string;
	canManageGuild: boolean;
}) {
	const {i18n} = useLingui();
	const [saving, setSaving] = useState(false);
	const guild = Guilds.getGuild(guildId);
	if (!guild || !GuildVisitsRollout.enabled) return null;
	const enabled = guild.features.has(GuildFeatures.VISITS_ENABLED);
	const handleChange = async (next: boolean) => {
		if (saving || !canManageGuild) return;
		setSaving(true);
		try {
			await GuildCommands.toggleFeature(guildId, GuildFeatures.VISITS_ENABLED, next);
		} catch (error) {
			logger.error('Failed to toggle visits', error);
			ToastCommands.error(i18n._(UPDATE_FAILED_DESCRIPTOR));
		} finally {
			setSaving(false);
		}
	};
	return (
		<SettingsSection
			title={i18n._(VISITORS_TITLE_DESCRIPTOR)}
			data-flx="guild.guild-tabs.guild-overview-tab.guild-visits-section.settings-section"
		>
			<Switch
				label={i18n._(LET_PEOPLE_LOOK_AROUND_DESCRIPTOR)}
				description={i18n._(LET_PEOPLE_LOOK_AROUND_HELP_DESCRIPTOR)}
				value={enabled}
				onChange={handleChange}
				disabled={!canManageGuild || saving}
				data-flx="guild.guild-tabs.guild-overview-tab.guild-visits-section.switch"
			/>
		</SettingsSection>
	);
});
