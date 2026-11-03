// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import styles from '@app/features/guild/components/modals/GuildPrivacySettingsModal.module.css';
import Guilds from '@app/features/guild/state/Guilds';
import {
	DIRECT_MESSAGES_DESCRIPTOR,
	PRIVACY_SETTINGS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import UserSettings from '@app/features/user/state/UserSettings';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const ALLOW_DIRECT_MESSAGES_FROM_OTHER_MEMBERS_IN_THIS_DESCRIPTOR = msg({
	message: 'Let members of this community DM you',
	comment: 'Toggle label in the community privacy modal: allow DMs from non-friend members of this community.',
});
const BOT_DIRECT_MESSAGES_DESCRIPTOR = msg({
	message: 'Bot direct messages',
	comment: 'Short label in the guild privacy settings modal. Keep it concise. Keep the tone plain and specific.',
});
const ALLOW_BOTS_FROM_THIS_COMMUNITY_TO_SEND_YOU_DESCRIPTOR = msg({
	message: 'Let bots in this community DM you',
	comment: 'Toggle label in the community privacy modal: allow DMs from bots that are in this community.',
});
export const GuildPrivacySettingsModal = observer(({guildId}: {guildId: string}) => {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const restrictedGuilds = UserSettings.restrictedGuilds;
	const botRestrictedGuilds = UserSettings.botRestrictedGuilds;
	if (!guild) return null;
	const isDMsAllowed = !restrictedGuilds.includes(guildId);
	const isBotDMsAllowed = !botRestrictedGuilds.includes(guildId);
	const handleToggleDMs = async (value: boolean) => {
		let newRestrictedGuilds: Array<string>;
		if (value) {
			newRestrictedGuilds = restrictedGuilds.filter((id) => id !== guildId);
		} else {
			newRestrictedGuilds = [...restrictedGuilds, guildId];
		}
		await UserSettingsCommands.update({
			restrictedGuilds: newRestrictedGuilds,
		});
	};
	const handleToggleBotDMs = async (value: boolean) => {
		let newRestrictedGuilds: Array<string>;
		if (value) {
			newRestrictedGuilds = botRestrictedGuilds.filter((id) => id !== guildId);
		} else {
			newRestrictedGuilds = [...botRestrictedGuilds, guildId];
		}
		await UserSettingsCommands.update({
			botRestrictedGuilds: newRestrictedGuilds,
		});
	};
	return (
		<Modal.Root size="small" centered data-flx="guild.guild-privacy-settings-modal.modal-root">
			<Modal.Header
				title={i18n._(PRIVACY_SETTINGS_DESCRIPTOR)}
				data-flx="guild.guild-privacy-settings-modal.modal-header"
			/>
			<Modal.Content data-flx="guild.guild-privacy-settings-modal.modal-content">
				<div className={styles.container} data-flx="guild.guild-privacy-settings-modal.container">
					<Switch
						label={i18n._(DIRECT_MESSAGES_DESCRIPTOR)}
						description={i18n._(ALLOW_DIRECT_MESSAGES_FROM_OTHER_MEMBERS_IN_THIS_DESCRIPTOR)}
						value={isDMsAllowed}
						onChange={handleToggleDMs}
						data-flx="guild.guild-privacy-settings-modal.switch.toggle-d-ms"
					/>
					<Switch
						label={i18n._(BOT_DIRECT_MESSAGES_DESCRIPTOR)}
						description={i18n._(ALLOW_BOTS_FROM_THIS_COMMUNITY_TO_SEND_YOU_DESCRIPTOR)}
						value={isBotDMsAllowed}
						onChange={handleToggleBotDMs}
						data-flx="guild.guild-privacy-settings-modal.switch.toggle-bot-d-ms"
					/>
				</div>
			</Modal.Content>
		</Modal.Root>
	);
});
