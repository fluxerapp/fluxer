// SPDX-License-Identifier: AGPL-3.0-or-later

import {openSelfAssignableRolesPage} from '@app/features/guild/commands/SelfAssignableRoleCommands';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildRolesTab.module.css';
import {GUILD_ROLES_TAB_ID} from '@app/features/guild/components/modals/guild_tabs/guild_roles_tab/shared';
import {EDIT_SECTIONS_DESCRIPTOR} from '@app/features/guild/components/self_assignable_roles/SelfAssignableRoleMessages';
import {useGuildFeatureToggle} from '@app/features/guild/hooks/useGuildFeatureToggle';
import Guilds from '@app/features/guild/state/Guilds';
import {canEditSelfAssignableRoles} from '@app/features/guild/utils/SelfAssignableRoles';
import Permission from '@app/features/permissions/state/Permission';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {useUnsavedChangesFlash} from '@app/features/user/hooks/useUnsavedChangesFlash';
import {extractErrorMessage} from '@app/lib/forms';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PencilSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const SELF_ASSIGNABLE_ROLES_SWITCH_LABEL_DESCRIPTOR = msg({
	message: 'Self-assignable roles',
	comment:
		'Switch label in the community roles settings. When on, members can give themselves the roles that managers list on the Roles page.',
});
const SELF_ASSIGNABLE_ROLES_SWITCH_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Members can give themselves roles from the Roles page.',
	comment:
		'Help text under the self-assignable roles switch in the community roles settings. "Roles page" is the page where members give themselves self-assignable roles.',
});

export const SelfAssignableRolesSwitch: React.FC<{guildId: string}> = observer(function SelfAssignableRolesSwitch({
	guildId,
}) {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const canManageGuild = Permission.can(Permissions.MANAGE_GUILD, {guildId});
	const canEdit = canEditSelfAssignableRoles(guildId);
	const {saving, setEnabled} = useGuildFeatureToggle(guildId, GuildFeatures.SELF_ASSIGNABLE_ROLES_ENABLED, (error) =>
		ToastCommands.error(extractErrorMessage(i18n, error)),
	);
	const {checkUnsavedChanges} = useUnsavedChangesFlash(GUILD_ROLES_TAB_ID);
	if (!guild) return null;
	const enabled = guild.selfAssignableRolesEnabled;
	const handleEditSections = () => {
		if (checkUnsavedChanges()) return;
		ModalCommands.pop();
		openSelfAssignableRolesPage(guildId);
	};
	return (
		<div
			className={styles.selfAssignableSwitch}
			data-flx="guild.guild-tabs.guild-roles-tab.self-assignable-roles-switch.self-assignable-switch"
		>
			<Switch
				label={i18n._(SELF_ASSIGNABLE_ROLES_SWITCH_LABEL_DESCRIPTOR)}
				description={i18n._(SELF_ASSIGNABLE_ROLES_SWITCH_DESCRIPTION_DESCRIPTOR)}
				value={enabled}
				onChange={setEnabled}
				disabled={!canManageGuild || saving}
				data-flx="guild.guild-tabs.guild-roles-tab.self-assignable-roles-switch.switch.change"
			/>
			{enabled && canEdit && (
				<Button
					variant="secondary"
					small={true}
					leftIcon={
						<PencilSimpleIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="guild.guild-tabs.guild-roles-tab.self-assignable-roles-switch.pencil-simple-icon"
						/>
					}
					onClick={handleEditSections}
					data-flx="guild.guild-tabs.guild-roles-tab.self-assignable-roles-switch.button.edit-sections"
				>
					{i18n._(EDIT_SECTIONS_DESCRIPTOR)}
				</Button>
			)}
		</div>
	);
});
