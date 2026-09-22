// SPDX-License-Identifier: AGPL-3.0-or-later

import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {CloneSettingUpdateFailedModal} from '@app/features/guild/components/alerts/CloneSettingUpdateFailedModal';
import styles from '@app/features/guild/components/modals/guild_tabs/CloneAllowedToggle.module.css';
import Guilds from '@app/features/guild/state/Guilds';
import Permission from '@app/features/permissions/state/Permission';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const logger = new Logger('CloneAllowedToggle');

interface CloneAllowedToggleProps {
	guildId: string;
	kind: 'emoji' | 'sticker';
}

export const CloneAllowedToggle = observer(({guildId, kind}: CloneAllowedToggleProps) => {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const canManageGuild = Permission.can(Permissions.MANAGE_GUILD, {guildId});
	const manageCommunityPermissionLabel = formatPermissionLabel(i18n, Permissions.MANAGE_GUILD);
	const [saving, setSaving] = useState(false);
	if (!guild) return null;
	const allowed = kind === 'emoji' ? guild.cloneEmojiAllowed : guild.cloneStickerAllowed;
	const handleChange = async (nextAllowed: boolean) => {
		if (saving || !canManageGuild) return;
		setSaving(true);
		try {
			const feature = kind === 'emoji' ? GuildFeatures.CLONE_EMOJI_ENABLED : GuildFeatures.CLONE_STICKER_ENABLED;
			await GuildCommands.toggleFeature(guildId, feature, nextAllowed);
		} catch (error) {
			logger.error(`Failed to toggle clone-${kind}-allowed for guild ${guildId}`, error);
			ModalCommands.push(
				modal(() => (
					<CloneSettingUpdateFailedModal
						kind={kind}
						data-flx="guild.guild-tabs.clone-allowed-toggle.handle-change.clone-setting-update-failed-modal"
					/>
				)),
			);
		} finally {
			setSaving(false);
		}
	};
	const label =
		kind === 'emoji' ? <Trans>Let others clone your emojis</Trans> : <Trans>Let others clone your stickers</Trans>;
	const description =
		kind === 'emoji' ? (
			<Trans>
				This is off until you turn it on. While it is on, members of other communities can use the in-app one-click
				"Clone" shortcut on your custom emojis. Turning it off does not prevent anyone from saving the image and
				uploading it themselves.
			</Trans>
		) : (
			<Trans>
				This is off until you turn it on. While it is on, members of other communities can use the in-app one-click
				"Clone" shortcut on your custom stickers. Turning it off does not prevent anyone from saving the image and
				uploading it themselves.
			</Trans>
		);
	return (
		<div className={styles.container} data-flx="guild.guild-tabs.clone-allowed-toggle.container">
			<Switch
				value={allowed}
				onChange={handleChange}
				disabled={!canManageGuild || saving}
				label={label}
				description={description}
				data-flx="guild.guild-tabs.clone-allowed-toggle.switch.change"
			/>
			{!canManageGuild && (
				<p className={styles.permissionHint} data-flx="guild.guild-tabs.clone-allowed-toggle.permission-hint">
					<Trans>Only members with the "{manageCommunityPermissionLabel}" permission can change this.</Trans>
				</p>
			)}
		</div>
	);
});
