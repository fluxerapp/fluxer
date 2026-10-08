// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {getActiveInstanceProductName} from '@app/features/app/state/ActiveInstanceProductName';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import Guilds from '@app/features/guild/state/Guilds';
import {CANCEL_DESCRIPTOR, PAUSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import styles from '@app/features/invite/components/DisableInvitesButton.module.css';
import {INVITES_PAUSED_BECAUSE_RAID_DESCRIPTOR} from '@app/features/invite/utils/InviteMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const ENABLE_INVITES_FOR_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Enable invites for this community',
	comment: 'Button or menu action label in the invites disable invites button. Keep it concise.',
});
const PAUSE_INVITES_FOR_THIS_COMMUNITY_DESCRIPTOR = msg({
	message: 'Pause invites for this community',
	comment: 'Label in the invites disable invites button.',
});
const ENABLE_DESCRIPTOR = msg({
	message: 'Enable',
	comment: 'Button or menu action label in the invites disable invites button. Keep it concise.',
});
const ENABLE_INVITES_CONFIRM_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Enable invites? Users will be able to join this community through invite links again.',
	comment: 'Confirmation body shown before re-enabling invite links for a community.',
});
const PAUSE_INVITES_CONFIRM_DESCRIPTION_DESCRIPTOR = msg({
	message:
		"Pause invites? New users won't be able to join through invite links until you re-enable them. Existing members won't be affected.",
	comment: 'Confirmation body shown before pausing invite links for a community.',
});
const ENABLE_INVITES_DESCRIPTOR = msg({
	message: 'Enable invites',
	comment: 'Button label that re-enables invite links for a community.',
});
const PAUSE_INVITES_DESCRIPTOR = msg({
	message: 'Pause invites',
	comment: 'Button label that pauses invite links for a community.',
});
const INVITES_PAUSED_FOR_COMMUNITY_DESCRIPTOR = msg({
	message: 'Invites are paused for this community.',
	comment: 'Invite settings notice shown after admins pause invite links for a community.',
});
export const DisableInvitesButton = observer(({guildId}: {guildId: string}) => {
	const {i18n} = useLingui();
	const guild = Guilds.getGuild(guildId);
	const invitesDisabled = guild?.features.has('INVITES_DISABLED') ?? false;
	const isRaidDetected = guild?.features.has(GuildFeatures.RAID_DETECTED) ?? false;
	const handleToggleInvites = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={
						invitesDisabled
							? i18n._(ENABLE_INVITES_FOR_THIS_COMMUNITY_DESCRIPTOR)
							: i18n._(PAUSE_INVITES_FOR_THIS_COMMUNITY_DESCRIPTOR)
					}
					description={
						invitesDisabled
							? i18n._(ENABLE_INVITES_CONFIRM_DESCRIPTION_DESCRIPTOR)
							: i18n._(PAUSE_INVITES_CONFIRM_DESCRIPTION_DESCRIPTOR)
					}
					primaryText={invitesDisabled ? i18n._(ENABLE_DESCRIPTOR) : i18n._(PAUSE_DESCRIPTOR)}
					primaryVariant={invitesDisabled ? 'primary' : 'danger'}
					secondaryText={i18n._(CANCEL_DESCRIPTOR)}
					onPrimary={async () => {
						await GuildCommands.toggleFeature(guildId, GuildFeatures.INVITES_DISABLED, !invitesDisabled);
					}}
					data-flx="invite.disable-invites-button.handle-toggle-invites.confirm-modal"
				/>
			)),
		);
	}, [guildId, i18n, invitesDisabled]);
	return (
		<div className={styles.container} data-flx="invite.disable-invites-button.container">
			<Button
				variant={invitesDisabled ? 'danger' : 'secondary'}
				small={true}
				onClick={handleToggleInvites}
				data-flx="invite.disable-invites-button.button.toggle-invites"
			>
				{invitesDisabled ? i18n._(ENABLE_INVITES_DESCRIPTOR) : i18n._(PAUSE_INVITES_DESCRIPTOR)}
			</Button>
			{invitesDisabled && (
				<p className={styles.message} data-flx="invite.disable-invites-button.message">
					{isRaidDetected
						? i18n._(INVITES_PAUSED_BECAUSE_RAID_DESCRIPTOR, {productName: getActiveInstanceProductName()})
						: i18n._(INVITES_PAUSED_FOR_COMMUNITY_DESCRIPTOR)}
				</p>
			)}
		</div>
	);
});
