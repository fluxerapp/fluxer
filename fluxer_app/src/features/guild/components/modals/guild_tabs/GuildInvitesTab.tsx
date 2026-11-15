// SPDX-License-Identifier: AGPL-3.0-or-later

import modalI18n from '@app/app/I18n';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import styles from '@app/features/guild/components/modals/guild_tabs/GuildInvitesTab.module.css';
import {
	TRY_AGAIN_DESCRIPTOR,
	TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {DisableInvitesButton} from '@app/features/invite/components/DisableInvitesButton';
import {InviteDateToggle} from '@app/features/invite/components/InviteDateToggle';
import {InviteListHeader, InviteListItem} from '@app/features/invite/components/InviteListItem';
import {useInviteRevoke} from '@app/features/invite/hooks/useInviteRevoke';
import Invites from '@app/features/invite/state/Invites';
import Permission from '@app/features/permissions/state/Permission';
import {Spinner} from '@app/features/ui/components/Spinner';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {UserPlusIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';

const InvitesLoadFailedModal_FAILED_TO_LOAD_INVITES_DESCRIPTOR = msg({
	message: "Couldn't load invites",
	comment: 'Error modal title shown when fetching invites fails.',
});

const GuildInvitesTab: React.FC<{guildId: string}> = observer(({guildId}) => {
	const {i18n} = useLingui();
	const invites = Invites.guildInvites.get(guildId) ?? null;
	const fetchStatus = Invites.getGuildInvitesFetchStatus(guildId);
	const handleRevoke = useInviteRevoke();
	const [showCreatedDate, setShowCreatedDate] = useState(false);
	const fetchInvites = useCallback(async () => {
		try {
			await GuildCommands.fetchGuildInvites(guildId);
		} catch (_error) {
			showGenericErrorModal({
				title: () => modalI18n._(InvitesLoadFailedModal_FAILED_TO_LOAD_INVITES_DESCRIPTOR),
				message: () => modalI18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx: 'invite.invites-load-failed-modal.generic-error-modal',
			});
		}
	}, [guildId]);
	const canManageGuild = Permission.can(Permissions.MANAGE_GUILD, {
		guildId,
	});
	useEffect(() => {
		if (fetchStatus === 'idle') {
			void fetchInvites();
		}
	}, [fetchStatus, fetchInvites]);
	return (
		<div className={styles.container} data-flx="guild.guild-tabs.guild-invites-tab.container">
			<div className={styles.header} data-flx="guild.guild-tabs.guild-invites-tab.header">
				<h2 className={styles.title} data-flx="guild.guild-tabs.guild-invites-tab.title">
					<Trans>Invites</Trans>
				</h2>
				<p className={styles.subtitle} data-flx="guild.guild-tabs.guild-invites-tab.subtitle">
					<Trans>
						View all invites for this community. To create a new invite, go to a channel and use the invite button.
					</Trans>
				</p>
			</div>
			{canManageGuild && (
				<DisableInvitesButton guildId={guildId} data-flx="guild.guild-tabs.guild-invites-tab.disable-invites-button" />
			)}
			{fetchStatus === 'pending' && (
				<div className={styles.spinnerContainer} data-flx="guild.guild-tabs.guild-invites-tab.spinner-container">
					<Spinner data-flx="guild.guild-tabs.guild-invites-tab.spinner" />
				</div>
			)}
			{fetchStatus === 'success' && invites && invites.length > 0 && (
				<div className={styles.invitesContainer} data-flx="guild.guild-tabs.guild-invites-tab.invites-container">
					<InviteDateToggle
						showCreatedDate={showCreatedDate}
						onToggle={setShowCreatedDate}
						data-flx="guild.guild-tabs.guild-invites-tab.invite-date-toggle"
					/>
					<div className={styles.inviteList} data-flx="guild.guild-tabs.guild-invites-tab.invite-list">
						<InviteListHeader
							showChannel={true}
							showCreatedDate={showCreatedDate}
							data-flx="guild.guild-tabs.guild-invites-tab.invite-list-header"
						/>
						<div className={styles.inviteItems} data-flx="guild.guild-tabs.guild-invites-tab.invite-items">
							{invites.map((invite) => (
								<InviteListItem
									key={invite.code}
									invite={invite}
									onRevoke={handleRevoke}
									showChannel={true}
									showCreatedDate={showCreatedDate}
									data-flx="guild.guild-tabs.guild-invites-tab.invite-list-item"
								/>
							))}
						</div>
					</div>
				</div>
			)}
			{fetchStatus === 'success' && invites && invites.length === 0 && (
				<StatusSlate
					Icon={UserPlusIcon}
					title={<Trans>No invite links</Trans>}
					description={
						<Trans>
							This community doesn't have any invite links yet. Go to a channel and create an invite to invite people.
						</Trans>
					}
					fullHeight={true}
					data-flx="guild.guild-tabs.guild-invites-tab.status-slate"
				/>
			)}
			{fetchStatus === 'error' && (
				<StatusSlate
					Icon={WarningCircleIcon}
					title={<Trans>Failed to load invites</Trans>}
					description={<Trans>There was an error loading the invites. Try again.</Trans>}
					actions={[
						{
							text: i18n._(TRY_AGAIN_DESCRIPTOR),
							onClick: fetchInvites,
							variant: 'primary',
						},
					]}
					fullHeight={true}
					data-flx="guild.guild-tabs.guild-invites-tab.status-slate--2"
				/>
			)}
		</div>
	);
});

export default GuildInvitesTab;
