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

import {Trans} from '@lingui/react/macro';
import {UserPlusIcon, WarningOctagonIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {Permissions} from '~/Constants';
import {InvitesLoadFailedModal} from '~/components/alerts/InvitesLoadFailedModal';
import {DisableInvitesButton} from '~/components/invites/DisableInvitesButton';
import {InviteDateToggle} from '~/components/invites/InviteDateToggle';
import {InviteListHeader, InviteListItem} from '~/components/invites/InviteListItem';
import {InviteModal} from '~/components/modals/InviteModal';
import {StatusSlate} from '~/components/modals/shared/StatusSlate';
import {Button} from '~/components/uikit/Button/Button';
import {Spinner} from '~/components/uikit/Spinner';
import {useInviteRevoke} from '~/hooks/useInviteRevoke';
import ChannelStore from '~/stores/ChannelStore';
import InviteStore from '~/stores/InviteStore';
import PermissionStore from '~/stores/PermissionStore';
import * as InviteUtils from '~/utils/InviteUtils';
import styles from './ChannelInvitesTab.module.css';

const ChannelInvitesTab: React.FC<{channelId: string}> = observer(({channelId}) => {
	const channel = ChannelStore.getChannel(channelId);
	const invites = InviteStore.channelInvites.get(channelId) ?? null;
	const fetchStatus = InviteStore.channelInvitesFetchStatus.get(channelId) ?? 'idle';
	const handleRevoke = useInviteRevoke();
	const [showCreatedDate, setShowCreatedDate] = React.useState(false);

	const canInvite = InviteUtils.canInviteToChannel(channelId, channel?.guildId);

	const canManageGuild = PermissionStore.can(Permissions.MANAGE_GUILD, {
		guildId: channel?.guildId,
	});

	const refreshInvites = React.useCallback(async () => {
		try {
			await ChannelActionCreators.fetchChannelInvites(channelId);
		} catch (_error) {
			ModalActionCreators.push(modal(() => <InvitesLoadFailedModal />));
		}
	}, [channelId]);

	React.useEffect(() => {
		if (fetchStatus === 'idle') {
			void refreshInvites();
		}
	}, [channelId, fetchStatus, refreshInvites]);

	const handleCreateInvite = React.useCallback(() => {
		ModalActionCreators.push(modal(() => <InviteModal channelId={channelId} />));
	}, [channelId]);

	return (
		<div className={styles.container}>
			<div>
				<h2 className={styles.header}>
					<Trans>Invite Links</Trans>
				</h2>
				<p className={styles.description}>
					<Trans>Manage invites for this channel.</Trans>
				</p>
			</div>

			<div className={styles.buttonGroup}>
				<Button small={true} disabled={!canInvite || fetchStatus === 'pending'} onClick={handleCreateInvite}>
					<Trans>Create Invite</Trans>
				</Button>
				{canManageGuild && channel?.guildId && <DisableInvitesButton guildId={channel.guildId} />}
			</div>

			{fetchStatus === 'pending' && (
				<div className={styles.spinnerContainer}>
					<Spinner />
				</div>
			)}

			{fetchStatus === 'success' && invites && invites.length > 0 && (
				<div className={styles.invitesContainer}>
					<InviteDateToggle showCreatedDate={showCreatedDate} onToggle={setShowCreatedDate} />
					<div className={styles.invitesList}>
						<InviteListHeader showCreatedDate={showCreatedDate} />
						<div className={styles.inviteItems}>
							{invites.map((invite) => (
								<InviteListItem
									key={invite.code}
									invite={invite}
									onRevoke={handleRevoke}
									showCreatedDate={showCreatedDate}
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
						<Trans>This channel doesn't have any invite links yet. Create one to invite people to this channel.</Trans>
					}
					actions={
						canInvite
							? [
									{
										text: <Trans>Create Invite</Trans>,
										onClick: handleCreateInvite,
										variant: 'primary',
									},
								]
							: undefined
					}
					fullHeight={true}
				/>
			)}

			{fetchStatus === 'error' && (
				<StatusSlate
					Icon={WarningOctagonIcon}
					title={<Trans>Failed to load invites</Trans>}
					description={<Trans>There was an error loading the invite links for this channel. Please try again.</Trans>}
					actions={[
						{
							text: <Trans>Try Again</Trans>,
							onClick: refreshInvites,
							variant: 'primary',
						},
					]}
					fullHeight={true}
				/>
			)}
		</div>
	);
});

export default ChannelInvitesTab;
