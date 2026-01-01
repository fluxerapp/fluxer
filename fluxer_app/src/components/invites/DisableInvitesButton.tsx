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
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as GuildActionCreators from '~/actions/GuildActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {Button} from '~/components/uikit/Button/Button';
import GuildStore from '~/stores/GuildStore';
import UserStore from '~/stores/UserStore';
import styles from './DisableInvitesButton.module.css';

export const DisableInvitesButton: React.FC<{guildId: string}> = observer(function DisableInvitesButton({guildId}) {
	const {t} = useLingui();
	const guild = GuildStore.getGuild(guildId);
	const currentUser = UserStore.currentUser;
	const invitesDisabled = guild?.features.has('INVITES_DISABLED') ?? false;

	const isOwner = guild?.ownerId === currentUser?.id;
	const isUnclaimed = currentUser != null && !currentUser.isClaimed();
	const isPreviewGuild = isOwner && isUnclaimed;

	const handleToggleInvites = React.useCallback(() => {
		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={invitesDisabled ? t`Enable invites for this community` : t`Disable invites for this community`}
					description={
						invitesDisabled ? (
							<Trans>
								Are you sure you want to enable invites? This will allow users to join this community through invite
								links again.
							</Trans>
						) : (
							<Trans>
								Are you sure you want to disable invites? This will prevent new users from joining through invite links
								until you re-enable them. Existing members will not be affected.
							</Trans>
						)
					}
					primaryText={invitesDisabled ? t`Enable` : t`Disable`}
					primaryVariant={invitesDisabled ? 'primary' : 'danger-primary'}
					secondaryText={t`Cancel`}
					onPrimary={async () => {
						await GuildActionCreators.toggleInvitesDisabled(guildId, !invitesDisabled);
					}}
				/>
			)),
		);
	}, [guildId, invitesDisabled]);

	if (isPreviewGuild) {
		return (
			<div className={styles.container}>
				<Button variant="secondary" small={true} disabled={true}>
					<Trans>Invites Locked</Trans>
				</Button>
				<p className={styles.message}>
					<Trans>
						Invites are locked for preview communities. Claim your account by setting an email and password to enable
						invites.
					</Trans>
				</p>
			</div>
		);
	}

	return (
		<div className={styles.container}>
			<Button variant={invitesDisabled ? 'danger-primary' : 'secondary'} small={true} onClick={handleToggleInvites}>
				{invitesDisabled ? <Trans>Enable Invites</Trans> : <Trans>Pause Invites</Trans>}
			</Button>
			{invitesDisabled && (
				<p className={styles.message}>
					<Trans>Invites are currently disabled for this community.</Trans>
				</p>
			)}
		</div>
	);
});
