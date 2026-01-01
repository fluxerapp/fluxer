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
import type React from 'react';
import * as GuildMemberActionCreators from '~/actions/GuildMemberActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import type {UserRecord} from '~/records/UserRecord';

export const KickMemberModal: React.FC<{guildId: string; targetUser: UserRecord}> = observer(
	({guildId, targetUser}) => {
		const {t} = useLingui();
		const handleKick = async () => {
			try {
				await GuildMemberActionCreators.kick(guildId, targetUser.id);
				ToastActionCreators.createToast({
					type: 'success',
					children: <Trans>Successfully kicked {targetUser.tag} from the community</Trans>,
				});
			} catch (error) {
				console.error('Failed to kick member:', error);
				ToastActionCreators.createToast({
					type: 'error',
					children: <Trans>Failed to kick member. Please try again.</Trans>,
				});
			}
		};

		return (
			<ConfirmModal
				title={t`Kick Member`}
				description={
					<div>
						<Trans>
							Are you sure you want to kick <strong>{targetUser.tag}</strong> from the community? They will be able to
							rejoin with a new invite.
						</Trans>
					</div>
				}
				primaryText={t`Kick`}
				primaryVariant="danger-primary"
				secondaryText={t`Cancel`}
				onPrimary={handleKick}
			/>
		);
	},
);
