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

import {useLingui} from '@lingui/react/macro';
import React from 'react';
import * as GuildActionCreators from '~/actions/GuildActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {GenericErrorModal} from '~/components/alerts/GenericErrorModal';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {Routes} from '~/Routes';
import * as RouterUtils from '~/utils/RouterUtils';

export const useLeaveGuild = () => {
	const {t} = useLingui();

	return React.useCallback(
		(guildId: string) => {
			ModalActionCreators.push(
				modal(() => (
					<ConfirmModal
						title={t`Leave Community`}
						description={t`Are you sure you want to leave this community? You will no longer be able to see any messages.`}
						primaryText={t`Leave Community`}
						primaryVariant="danger-primary"
						onPrimary={async () => {
							try {
								await GuildActionCreators.leave(guildId);
								RouterUtils.transitionTo(Routes.ME);
								ToastActionCreators.createToast({
									type: 'success',
									children: t`Left community`,
								});
							} catch (error) {
								console.error('Failed to leave community:', error);
								ModalActionCreators.push(
									modal(() => (
										<GenericErrorModal
											title={t`Failed to leave community`}
											message={t`We couldn't remove you from the community at this time.`}
										/>
									)),
								);
							}
						}}
					/>
				)),
			);
		},
		[t],
	);
};
