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
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as NagbarActionCreators from '~/actions/NagbarActionCreators';
import {Nagbar} from '~/components/layout/Nagbar';
import {NagbarButton} from '~/components/layout/NagbarButton';
import {NagbarContent} from '~/components/layout/NagbarContent';
import {UserSettingsModal} from '~/components/modals/UserSettingsModal';
import UserStore from '~/stores/UserStore';

export const PendingBulkDeletionNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	const pending = UserStore.currentUser?.getPendingBulkMessageDeletion();
	const countFormatter = React.useMemo(() => new Intl.NumberFormat(), []);
	const scheduleKey = pending?.scheduledAt.toISOString();
	const handleHideNagbar = React.useCallback(() => {
		if (!scheduleKey) {
			return;
		}

		NagbarActionCreators.dismissPendingBulkDeletionNagbar(scheduleKey);
	}, [scheduleKey]);

	if (!pending) {
		return null;
	}

	const channelCountLabel = countFormatter.format(pending.channelCount);
	const messageCountLabel = countFormatter.format(pending.messageCount);
	const scheduledLabel = pending.scheduledAt.toLocaleString();

	const openDeletionSettings = () => {
		ModalActionCreators.push(
			modal(() => <UserSettingsModal initialTab="privacy_safety" initialSubtab="data-deletion" />),
		);
	};

	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor="var(--status-danger)"
			textColor="#ffffff"
			dismissible
			onDismiss={handleHideNagbar}
		>
			<NagbarContent
				isMobile={isMobile}
				message={
					<Trans>
						Deletion of <strong>{messageCountLabel}</strong> messages from <strong>{channelCountLabel}</strong> channels
						is scheduled for <strong>{scheduledLabel}</strong>. Cancel it from the Privacy Dashboard.
					</Trans>
				}
				actions={
					<>
						{isMobile && (
							<NagbarButton isMobile={isMobile} onClick={handleHideNagbar}>
								<Trans>Dismiss</Trans>
							</NagbarButton>
						)}
						<NagbarButton isMobile={isMobile} onClick={openDeletionSettings}>
							<Trans>Review deletion</Trans>
						</NagbarButton>
					</>
				}
			/>
		</Nagbar>
	);
});
