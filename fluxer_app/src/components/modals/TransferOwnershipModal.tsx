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
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import type {GuildMemberRecord} from '~/records/GuildMemberRecord';
import type {UserRecord} from '~/records/UserRecord';
import {isAbortError} from '~/stores/SudoPromptStore';
import styles from './TransferOwnershipModal.module.css';

export const TransferOwnershipModal: React.FC<{
	guildId: string;
	targetUser: UserRecord;
	targetMember: GuildMemberRecord;
}> = observer(({guildId, targetUser}) => {
	const {t} = useLingui();
	const [isTransferring, setIsTransferring] = React.useState(false);

	const handleTransfer = async () => {
		setIsTransferring(true);
		try {
			await GuildActionCreators.transferOwnership(guildId, targetUser.id);
			ToastActionCreators.createToast({
				type: 'success',
				children: <Trans>Successfully transferred ownership to {targetUser.username}</Trans>,
			});
			ModalActionCreators.pop();
		} catch (error) {
			if (isAbortError(error)) {
				return;
			}
			console.error('Failed to transfer ownership:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: <Trans>Failed to transfer ownership. Please try again.</Trans>,
			});
		} finally {
			setIsTransferring(false);
		}
	};

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Transfer Community Ownership`} />
			<Modal.Content>
				<div className={styles.content}>
					<div className={styles.warningBox}>
						<p className={styles.warningText}>
							<Trans>
								You are about to transfer ownership of this community to <strong>{targetUser.username}</strong>. This
								action is <strong>irreversible</strong> and you will lose all owner privileges.
							</Trans>
						</p>
					</div>
				</div>
			</Modal.Content>
			<Modal.Footer>
				<Button variant="secondary" onClick={() => ModalActionCreators.pop()} disabled={isTransferring}>
					<Trans>Cancel</Trans>
				</Button>
				<Button variant="danger-primary" onClick={handleTransfer} disabled={isTransferring}>
					<Trans>Transfer Ownership</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
