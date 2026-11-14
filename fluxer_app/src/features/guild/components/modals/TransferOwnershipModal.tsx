// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {GroupOwnershipTransferFailedModal} from '@app/features/guild/components/alerts/GroupOwnershipTransferFailedModal';
import styles from '@app/features/guild/components/modals/TransferOwnershipModal.module.css';
import {
	TRANSFER_COMMUNITY_OWNERSHIP_DESCRIPTOR,
	TRANSFER_OWNERSHIP_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {isAbortError} from '@app/features/platform/utils/PlatformHelpers';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {WarningIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useState} from 'react';

const TRANSFER_OWNERSHIP_COUNTDOWN_DESCRIPTOR = msg({
	message: 'Transfer ownership ({secondsRemaining})',
	comment:
		'Disabled confirm button in the transfer ownership modal. The number is a countdown in seconds until the button can be pressed.',
});
const logger = new Logger('TransferOwnershipModal');
const TRANSFER_CONFIRMATION_SECONDS = 10;
export const TransferOwnershipModal: React.FC<{
	guildId: string;
	targetUser: User;
}> = observer(({guildId, targetUser}) => {
	const {i18n} = useLingui();
	const targetUserId = targetUser.id;
	const [isTransferring, setIsTransferring] = useState(false);
	const [secondsRemaining, setSecondsRemaining] = useState(TRANSFER_CONFIRMATION_SECONDS);
	useEffect(() => {
		if (secondsRemaining <= 0) return;
		const timer = window.setTimeout(() => setSecondsRemaining((seconds) => seconds - 1), 1000);
		return () => window.clearTimeout(timer);
	}, [secondsRemaining]);
	const handleTransfer = async () => {
		setIsTransferring(true);
		try {
			await GuildCommands.transferOwnership(guildId, targetUser.id);
			ToastCommands.createToast({
				type: 'success',
				children: <Trans>Transferred ownership to {NicknameUtils.getNickname(targetUser, guildId)}</Trans>,
			});
			ModalCommands.pop();
		} catch (error) {
			if (isAbortError(error)) {
				return;
			}
			logger.error('Failed to transfer ownership:', error);
			window.setTimeout(() => {
				ModalCommands.push(
					modal(() => (
						<GroupOwnershipTransferFailedModal
							username={NicknameUtils.getNickname(targetUser, guildId)}
							data-flx="guild.transfer-ownership-modal.group-ownership-transfer-failed-modal"
						/>
					)),
				);
			}, 0);
		} finally {
			setIsTransferring(false);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="guild.transfer-ownership-modal.modal-root">
			<Modal.Header
				title={i18n._(TRANSFER_COMMUNITY_OWNERSHIP_DESCRIPTOR)}
				data-flx="guild.transfer-ownership-modal.modal-header"
			/>
			<Modal.Content data-flx="guild.transfer-ownership-modal.modal-content">
				<Modal.ContentLayout data-flx="guild.transfer-ownership-modal.modal-content-layout">
					<div className={styles.warningBox} data-flx="guild.transfer-ownership-modal.warning-box">
						<WarningIcon
							className={styles.warningIcon}
							size={20}
							weight="fill"
							data-flx="guild.transfer-ownership-modal.warning-icon"
						/>
						<Modal.Description className={styles.warningText} data-flx="guild.transfer-ownership-modal.warning-text">
							<Trans comment="Warning in the ownership transfer confirmation. {0} is the new owner's display name and targetUserId is their numeric user ID, shown in brackets after the name.">
								You are about to transfer ownership of this community to{' '}
								<strong data-flx="guild.transfer-ownership-modal.strong">
									{NicknameUtils.getNickname(targetUser, guildId)}
								</strong>{' '}
								({targetUserId}). This action is{' '}
								<strong className={styles.dangerText} data-flx="guild.transfer-ownership-modal.danger-text">
									irreversible
								</strong>{' '}
								and you will lose all owner privileges.
							</Trans>
						</Modal.Description>
					</div>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="guild.transfer-ownership-modal.modal-footer">
				<Button
					variant="secondary"
					onClick={() => ModalCommands.pop()}
					disabled={isTransferring}
					data-flx="guild.transfer-ownership-modal.button.pop"
				>
					<Trans>Cancel</Trans>
				</Button>
				<Button
					variant="danger"
					onClick={handleTransfer}
					disabled={isTransferring || secondsRemaining > 0}
					leftIcon={<WarningIcon size={16} weight="fill" data-flx="guild.transfer-ownership-modal.warning-icon--2" />}
					data-flx="guild.transfer-ownership-modal.button.transfer"
				>
					{secondsRemaining > 0
						? i18n._(TRANSFER_OWNERSHIP_COUNTDOWN_DESCRIPTOR, {secondsRemaining})
						: i18n._(TRANSFER_OWNERSHIP_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
