// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {WarningAlert} from '@app/features/ui/warning_alert/WarningAlert';
import * as RecoveryKitCommands from '@app/features/user/commands/RecoveryKitCommands';
import {AccountSecurityRow} from '@app/features/user/components/modals/tabs/account_security_tab/AccountSecurityCard';
import type {User} from '@app/features/user/models/User';
import RecoveryKitStatus from '@app/features/user/state/RecoveryKitStatus';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import * as FormUtils from '@app/lib/forms';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';

const RECOVERY_KIT_ERROR_DESCRIPTOR = msg({
	message: 'Could not create a recovery kit',
	comment: 'Error title shown when creating a new account recovery kit fails.',
});

const REPLACE_RECOVERY_KIT_TITLE_DESCRIPTOR = msg({
	message: 'Create a new recovery kit?',
	comment: 'Title of the confirmation shown before replacing an existing account recovery kit.',
});
const REPLACE_RECOVERY_KIT_DESCRIPTION_DESCRIPTOR = msg({
	message: 'Your current kit stops working as soon as the new one is created.',
	comment: 'Body of the confirmation shown before replacing an existing account recovery kit.',
});
const REPLACE_RECOVERY_KIT_CONFIRM_DESCRIPTOR = msg({
	message: 'Create a new kit',
	comment: 'Confirm button that replaces the existing account recovery kit with a new one.',
});

export function confirmNewRecoveryKit(i18n: I18n, onConfirm: () => Promise<void> | void): void {
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(REPLACE_RECOVERY_KIT_TITLE_DESCRIPTOR)}
				description={i18n._(REPLACE_RECOVERY_KIT_DESCRIPTION_DESCRIPTOR)}
				primaryText={i18n._(REPLACE_RECOVERY_KIT_CONFIRM_DESCRIPTOR)}
				onPrimary={onConfirm}
				data-flx="user.account-security-tab.recovery-kit-settings.replace-confirm-modal"
			/>
		)),
	);
}

function useRecoveryKitStatus(user: User) {
	const status = RecoveryKitStatus.get(user.id);
	useEffect(() => {
		if (status) return;
		void RecoveryKitCommands.fetchRecoveryKitStatus().catch(() => {});
	}, [status, user.id]);
	return status;
}

function useCreateRecoveryKit() {
	const {i18n} = useLingui();
	const [creating, setCreating] = useState(false);
	const create = useCallback(async () => {
		setCreating(true);
		try {
			await RecoveryKitCommands.createAndShowRecoveryKit();
		} catch (error) {
			FormUtils.pushApiErrorModal(i18n, error, i18n._(RECOVERY_KIT_ERROR_DESCRIPTOR));
		} finally {
			setCreating(false);
		}
	}, [i18n]);
	return {creating, create};
}

export const RecoveryKitRow = observer(({user}: {user: User}) => {
	const {i18n} = useLingui();
	const status = useRecoveryKitStatus(user);
	const {creating, create} = useCreateRecoveryKit();
	const createdAt = status?.createdAt ?? null;
	const missing = status != null && !status.hasRecoveryKit;
	return (
		<AccountSecurityRow
			label={<Trans>Recovery kit</Trans>}
			description={
				missing ? null : createdAt ? (
					<Trans>Created {DateUtils.getRelativeDateString(createdAt, i18n)}</Trans>
				) : (
					<Trans>Lets you reset your password if you forget it.</Trans>
				)
			}
			warning={missing ? <Trans>You don't have a recovery kit yet</Trans> : null}
			data-flx="user.account-security-tab.recovery-kit-settings.recovery-kit-row"
		>
			<Button
				small={true}
				variant={missing ? 'primary' : 'secondary'}
				submitting={creating}
				onClick={status?.hasRecoveryKit === false ? create : () => confirmNewRecoveryKit(i18n, create)}
				data-flx="user.account-security-tab.recovery-kit-settings.button.create"
			>
				{status?.hasRecoveryKit === false ? <Trans>Create recovery kit</Trans> : <Trans>Create a new kit</Trans>}
			</Button>
		</AccountSecurityRow>
	);
});

export const RecoveryKitReminderAlert = observer(({user}: {user: User}) => {
	const status = useRecoveryKitStatus(user);
	const {creating, create} = useCreateRecoveryKit();
	if (!status || status.hasRecoveryKit || RecoveryKitStatus.isReminderDismissed(user.id)) {
		return null;
	}
	return (
		<WarningAlert
			title={<Trans>Save a recovery kit</Trans>}
			actions={
				<>
					<Button
						small={true}
						submitting={creating}
						onClick={create}
						data-flx="user.account-security-tab.recovery-kit-settings.reminder.button.create"
					>
						<Trans>Create recovery kit</Trans>
					</Button>
					<Button
						small={true}
						variant="secondary"
						onClick={() => RecoveryKitStatus.dismissReminder(user.id)}
						data-flx="user.account-security-tab.recovery-kit-settings.reminder.button.dismiss"
					>
						<Trans>Not now</Trans>
					</Button>
				</>
			}
			data-flx="user.account-security-tab.recovery-kit-settings.reminder"
		>
			<Trans>If you forget your password, a recovery kit is the only way back into your account.</Trans>
		</WarningAlert>
	);
});
