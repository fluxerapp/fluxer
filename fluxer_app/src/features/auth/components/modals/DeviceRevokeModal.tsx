// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import * as AuthSessionCommands from '@app/features/auth/commands/AuthSessionCommands';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as FormUtils from '@app/lib/forms';
import {msg, plural} from '@lingui/core/macro';
import {Plural, Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const SIGN_OUT_ALL_OTHER_DEVICES_DESCRIPTOR = msg({
	message: 'Sign out all other devices',
	comment: 'Security modal title for revoking every saved login session except the current one.',
});
const DEVICE_REVOKED_DESCRIPTOR = msg({
	message: 'Device revoked',
	comment: 'Toast shown after a saved login session was signed out.',
});

interface DeviceRevokeModalProps {
	sessionIdHashes: Array<string>;
}

export const DeviceRevokeModal = observer(({sessionIdHashes}: DeviceRevokeModalProps) => {
	const {i18n} = useLingui();
	const [isSubmitting, setIsSubmitting] = useState(false);
	const sessionCount = sessionIdHashes.length;
	const title =
		sessionCount === 0
			? i18n._(SIGN_OUT_ALL_OTHER_DEVICES_DESCRIPTOR)
			: plural(
					{count: sessionCount},
					{
						one: 'Sign out # device',
						other: 'Sign out # devices',
					},
				);
	const handleConfirm = async () => {
		setIsSubmitting(true);
		try {
			await AuthSessionCommands.logout(sessionIdHashes);
			ModalCommands.pop();
			ToastCommands.createToast({
				type: 'success',
				children: i18n._(DEVICE_REVOKED_DESCRIPTOR),
			});
		} catch (error) {
			FormUtils.pushApiErrorModal(i18n, error);
		} finally {
			setIsSubmitting(false);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="auth.device-revoke-modal.modal-root">
			<Modal.Header title={title} data-flx="auth.device-revoke-modal.modal-header" />
			<Modal.Content data-flx="auth.device-revoke-modal.modal-content">
				<Modal.ContentLayout data-flx="auth.device-revoke-modal.modal-content-layout">
					<Modal.Description data-flx="auth.device-revoke-modal.modal-description">
						<Trans comment="Security warning explaining that selected saved login sessions will be signed out.">
							This will sign out the selected{' '}
							<Plural value={sessionCount} one="device" other="devices" data-flx="auth.device-revoke-modal.plural" />{' '}
							from your account. You will need to sign in again on each device.
						</Trans>
					</Modal.Description>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="auth.device-revoke-modal.modal-footer">
				<Button onClick={ModalCommands.pop} variant="secondary" data-flx="auth.device-revoke-modal.button.pop">
					<Trans comment="Button that closes the device logout confirmation without changes.">Cancel</Trans>
				</Button>
				<Button onClick={handleConfirm} submitting={isSubmitting} data-flx="auth.device-revoke-modal.button.confirm">
					<Trans comment="Button that confirms signing out selected saved devices.">Continue</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
