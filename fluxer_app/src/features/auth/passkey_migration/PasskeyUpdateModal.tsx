// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {Endpoints} from '@app/features/app/constants/Endpoints';
import {writePasskeyLoginRoute} from '@app/features/auth/passkey_migration/PasskeyLoginRoute';
import PasskeyMigration, {type PendingPasskeyMigration} from '@app/features/auth/passkey_migration/PasskeyMigration';
import styles from '@app/features/auth/passkey_migration/PasskeyUpdateModal.module.css';
import * as WebAuthnUtils from '@app/features/auth/utils/WebAuthnUtils';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import type {PublicKeyCredentialCreationOptionsJSON} from '@simplewebauthn/browser';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';

const PASSKEY_UPDATE_MODAL_KEY = 'passkey-update-modal';
const logger = new Logger('PasskeyUpdateModal');

const UPDATE_YOUR_PASSKEY_DESCRIPTOR = msg({
	message: 'Update your passkey',
	comment: 'Title of the modal that asks the user to save an updated passkey after signing in with an older one.',
});
const UPDATE_PASSKEY_BODY_DESCRIPTOR = msg({
	message: 'Your device will ask you to save an updated passkey for {name}. It replaces the old one.',
	comment:
		'Body of the modal that asks the user to save an updated passkey. name is the name the user gave the passkey. Keep plain.',
});
const UPDATE_PASSKEY_CROSS_DEVICE_BODY_DESCRIPTOR = msg({
	message:
		'Your device will ask you to save an updated passkey for {name}. Use the same phone or security key you just used.',
	comment:
		'Body of the modal that asks the user to save an updated passkey when they signed in with a phone or security key. name is the name the user gave the passkey. Keep plain.',
});
const UPDATE_PASSKEY_DESCRIPTOR = msg({
	message: 'Update passkey',
	comment: 'Primary button in the modal that saves an updated passkey.',
});
const NOT_NOW_DESCRIPTOR = msg({
	message: 'Not now',
	comment: 'Secondary button that closes the passkey update modal after an attempt failed.',
});
const COULDN_T_UPDATE_YOUR_PASSKEY_DESCRIPTOR = msg({
	message: "Couldn't update your passkey. Try again.",
	comment: 'Error shown in the passkey update modal when saving the updated passkey failed. Keep plain.',
});
const PASSKEY_UPDATED_DESCRIPTOR = msg({
	message: 'Passkey updated',
	comment: 'Toast shown after the user saved an updated passkey.',
});

const isAlreadyRegisteredError = (error: unknown): boolean =>
	error instanceof Error && error.name === 'InvalidStateError';

const PasskeyUpdateModal = observer(({pending}: {pending: PendingPasskeyMigration}) => {
	const {i18n} = useLingui();
	const [options, setOptions] = useState<PublicKeyCredentialCreationOptionsJSON | null>(null);
	const [optionsRequest, setOptionsRequest] = useState(0);
	const [submitting, setSubmitting] = useState(false);
	const [failed, setFailed] = useState(false);
	useEffect(() => {
		let current = true;
		setOptions(null);
		http
			.post<PublicKeyCredentialCreationOptionsJSON>(Endpoints.USER_MFA_WEBAUTHN_MIGRATION_REGISTRATION_OPTIONS)
			.then((response) => {
				if (current) {
					setOptions(response.body);
				}
			})
			.catch((error: unknown) => {
				if (current) {
					logger.error('Failed to get registration options for the passkey update', error);
					setFailed(true);
				}
			});
		return () => {
			current = false;
		};
	}, [optionsRequest]);
	const close = useCallback(() => {
		PasskeyMigration.clear();
		ModalCommands.popWithKey(PASSKEY_UPDATE_MODAL_KEY);
	}, []);
	const handleClose = useCallback(() => {
		if (failed && !submitting) {
			close();
		}
	}, [close, failed, submitting]);
	const handleUpdate = async () => {
		if (options === null) {
			setOptionsRequest((request) => request + 1);
			return;
		}
		setSubmitting(true);
		try {
			const credential = await WebAuthnUtils.performRegistration(options);
			await http.post(Endpoints.USER_MFA_WEBAUTHN_MIGRATION, {
				body: {response: credential, challenge: options.challenge},
			});
			close();
			ToastCommands.success(i18n._(PASSKEY_UPDATED_DESCRIPTOR));
		} catch (error) {
			if (isAlreadyRegisteredError(error)) {
				writePasskeyLoginRoute('native');
				close();
				return;
			}
			logger.error('Failed to update passkey', error);
			setFailed(true);
			setOptionsRequest((request) => request + 1);
		} finally {
			setSubmitting(false);
		}
	};
	return (
		<Modal.Root size="small" centered onClose={handleClose} data-flx="auth.passkey-update-modal.modal-root">
			<Modal.Header
				title={i18n._(UPDATE_YOUR_PASSKEY_DESCRIPTOR)}
				hideCloseButton={!failed}
				onClose={handleClose}
				data-flx="auth.passkey-update-modal.modal-header"
			/>
			<Modal.Content data-flx="auth.passkey-update-modal.modal-content">
				<Modal.ContentLayout data-flx="auth.passkey-update-modal.modal-content-layout">
					<Modal.Description data-flx="auth.passkey-update-modal.description">
						{i18n._(
							pending.cross_device ? UPDATE_PASSKEY_CROSS_DEVICE_BODY_DESCRIPTOR : UPDATE_PASSKEY_BODY_DESCRIPTOR,
							{name: pending.name},
						)}
					</Modal.Description>
					{failed && (
						<p className={styles.error} role="alert" data-flx="auth.passkey-update-modal.error">
							{i18n._(COULDN_T_UPDATE_YOUR_PASSKEY_DESCRIPTOR)}
						</p>
					)}
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="auth.passkey-update-modal.modal-footer">
				{failed && (
					<Button
						variant="secondary"
						onClick={close}
						disabled={submitting}
						data-flx="auth.passkey-update-modal.button.not-now"
					>
						{i18n._(NOT_NOW_DESCRIPTOR)}
					</Button>
				)}
				<Button
					onClick={handleUpdate}
					submitting={submitting || (options === null && !failed)}
					autoFocus
					data-flx="auth.passkey-update-modal.button.update"
				>
					{i18n._(UPDATE_PASSKEY_DESCRIPTOR)}
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});

export function openPasskeyUpdateModal(pending: PendingPasskeyMigration): void {
	ModalCommands.pushWithKey(
		modal(() => <PasskeyUpdateModal pending={pending} data-flx="auth.passkey-update-modal" />),
		PASSKEY_UPDATE_MODAL_KEY,
	);
}
