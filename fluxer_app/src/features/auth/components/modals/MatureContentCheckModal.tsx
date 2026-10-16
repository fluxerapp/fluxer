// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {
	CANCEL_DESCRIPTOR,
	COMPLETE_MATURE_CONTENT_CHECK_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as MatureContentCheckCommands from '@app/features/mature_content/commands/MatureContentCheckCommands';
import {isMatureContentCheckAvailableInRegion} from '@app/features/moderation/utils/MatureContentGeoUtils';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useRef, useState} from 'react';

const SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'Something went wrong. Try again.',
	comment: 'Toast error shown when the mature content check request fails unexpectedly.',
});
const MATURE_CONTENT_CHECK_DESCRIPTOR = msg({
	message: 'Mature content check',
	comment: 'Title of the modal that starts the credit-card check for mature content access.',
});
const MATURE_CONTENT_CHECK_REQUIRED_DESCRIPTOR = msg({
	message:
		"UK law requires a mature content check. To continue, you'll complete a zero-amount credit card authorization through Stripe. Your card will not be charged.",
	comment:
		'Body copy in the mature content check modal. The Stripe authorization has no amount and the card is never charged.',
});
const CREDIT_CARDS_ONLY_DESCRIPTOR = msg({
	message: 'Only credit cards are accepted. Debit and prepaid cards cannot be used for the mature content check.',
	comment: 'Body copy in the mature content check modal.',
});
const MATURE_CONTENT_CHECKS_UK_ONLY_DESCRIPTOR = msg({
	message: 'Mature content checks are currently available only in the UK.',
	comment: 'Body copy in the mature content check modal when the check is unavailable.',
});
export const MatureContentCheckModal = observer(() => {
	const {i18n} = useLingui();
	const [submitting, setSubmitting] = useState(false);
	const [error, setError] = useState<string | null>(null);
	const initialFocusRef = useRef<HTMLButtonElement | null>(null);
	const matureContentCheckAvailable = isMatureContentCheckAvailableInRegion();
	const handleVerify = useCallback(async () => {
		setSubmitting(true);
		setError(null);
		try {
			const url = await MatureContentCheckCommands.createMatureContentCheckSession();
			ModalCommands.pop();
			await openExternalUrl(url);
		} catch {
			setError(i18n._(SOMETHING_WENT_WRONG_PLEASE_TRY_AGAIN_DESCRIPTOR));
		} finally {
			setSubmitting(false);
		}
	}, [i18n]);
	const handleCancel = useCallback(() => {
		ModalCommands.pop();
	}, []);
	return (
		<Modal.Root
			size="small"
			initialFocusRef={initialFocusRef}
			centered
			data-flx="auth.mature-content-check-modal.modal-root"
		>
			<Modal.Header
				title={i18n._(MATURE_CONTENT_CHECK_DESCRIPTOR)}
				data-flx="auth.mature-content-check-modal.modal-header"
			/>
			<Modal.Content data-flx="auth.mature-content-check-modal.modal-content">
				<Modal.ContentLayout data-flx="auth.mature-content-check-modal.modal-content-layout">
					{matureContentCheckAvailable ? (
						<>
							<Modal.Description data-flx="auth.mature-content-check-modal.modal-description">
								{i18n._(MATURE_CONTENT_CHECK_REQUIRED_DESCRIPTOR)}
							</Modal.Description>
							<Modal.Description data-flx="auth.mature-content-check-modal.modal-description--2">
								{i18n._(CREDIT_CARDS_ONLY_DESCRIPTOR)}
							</Modal.Description>
						</>
					) : (
						<Modal.Description data-flx="auth.mature-content-check-modal.modal-description--3">
							{i18n._(MATURE_CONTENT_CHECKS_UK_ONLY_DESCRIPTOR)}
						</Modal.Description>
					)}
					{error != null && (
						<Modal.Description data-flx="auth.mature-content-check-modal.modal-description--4">
							{error}
						</Modal.Description>
					)}
				</Modal.ContentLayout>
			</Modal.Content>
			{matureContentCheckAvailable && (
				<Modal.Footer data-flx="auth.mature-content-check-modal.modal-footer">
					<Button onClick={handleCancel} variant="secondary" data-flx="auth.mature-content-check-modal.button.cancel">
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button
						onClick={handleVerify}
						submitting={submitting}
						variant="primary"
						ref={initialFocusRef}
						data-flx="auth.mature-content-check-modal.button.verify"
					>
						{i18n._(COMPLETE_MATURE_CONTENT_CHECK_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			)}
		</Modal.Root>
	);
});
