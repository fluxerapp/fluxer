// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {EXAMPLE_PERSONAL_EMAIL} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import {VERIFICATION_CODE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import Users from '@app/features/user/state/Users';
import * as FormUtils from '@app/lib/forms';
import {pushApiErrorModal} from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo, useState} from 'react';
import {useForm} from 'react-hook-form';

const ADD_A_WORKING_EMAIL_DESCRIPTOR = msg({
	message: 'Add a working email',
	comment: 'Title of the modal that replaces an email address that bounced. Keep it concise.',
});
const NEW_EMAIL_FORM_DESCRIPTOR = msg({
	message: 'New email form',
	comment: 'Short label in the email change modal. Keep it concise.',
});
const NEW_EMAIL_DESCRIPTOR = msg({
	message: 'New email',
	comment: 'Short label in the email change modal. Keep it concise.',
});
const INVALID_OR_EXPIRED_CODE_DESCRIPTOR = msg({
	message: 'Invalid or expired code',
	comment: 'Error message in the email change modal.',
});
const UNABLE_TO_RESEND_CODE_RIGHT_NOW_DESCRIPTOR = msg({
	message: 'Unable to resend code right now',
	comment: 'Error message in the email change modal.',
});
const EMAIL_CHANGED_DESCRIPTOR = msg({
	message: 'Email changed',
	comment: 'Short label in the email change modal. Keep it concise.',
});

type Stage = 'newEmail' | 'verifyNew';

const STAGE_ORDER: ReadonlyArray<Stage> = ['newEmail', 'verifyNew'];
const RESEND_COOLDOWN_MS = 30 * 1000;

interface NewEmailForm {
	email: string;
}

export const BouncedEmailChangeModal = observer(() => {
	const {i18n} = useLingui();
	const newEmailForm = useForm<NewEmailForm>({defaultValues: {email: ''}});
	const [stage, setStage] = useState<Stage>('newEmail');
	const [ticket, setTicket] = useState<string | null>(null);
	const [code, setCode] = useState<string>('');
	const [codeError, setCodeError] = useState<string | null>(null);
	const [resendAt, setResendAt] = useState<Date | null>(null);
	const [submitting, setSubmitting] = useState<boolean>(false);
	const [now, setNow] = useState<number>(Date.now());
	useEffect(() => {
		const id = window.setInterval(() => setNow(Date.now()), 1000);
		return () => window.clearInterval(id);
	}, []);
	const canResend = useMemo(() => !resendAt || resendAt.getTime() <= now, [resendAt, now]);
	const secondsRemaining = useMemo(
		() => (resendAt ? Math.max(0, Math.ceil((resendAt.getTime() - now) / 1000)) : 0),
		[resendAt, now],
	);
	const handleRequestNew = async (data: NewEmailForm) => {
		setSubmitting(true);
		try {
			const result = await UserCommands.requestBouncedEmailChangeNew(data.email);
			setTicket(result.ticket);
			setResendAt(result.resend_available_at ? new Date(result.resend_available_at) : null);
			setCode('');
			setCodeError(null);
			setStage('verifyNew');
		} finally {
			setSubmitting(false);
		}
	};
	const handleResend = async () => {
		if (!ticket || !canResend) return;
		setSubmitting(true);
		try {
			await UserCommands.resendBouncedEmailChangeNew(ticket);
			setResendAt(new Date(Date.now() + RESEND_COOLDOWN_MS));
		} catch (error: unknown) {
			pushApiErrorModal(i18n, error, i18n._(UNABLE_TO_RESEND_CODE_RIGHT_NOW_DESCRIPTOR));
		} finally {
			setSubmitting(false);
		}
	};
	const handleVerify = async () => {
		if (!ticket) return;
		setSubmitting(true);
		setCodeError(null);
		try {
			const updatedUser = await UserCommands.verifyBouncedEmailChangeNew(ticket, code.split(' ').join(''));
			Users.handleUserUpdate(updatedUser, {clearMissingOptionalFields: true});
			ToastCommands.createToast({type: 'success', children: i18n._(EMAIL_CHANGED_DESCRIPTOR)});
			ModalCommands.pop();
		} catch (error: unknown) {
			setCodeError(
				error && typeof error === 'object' && 'body' in error
					? FormUtils.extractErrorMessage(i18n, error)
					: i18n._(INVALID_OR_EXPIRED_CODE_DESCRIPTOR),
			);
		} finally {
			setSubmitting(false);
		}
	};
	const {handleSubmit: handleNewEmailSubmit} = useFormSubmit({
		form: newEmailForm,
		onSubmit: handleRequestNew,
		defaultErrorField: 'email',
	});
	const renderNewEmailStage = () => (
		<Form
			form={newEmailForm}
			onSubmit={handleNewEmailSubmit}
			aria-label={i18n._(NEW_EMAIL_FORM_DESCRIPTOR)}
			data-flx="user.bounced-email-change-modal.form.new-email-submit"
		>
			<Modal.Description data-flx="user.bounced-email-change-modal.modal-description">
				<Trans>
					Emails to your current address are not being delivered. Enter a new email and we'll send a code there.
				</Trans>
			</Modal.Description>
			<Modal.InputGroup data-flx="user.bounced-email-change-modal.modal-input-group">
				<Input
					data-flx="user.bounced-email-change-modal.input.email"
					{...newEmailForm.register('email')}
					autoComplete="email"
					autoFocus={true}
					error={newEmailForm.formState.errors.email?.message}
					label={i18n._(NEW_EMAIL_DESCRIPTOR)}
					maxLength={256}
					minLength={1}
					placeholder={EXAMPLE_PERSONAL_EMAIL}
					required={true}
					type="email"
				/>
			</Modal.InputGroup>
		</Form>
	);
	const renderVerifyNewStage = () => (
		<>
			<Modal.Description data-flx="user.bounced-email-change-modal.modal-description--2">
				<Trans>Enter the code we emailed to your new address.</Trans>
			</Modal.Description>
			<Modal.InputGroup data-flx="user.bounced-email-change-modal.modal-input-group--2">
				<Input
					autoFocus={true}
					value={code}
					onChange={(event: React.ChangeEvent<HTMLInputElement>) => setCode(event.target.value)}
					label={i18n._(VERIFICATION_CODE_DESCRIPTOR)}
					placeholder="XXXX-XXXX"
					required={true}
					error={codeError ?? undefined}
					data-flx="user.bounced-email-change-modal.input.set-code"
				/>
			</Modal.InputGroup>
		</>
	);
	return (
		<Modal.Root size="small" centered data-flx="user.bounced-email-change-modal.modal-root">
			<Modal.Header
				title={i18n._(ADD_A_WORKING_EMAIL_DESCRIPTOR)}
				data-flx="user.bounced-email-change-modal.modal-header"
			/>
			<Modal.Content data-flx="user.bounced-email-change-modal.modal-content">
				<Modal.ContentLayout data-flx="user.bounced-email-change-modal.modal-content-layout">
					<SteppedCarousel step={stage} steps={STAGE_ORDER} data-flx="user.bounced-email-change-modal.stepped-carousel">
						{stage === 'newEmail' ? renderNewEmailStage() : renderVerifyNewStage()}
					</SteppedCarousel>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="user.bounced-email-change-modal.footer">
				<Button onClick={ModalCommands.pop} variant="secondary" data-flx="user.bounced-email-change-modal.button.pop">
					<Trans>Cancel</Trans>
				</Button>
				{stage === 'newEmail' ? (
					<Button
						onClick={handleNewEmailSubmit}
						submitting={submitting}
						data-flx="user.bounced-email-change-modal.button.submit"
					>
						<Trans>Send code</Trans>
					</Button>
				) : (
					<>
						<Button
							onClick={handleResend}
							disabled={!canResend || submitting}
							data-flx="user.bounced-email-change-modal.button.resend"
						>
							{canResend ? <Trans>Resend</Trans> : <Trans>Resend ({secondsRemaining}s)</Trans>}
						</Button>
						<Button
							onClick={handleVerify}
							submitting={submitting}
							data-flx="user.bounced-email-change-modal.button.verify"
						>
							<Trans>Confirm</Trans>
						</Button>
					</>
				)}
			</Modal.Footer>
		</Modal.Root>
	);
});
