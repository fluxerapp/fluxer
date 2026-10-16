// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {EXAMPLE_PERSONAL_EMAIL} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import {VERIFICATION_CODE_DESCRIPTOR, VERIFY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import type {User} from '@app/features/user/models/User';
import {
	EMAIL_CHANGED_DESCRIPTOR,
	INVALID_OR_EXPIRED_CODE_DESCRIPTOR,
	NEW_EMAIL_DESCRIPTOR,
	NEW_EMAIL_FORM_DESCRIPTOR,
	UNABLE_TO_RESEND_CODE_RIGHT_NOW_DESCRIPTOR,
} from '@app/features/user/utils/UserMessageDescriptors';
import * as FormUtils from '@app/lib/forms';
import {pushApiErrorModal} from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo, useState} from 'react';
import {useForm} from 'react-hook-form';

const UNABLE_TO_START_EMAIL_CHANGE_DESCRIPTOR = msg({
	message: 'Unable to start email change',
	comment: 'Error message in the email change modal.',
});

const CHANGE_YOUR_EMAIL_DESCRIPTOR = msg({
	message: 'Change your email',
	comment: 'Short label in the email change modal. Keep it concise.',
});

type Stage = 'intro' | 'verifyOriginal' | 'newEmail' | 'verifyNew';

const STAGE_ORDER: ReadonlyArray<Stage> = ['intro', 'verifyOriginal', 'newEmail', 'verifyNew'];

interface NewEmailForm {
	email: string;
}

interface EmailChangeModalProps {
	user: User;
}

export const EmailChangeModal = observer(({user}: EmailChangeModalProps) => {
	const {i18n} = useLingui();
	const resolveApiError = (error: unknown, fallback: string): string =>
		error && typeof error === 'object' && 'body' in error ? FormUtils.extractErrorMessage(i18n, error) : fallback;
	const newEmailForm = useForm<NewEmailForm>({defaultValues: {email: ''}});
	const [stage, setStage] = useState<Stage>('intro');
	const [ticket, setTicket] = useState<string | null>(null);
	const [originalProof, setOriginalProof] = useState<string | null>(null);
	const [originalCode, setOriginalCode] = useState<string>('');
	const [newCode, setNewCode] = useState<string>('');
	const [resendOriginalAt, setResendOriginalAt] = useState<Date | null>(null);
	const [resendNewAt, setResendNewAt] = useState<Date | null>(null);
	const [submitting, setSubmitting] = useState<boolean>(false);
	const [originalCodeError, setOriginalCodeError] = useState<string | null>(null);
	const [newCodeError, setNewCodeError] = useState<string | null>(null);
	const isEmailVerified = user.verified === true;
	const [now, setNow] = useState<number>(Date.now());
	useEffect(() => {
		const id = window.setInterval(() => setNow(Date.now()), 1000);
		return () => window.clearInterval(id);
	}, []);
	const canResendOriginal = useMemo(
		() => !resendOriginalAt || resendOriginalAt.getTime() <= now,
		[resendOriginalAt, now],
	);
	const canResendNew = useMemo(() => !resendNewAt || resendNewAt.getTime() <= now, [resendNewAt, now]);
	const originalSecondsRemaining = useMemo(
		() => (resendOriginalAt ? Math.max(0, Math.ceil((resendOriginalAt.getTime() - now) / 1000)) : 0),
		[resendOriginalAt, now],
	);
	const newSecondsRemaining = useMemo(
		() => (resendNewAt ? Math.max(0, Math.ceil((resendNewAt.getTime() - now) / 1000)) : 0),
		[resendNewAt, now],
	);
	const startFlow = async () => {
		setSubmitting(true);
		setOriginalCodeError(null);
		try {
			const result = await UserCommands.startEmailChange();
			setTicket(result.ticket);
			if (result.original_proof) {
				setOriginalProof(result.original_proof);
			}
			if (result.resend_available_at) {
				setResendOriginalAt(new Date(result.resend_available_at));
			}
			setStage(result.require_original ? 'verifyOriginal' : 'newEmail');
		} catch (error: unknown) {
			pushApiErrorModal(i18n, error, i18n._(UNABLE_TO_START_EMAIL_CHANGE_DESCRIPTOR));
		} finally {
			setSubmitting(false);
		}
	};
	const handleVerifyOriginal = async () => {
		if (!ticket) return;
		setSubmitting(true);
		setOriginalCodeError(null);
		try {
			const result = await UserCommands.verifyEmailChangeOriginal(ticket, originalCode);
			setOriginalProof(result.original_proof);
			setStage('newEmail');
		} catch (error: unknown) {
			setOriginalCodeError(resolveApiError(error, i18n._(INVALID_OR_EXPIRED_CODE_DESCRIPTOR)));
		} finally {
			setSubmitting(false);
		}
	};
	const handleResendOriginal = async () => {
		if (!ticket || !canResendOriginal) return;
		setSubmitting(true);
		try {
			await UserCommands.resendEmailChangeOriginal(ticket);
			setResendOriginalAt(new Date(Date.now() + 30 * 1000));
		} catch (error: unknown) {
			pushApiErrorModal(i18n, error, i18n._(UNABLE_TO_RESEND_CODE_RIGHT_NOW_DESCRIPTOR));
		} finally {
			setSubmitting(false);
		}
	};
	const handleRequestNew = async (data: NewEmailForm) => {
		if (!ticket || !originalProof) return;
		setSubmitting(true);
		try {
			const result = await UserCommands.requestEmailChangeNew(ticket, data.email, originalProof);
			setResendNewAt(result.resend_available_at ? new Date(result.resend_available_at) : null);
			setStage('verifyNew');
		} finally {
			setSubmitting(false);
		}
	};
	const handleResendNew = async () => {
		if (!ticket || !canResendNew) return;
		setSubmitting(true);
		try {
			await UserCommands.resendEmailChangeNew(ticket);
			setResendNewAt(new Date(Date.now() + 30 * 1000));
		} catch (error: unknown) {
			pushApiErrorModal(i18n, error, i18n._(UNABLE_TO_RESEND_CODE_RIGHT_NOW_DESCRIPTOR));
		} finally {
			setSubmitting(false);
		}
	};
	const handleVerifyNew = async () => {
		if (!ticket || !originalProof) return;
		setSubmitting(true);
		setNewCodeError(null);
		try {
			const result = await UserCommands.verifyEmailChangeNew(ticket, newCode, originalProof);
			await UserCommands.update({email_token: result.email_token});
			ToastCommands.createToast({type: 'success', children: i18n._(EMAIL_CHANGED_DESCRIPTOR)});
			ModalCommands.pop();
		} catch (error: unknown) {
			setNewCodeError(resolveApiError(error, i18n._(INVALID_OR_EXPIRED_CODE_DESCRIPTOR)));
		} finally {
			setSubmitting(false);
		}
	};
	const {handleSubmit: handleNewEmailSubmit} = useFormSubmit({
		form: newEmailForm,
		onSubmit: handleRequestNew,
		defaultErrorField: 'email',
	});
	const renderIntroStage = () => (
		<Modal.Description data-flx="user.email-change-modal.modal-description">
			{isEmailVerified ? (
				<Trans>We'll verify your current email and then your new email with one-time codes.</Trans>
			) : (
				<Trans>We'll verify your new email with a one-time code.</Trans>
			)}
		</Modal.Description>
	);
	const renderVerifyOriginalStage = () => (
		<>
			<Modal.Description data-flx="user.email-change-modal.modal-description--2">
				<Trans>Enter the code sent to your current email.</Trans>
			</Modal.Description>
			<Modal.InputGroup data-flx="user.email-change-modal.modal-input-group">
				<Input
					autoFocus={true}
					value={originalCode}
					onChange={(event: React.ChangeEvent<HTMLInputElement>) => setOriginalCode(event.target.value)}
					label={i18n._(VERIFICATION_CODE_DESCRIPTOR)}
					placeholder="XXXX-XXXX"
					required={true}
					error={originalCodeError ?? undefined}
					data-flx="user.email-change-modal.input.set-original-code"
				/>
			</Modal.InputGroup>
		</>
	);
	const renderNewEmailStage = () => (
		<Form
			form={newEmailForm}
			onSubmit={handleNewEmailSubmit}
			aria-label={i18n._(NEW_EMAIL_FORM_DESCRIPTOR)}
			data-flx="user.email-change-modal.form.new-email-submit"
		>
			<Modal.Description data-flx="user.email-change-modal.modal-description--3">
				<Trans>Enter the new email you want to use. We'll send a code there next.</Trans>
			</Modal.Description>
			<Modal.InputGroup data-flx="user.email-change-modal.modal-input-group--2">
				<Input
					data-flx="user.email-change-modal.input.email"
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
			<Modal.Description data-flx="user.email-change-modal.modal-description--4">
				<Trans>Enter the code we emailed to your new address.</Trans>
			</Modal.Description>
			<Modal.InputGroup data-flx="user.email-change-modal.modal-input-group--3">
				<Input
					autoFocus={true}
					value={newCode}
					onChange={(event: React.ChangeEvent<HTMLInputElement>) => setNewCode(event.target.value)}
					label={i18n._(VERIFICATION_CODE_DESCRIPTOR)}
					placeholder="XXXX-XXXX"
					required={true}
					error={newCodeError ?? undefined}
					data-flx="user.email-change-modal.input.set-new-code"
				/>
			</Modal.InputGroup>
		</>
	);
	const renderStageBody = () => {
		switch (stage) {
			case 'intro':
				return renderIntroStage();
			case 'verifyOriginal':
				return renderVerifyOriginalStage();
			case 'newEmail':
				return renderNewEmailStage();
			case 'verifyNew':
				return renderVerifyNewStage();
		}
	};
	const renderStageFooter = () => {
		switch (stage) {
			case 'intro':
				return (
					<>
						<Button onClick={ModalCommands.pop} variant="secondary" data-flx="user.email-change-modal.button.pop">
							<Trans>Cancel</Trans>
						</Button>
						<Button onClick={startFlow} submitting={submitting} data-flx="user.email-change-modal.button.start-flow">
							<Trans>Start</Trans>
						</Button>
					</>
				);
			case 'verifyOriginal':
				return (
					<>
						<Button onClick={ModalCommands.pop} variant="secondary" data-flx="user.email-change-modal.button.pop--2">
							<Trans>Cancel</Trans>
						</Button>
						<Button
							onClick={handleResendOriginal}
							disabled={!canResendOriginal || submitting}
							data-flx="user.email-change-modal.button.resend-original"
						>
							{canResendOriginal ? <Trans>Resend</Trans> : <Trans>Resend ({originalSecondsRemaining}s)</Trans>}
						</Button>
						<Button
							onClick={handleVerifyOriginal}
							submitting={submitting}
							data-flx="user.email-change-modal.button.verify-original"
						>
							{i18n._(VERIFY_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'newEmail':
				return (
					<>
						<Button onClick={ModalCommands.pop} variant="secondary" data-flx="user.email-change-modal.button.pop--3">
							<Trans>Cancel</Trans>
						</Button>
						<Button
							onClick={handleNewEmailSubmit}
							submitting={submitting}
							data-flx="user.email-change-modal.button.submit"
						>
							<Trans>Send code</Trans>
						</Button>
					</>
				);
			case 'verifyNew':
				return (
					<>
						<Button onClick={ModalCommands.pop} variant="secondary" data-flx="user.email-change-modal.button.pop--4">
							<Trans>Cancel</Trans>
						</Button>
						<Button
							onClick={handleResendNew}
							disabled={!canResendNew || submitting}
							data-flx="user.email-change-modal.button.resend-new"
						>
							{canResendNew ? <Trans>Resend</Trans> : <Trans>Resend ({newSecondsRemaining}s)</Trans>}
						</Button>
						<Button
							onClick={handleVerifyNew}
							submitting={submitting}
							data-flx="user.email-change-modal.button.verify-new"
						>
							<Trans>Confirm</Trans>
						</Button>
					</>
				);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="user.email-change-modal.modal-root">
			<Modal.Header title={i18n._(CHANGE_YOUR_EMAIL_DESCRIPTOR)} data-flx="user.email-change-modal.modal-header" />
			<Modal.Content data-flx="user.email-change-modal.modal-content">
				<Modal.ContentLayout data-flx="user.email-change-modal.modal-content-layout">
					<SteppedCarousel step={stage} steps={STAGE_ORDER} data-flx="user.email-change-modal.stepped-carousel">
						{renderStageBody()}
					</SteppedCarousel>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="user.email-change-modal.footer">{renderStageFooter()}</Modal.Footer>
		</Modal.Root>
	);
});
