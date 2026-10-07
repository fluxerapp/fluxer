// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	CONFIRM_NEW_PASSWORD_DESCRIPTOR,
	NEW_PASSWORD_DESCRIPTOR,
	PASSWORDS_DO_NOT_MATCH_DESCRIPTOR,
	VERIFICATION_CODE_DESCRIPTOR,
	VERIFY_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import * as RecoveryKitCommands from '@app/features/user/commands/RecoveryKitCommands';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import RecoveryKitStatus from '@app/features/user/state/RecoveryKitStatus';
import Users from '@app/features/user/state/Users';
import * as FormUtils from '@app/lib/forms';
import {pushApiErrorModal} from '@app/lib/forms';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useState} from 'react';
import {useForm} from 'react-hook-form';

const UNABLE_TO_START_PASSWORD_CHANGE_DESCRIPTOR = msg({
	message: 'Unable to start password change',
	comment: 'Error message in the password change modal. Keep the tone plain and specific.',
});
const INVALID_OR_EXPIRED_CODE_DESCRIPTOR = msg({
	message: 'Invalid or expired code',
	comment: 'Error message in the password change modal. Keep the tone plain and specific.',
});
const UNABLE_TO_RESEND_CODE_RIGHT_NOW_DESCRIPTOR = msg({
	message: 'Unable to resend code right now',
	comment: 'Error message in the password change modal. Keep the tone plain and specific.',
});
const UPDATE_YOUR_PASSWORD_DESCRIPTOR = msg({
	message: 'Update your password',
	comment: 'Short label in the password change modal. Keep it concise. Keep the tone plain and specific.',
});
const CHANGE_PASSWORD_FORM_DESCRIPTOR = msg({
	message: 'Change password form',
	comment: 'Short label in the password change modal. Keep it concise. Keep the tone plain and specific.',
});

function resolveApiError(i18n: I18n, error: unknown, fallback: string): string {
	return error && typeof error === 'object' && 'body' in error ? FormUtils.extractErrorMessage(i18n, error) : fallback;
}

type Stage = 'intro' | 'verifyEmail' | 'changePassword';

const STAGE_ORDER: ReadonlyArray<Stage> = ['intro', 'verifyEmail', 'changePassword'];

interface PasswordForm {
	new_password: string;
	confirm_password: string;
}

export const PasswordChangeModal = observer(() => {
	const {i18n} = useLingui();
	const passwordForm = useForm<PasswordForm>();
	const usesSudo = RuntimeConfig.usesUsernameSignIn;
	const [stage, setStage] = useState<Stage>(usesSudo ? 'changePassword' : 'intro');
	const [ticket, setTicket] = useState<string | null>(null);
	const [verificationProof, setVerificationProof] = useState<string | null>(null);
	const [code, setCode] = useState<string>('');
	const [resendAt, setResendAt] = useState<Date | null>(null);
	const [submitting, setSubmitting] = useState<boolean>(false);
	const [codeError, setCodeError] = useState<string | null>(null);
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
	const startFlow = useCallback(async () => {
		setSubmitting(true);
		try {
			const result = await UserCommands.startPasswordChange();
			setTicket(result.ticket);
			if (result.resend_available_at) {
				setResendAt(new Date(result.resend_available_at));
			}
			setStage('verifyEmail');
		} catch (error: unknown) {
			pushApiErrorModal(i18n, error, i18n._(UNABLE_TO_START_PASSWORD_CHANGE_DESCRIPTOR));
		} finally {
			setSubmitting(false);
		}
	}, [i18n]);
	const handleVerify = useCallback(async () => {
		if (!ticket) return;
		setSubmitting(true);
		setCodeError(null);
		try {
			const result = await UserCommands.verifyPasswordChangeCode(ticket, code);
			setVerificationProof(result.verification_proof);
			setStage('changePassword');
		} catch (error: unknown) {
			setCodeError(resolveApiError(i18n, error, i18n._(INVALID_OR_EXPIRED_CODE_DESCRIPTOR)));
		} finally {
			setSubmitting(false);
		}
	}, [ticket, code, i18n]);
	const handleResend = useCallback(async () => {
		if (!ticket || !canResend) return;
		setSubmitting(true);
		try {
			await UserCommands.resendPasswordChangeCode(ticket);
			setResendAt(new Date(Date.now() + 30 * 1000));
		} catch (error: unknown) {
			pushApiErrorModal(i18n, error, i18n._(UNABLE_TO_RESEND_CODE_RIGHT_NOW_DESCRIPTOR));
		} finally {
			setSubmitting(false);
		}
	}, [ticket, canResend, i18n]);
	const onPasswordSubmit = useCallback(
		async (data: PasswordForm) => {
			const emailProof = ticket && verificationProof ? {ticket, verificationProof} : null;
			if (!usesSudo && !emailProof) return;
			if (data.new_password !== data.confirm_password) {
				passwordForm.setError('confirm_password', {message: i18n._(PASSWORDS_DO_NOT_MATCH_DESCRIPTOR)});
				return;
			}
			if (emailProof) {
				await UserCommands.completePasswordChange(emailProof.ticket, emailProof.verificationProof, data.new_password);
				ModalCommands.pop();
			} else {
				const currentUserId = Users.currentUserId;
				const hadRecoveryKit = currentUserId ? RecoveryKitStatus.get(currentUserId)?.hasRecoveryKit : undefined;
				await UserCommands.updatePasswordWithSudo(data.new_password);
				ModalCommands.popByType(PasswordChangeModal);
				if (hadRecoveryKit !== false) {
					void RecoveryKitCommands.replaceRecoveryKitAfterPasswordChange(data.new_password);
				}
			}
			ToastCommands.createToast({type: 'success', children: <Trans>Password changed</Trans>});
		},
		[usesSudo, ticket, verificationProof, passwordForm, i18n],
	);
	const {handleSubmit: handlePasswordSubmit, isSubmitting: isPasswordSubmitting} = useFormSubmit({
		form: passwordForm,
		onSubmit: onPasswordSubmit,
		defaultErrorField: 'new_password',
	});
	const renderIntroStage = () => (
		<Modal.Description data-flx="user.password-change-modal.modal-description">
			<Trans>We'll send a verification code to your email before you can change your password.</Trans>
		</Modal.Description>
	);
	const renderVerifyEmailStage = () => (
		<>
			<Modal.Description data-flx="user.password-change-modal.modal-description--2">
				<Trans>Enter the code sent to your email address.</Trans>
			</Modal.Description>
			<Modal.InputGroup data-flx="user.password-change-modal.modal-input-group">
				<Input
					autoFocus={true}
					value={code}
					onChange={(event) => setCode(event.target.value)}
					label={i18n._(VERIFICATION_CODE_DESCRIPTOR)}
					placeholder="XXXX-XXXX"
					required={true}
					error={codeError ?? undefined}
					data-flx="user.password-change-modal.input.set-code"
				/>
			</Modal.InputGroup>
		</>
	);
	const renderChangePasswordStage = () => (
		<Form
			form={passwordForm}
			onSubmit={handlePasswordSubmit}
			aria-label={i18n._(CHANGE_PASSWORD_FORM_DESCRIPTOR)}
			data-flx="user.password-change-modal.form.password-submit"
		>
			<Modal.Description data-flx="user.password-change-modal.modal-description--3">
				<Trans>Choose a new password.</Trans>
			</Modal.Description>
			<Modal.InputGroup data-flx="user.password-change-modal.modal-input-group--2">
				<Input
					data-flx="user.password-change-modal.input.password"
					{...passwordForm.register('new_password')}
					autoComplete="new-password"
					autoFocus={true}
					error={passwordForm.formState.errors.new_password?.message}
					label={i18n._(NEW_PASSWORD_DESCRIPTOR)}
					maxLength={128}
					minLength={8}
					placeholder={'•'.repeat(32)}
					required={true}
					type="password"
				/>
				<Input
					data-flx="user.password-change-modal.input.password--2"
					{...passwordForm.register('confirm_password')}
					autoComplete="new-password"
					error={passwordForm.formState.errors.confirm_password?.message}
					label={i18n._(CONFIRM_NEW_PASSWORD_DESCRIPTOR)}
					maxLength={128}
					minLength={8}
					placeholder={'•'.repeat(32)}
					required={true}
					type="password"
				/>
			</Modal.InputGroup>
		</Form>
	);
	const renderStageBody = () => {
		switch (stage) {
			case 'intro':
				return renderIntroStage();
			case 'verifyEmail':
				return renderVerifyEmailStage();
			case 'changePassword':
				return renderChangePasswordStage();
		}
	};
	const renderStageFooter = () => {
		switch (stage) {
			case 'intro':
				return (
					<>
						<Button onClick={ModalCommands.pop} variant="secondary" data-flx="user.password-change-modal.button.pop">
							<Trans>Cancel</Trans>
						</Button>
						<Button onClick={startFlow} submitting={submitting} data-flx="user.password-change-modal.button.start-flow">
							<Trans>Start</Trans>
						</Button>
					</>
				);
			case 'verifyEmail':
				return (
					<>
						<Button onClick={ModalCommands.pop} variant="secondary" data-flx="user.password-change-modal.button.pop--2">
							<Trans>Cancel</Trans>
						</Button>
						<Button
							onClick={handleResend}
							disabled={!canResend || submitting}
							data-flx="user.password-change-modal.button.resend"
						>
							{canResend ? <Trans>Resend</Trans> : <Trans>Resend ({secondsRemaining}s)</Trans>}
						</Button>
						<Button onClick={handleVerify} submitting={submitting} data-flx="user.password-change-modal.button.verify">
							{i18n._(VERIFY_DESCRIPTOR)}
						</Button>
					</>
				);
			case 'changePassword':
				return (
					<>
						<Button onClick={ModalCommands.pop} variant="secondary" data-flx="user.password-change-modal.button.pop--3">
							<Trans>Cancel</Trans>
						</Button>
						<Button
							onClick={handlePasswordSubmit}
							submitting={isPasswordSubmitting}
							data-flx="user.password-change-modal.button.submit"
						>
							<Trans>Change password</Trans>
						</Button>
					</>
				);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="user.password-change-modal.modal-root">
			<Modal.Header
				title={i18n._(UPDATE_YOUR_PASSWORD_DESCRIPTOR)}
				data-flx="user.password-change-modal.modal-header"
			/>
			<Modal.Content data-flx="user.password-change-modal.modal-content">
				<Modal.ContentLayout data-flx="user.password-change-modal.modal-content-layout">
					<SteppedCarousel step={stage} steps={STAGE_ORDER} data-flx="user.password-change-modal.stepped-carousel">
						{renderStageBody()}
					</SteppedCarousel>
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="user.password-change-modal.footer">{renderStageFooter()}</Modal.Footer>
		</Modal.Root>
	);
});
