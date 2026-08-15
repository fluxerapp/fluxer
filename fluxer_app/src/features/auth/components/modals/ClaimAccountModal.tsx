// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {EXAMPLE_PERSONAL_EMAIL} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import {
	CLAIM_ACCOUNT_DESCRIPTOR,
	EMAIL_DESCRIPTOR,
	PASSWORD_DESCRIPTOR,
	VERIFICATION_CODE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {HttpError} from '@app/features/platform/types/EndpointError';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input} from '@app/features/ui/components/form/FormInput';
import ModalState from '@app/features/ui/state/Modal';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo, useState} from 'react';
import {useForm} from 'react-hook-form';

const VERIFICATION_CODE_SENT_DESCRIPTOR = msg({
	message: 'Verification code sent',
	comment: 'Short label in the authentication claim account modal. Keep the tone plain and specific.',
});
const ACCOUNT_CLAIMED_SUCCESSFULLY_DESCRIPTOR = msg({
	message: 'Account claimed successfully',
	comment: 'Short label in the authentication claim account modal. Keep the tone plain and specific.',
});
const CODE_RESENT_DESCRIPTOR = msg({
	message: 'Code resent',
	comment: 'Short label in the authentication claim account modal. Keep the tone plain and specific.',
});
const CLAIM_YOUR_ACCOUNT_DESCRIPTOR = msg({
	message: 'Claim your account',
	comment: 'Short label in the authentication claim account modal. Keep the tone plain and specific.',
});

interface FormInputs {
	email: string;
	newPassword: string;
	verificationCode: string;
}

type Stage = 'collect' | 'verify';

const STAGE_ORDER: ReadonlyArray<Stage> = ['collect', 'verify'];
export const ClaimAccountModal = observer(() => {
	const {i18n} = useLingui();
	const form = useForm<FormInputs>({
		defaultValues: {email: '', newPassword: '', verificationCode: ''},
	});
	const [stage, setStage] = useState<Stage>('collect');
	const [ticket, setTicket] = useState<string | null>(null);
	const [originalProof, setOriginalProof] = useState<string | null>(null);
	const [emailToken, setEmailToken] = useState<string | null>(null);
	const [resendNewAt, setResendNewAt] = useState<Date | null>(null);
	const [submittingAction, setSubmittingAction] = useState<boolean>(false);
	const [now, setNow] = useState<number>(Date.now());
	useEffect(() => {
		const id = window.setInterval(() => setNow(Date.now()), 1000);
		return () => window.clearInterval(id);
	}, []);
	const canResendNew = useMemo(() => !resendNewAt || resendNewAt.getTime() <= now, [resendNewAt, now]);
	const resendSecondsRemaining = useMemo(() => {
		if (!resendNewAt) return 0;
		return Math.max(0, Math.ceil((resendNewAt.getTime() - now) / 1000));
	}, [resendNewAt, now]);
	const startEmailTokenFlow = async (data: FormInputs) => {
		let activeTicket = ticket;
		let activeProof = originalProof;
		if (!activeTicket || !activeProof) {
			const startResult = await UserCommands.startEmailChange();
			activeTicket = startResult.ticket;
			activeProof = startResult.original_proof ?? null;
			setTicket(startResult.ticket);
			setOriginalProof(activeProof);
			if (startResult.resend_available_at) {
				setResendNewAt(new Date(startResult.resend_available_at));
			}
		}
		if (!activeProof) {
			throw new Error('Missing original proof token');
		}
		const result = await UserCommands.requestEmailChangeNew(activeTicket!, data.email, activeProof, data.newPassword);
		setResendNewAt(result.resend_available_at ? new Date(result.resend_available_at) : null);
		form.setValue('verificationCode', '');
		form.clearErrors('verificationCode');
		setStage('verify');
		ToastCommands.createToast({type: 'success', children: i18n._(VERIFICATION_CODE_SENT_DESCRIPTOR)});
	};
	const handleVerifyNew = async (data: FormInputs) => {
		if (!ticket || !originalProof) return;
		setSubmittingAction(true);
		try {
			let activeEmailToken = emailToken;
			if (!activeEmailToken) {
				const verified = await UserCommands.verifyEmailChangeNew(ticket, data.verificationCode, originalProof);
				activeEmailToken = verified.email_token;
				setEmailToken(activeEmailToken);
			}
			await UserCommands.update({
				email_token: activeEmailToken,
				new_password: data.newPassword,
			});
			ToastCommands.createToast({type: 'success', children: i18n._(ACCOUNT_CLAIMED_SUCCESSFULLY_DESCRIPTOR)});
			ModalCommands.pop();
		} catch (error: unknown) {
			FormUtils.handleError(i18n, form, error as HttpError, 'verificationCode', {
				pathMap: {new_password: 'newPassword'},
			});
		} finally {
			setSubmittingAction(false);
		}
	};
	const handleResendNew = async () => {
		if (!ticket || !canResendNew) return;
		setSubmittingAction(true);
		try {
			await UserCommands.resendEmailChangeNew(ticket);
			setResendNewAt(new Date(Date.now() + 30 * 1000));
			ToastCommands.createToast({type: 'success', children: i18n._(CODE_RESENT_DESCRIPTOR)});
		} catch (error: unknown) {
			FormUtils.handleError(i18n, form, error as HttpError, 'verificationCode');
		} finally {
			setSubmittingAction(false);
		}
	};
	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit: stage === 'collect' ? startEmailTokenFlow : handleVerifyNew,
		defaultErrorField: stage === 'collect' ? 'email' : 'verificationCode',
	});
	const renderCollectStage = () => (
		<>
			<Modal.Description data-flx="auth.claim-account-modal.modal-description">
				<Trans>
					Claim your account by adding an email and password. We will send a verification code to confirm your email
					before finishing.
				</Trans>
			</Modal.Description>
			<Modal.InputGroup data-flx="auth.claim-account-modal.modal-input-group">
				<Input
					data-flx="auth.claim-account-modal.input.email"
					{...form.register('email')}
					autoComplete="email"
					autoFocus={true}
					error={form.formState.errors.email?.message}
					label={i18n._(EMAIL_DESCRIPTOR)}
					maxLength={256}
					minLength={1}
					placeholder={EXAMPLE_PERSONAL_EMAIL}
					required={true}
					type="email"
				/>
				<Input
					data-flx="auth.claim-account-modal.input.password"
					{...form.register('newPassword')}
					autoComplete="new-password"
					error={form.formState.errors.newPassword?.message}
					label={i18n._(PASSWORD_DESCRIPTOR)}
					maxLength={128}
					minLength={8}
					placeholder={'•'.repeat(32)}
					required={true}
					type="password"
				/>
			</Modal.InputGroup>
		</>
	);
	const renderVerifyStage = () => (
		<>
			<Modal.Description data-flx="auth.claim-account-modal.modal-description--2">
				<Trans>
					Enter the code we sent to your email to verify it. Your password will be set once the code is confirmed.
				</Trans>
			</Modal.Description>
			<Modal.InputGroup data-flx="auth.claim-account-modal.modal-input-group--2">
				<Input
					data-flx="auth.claim-account-modal.input"
					{...form.register('verificationCode')}
					autoFocus={true}
					label={i18n._(VERIFICATION_CODE_DESCRIPTOR)}
					placeholder="XXXX-XXXX"
					required={true}
					error={form.formState.errors.verificationCode?.message}
				/>
				<Input
					data-flx="auth.claim-account-modal.input.password--2"
					{...form.register('newPassword')}
					autoComplete="new-password"
					error={form.formState.errors.newPassword?.message}
					label={i18n._(PASSWORD_DESCRIPTOR)}
					maxLength={128}
					minLength={8}
					placeholder={'•'.repeat(32)}
					required={true}
					type="password"
				/>
			</Modal.InputGroup>
		</>
	);
	const renderStageBody = () => (stage === 'collect' ? renderCollectStage() : renderVerifyStage());
	const renderStageFooter = () => {
		if (stage === 'collect') {
			return (
				<>
					<Button onClick={ModalCommands.pop} variant="secondary" data-flx="auth.claim-account-modal.button.pop">
						<Trans>Cancel</Trans>
					</Button>
					<Button type="submit" submitting={isSubmitting} data-flx="auth.claim-account-modal.button.submit">
						<Trans>Send code</Trans>
					</Button>
				</>
			);
		}
		return (
			<>
				<Button
					onClick={ModalCommands.pop}
					variant="secondary"
					type="button"
					data-flx="auth.claim-account-modal.button.pop--2"
				>
					<Trans>Cancel</Trans>
				</Button>
				<Button
					type="button"
					onClick={handleResendNew}
					disabled={!canResendNew || submittingAction}
					data-flx="auth.claim-account-modal.button.resend-new"
				>
					{canResendNew ? <Trans>Resend</Trans> : <Trans>Resend ({resendSecondsRemaining}s)</Trans>}
				</Button>
				<Button
					type="submit"
					submitting={submittingAction || isSubmitting}
					data-flx="auth.claim-account-modal.button.submit--2"
				>
					{i18n._(CLAIM_ACCOUNT_DESCRIPTOR)}
				</Button>
			</>
		);
	};
	return (
		<Modal.Root size="small" centered data-flx="auth.claim-account-modal.modal-root">
			<Modal.Header title={i18n._(CLAIM_YOUR_ACCOUNT_DESCRIPTOR)} data-flx="auth.claim-account-modal.modal-header" />
			<Form form={form} onSubmit={handleSubmit} data-flx="auth.claim-account-modal.form.submit">
				<Modal.Content data-flx="auth.claim-account-modal.modal-content">
					<Modal.ContentLayout data-flx="auth.claim-account-modal.modal-content-layout">
						<SteppedCarousel step={stage} steps={STAGE_ORDER} data-flx="auth.claim-account-modal.stepped-carousel">
							{renderStageBody()}
						</SteppedCarousel>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="auth.claim-account-modal.footer">{renderStageFooter()}</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});
const CLAIM_ACCOUNT_MODAL_KEY = 'claim-account-modal';

let hasShownClaimAccountModalThisSession = false;

export const openClaimAccountModal = ({force = false}: {force?: boolean} = {}): void => {
	if (ModalState.hasModal(CLAIM_ACCOUNT_MODAL_KEY)) {
		return;
	}
	if (!force && hasShownClaimAccountModalThisSession) {
		return;
	}
	hasShownClaimAccountModalThisSession = true;
	ModalCommands.pushWithKey(
		modal(() => <ClaimAccountModal data-flx="auth.claim-account-modal.open-claim-account-modal.claim-account-modal" />),
		CLAIM_ACCOUNT_MODAL_KEY,
	);
};
