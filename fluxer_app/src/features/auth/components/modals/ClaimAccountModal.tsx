// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {EXAMPLE_PERSONAL_EMAIL} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {NEW_CODE_SENT_DESCRIPTOR} from '@app/features/auth/AuthMessageDescriptors';
import {offerAuthenticatorCalmly} from '@app/features/auth/components/modals/AuthenticatorOfferModal';
import styles from '@app/features/auth/components/modals/ClaimAccountModal.module.css';
import {EmailCodeField} from '@app/features/auth/flow/EmailCodeField';
import {isCompleteEmailCode, parseIsoTimestamp} from '@app/features/auth/flow/identifier_first/IdentifierFirstState';
import {QuietButton} from '@app/features/auth/flow/QuietButton';
import {useResendCooldown} from '@app/features/auth/hooks/useResendCooldown';
import {
	ADD_EMAIL_DESCRIPTOR,
	CLAIM_ACCOUNT_DESCRIPTOR,
	EMAIL_DESCRIPTOR,
	PASSWORD_DESCRIPTOR,
	USERNAME_DESCRIPTOR,
	VERIFICATION_CODE_DESCRIPTOR,
	VERIFY_DESCRIPTOR,
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
import * as AccountTierCommands from '@app/features/user/commands/AccountTierCommands';
import {createRecoveryKitWithPasswordAndOpen} from '@app/features/user/commands/RecoveryKitCommands';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import Users from '@app/features/user/state/Users';
import * as FormUtils from '@app/lib/forms';
import {ClaimAccountNextSteps, VerificationBases} from '@fluxer/constants/src/AccountConfigConstants';
import {AccountTiers, AccountVerificationNeeds, LocalCredentialKinds} from '@fluxer/constants/src/AccountTierConstants';
import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo, useRef, useState} from 'react';
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
const CONFIRM_PASSWORD_DESCRIPTOR = msg({
	message: 'Confirm password',
	comment: 'Label for the password confirmation field in the claim account modal.',
});
const PASSWORDS_DO_NOT_MATCH_DESCRIPTOR = msg({
	message: 'Passwords do not match',
	comment: 'Claim account modal error when the password and its confirmation differ.',
});

interface FormInputs {
	email: string;
	newPassword: string;
	verificationCode: string;
}

type Stage = 'collect' | 'verify';

const STAGE_ORDER: ReadonlyArray<Stage> = ['collect', 'verify'];
const CLAIM_ACCOUNT_MODAL_KEY = 'claim-account-modal';

interface UsernameClaimInputs {
	username: string;
	newPassword: string;
	confirmPassword: string;
}

const USERNAME_CLAIM_PATH_MAP = {new_password: 'newPassword'} as const;

interface ClaimFlowOptions {
	readonly onComplete?: () => void;
	readonly onDismiss?: () => void;
	readonly reason?: string | null;
}

const UsernameClaimAccountModal = observer(({onComplete, onDismiss, reason}: ClaimFlowOptions) => {
	const {i18n} = useLingui();
	const dismiss = () => {
		ModalCommands.popWithKey(CLAIM_ACCOUNT_MODAL_KEY);
		onDismiss?.();
	};
	const form = useForm<UsernameClaimInputs>({
		defaultValues: {username: Users.currentUser?.username ?? '', newPassword: '', confirmPassword: ''},
	});
	const hasPasskey = hasRegisteredPasskey();
	const handleClaim = async (data: UsernameClaimInputs) => {
		if (data.newPassword !== data.confirmPassword) {
			form.setError('confirmPassword', {type: 'validate', message: i18n._(PASSWORDS_DO_NOT_MATCH_DESCRIPTOR)});
			return;
		}
		const username = data.username.trim();
		const updated = await UserCommands.update(
			data.newPassword === '' ? {username} : {username, new_password: data.newPassword},
		);
		ModalCommands.popWithKey(CLAIM_ACCOUNT_MODAL_KEY);
		ToastCommands.createToast({type: 'success', children: i18n._(ACCOUNT_CLAIMED_SUCCESSFULLY_DESCRIPTOR)});
		onComplete?.();
		if (data.newPassword === '') return;
		void createRecoveryKitWithPasswordAndOpen({
			userId: updated.id,
			password: data.newPassword,
			username: updated.username,
			discriminator: updated.discriminator,
		});
	};
	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit: handleClaim,
		defaultErrorField: 'username',
		pathMap: USERNAME_CLAIM_PATH_MAP,
	});
	return (
		<Modal.Root size="small" centered onClose={dismiss} data-flx="auth.claim-account-modal.username.modal-root">
			<Modal.Header
				title={i18n._(CLAIM_YOUR_ACCOUNT_DESCRIPTOR)}
				data-flx="auth.claim-account-modal.username.modal-header"
			/>
			<Form form={form} onSubmit={handleSubmit} data-flx="auth.claim-account-modal.username.form.submit">
				<Modal.Content data-flx="auth.claim-account-modal.username.modal-content">
					<Modal.ContentLayout data-flx="auth.claim-account-modal.username.modal-content-layout">
						{reason == null ? null : (
							<Modal.Description dir="auto" data-flx="auth.claim-account-modal.username.reason">
								{reason}
							</Modal.Description>
						)}
						<Modal.Description data-flx="auth.claim-account-modal.username.modal-description">
							<Trans>
								Claim your account by choosing a username and password. You sign in with them, so pick ones you will
								remember.
							</Trans>
						</Modal.Description>
						<Modal.InputGroup data-flx="auth.claim-account-modal.username.modal-input-group">
							<Input
								data-flx="auth.claim-account-modal.username.input.username"
								{...form.register('username')}
								autoCapitalize="none"
								autoComplete="username"
								autoCorrect="off"
								autoFocus={true}
								error={form.formState.errors.username?.message}
								label={i18n._(USERNAME_DESCRIPTOR)}
								maxLength={32}
								minLength={1}
								required={true}
								spellCheck={false}
							/>
							<Input
								data-flx="auth.claim-account-modal.username.input.password"
								{...form.register('newPassword')}
								autoComplete="new-password"
								error={form.formState.errors.newPassword?.message}
								label={i18n._(PASSWORD_DESCRIPTOR)}
								maxLength={128}
								minLength={8}
								placeholder={'•'.repeat(32)}
								required={!hasPasskey}
								type="password"
							/>
							<Input
								data-flx="auth.claim-account-modal.username.input.confirm-password"
								{...form.register('confirmPassword')}
								autoComplete="new-password"
								error={form.formState.errors.confirmPassword?.message}
								label={i18n._(CONFIRM_PASSWORD_DESCRIPTOR)}
								maxLength={128}
								minLength={8}
								placeholder={'•'.repeat(32)}
								required={!hasPasskey}
								type="password"
							/>
						</Modal.InputGroup>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="auth.claim-account-modal.username.footer">
					<Button
						onClick={dismiss}
						variant="secondary"
						type="button"
						data-flx="auth.claim-account-modal.username.button.pop"
					>
						<Trans>Cancel</Trans>
					</Button>
					<Button type="submit" submitting={isSubmitting} data-flx="auth.claim-account-modal.username.button.submit">
						{i18n._(CLAIM_ACCOUNT_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});

const EmailClaimAccountModal = observer(() => {
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
					Enter the code we sent to your email to confirm it. Your password will be set once the code is confirmed.
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
const EMAIL_CONFIRMED_DESCRIPTOR = msg({
	message: 'Email confirmed',
	comment: 'Toast after someone confirms their email address with the emailed code.',
});
const EMAIL_ADDED_DESCRIPTOR = msg({
	message: 'Email added',
	comment: 'Toast after someone adds an email and password on an instance that cannot send email codes.',
});
const ADD_EMAIL_TITLE_DESCRIPTOR = msg({
	message: 'Add an email',
	comment: 'Heading of the dialog where someone with an unclaimed account adds an email.',
});
const ADD_EMAIL_TEXT_DESCRIPTOR = msg({
	message: "Add an email to sign in on other devices. We'll send you a code to confirm it.",
	comment:
		'Text in the dialog where someone with an unclaimed account adds an email, on an instance that sends email codes.',
});
const ADD_EMAIL_NO_DELIVERY_TEXT_DESCRIPTOR = msg({
	message: 'Add an email and a password to sign in on other devices.',
	comment: 'Text in the add email dialog on an instance that cannot send email codes.',
});
const SET_PASSWORD_TITLE_DESCRIPTOR = msg({
	message: 'Set a password',
	comment: 'Heading of the dialog where someone whose account has an email but no password sets one.',
});
const SET_PASSWORD_TEXT_DESCRIPTOR = msg({
	message: 'Set a password to sign in on other devices with this email.',
	comment:
		'Text in the dialog where someone whose account has an email but no password sets one, on an instance that cannot send email codes.',
});
const SET_PASSWORD_BUTTON_DESCRIPTOR = msg({
	message: 'Set password',
	comment: 'Button that saves the password in the dialog for an account that has an email but no password.',
});
const PASSWORD_SET_DESCRIPTOR = msg({
	message: 'Password set',
	comment: 'Toast after someone whose account has an email but no password sets one.',
});
const CONFIRM_EMAIL_TITLE_DESCRIPTOR = msg({
	message: 'Confirm your email',
	comment: 'Heading of the dialog where someone types the code we emailed them to confirm their address.',
});
const CONFIRM_LATER_DESCRIPTOR = msg({
	message: 'Confirm later',
	comment: 'Secondary button that closes the email code dialog without confirming the address yet.',
});
const SEND_CODE_DESCRIPTOR = msg({
	message: 'Send code',
	comment: 'Button that emails a confirmation code to the address the person typed.',
});
const RESEND_CODE_DESCRIPTOR = msg({
	message: 'Send a new code',
	comment: 'Quiet text link under the code field that emails a fresh confirmation code.',
});
const RESEND_IN_DESCRIPTOR = msg({
	message: 'Send a new code in {seconds}s',
	comment: 'Disabled quiet text link while a new confirmation code cannot be sent yet. seconds counts down.',
});

interface AccountEmailFlowProps extends ClaimFlowOptions {
	readonly startWithCode: boolean;
}

interface AccountEmailInputs {
	email: string;
	password: string;
}

function hasRegisteredPasskey(): boolean {
	const credential = Users.currentUser?.localCredential;
	return credential === LocalCredentialKinds.PASSKEY || credential === LocalCredentialKinds.PASSWORD_AND_PASSKEY;
}

function useCodeDelivery() {
	const [resendAt, setResendAt] = useState<number | null>(null);
	const seconds = useResendCooldown(resendAt);
	const applyDelivery = (delivery: {resend_available_at: string}) =>
		setResendAt(parseIsoTimestamp(delivery.resend_available_at, Date.now() + 30_000));
	return {seconds, applyDelivery};
}

const AccountEmailFlowModal = observer(function AccountEmailFlowModal({
	startWithCode,
	onComplete,
	onDismiss,
	reason,
}: AccountEmailFlowProps) {
	const {i18n} = useLingui();
	const user = Users.currentUser;
	const existingEmail = user?.verificationNeed === AccountVerificationNeeds.SET_PASSWORD ? (user.email ?? null) : null;
	const form = useForm<AccountEmailInputs>({defaultValues: {email: existingEmail ?? '', password: ''}});
	const [stage, setStage] = useState<Stage>(startWithCode ? 'verify' : 'collect');
	const [code, setCode] = useState('');
	const [codeError, setCodeError] = useState<string | null>(null);
	const [busy, setBusy] = useState(false);
	const [resent, setResent] = useState(false);
	const {seconds, applyDelivery} = useCodeDelivery();
	const codeInputRef = useRef<HTMLInputElement | null>(null);
	const sentRef = useRef(false);
	const emailsEnabled = RuntimeConfig.emailsEnabled;
	const asksPassword = existingEmail !== null || RuntimeConfig.verificationBasis === VerificationBases.LOCAL_CREDENTIAL;
	const passwordRequired = asksPassword && !hasRegisteredPasskey();
	const sentTo = user?.email ?? form.getValues('email');
	const close = () => {
		ModalCommands.popWithKey(CLAIM_ACCOUNT_MODAL_KEY);
	};
	const finishLater = () => {
		close();
		onDismiss?.();
	};
	const finish = () => {
		close();
		if (onComplete === undefined) {
			void offerAuthenticatorCalmly();
		} else {
			onComplete();
		}
	};
	const sendVerificationCode = async (): Promise<boolean> => {
		setBusy(true);
		setCodeError(null);
		setResent(false);
		try {
			applyDelivery(await AccountTierCommands.sendEmailVerificationCode());
			return true;
		} catch (error) {
			setCodeError(FormUtils.extractErrorMessage(i18n, error));
			return false;
		} finally {
			setBusy(false);
		}
	};
	useEffect(() => {
		if (!startWithCode || sentRef.current) return;
		sentRef.current = true;
		void sendVerificationCode();
	}, [startWithCode]);
	const handleCollect = async (data: AccountEmailInputs) => {
		const delivery = await AccountTierCommands.claimAccount(
			existingEmail ?? data.email.trim(),
			asksPassword ? data.password : null,
		);
		if (delivery.next_step === ClaimAccountNextSteps.NONE) {
			ToastCommands.createToast({
				type: 'success',
				children: i18n._(existingEmail === null ? EMAIL_ADDED_DESCRIPTOR : PASSWORD_SET_DESCRIPTOR),
			});
			finish();
			return;
		}
		applyDelivery(delivery);
		setCode('');
		setStage('verify');
	};
	const {handleSubmit, isSubmitting} = useFormSubmit({form, onSubmit: handleCollect, defaultErrorField: 'email'});
	const confirm = async (value: string) => {
		if (!isCompleteEmailCode(value) || busy) return;
		setBusy(true);
		setCodeError(null);
		try {
			await AccountTierCommands.confirmEmailVerification(value);
			ToastCommands.createToast({type: 'success', children: i18n._(EMAIL_CONFIRMED_DESCRIPTOR)});
			finish();
		} catch (error) {
			setCodeError(FormUtils.extractErrorMessage(i18n, error));
		} finally {
			setBusy(false);
		}
	};
	const handleResend = async () => {
		if (seconds > 0 || busy) return;
		setResent(await sendVerificationCode());
	};
	let collectText: MessageDescriptor;
	if (existingEmail !== null) {
		collectText = SET_PASSWORD_TEXT_DESCRIPTOR;
	} else if (emailsEnabled) {
		collectText = ADD_EMAIL_TEXT_DESCRIPTOR;
	} else {
		collectText = ADD_EMAIL_NO_DELIVERY_TEXT_DESCRIPTOR;
	}
	let collectButton: MessageDescriptor;
	if (existingEmail !== null) {
		collectButton = SET_PASSWORD_BUTTON_DESCRIPTOR;
	} else if (emailsEnabled) {
		collectButton = SEND_CODE_DESCRIPTOR;
	} else {
		collectButton = ADD_EMAIL_DESCRIPTOR;
	}
	if (stage === 'verify') {
		return (
			<Modal.Root
				size="small"
				centered
				initialFocusRef={codeInputRef}
				onClose={finishLater}
				data-flx="auth.claim-account-modal.verify.modal-root"
			>
				<Modal.Header
					title={i18n._(CONFIRM_EMAIL_TITLE_DESCRIPTOR)}
					data-flx="auth.claim-account-modal.verify.header"
				/>
				<Form form={form} onSubmit={() => void confirm(code)} data-flx="auth.claim-account-modal.verify.form">
					<Modal.Content data-flx="auth.claim-account-modal.verify.content">
						<Modal.ContentLayout data-flx="auth.claim-account-modal.verify.content-layout">
							{reason == null ? null : (
								<Modal.Description dir="auto" data-flx="auth.claim-account-modal.verify.reason">
									{reason}
								</Modal.Description>
							)}
							<Modal.Description dir="auto" data-flx="auth.claim-account-modal.verify.description">
								<Trans comment="Code step in the claim account dialog. The tag wraps the email address the code was sent to.">
									Enter the code we sent to{' '}
									<bdi data-flx="auth.claim-account-modal.account-email-flow-modal.bdi">{sentTo}</bdi>.
								</Trans>
							</Modal.Description>
							<EmailCodeField
								value={code}
								error={codeError}
								disabled={busy}
								onChange={setCode}
								onComplete={(value) => void confirm(value)}
								inputRef={codeInputRef}
								data-flx="auth.claim-account-modal.verify.code"
							/>
							{resent ? (
								<p className={styles.status} role="status" data-flx="auth.claim-account-modal.verify.status">
									{i18n._(NEW_CODE_SENT_DESCRIPTOR)}
								</p>
							) : null}
							<div className={styles.resendRow} data-flx="auth.claim-account-modal.verify.resend-row">
								<QuietButton
									disabled={seconds > 0 || busy}
									onClick={() => void handleResend()}
									data-flx="auth.claim-account-modal.verify.button.resend"
								>
									{seconds > 0 ? i18n._(RESEND_IN_DESCRIPTOR, {seconds}) : i18n._(RESEND_CODE_DESCRIPTOR)}
								</QuietButton>
							</div>
						</Modal.ContentLayout>
					</Modal.Content>
					<Modal.Footer data-flx="auth.claim-account-modal.verify.footer">
						<Button
							variant="secondary"
							type="button"
							onClick={finishLater}
							data-flx="auth.claim-account-modal.verify.button.later"
						>
							{i18n._(CONFIRM_LATER_DESCRIPTOR)}
						</Button>
						<Button
							type="submit"
							submitting={busy}
							disabled={!isCompleteEmailCode(code)}
							data-flx="auth.claim-account-modal.verify.button.submit"
						>
							{i18n._(VERIFY_DESCRIPTOR)}
						</Button>
					</Modal.Footer>
				</Form>
			</Modal.Root>
		);
	}
	return (
		<Modal.Root size="small" centered onClose={finishLater} data-flx="auth.claim-account-modal.collect.modal-root">
			<Modal.Header
				title={i18n._(existingEmail === null ? ADD_EMAIL_TITLE_DESCRIPTOR : SET_PASSWORD_TITLE_DESCRIPTOR)}
				data-flx="auth.claim-account-modal.collect.header"
			/>
			<Form form={form} onSubmit={handleSubmit} data-flx="auth.claim-account-modal.collect.form.submit">
				<Modal.Content data-flx="auth.claim-account-modal.collect.content">
					<Modal.ContentLayout data-flx="auth.claim-account-modal.collect.content-layout">
						<Modal.Description dir="auto" data-flx="auth.claim-account-modal.collect.description">
							{reason ?? i18n._(collectText)}
						</Modal.Description>
						<Modal.InputGroup data-flx="auth.claim-account-modal.collect.input-group">
							<Input
								data-flx="auth.claim-account-modal.collect.input.email"
								{...form.register('email')}
								autoComplete="email"
								autoFocus={existingEmail === null}
								error={form.formState.errors.email?.message}
								label={i18n._(EMAIL_DESCRIPTOR)}
								maxLength={256}
								minLength={1}
								placeholder={EXAMPLE_PERSONAL_EMAIL}
								readOnly={existingEmail !== null}
								required={true}
								type="email"
							/>
							{asksPassword ? (
								<Input
									data-flx="auth.claim-account-modal.collect.input.password"
									{...form.register('password')}
									autoComplete="new-password"
									error={form.formState.errors.password?.message}
									autoFocus={existingEmail !== null}
									label={i18n._(PASSWORD_DESCRIPTOR)}
									maxLength={128}
									minLength={8}
									required={passwordRequired}
									type="password"
								/>
							) : null}
						</Modal.InputGroup>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="auth.claim-account-modal.collect.footer">
					<Button
						onClick={finishLater}
						variant="secondary"
						type="button"
						data-flx="auth.claim-account-modal.collect.button.cancel"
					>
						<Trans>Cancel</Trans>
					</Button>
					<Button type="submit" submitting={isSubmitting} data-flx="auth.claim-account-modal.collect.button.submit">
						{i18n._(collectButton)}
					</Button>
				</Modal.Footer>
			</Form>
		</Modal.Root>
	);
});

interface ClaimAccountModalProps extends ClaimFlowOptions {
	readonly startWithCode: boolean;
}

const ClaimAccountModal = observer(({onComplete, onDismiss, reason, startWithCode}: ClaimAccountModalProps) => {
	const user = Users.currentUser;
	if (RuntimeConfig.usesUsernameSignIn) {
		return (
			<UsernameClaimAccountModal
				onComplete={onComplete}
				onDismiss={onDismiss}
				reason={reason}
				data-flx="auth.claim-account-modal.username-claim-account-modal"
			/>
		);
	}
	if (user?.reportsAccountTier !== true) {
		return <EmailClaimAccountModal data-flx="auth.claim-account-modal.email-claim-account-modal" />;
	}
	return (
		<AccountEmailFlowModal
			startWithCode={startWithCode}
			onComplete={onComplete}
			onDismiss={onDismiss}
			reason={reason}
			data-flx="auth.claim-account-modal.account-email-flow-modal"
		/>
	);
});

export const openClaimAccountModal = (options: ClaimFlowOptions = {}): void => {
	if (ModalState.hasModal(CLAIM_ACCOUNT_MODAL_KEY)) {
		return;
	}
	const startWithCode = Users.currentUser?.accountTier === AccountTiers.UNVERIFIED;
	ModalCommands.pushWithKey(
		modal(() => (
			<ClaimAccountModal
				onComplete={options.onComplete}
				onDismiss={options.onDismiss}
				reason={options.reason}
				startWithCode={startWithCode}
				data-flx="auth.claim-account-modal.open-claim-account-modal.claim-account-modal"
			/>
		)),
		CLAIM_ACCOUNT_MODAL_KEY,
	);
};
