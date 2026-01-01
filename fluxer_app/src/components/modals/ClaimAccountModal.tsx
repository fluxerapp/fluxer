/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo, useState} from 'react';
import {useForm} from 'react-hook-form';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import * as UserActionCreators from '~/actions/UserActionCreators';
import {Form} from '~/components/form/Form';
import {Input} from '~/components/form/Input';
import styles from '~/components/modals/ClaimAccountModal.module.css';
import confirmStyles from '~/components/modals/ConfirmModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {useFormSubmit} from '~/hooks/useFormSubmit';

interface FormInputs {
	email: string;
	newPassword: string;
}

type Stage = 'collect' | 'verify';

export const ClaimAccountModal = observer(() => {
	const {t} = useLingui();
	const form = useForm<FormInputs>({defaultValues: {email: '', newPassword: ''}});
	const [stage, setStage] = useState<Stage>('collect');
	const [ticket, setTicket] = useState<string | null>(null);
	const [originalProof, setOriginalProof] = useState<string | null>(null);
	const [verificationCode, setVerificationCode] = useState<string>('');
	const [resendNewAt, setResendNewAt] = useState<Date | null>(null);
	const [verificationError, setVerificationError] = useState<string | null>(null);
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
		setVerificationError(null);
		let activeTicket = ticket;
		let activeProof = originalProof;
		if (!activeTicket || !activeProof) {
			const startResult = await UserActionCreators.startEmailChange();
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

		const result = await UserActionCreators.requestEmailChangeNew(activeTicket!, data.email, activeProof);
		setResendNewAt(result.resend_available_at ? new Date(result.resend_available_at) : null);
		setVerificationCode('');
		setStage('verify');
		ToastActionCreators.createToast({type: 'success', children: t`Verification code sent`});
	};

	const handleVerifyNew = async () => {
		if (!ticket || !originalProof) return;
		const passwordValid = await form.trigger('newPassword');
		if (!passwordValid) {
			return;
		}
		setSubmittingAction(true);
		setVerificationError(null);
		try {
			const {email_token} = await UserActionCreators.verifyEmailChangeNew(ticket, verificationCode, originalProof);
			await UserActionCreators.update({
				email_token,
				new_password: form.getValues('newPassword'),
			});
			ToastActionCreators.createToast({type: 'success', children: t`Account claimed successfully`});
			ModalActionCreators.pop();
		} catch (error: any) {
			const message = error?.message ?? t`Invalid or expired code`;
			setVerificationError(message);
			ToastActionCreators.createToast({type: 'error', children: message});
		} finally {
			setSubmittingAction(false);
		}
	};

	const handleResendNew = async () => {
		if (!ticket || !canResendNew) return;
		setSubmittingAction(true);
		setVerificationError(null);
		try {
			await UserActionCreators.resendEmailChangeNew(ticket);
			setResendNewAt(new Date(Date.now() + 30 * 1000));
			ToastActionCreators.createToast({type: 'success', children: t`Code resent`});
		} catch (error: any) {
			const message = error?.message ?? t`Unable to resend code right now`;
			setVerificationError(message);
			ToastActionCreators.createToast({type: 'error', children: message});
		} finally {
			setSubmittingAction(false);
		}
	};

	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit: startEmailTokenFlow,
		defaultErrorField: 'email',
	});

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Claim your account`} />
			{stage === 'collect' ? (
				<Form form={form} onSubmit={handleSubmit}>
					<Modal.Content className={styles.content}>
						<p className={confirmStyles.descriptionText}>
							<Trans>
								Claim your account by adding an email and password. We will send a verification code to confirm your
								email before finishing.
							</Trans>
						</p>
						<div className={confirmStyles.inputContainer}>
							<Input
								{...form.register('email')}
								autoComplete="email"
								autoFocus={true}
								error={form.formState.errors.email?.message}
								label={t`Email`}
								maxLength={256}
								minLength={1}
								placeholder={t`marty@example.com`}
								required={true}
								type="email"
							/>
							<Input
								{...form.register('newPassword')}
								autoComplete="new-password"
								error={form.formState.errors.newPassword?.message}
								label={t`Password`}
								maxLength={128}
								minLength={8}
								placeholder={'•'.repeat(32)}
								required={true}
								type="password"
							/>
						</div>
					</Modal.Content>
					<Modal.Footer className={styles.footer}>
						<Button onClick={ModalActionCreators.pop} variant="secondary">
							<Trans>Cancel</Trans>
						</Button>
						<Button type="submit" submitting={isSubmitting}>
							<Trans>Send code</Trans>
						</Button>
					</Modal.Footer>
				</Form>
			) : (
				<>
					<Modal.Content className={styles.content}>
						<p className={confirmStyles.descriptionText}>
							<Trans>
								Enter the code we sent to your email to verify it. Your password will be set once the code is confirmed.
							</Trans>
						</p>
						<div className={confirmStyles.inputContainer}>
							<Input
								value={verificationCode}
								onChange={(event) => setVerificationCode(event.target.value)}
								autoFocus={true}
								label={t`Verification code`}
								placeholder="XXXX-XXXX"
								required={true}
								error={verificationError ?? undefined}
							/>
							<Input
								{...form.register('newPassword')}
								autoComplete="new-password"
								error={form.formState.errors.newPassword?.message}
								label={t`Password`}
								maxLength={128}
								minLength={8}
								placeholder={'•'.repeat(32)}
								required={true}
								type="password"
							/>
						</div>
					</Modal.Content>
					<Modal.Footer className={styles.footer}>
						<Button onClick={ModalActionCreators.pop} variant="secondary">
							<Trans>Cancel</Trans>
						</Button>
						<Button onClick={handleResendNew} disabled={!canResendNew || submittingAction}>
							{canResendNew ? <Trans>Resend</Trans> : <Trans>Resend ({resendSecondsRemaining}s)</Trans>}
						</Button>
						<Button onClick={handleVerifyNew} submitting={submittingAction}>
							<Trans>Claim Account</Trans>
						</Button>
					</Modal.Footer>
				</>
			)}
		</Modal.Root>
	);
});
