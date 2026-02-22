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

import * as ModalActionCreators from '@app/actions/ModalActionCreators';
import * as ToastActionCreators from '@app/actions/ToastActionCreators';
import * as UserActionCreators from '@app/actions/UserActionCreators';
import {Form} from '@app/components/form/Form';
import {Input} from '@app/components/form/Input';
import styles from '@app/components/modals/EmailChangeModal.module.css';
import * as Modal from '@app/components/modals/Modal';
import {Button} from '@app/components/uikit/button/Button';
import {useFormSubmit} from '@app/hooks/useFormSubmit';
import type {UserRecord} from '@app/records/UserRecord';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useEffect, useMemo, useState} from 'react';
import {useForm} from 'react-hook-form';

type Stage = 'intro' | 'verifyOriginal' | 'newEmail' | 'verifyNew';

interface NewEmailForm {
	email: string;
}

interface EmailChangeModalProps {
	user: UserRecord;
}

export const EmailChangeModal = observer(({user}: EmailChangeModalProps) => {
	const {t} = useLingui();
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
			const result = await UserActionCreators.startEmailChange();
			setTicket(result.ticket);
			if (result.original_proof) {
				setOriginalProof(result.original_proof);
			}
			if (result.resend_available_at) {
				setResendOriginalAt(new Date(result.resend_available_at));
			}
			setStage(result.require_original ? 'verifyOriginal' : 'newEmail');
		} catch (error: unknown) {
			const errorMessage = error instanceof Error ? error.message : t`Unable to start email change`;
			ToastActionCreators.error(errorMessage);
		} finally {
			setSubmitting(false);
		}
	};

	const handleVerifyOriginal = async () => {
		if (!ticket) return;
		setSubmitting(true);
		setOriginalCodeError(null);
		try {
			const result = await UserActionCreators.verifyEmailChangeOriginal(ticket, originalCode);
			setOriginalProof(result.original_proof);
			setStage('newEmail');
		} catch (error: unknown) {
			const errorMessage = error instanceof Error ? error.message : t`Invalid or expired code`;
			setOriginalCodeError(errorMessage);
		} finally {
			setSubmitting(false);
		}
	};

	const handleResendOriginal = async () => {
		if (!ticket || !canResendOriginal) return;
		setSubmitting(true);
		try {
			await UserActionCreators.resendEmailChangeOriginal(ticket);
			setResendOriginalAt(new Date(Date.now() + 30 * 1000));
		} catch (error: unknown) {
			const errorMessage = error instanceof Error ? error.message : t`Unable to resend code right now`;
			ToastActionCreators.error(errorMessage);
		} finally {
			setSubmitting(false);
		}
	};

	const handleRequestNew = async (data: NewEmailForm) => {
		if (!ticket || !originalProof) return;
		setSubmitting(true);
		try {
			const result = await UserActionCreators.requestEmailChangeNew(ticket, data.email, originalProof);
			setResendNewAt(result.resend_available_at ? new Date(result.resend_available_at) : null);
			setStage('verifyNew');
		} catch (error: unknown) {
			const errorMessage = error instanceof Error ? error.message : t`Unable to send code to new email`;
			ToastActionCreators.error(errorMessage);
			throw error;
		} finally {
			setSubmitting(false);
		}
	};

	const handleResendNew = async () => {
		if (!ticket || !canResendNew) return;
		setSubmitting(true);
		try {
			await UserActionCreators.resendEmailChangeNew(ticket);
			setResendNewAt(new Date(Date.now() + 30 * 1000));
		} catch (error: unknown) {
			const errorMessage = error instanceof Error ? error.message : t`Unable to resend code right now`;
			ToastActionCreators.error(errorMessage);
		} finally {
			setSubmitting(false);
		}
	};

	const handleVerifyNew = async () => {
		if (!ticket || !originalProof) return;
		setSubmitting(true);
		setNewCodeError(null);
		try {
			const result = await UserActionCreators.verifyEmailChangeNew(ticket, newCode, originalProof);
			await UserActionCreators.update({email_token: result.email_token});
			ToastActionCreators.createToast({type: 'success', children: t`Email changed`});
			ModalActionCreators.pop();
		} catch (error: unknown) {
			const errorMessage = error instanceof Error ? error.message : t`Invalid or expired code`;
			setNewCodeError(errorMessage);
		} finally {
			setSubmitting(false);
		}
	};

	const {handleSubmit: handleNewEmailSubmit} = useFormSubmit({
		form: newEmailForm,
		onSubmit: handleRequestNew,
		defaultErrorField: 'email',
	});

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Change Your Email`} />
			{stage === 'intro' && (
				<>
					<Modal.Content>
						<Modal.ContentLayout>
							<Modal.Description>
								{isEmailVerified ? (
									<Trans>We'll verify your current email and then your new email with one-time codes.</Trans>
								) : (
									<Trans>We'll verify your new email with a one-time code.</Trans>
								)}
							</Modal.Description>
						</Modal.ContentLayout>
					</Modal.Content>
					<Modal.Footer className={styles.footer}>
						<Button onClick={ModalActionCreators.pop} variant="secondary">
							<Trans>Cancel</Trans>
						</Button>
						<Button onClick={startFlow} submitting={submitting}>
							<Trans>Start</Trans>
						</Button>
					</Modal.Footer>
				</>
			)}

			{stage === 'verifyOriginal' && (
				<>
					<Modal.Content>
						<Modal.ContentLayout>
							<Modal.Description>
								<Trans>Enter the code sent to your current email.</Trans>
							</Modal.Description>
							<Modal.InputGroup>
								<Input
									autoFocus={true}
									value={originalCode}
									onChange={(event: React.ChangeEvent<HTMLInputElement>) => setOriginalCode(event.target.value)}
									label={t`Verification Code`}
									placeholder="XXXX-XXXX"
									required={true}
									error={originalCodeError ?? undefined}
								/>
							</Modal.InputGroup>
						</Modal.ContentLayout>
					</Modal.Content>
					<Modal.Footer className={styles.footer}>
						<Button onClick={ModalActionCreators.pop} variant="secondary">
							<Trans>Cancel</Trans>
						</Button>
						<Button onClick={handleResendOriginal} disabled={!canResendOriginal || submitting}>
							{canResendOriginal ? <Trans>Resend</Trans> : <Trans>Resend ({originalSecondsRemaining}s)</Trans>}
						</Button>
						<Button onClick={handleVerifyOriginal} submitting={submitting}>
							<Trans>Verify</Trans>
						</Button>
					</Modal.Footer>
				</>
			)}

			{stage === 'newEmail' && (
				<Form form={newEmailForm} onSubmit={handleNewEmailSubmit} aria-label={t`New email form`}>
					<Modal.Content>
						<Modal.ContentLayout>
							<Modal.Description>
								<Trans>Enter the new email you want to use. We'll send a code there next.</Trans>
							</Modal.Description>
							<Modal.InputGroup>
								<Input
									{...newEmailForm.register('email')}
									autoComplete="email"
									autoFocus={true}
									error={newEmailForm.formState.errors.email?.message}
									label={t`New Email`}
									maxLength={256}
									minLength={1}
									placeholder={t`marty@example.com`}
									required={true}
									type="email"
								/>
							</Modal.InputGroup>
						</Modal.ContentLayout>
					</Modal.Content>
					<Modal.Footer className={styles.footer}>
						<Button onClick={ModalActionCreators.pop} variant="secondary">
							<Trans>Cancel</Trans>
						</Button>
						<Button type="submit" submitting={submitting}>
							<Trans>Send code</Trans>
						</Button>
					</Modal.Footer>
				</Form>
			)}

			{stage === 'verifyNew' && (
				<>
					<Modal.Content>
						<Modal.ContentLayout>
							<Modal.Description>
								<Trans>Enter the code we emailed to your new address.</Trans>
							</Modal.Description>
							<Modal.InputGroup>
								<Input
									autoFocus={true}
									value={newCode}
									onChange={(event: React.ChangeEvent<HTMLInputElement>) => setNewCode(event.target.value)}
									label={t`Verification Code`}
									placeholder="XXXX-XXXX"
									required={true}
									error={newCodeError ?? undefined}
								/>
							</Modal.InputGroup>
						</Modal.ContentLayout>
					</Modal.Content>
					<Modal.Footer className={styles.footer}>
						<Button onClick={ModalActionCreators.pop} variant="secondary">
							<Trans>Cancel</Trans>
						</Button>
						<Button onClick={handleResendNew} disabled={!canResendNew || submitting}>
							{canResendNew ? <Trans>Resend</Trans> : <Trans>Resend ({newSecondsRemaining}s)</Trans>}
						</Button>
						<Button onClick={handleVerifyNew} submitting={submitting}>
							<Trans>Confirm</Trans>
						</Button>
					</Modal.Footer>
				</>
			)}
		</Modal.Root>
	);
});
