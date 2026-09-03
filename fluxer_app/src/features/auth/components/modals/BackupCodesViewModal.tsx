// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import * as MfaCommands from '@app/features/auth/commands/MfaCommands';
import {BackupCodesModal} from '@app/features/auth/components/modals/BackupCodesModal';
import {VERIFICATION_CODE_DESCRIPTOR, VERIFY_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import type {User} from '@app/features/user/models/User';
import * as FormUtils from '@app/lib/forms';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useState} from 'react';

const VIEW_BACKUP_CODES_DESCRIPTOR = msg({
	message: 'View backup codes',
	comment: 'Short label in the authentication backup codes view modal. Keep the tone plain and specific.',
});
const UNABLE_TO_SEND_VERIFICATION_CODE_DESCRIPTOR = msg({
	message: 'Unable to send verification code',
	comment: 'Error message in the authentication backup codes view modal. Keep the tone plain and specific.',
});
const INVALID_OR_EXPIRED_CODE_DESCRIPTOR = msg({
	message: 'Invalid or expired code',
	comment: 'Error message in the authentication backup codes view modal. Keep the tone plain and specific.',
});
const UNABLE_TO_RESEND_CODE_RIGHT_NOW_DESCRIPTOR = msg({
	message: 'Unable to resend code right now',
	comment: 'Error message in the authentication backup codes view modal. Keep the tone plain and specific.',
});
const CHALLENGE_EXPIRED_DESCRIPTOR = msg({
	message: 'Verification expired',
	comment: 'Error title shown when the backup codes email verification expired and must be started again.',
});

function resolveApiError(i18n: I18n, error: unknown, fallback: string): string {
	return error && typeof error === 'object' && 'body' in error ? FormUtils.extractErrorMessage(i18n, error) : fallback;
}

function isExpiredTicketError(error: unknown): boolean {
	if (!error || typeof error !== 'object' || !('body' in error)) {
		return false;
	}
	const body = (error as {body?: {errors?: Array<{path?: string}>}}).body;
	return body?.errors?.some((validationError) => validationError.path === 'ticket') ?? false;
}

type Stage = 'intro' | 'verify';

interface BackupCodesViewModalProps {
	user: User;
}

export const BackupCodesViewModal = observer(({user}: BackupCodesViewModalProps) => {
	const {i18n} = useLingui();
	const [stage, setStage] = useState<Stage>('intro');
	const [ticket, setTicket] = useState<string | null>(null);
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
	const resetToIntro = useCallback(() => {
		setTicket(null);
		setCode('');
		setResendAt(null);
		setStage('intro');
	}, []);
	const startChallenge = useCallback(async () => {
		setSubmitting(true);
		try {
			const result = await MfaCommands.startBackupCodesChallenge();
			setTicket(result.ticket);
			if (result.resend_available_at) {
				setResendAt(new Date(result.resend_available_at));
			}
			setStage('verify');
		} catch (error: unknown) {
			FormUtils.pushApiErrorModal(i18n, error, i18n._(UNABLE_TO_SEND_VERIFICATION_CODE_DESCRIPTOR));
		} finally {
			setSubmitting(false);
		}
	}, [i18n]);
	const handleVerify = useCallback(async () => {
		if (!ticket) return;
		setSubmitting(true);
		setCodeError(null);
		try {
			const result = await MfaCommands.verifyBackupCodesChallenge(ticket, code);
			const challenge = {ticket, verificationProof: result.verification_proof};
			ModalCommands.pop();
			ModalCommands.pushWithKey(
				modal(() => (
					<BackupCodesModal
						backupCodes={result.backup_codes}
						user={user}
						challenge={challenge}
						data-flx="auth.backup-codes-view-modal.handle-verify.backup-codes-modal"
					/>
				)),
				'backup-codes',
			);
		} catch (error: unknown) {
			if (isExpiredTicketError(error)) {
				resetToIntro();
				FormUtils.pushApiErrorModal(i18n, error, i18n._(CHALLENGE_EXPIRED_DESCRIPTOR));
				return;
			}
			setCodeError(resolveApiError(i18n, error, i18n._(INVALID_OR_EXPIRED_CODE_DESCRIPTOR)));
		} finally {
			setSubmitting(false);
		}
	}, [ticket, code, user, i18n, resetToIntro]);
	const handleResend = useCallback(async () => {
		if (!ticket || !canResend) return;
		setSubmitting(true);
		try {
			await MfaCommands.resendBackupCodesChallengeCode(ticket);
			setResendAt(new Date(Date.now() + 30 * 1000));
		} catch (error: unknown) {
			if (isExpiredTicketError(error)) {
				resetToIntro();
				FormUtils.pushApiErrorModal(i18n, error, i18n._(CHALLENGE_EXPIRED_DESCRIPTOR));
				return;
			}
			FormUtils.pushApiErrorModal(i18n, error, i18n._(UNABLE_TO_RESEND_CODE_RIGHT_NOW_DESCRIPTOR));
		} finally {
			setSubmitting(false);
		}
	}, [ticket, canResend, i18n, resetToIntro]);
	const renderIntroStage = () => (
		<Modal.Description data-flx="auth.backup-codes-view-modal.modal-description">
			<Trans>We'll send a verification code to your email before you can view your backup codes.</Trans>
		</Modal.Description>
	);
	const renderVerifyStage = () => (
		<>
			<Modal.Description data-flx="auth.backup-codes-view-modal.modal-description--2">
				<Trans>Enter the code sent to your email address.</Trans>
			</Modal.Description>
			<Modal.InputGroup data-flx="auth.backup-codes-view-modal.modal-input-group">
				<Input
					autoFocus={true}
					value={code}
					onChange={(event) => setCode(event.target.value)}
					label={i18n._(VERIFICATION_CODE_DESCRIPTOR)}
					placeholder="XXXX-XXXX"
					required={true}
					error={codeError ?? undefined}
					data-flx="auth.backup-codes-view-modal.input.set-code"
				/>
			</Modal.InputGroup>
		</>
	);
	const renderStageFooter = () => {
		switch (stage) {
			case 'intro':
				return (
					<>
						<Button onClick={ModalCommands.pop} variant="secondary" data-flx="auth.backup-codes-view-modal.button.pop">
							<Trans>Cancel</Trans>
						</Button>
						<Button
							onClick={startChallenge}
							submitting={submitting}
							data-flx="auth.backup-codes-view-modal.button.start-challenge"
						>
							<Trans>Continue</Trans>
						</Button>
					</>
				);
			case 'verify':
				return (
					<>
						<Button
							onClick={ModalCommands.pop}
							variant="secondary"
							data-flx="auth.backup-codes-view-modal.button.pop--2"
						>
							<Trans>Cancel</Trans>
						</Button>
						<Button
							onClick={handleResend}
							disabled={!canResend || submitting}
							data-flx="auth.backup-codes-view-modal.button.resend"
						>
							{canResend ? <Trans>Resend</Trans> : <Trans>Resend ({secondsRemaining}s)</Trans>}
						</Button>
						<Button
							onClick={handleVerify}
							submitting={submitting}
							data-flx="auth.backup-codes-view-modal.button.verify"
						>
							{i18n._(VERIFY_DESCRIPTOR)}
						</Button>
					</>
				);
		}
	};
	return (
		<Modal.Root size="small" centered data-flx="auth.backup-codes-view-modal.modal-root">
			<Modal.Header title={i18n._(VIEW_BACKUP_CODES_DESCRIPTOR)} data-flx="auth.backup-codes-view-modal.modal-header" />
			<Modal.Content data-flx="auth.backup-codes-view-modal.modal-content">
				<Modal.ContentLayout data-flx="auth.backup-codes-view-modal.modal-content-layout">
					{stage === 'intro' ? renderIntroStage() : renderVerifyStage()}
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="auth.backup-codes-view-modal.modal-footer">{renderStageFooter()}</Modal.Footer>
		</Modal.Root>
	);
});
