// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/flow/BrowserLoginHandoffModal.module.css';
import {HandoffCodeDisplay} from '@app/features/auth/flow/HandoffCodeDisplay';
import type {LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useRef, useState} from 'react';

const ADD_ACCOUNT_DESCRIPTOR = msg({
	message: 'Add account',
	comment: 'Short label in the authentication browser login handoff modal. Keep the tone plain and specific.',
});

export const SIGN_IN_WITH_OLD_APP_DESCRIPTOR = msg({
	message: 'Sign in with your old {productName} app',
	comment:
		'Sign-in option and modal title on fluxer.com that pairs a newly installed app with the old installed app still signed in on the previous domain. productName is the app name.',
});

export type BrowserLoginHandoffVariant = 'browser' | 'old_app';

interface BrowserLoginHandoffModalProps {
	onSuccess: (payload: LoginSuccessPayload) => Promise<void>;
	prefillEmail?: string;
	variant?: BrowserLoginHandoffVariant;
}

const POLL_INTERVAL_MS = 2000;

const BrowserLoginHandoffModal = observer(({onSuccess, prefillEmail, variant}: BrowserLoginHandoffModalProps) => {
	const {i18n} = useLingui();
	const isOldAppVariant = variant === 'old_app';
	const currentWebAppUrl = RuntimeConfig.webAppBaseUrl;
	const [handoffCode, setHandoffCode] = useState<string | null>(null);
	const [handoffExpiresAt, setHandoffExpiresAt] = useState<string | null>(null);
	const [isGenerating, setIsGenerating] = useState(false);
	const [error, setError] = useState<string | null>(null);
	const pollingRef = useRef(false);
	const handoffPollSecretRef = useRef<string | null>(null);
	const completedRef = useRef(false);
	const generateCode = useCallback(async () => {
		setIsGenerating(true);
		setError(null);
		setHandoffCode(null);
		setHandoffExpiresAt(null);
		try {
			const result = await AuthenticationCommands.initiateDesktopHandoff();
			handoffPollSecretRef.current = result.poll_secret ?? null;
			setHandoffCode(result.code);
			setHandoffExpiresAt(result.expires_at);
		} catch (e) {
			setError(FormUtils.extractErrorMessage(i18n, e));
		} finally {
			setIsGenerating(false);
		}
	}, [i18n]);
	useEffect(() => {
		void generateCode();
	}, [generateCode]);
	useEffect(() => {
		if (!handoffCode || completedRef.current) return;
		pollingRef.current = true;
		const timer = setInterval(async () => {
			if (!pollingRef.current) return;
			try {
				const result = await AuthenticationCommands.pollDesktopHandoffStatus(handoffCode, handoffPollSecretRef.current);
				if (result.status === 'completed' && result.token && result.user_id) {
					pollingRef.current = false;
					completedRef.current = true;
					const userData = AuthenticationCommands.authResponseUserToUserData(result.user);
					await onSuccess({
						token: result.token,
						userId: result.user_id,
						...(userData ? {userData} : {}),
					});
					ModalCommands.pop();
				} else if (result.status === 'expired') {
					pollingRef.current = false;
				}
			} catch {
				pollingRef.current = false;
			}
		}, POLL_INTERVAL_MS);
		return () => {
			pollingRef.current = false;
			clearInterval(timer);
		};
	}, [handoffCode, onSuccess]);
	const handleOpenBrowser = useCallback(async () => {
		const loginUrl = new URL('/login', currentWebAppUrl);
		loginUrl.searchParams.set('handoff', '1');
		if (prefillEmail) {
			loginUrl.searchParams.set('email', prefillEmail);
		}
		await openExternalUrl(loginUrl.toString());
	}, [currentWebAppUrl, prefillEmail]);
	return (
		<Modal.Root
			size="small"
			centered
			onClose={ModalCommands.pop}
			data-flx="auth.flow.browser-login-handoff-modal.modal-root"
		>
			<Modal.Header
				title={
					isOldAppVariant
						? i18n._(SIGN_IN_WITH_OLD_APP_DESCRIPTOR, {productName: PRODUCT_NAME})
						: i18n._(ADD_ACCOUNT_DESCRIPTOR)
				}
				data-flx="auth.flow.browser-login-handoff-modal.modal-header"
			/>
			<Modal.Content data-flx="auth.flow.browser-login-handoff-modal.modal-content">
				<Modal.ContentLayout className={styles.content} data-flx="auth.flow.browser-login-handoff-modal.content">
					<Modal.Description data-flx="auth.flow.browser-login-handoff-modal.description">
						{isOldAppVariant ? (
							<Trans>Open your old {PRODUCT_NAME} app and choose Link a new device, then enter the code below.</Trans>
						) : (
							<Trans>Open your browser, sign in, then enter the code below to link your account.</Trans>
						)}
					</Modal.Description>
					<HandoffCodeDisplay
						code={handoffCode}
						expiresAt={handoffExpiresAt}
						isGenerating={isGenerating}
						error={error}
						onRetry={generateCode}
						description={
							isOldAppVariant ? (
								<Trans>Enter this code in your old {PRODUCT_NAME} app to complete sign-in.</Trans>
							) : undefined
						}
						data-flx="auth.flow.browser-login-handoff-modal.handoff-code-display"
					/>
					{prefillEmail && !isOldAppVariant ? (
						<Modal.Description
							className={styles.prefillHint}
							data-flx="auth.flow.browser-login-handoff-modal.prefill-hint"
						>
							<Trans>We will prefill {prefillEmail} once browser sign-in opens.</Trans>
						</Modal.Description>
					) : null}
				</Modal.ContentLayout>
			</Modal.Content>
			<Modal.Footer data-flx="auth.flow.browser-login-handoff-modal.modal-footer">
				<Button
					variant="secondary"
					onClick={ModalCommands.pop}
					disabled={isGenerating}
					data-flx="auth.flow.browser-login-handoff-modal.button.pop"
				>
					<Trans>Cancel</Trans>
				</Button>
				{isOldAppVariant ? null : (
					<Button
						variant="primary"
						onClick={handleOpenBrowser}
						submitting={isGenerating}
						data-flx="auth.flow.browser-login-handoff-modal.button.open-browser"
					>
						<ArrowSquareOutIcon
							size={remFromPx(16)}
							weight="bold"
							data-flx="auth.flow.browser-login-handoff-modal.arrow-square-out-icon"
						/>
						<Trans>Open browser</Trans>
					</Button>
				)}
			</Modal.Footer>
		</Modal.Root>
	);
});

export function showBrowserLoginHandoffModal(
	onSuccess: (payload: LoginSuccessPayload) => Promise<void>,
	prefillEmail?: string,
	variant: BrowserLoginHandoffVariant = 'browser',
): void {
	ModalCommands.push(
		modal(() => (
			<BrowserLoginHandoffModal
				onSuccess={async (payload) => {
					await onSuccess(payload);
				}}
				prefillEmail={prefillEmail}
				variant={variant}
				data-flx="auth.flow.browser-login-handoff-modal.show-browser-login-handoff-modal.browser-login-handoff-modal"
			/>
		)),
	);
}

export default BrowserLoginHandoffModal;
