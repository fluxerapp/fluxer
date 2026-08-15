// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import styles from '@app/features/auth/flow/IpAuthorizationScreen.module.css';
import type {IpAuthorizationChallenge, LoginSuccessPayload} from '@app/features/auth/state/AuthFlow';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {EnvelopeSimpleIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {useCallback, useEffect, useRef, useState} from 'react';

type PollingState = 'polling' | 'error';

interface IpAuthorizationScreenProps {
	challenge: IpAuthorizationChallenge;
	onAuthorized: (payload: LoginSuccessPayload) => Promise<void> | void;
	onBack?: () => void;
}

const POLL_INTERVAL_MS = 2000;
const MAX_POLL_ERRORS = 3;
const CHECK_INBOX_FOR_AUTHORIZATION_LINK_DESCRIPTOR = msg({
	message: 'Check your inbox at {emailAddress} for the authorization link.',
	comment: 'Instruction on the IP authorization screen. emailAddress is the address that received the login email.',
});
const logger = new Logger('IpAuthorizationScreen');
const IpAuthorizationScreen = ({challenge, onAuthorized, onBack}: IpAuthorizationScreenProps) => {
	const {i18n} = useLingui();
	const [resendUsed, setResendUsed] = useState(false);
	const [resendIn, setResendIn] = useState(challenge.resendAvailableIn);
	const [pollingState, setPollingState] = useState<PollingState>('polling');
	const onAuthorizedRef = useRef(onAuthorized);
	onAuthorizedRef.current = onAuthorized;
	useEffect(() => {
		setResendUsed(false);
		setResendIn(challenge.resendAvailableIn);
		setPollingState('polling');
	}, [challenge]);
	useEffect(() => {
		let pollTimeout: NodeJS.Timeout | null = null;
		let isMounted = true;
		let consecutiveErrors = 0;
		const poll = async () => {
			if (!isMounted) return;
			try {
				const result = await AuthenticationCommands.pollIpAuthorization(challenge.ticket);
				if (!isMounted) return;
				if (result.completed && result.token && result.user_id) {
					const userData = AuthenticationCommands.authResponseUserToUserData(result.user);
					await onAuthorizedRef.current({
						token: result.token,
						userId: result.user_id,
						...(userData ? {userData} : {}),
					});
					return;
				}
				consecutiveErrors = 0;
				pollTimeout = setTimeout(poll, POLL_INTERVAL_MS);
			} catch (error) {
				if (!isMounted) return;
				consecutiveErrors++;
				if (consecutiveErrors >= MAX_POLL_ERRORS) {
					setPollingState('error');
					logger.error('Failed to poll IP authorization after max retries', error);
				} else {
					pollTimeout = setTimeout(poll, POLL_INTERVAL_MS);
				}
			}
		};
		poll();
		return () => {
			isMounted = false;
			if (pollTimeout) {
				clearTimeout(pollTimeout);
			}
		};
	}, [challenge.ticket, pollingState]);
	useEffect(() => {
		if (resendIn <= 0) return;
		const interval = setInterval(() => {
			setResendIn((prev) => (prev > 0 ? prev - 1 : 0));
		}, 1000);
		return () => clearInterval(interval);
	}, [resendIn]);
	const handleResend = useCallback(async () => {
		if (resendIn > 0 || resendUsed) return;
		try {
			await AuthenticationCommands.resendIpAuthorization(challenge.ticket);
			setResendUsed(true);
			setResendIn(30);
		} catch (error) {
			logger.error('Failed to resend IP authorization email', error);
		}
	}, [challenge.ticket, resendIn, resendUsed]);
	const handleRetry = useCallback(() => {
		setPollingState('polling');
	}, []);
	return (
		<div className={styles.container} data-flx="auth.flow.ip-authorization-screen.container">
			<div className={styles.icon} data-flx="auth.flow.ip-authorization-screen.icon">
				{pollingState === 'error' ? (
					<WarningCircleIcon
						size={remFromPx(48)}
						weight="fill"
						data-flx="auth.flow.ip-authorization-screen.warning-circle-icon"
					/>
				) : (
					<EnvelopeSimpleIcon
						size={remFromPx(48)}
						weight="fill"
						data-flx="auth.flow.ip-authorization-screen.envelope-simple-icon"
					/>
				)}
			</div>
			<h1 className={styles.title} data-flx="auth.flow.ip-authorization-screen.title">
				{pollingState === 'error' ? <Trans>Connection lost</Trans> : <Trans>Check your email</Trans>}
			</h1>
			<p
				className={styles.description}
				role={pollingState === 'error' ? 'alert' : 'status'}
				data-flx="auth.flow.ip-authorization-screen.description"
			>
				{pollingState === 'error' ? (
					<Trans>Lost connection while waiting for authorization. Try again.</Trans>
				) : (
					i18n._(CHECK_INBOX_FOR_AUTHORIZATION_LINK_DESCRIPTOR, {emailAddress: challenge.email})
				)}
			</p>
			<div className={styles.actions} data-flx="auth.flow.ip-authorization-screen.actions">
				{pollingState === 'error' ? (
					<Button variant="primary" onClick={handleRetry} data-flx="auth.flow.ip-authorization-screen.button.retry">
						{i18n._(TRY_AGAIN_DESCRIPTOR)}
					</Button>
				) : (
					<Button
						variant="secondary"
						onClick={handleResend}
						disabled={resendIn > 0 || resendUsed}
						data-flx="auth.flow.ip-authorization-screen.button.resend"
					>
						{resendUsed ? <Trans>Resent</Trans> : <Trans>Resend email</Trans>}
						{resendIn > 0 ? ` (${resendIn}s)` : ''}
					</Button>
				)}
				{onBack ? (
					<Button variant="secondary" onClick={onBack} data-flx="auth.flow.ip-authorization-screen.button.back">
						<Trans>Back</Trans>
					</Button>
				) : null}
			</div>
		</div>
	);
};

export default IpAuthorizationScreen;
