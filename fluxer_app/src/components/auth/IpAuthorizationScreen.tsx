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

import {Trans} from '@lingui/react/macro';
import {EnvelopeSimpleIcon, WarningCircleIcon} from '@phosphor-icons/react';
import {useCallback, useEffect, useRef, useState} from 'react';
import * as AuthenticationActionCreators from '~/actions/AuthenticationActionCreators';
import {Button} from '~/components/uikit/Button/Button';
import type {IpAuthorizationChallenge} from '~/hooks/useLoginFlow';
import styles from './IpAuthorizationScreen.module.css';

type ConnectionState = 'connecting' | 'connected' | 'error';

interface IpAuthorizationScreenProps {
	challenge: IpAuthorizationChallenge;
	onAuthorized: (payload: {token: string; userId: string}) => Promise<void> | void;
	onBack?: () => void;
}

const MAX_RETRY_ATTEMPTS = 3;
const RETRY_DELAY_MS = 2000;

const IpAuthorizationScreen = ({challenge, onAuthorized, onBack}: IpAuthorizationScreenProps) => {
	const [resendUsed, setResendUsed] = useState(false);
	const [resendIn, setResendIn] = useState(challenge.resendAvailableIn);
	const [connectionState, setConnectionState] = useState<ConnectionState>('connecting');
	const [retryCount, setRetryCount] = useState(0);
	const onAuthorizedRef = useRef(onAuthorized);
	onAuthorizedRef.current = onAuthorized;

	useEffect(() => {
		setResendUsed(false);
		setResendIn(challenge.resendAvailableIn);
		setConnectionState('connecting');
		setRetryCount(0);
	}, [challenge]);

	useEffect(() => {
		let es: EventSource | null = null;
		let retryTimeout: ReturnType<typeof setTimeout> | null = null;
		let isMounted = true;

		const connect = () => {
			if (!isMounted) return;

			es = AuthenticationActionCreators.subscribeToIpAuthorization(challenge.ticket);

			es.onopen = () => {
				if (isMounted) {
					setConnectionState('connected');
					setRetryCount(0);
				}
			};

			es.onmessage = async (event) => {
				if (!event.data) return;
				try {
					const data = JSON.parse(event.data);
					if (data?.token && data?.user_id) {
						es?.close();
						await onAuthorizedRef.current({token: data.token, userId: data.user_id});
					}
				} catch {}
			};

			es.onerror = () => {
				es?.close();
				if (!isMounted) return;

				setRetryCount((prev) => {
					const newCount = prev + 1;
					if (newCount < MAX_RETRY_ATTEMPTS) {
						setConnectionState('connecting');
						retryTimeout = setTimeout(connect, RETRY_DELAY_MS);
					} else {
						setConnectionState('error');
					}
					return newCount;
				});
			};
		};

		connect();

		return () => {
			isMounted = false;
			es?.close();
			if (retryTimeout) {
				clearTimeout(retryTimeout);
			}
		};
	}, [challenge.ticket]);

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
			await AuthenticationActionCreators.resendIpAuthorization(challenge.ticket);
			setResendUsed(true);
			setResendIn(30);
		} catch (error) {
			console.error('Failed to resend IP authorization email', error);
		}
	}, [challenge.ticket, resendIn, resendUsed]);

	const handleRetryConnection = useCallback(() => {
		setRetryCount(0);
		setConnectionState('connecting');
	}, []);

	return (
		<div className={styles.container}>
			<div className={styles.icon}>
				{connectionState === 'error' ? (
					<WarningCircleIcon size={48} weight="fill" />
				) : (
					<EnvelopeSimpleIcon size={48} weight="fill" />
				)}
			</div>
			<h1 className={styles.title}>
				{connectionState === 'error' ? <Trans>Connection lost</Trans> : <Trans>Check your email</Trans>}
			</h1>
			<p className={styles.description}>
				{connectionState === 'error' ? (
					<Trans>We lost the connection while waiting for authorization. Please try again.</Trans>
				) : (
					<Trans>We emailed a link to authorize this login. Please open your inbox for {challenge.email}.</Trans>
				)}
			</p>
			{connectionState === 'connecting' && retryCount > 0 && (
				<p className={styles.retryingText}>
					<Trans>Reconnecting...</Trans>
				</p>
			)}
			<div className={styles.actions}>
				{connectionState === 'error' ? (
					<Button variant="primary" onClick={handleRetryConnection}>
						<Trans>Retry</Trans>
					</Button>
				) : (
					<Button variant="secondary" onClick={handleResend} disabled={resendIn > 0 || resendUsed}>
						{resendUsed ? <Trans>Resent</Trans> : <Trans>Resend email</Trans>}
						{resendIn > 0 ? ` (${resendIn}s)` : ''}
					</Button>
				)}
				{onBack ? (
					<Button variant="secondary" onClick={onBack}>
						<Trans>Back</Trans>
					</Button>
				) : null}
			</div>
		</div>
	);
};

export default IpAuthorizationScreen;
