// SPDX-License-Identifier: AGPL-3.0-or-later

import {type AltchaChallenge, solveAltchaChallenge} from '@app/features/auth/altcha/AltchaSolver';
import styles from '@app/features/auth/components/AltchaVerification.module.css';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import {Spinner} from '@app/features/ui/components/Spinner';
import {Trans} from '@lingui/react/macro';
import {useCallback, useEffect, useRef, useState} from 'react';

const logger = new Logger('AltchaVerification');

interface AltchaVerificationProps {
	challenge: AltchaChallenge;
	onVerify: (token: string) => void;
}

export function AltchaVerification({challenge, onVerify}: AltchaVerificationProps) {
	const onVerifyRef = useRef(onVerify);
	const [attempt, setAttempt] = useState(0);
	const [failed, setFailed] = useState(false);
	useEffect(() => {
		onVerifyRef.current = onVerify;
	}, [onVerify]);
	useEffect(() => {
		const controller = new AbortController();
		setFailed(false);
		solveAltchaChallenge(challenge, controller).then(
			(token) => {
				if (controller.signal.aborted) return;
				if (token) {
					onVerifyRef.current(token);
				} else {
					setFailed(true);
				}
			},
			(error: unknown) => {
				if (controller.signal.aborted) return;
				logger.error('ALTCHA solve failed:', error);
				setFailed(true);
			},
		);
		return () => controller.abort();
	}, [challenge, attempt]);
	const handleRetry = useCallback(() => setAttempt((value) => value + 1), []);
	if (failed) {
		return (
			<div className={styles.container} data-flx="auth.altcha-verification.failed">
				<p className={styles.text} data-flx="auth.altcha-verification.failed-text">
					<Trans>Your browser couldn't finish the check.</Trans>
				</p>
				<Button small variant="secondary" onClick={handleRetry} data-flx="auth.altcha-verification.retry-button">
					<Trans>Try again</Trans>
				</Button>
			</div>
		);
	}
	return (
		<div className={styles.container} role="status" aria-live="polite" data-flx="auth.altcha-verification.solving">
			<Spinner data-flx="auth.altcha-verification.spinner" />
			<p className={styles.text} data-flx="auth.altcha-verification.solving-text">
				<Trans>Checking your browser. This takes a few seconds.</Trans>
			</p>
		</div>
	);
}
