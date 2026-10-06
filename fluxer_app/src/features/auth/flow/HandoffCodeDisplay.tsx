// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/auth/flow/HandoffCodeDisplay.module.css';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckCircleIcon, ClipboardIcon} from '@phosphor-icons/react';
import {type ReactNode, useCallback, useEffect, useRef, useState} from 'react';

interface HandoffCodeDisplayProps {
	code: string | null;
	expiresAt: string | null;
	isGenerating: boolean;
	error: string | null;
	onRetry?: () => void;
	description?: ReactNode;
}

function useCountdown(expiresAt: string | null): number | null {
	const [remaining, setRemaining] = useState<number | null>(() => {
		if (!expiresAt) return null;
		return Math.max(0, Math.ceil((new Date(expiresAt).getTime() - Date.now()) / 1000));
	});
	const expiresAtRef = useRef(expiresAt);
	useEffect(() => {
		if (expiresAt !== expiresAtRef.current) {
			expiresAtRef.current = expiresAt;
			if (expiresAt) {
				setRemaining(Math.max(0, Math.ceil((new Date(expiresAt).getTime() - Date.now()) / 1000)));
			} else {
				setRemaining(null);
			}
		}
	}, [expiresAt]);
	useEffect(() => {
		if (remaining == null || remaining <= 0) return;
		const timer = setInterval(() => {
			const now = Date.now();
			const target = new Date(expiresAtRef.current!).getTime();
			const next = Math.max(0, Math.ceil((target - now) / 1000));
			setRemaining(next);
			if (next <= 0) {
				clearInterval(timer);
			}
		}, 500);
		return () => clearInterval(timer);
	}, [remaining]);
	return remaining;
}

export function HandoffCodeDisplay({
	code,
	expiresAt,
	isGenerating,
	error,
	onRetry,
	description,
}: HandoffCodeDisplayProps) {
	const {i18n} = useLingui();
	const [copied, setCopied] = useState(false);
	const remaining = useCountdown(expiresAt);
	const isExpired = remaining != null && remaining <= 0;
	const handleCopyCode = useCallback(async () => {
		if (!code) return;
		await TextCopyCommands.copy(i18n, code);
		setCopied(true);
		setTimeout(() => setCopied(false), 2000);
	}, [code]);
	const handleRetry = useCallback(() => {
		setCopied(false);
		onRetry?.();
	}, [onRetry]);
	if (isGenerating) {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-code-display.container">
				<h1 className={styles.title} data-flx="auth.flow.handoff-code-display.title">
					<Trans>Generating code…</Trans>
				</h1>
				<div className={styles.spinner} data-flx="auth.flow.handoff-code-display.spinner">
					<span className={styles.spinnerIcon} data-flx="auth.flow.handoff-code-display.spinner-icon" />
				</div>
			</div>
		);
	}
	if (error) {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-code-display.container--2">
				<h1 className={styles.title} data-flx="auth.flow.handoff-code-display.title--2">
					<Trans>Something went wrong</Trans>
				</h1>
				<p className={styles.error} data-flx="auth.flow.handoff-code-display.error">
					{error}
				</p>
				{onRetry && (
					<Button onClick={handleRetry} fitContainer data-flx="auth.flow.handoff-code-display.button.retry">
						{i18n._(TRY_AGAIN_DESCRIPTOR)}
					</Button>
				)}
			</div>
		);
	}
	if (!code) {
		return null;
	}
	if (isExpired) {
		return (
			<div className={styles.container} data-flx="auth.flow.handoff-code-display.container--3">
				<h1 className={styles.title} data-flx="auth.flow.handoff-code-display.title--3">
					<Trans>Code expired</Trans>
				</h1>
				<p className={styles.expiredDescription} data-flx="auth.flow.handoff-code-display.expired-description">
					<Trans>This handoff code has expired. Generate a new one to continue.</Trans>
				</p>
				{onRetry && (
					<Button onClick={handleRetry} fitContainer data-flx="auth.flow.handoff-code-display.button.retry--2">
						<Trans>Generate new code</Trans>
					</Button>
				)}
			</div>
		);
	}
	const codeWithoutHyphen = code.replace(/-/g, '');
	const codePart1 = codeWithoutHyphen.slice(0, 6);
	const codePart2 = codeWithoutHyphen.slice(6, 12);
	return (
		<div className={styles.container} data-flx="auth.flow.handoff-code-display.container--4">
			<h1 className={styles.title} data-flx="auth.flow.handoff-code-display.title--4">
				<Trans>Your code is ready</Trans>
			</h1>
			<p className={styles.description} data-flx="auth.flow.handoff-code-display.description">
				{description ?? <Trans>Enter this code in your browser to complete sign-in.</Trans>}
			</p>
			<div className={styles.codeSection} data-flx="auth.flow.handoff-code-display.code-section">
				<p className={styles.codeLabel} data-flx="auth.flow.handoff-code-display.code-label">
					<Trans>Your code</Trans>
				</p>
				<div className={styles.codeDisplay} data-flx="auth.flow.handoff-code-display.code-display">
					<span className={styles.codeChar} data-flx="auth.flow.handoff-code-display.code-char">
						{codePart1}
					</span>
					<span className={styles.codeSeparator} data-flx="auth.flow.handoff-code-display.code-separator">
						-
					</span>
					<span className={styles.codeChar} data-flx="auth.flow.handoff-code-display.code-char--2">
						{codePart2}
					</span>
				</div>
				<div className={styles.timer} data-flx="auth.flow.handoff-code-display.timer">
					<Trans>Expires in {remaining}s</Trans>
				</div>
				<Button
					type="button"
					onClick={handleCopyCode}
					leftIcon={
						copied ? (
							<CheckCircleIcon size={16} weight="bold" data-flx="auth.flow.handoff-code-display.check-circle-icon" />
						) : (
							<ClipboardIcon size={16} data-flx="auth.flow.handoff-code-display.clipboard-icon" />
						)
					}
					variant="secondary"
					data-flx="auth.flow.handoff-code-display.button.copy-code"
				>
					{copied ? <Trans>Copied</Trans> : <Trans>Copy code</Trans>}
				</Button>
			</div>
		</div>
	);
}
