// SPDX-License-Identifier: AGPL-3.0-or-later

import {BootstrapErrorKind, classifyBootstrapError} from '@app/features/app/components/BootstrapErrorKind';
import styles from '@app/features/app/components/ErrorFallback.module.css';
import {
	BLUESKY_PROVIDER_NAME,
	FLUXER_BLUESKY_HANDLE,
	PRODUCT_NAME,
} from '@app/features/app/config/I18nDisplayConstants';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {ResetClientStateReason, resetClientState} from '@app/features/platform/state/ResetClientState';
import {Button} from '@app/features/ui/button/Button';
import {APPLICATION_ICON_DESCRIPTOR, FluxerIconMark} from '@app/features/ui/components/icons/FluxerIconMark';
import {ExternalUrls} from '@fluxer/constants/src/ExternalUrls';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';

const UNREACHABLE_RETRY_INTERVAL_MS = 30_000;

interface BootstrapErrorScreenProps {
	error?: Error;
}

function reloadApp(): void {
	window.location.reload();
}

function describeError(error: Error): string {
	return error.stack?.trim() || `${error.name}: ${error.message}`;
}

export const BootstrapErrorScreen: React.FC<BootstrapErrorScreenProps> = ({error}) => {
	const {i18n} = useLingui();
	const unreachable = classifyBootstrapError(error, navigator.onLine) === BootstrapErrorKind.UNREACHABLE;
	useEffect(() => {
		if (!unreachable) {
			return;
		}
		window.addEventListener('online', reloadApp);
		const retry = window.setInterval(() => {
			if (navigator.onLine) {
				reloadApp();
			}
		}, UNREACHABLE_RETRY_INTERVAL_MS);
		return () => {
			window.removeEventListener('online', reloadApp);
			window.clearInterval(retry);
		};
	}, [unreachable]);
	const [copied, setCopied] = useState(false);
	const handleCopyDetails = useCallback(() => {
		if (!error) {
			return;
		}
		void import('@app/features/ui/commands/TextCopyCommands')
			.then(({copy}) => copy(i18n, describeError(error), true))
			.then(setCopied, () => setCopied(false));
	}, [error, i18n]);
	const handleReset = useCallback(() => {
		void resetClientState({reason: ResetClientStateReason.RESET_APP_DATA, keepDrafts: true}).finally(() =>
			window.location.reload(),
		);
	}, []);
	return (
		<div className={styles.errorFallbackContainer} data-flx="app.bootstrap-error-screen.error-fallback-container">
			<FluxerIconMark
				aria-label={i18n._(APPLICATION_ICON_DESCRIPTOR, {productName: PRODUCT_NAME})}
				className={styles.errorFallbackIcon}
				data-flx="app.bootstrap-error-screen.error-fallback-icon"
			/>
			<div className={styles.errorFallbackContent} data-flx="app.bootstrap-error-screen.error-fallback-content">
				<h1 className={styles.errorFallbackTitle} data-flx="app.bootstrap-error-screen.error-fallback-title">
					{unreachable ? <Trans>Can't connect</Trans> : <Trans>Failed to start</Trans>}
				</h1>
				<p className={styles.errorFallbackDescription} data-flx="app.bootstrap-error-screen.error-fallback-description">
					{unreachable ? (
						<Trans>Check your connection and try again.</Trans>
					) : (
						<Trans>
							{PRODUCT_NAME} failed to start properly. This could be due to corrupted data or a temporary issue.
						</Trans>
					)}
				</p>
				{error && !unreachable && (
					<p
						className={styles.errorFallbackDescription}
						style={{fontSize: '0.875rem', opacity: 0.8}}
						data-flx="app.bootstrap-error-screen.error-fallback-description--2"
					>
						{error.message}
					</p>
				)}
				<p
					className={styles.errorFallbackDescription}
					data-flx="app.bootstrap-error-screen.error-fallback-description--3"
				>
					<Trans>
						Check our{' '}
						<a
							href={ExternalUrls.BLUESKY}
							target="_blank"
							rel="noopener noreferrer"
							data-flx="app.bootstrap-error-screen.a"
						>
							{BLUESKY_PROVIDER_NAME} ({FLUXER_BLUESKY_HANDLE})
						</a>{' '}
						for status updates.
					</Trans>
				</p>
			</div>
			<div className={styles.errorFallbackActions} data-flx="app.bootstrap-error-screen.error-fallback-actions">
				<Button onClick={reloadApp} data-flx="app.bootstrap-error-screen.button.retry">
					{i18n._(TRY_AGAIN_DESCRIPTOR)}
				</Button>
				{error && unreachable && (
					<Button
						onClick={handleCopyDetails}
						variant="secondary"
						data-flx="app.bootstrap-error-screen.button.copy-details"
					>
						{copied ? (
							<Trans comment="Toast shown after copying text to the clipboard succeeds.">Copied to clipboard</Trans>
						) : (
							<Trans comment="Button on the crash screen that copies developer diagnostic text.">
								Copy stack trace
							</Trans>
						)}
					</Button>
				)}
				{!unreachable && (
					<Button onClick={handleReset} variant="danger" data-flx="app.bootstrap-error-screen.button.reset">
						<Trans comment="Destructive button on the startup failure screen. Clears local app data except preserved drafts.">
							Reset app data
						</Trans>
					</Button>
				)}
			</div>
		</div>
	);
};
