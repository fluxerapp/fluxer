// SPDX-License-Identifier: AGPL-3.0-or-later

import {BootstrapErrorKind, classifyBootstrapError} from '@app/features/app/components/BootstrapErrorKind';
import styles from '@app/features/app/components/ErrorFallback.module.css';
import {BLUESKY_PROVIDER_NAME, FLUXER_BLUESKY_HANDLE} from '@app/features/app/config/I18nDisplayConstants';
import {resolveAppShellBranding} from '@app/features/app/state/AppShellBranding';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	ACCOUNT_DETAILS_UNAVAILABLE_DESCRIPTOR,
	getAccountDisplayLabels,
	resolveAccountInstanceLabel,
} from '@app/features/auth/AccountDisplayUtils';
import {TRY_AGAIN_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {Account} from '@app/features/platform/state/AuthSession';
import {ResetClientStateReason, resetClientState} from '@app/features/platform/state/ResetClientState';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {Button} from '@app/features/ui/button/Button';
import {FluxerIcon} from '@app/features/ui/components/icons/FluxerIcon';
import {ExternalUrls} from '@fluxer/constants/src/ExternalUrls';
import {MS_PER_SECOND} from '@fluxer/date_utils/src/DateConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';

const UNREACHABLE_RETRY_INTERVAL_MS = 30 * MS_PER_SECOND;
const logger = new Logger('BootstrapErrorScreen');

export interface BootstrapAccounts {
	readonly restoring: Account | null;
	readonly others: ReadonlyArray<Account>;
	readonly restoreOnNextStart: (accountKey: string) => Promise<void>;
}

interface BootstrapErrorScreenProps {
	error?: Error;
	accounts?: BootstrapAccounts | null;
}

function reloadApp(): void {
	window.location.reload();
}

function describeError(error: Error): string {
	return error.stack?.trim() || `${error.name}: ${error.message}`;
}

function AccountChoices({accounts}: {accounts: BootstrapAccounts}) {
	const {i18n} = useLingui();
	const [pendingAccountKey, setPendingAccountKey] = useState<string | null>(null);
	const handleChoose = useCallback(
		(accountKey: string) => {
			setPendingAccountKey(accountKey);
			accounts.restoreOnNextStart(accountKey).then(reloadApp, (error: unknown) => {
				logger.error('Failed to choose another account on the bootstrap error screen:', error);
				setPendingAccountKey(null);
			});
		},
		[accounts],
	);
	return (
		<div className={styles.errorFallbackContent} data-flx="app.bootstrap-error-screen.account-choices">
			<h2 className={styles.errorFallbackSubtitle} data-flx="app.bootstrap-error-screen.account-choices-title">
				<Trans comment="Heading on the startup screen shown when the current account's instance cannot be reached. Lists the other signed-in accounts.">
					Use another account
				</Trans>
			</h2>
			<div className={styles.errorFallbackAccounts} data-flx="app.bootstrap-error-screen.account-choices-list">
				{accounts.others.map((account) => {
					const labels = getAccountDisplayLabels(account);
					const accountName = labels.available ? labels.tagLabel : i18n._(ACCOUNT_DETAILS_UNAVAILABLE_DESCRIPTOR);
					const instanceHost = resolveAccountInstanceLabel(account);
					return (
						<Button
							key={account.storageKey}
							variant="secondary"
							submitting={pendingAccountKey === account.storageKey}
							disabled={pendingAccountKey !== null}
							onClick={() => handleChoose(account.storageKey)}
							data-flx="app.bootstrap-error-screen.button.account-choice"
						>
							{instanceHost ? (
								<Trans comment="Button on the startup screen that restarts the app on another signed-in account. accountName is the account's display name and instanceHost the host of its instance, such as chat.example.com.">
									{accountName} on {instanceHost}
								</Trans>
							) : (
								accountName
							)}
						</Button>
					);
				})}
			</div>
		</div>
	);
}

export const BootstrapErrorScreen: React.FC<BootstrapErrorScreenProps> = ({error, accounts = null}) => {
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
			.then(setCopied, (copyError: unknown) => {
				logger.warn('Failed to load or run the bootstrap diagnostic copy action:', copyError);
				setCopied(false);
			});
	}, [error, i18n]);
	const handleReset = useCallback(() => {
		void resetClientState({reason: ResetClientStateReason.RESET_APP_DATA, keepDrafts: true}).finally(() =>
			window.location.reload(),
		);
	}, []);
	const runtimeSnapshot = RuntimeConfig.getSnapshotOrNull();
	const storedInstance = accounts?.restoring?.instance ?? null;
	const productName = resolveAppShellBranding(runtimeSnapshot ?? storedInstance).productName;
	const statusPageUrl = RuntimeConfig.statusPageUrl;
	const selfHosted =
		runtimeSnapshot !== null ? RuntimeConfig.isSelfHosted() : storedInstance?.features.self_hosted === true;
	const instanceHost = accounts?.restoring ? resolveAccountInstanceLabel(accounts.restoring) : null;
	const otherAccounts = unreachable && accounts !== null && accounts.others.length > 0 ? accounts : null;
	let description: React.ReactNode;
	if (unreachable && instanceHost) {
		description = (
			<Trans comment="Startup screen text when the current account's instance cannot be reached. instanceHost is its host, such as chat.example.com.">
				We couldn't connect to {instanceHost}. Check your connection and try again.
			</Trans>
		);
	} else if (unreachable) {
		description = <Trans>Check your connection and try again.</Trans>;
	} else {
		description = (
			<Trans>{productName} failed to start properly. This could be due to corrupted data or a temporary issue.</Trans>
		);
	}
	return (
		<div className={styles.errorFallbackContainer} data-flx="app.bootstrap-error-screen.error-fallback-container">
			<FluxerIcon className={styles.errorFallbackIcon} data-flx="app.bootstrap-error-screen.error-fallback-icon" />
			<div className={styles.errorFallbackContent} data-flx="app.bootstrap-error-screen.error-fallback-content">
				<h1 className={styles.errorFallbackTitle} data-flx="app.bootstrap-error-screen.error-fallback-title">
					{unreachable ? <Trans>Can't connect</Trans> : <Trans>Failed to start</Trans>}
				</h1>
				<p className={styles.errorFallbackDescription} data-flx="app.bootstrap-error-screen.error-fallback-description">
					{description}
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
				{statusPageUrl && (
					<p
						className={styles.errorFallbackDescription}
						data-flx="app.bootstrap-error-screen.error-fallback-description--status"
					>
						<Trans>
							Check the{' '}
							<a
								href={statusPageUrl}
								target="_blank"
								rel="noopener noreferrer"
								data-flx="app.bootstrap-error-screen.status-page-link"
							>
								status page
							</a>{' '}
							for updates.
						</Trans>
					</p>
				)}
				{!selfHosted && (
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
				)}
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
			{otherAccounts ? (
				<AccountChoices accounts={otherAccounts} data-flx="app.bootstrap-error-screen.account-choices" />
			) : null}
		</div>
	);
};
