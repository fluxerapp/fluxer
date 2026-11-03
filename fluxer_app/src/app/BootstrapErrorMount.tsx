// SPDX-License-Identifier: AGPL-3.0-or-later

import {ensureActiveLocale, initI18n} from '@app/app/I18n';
import {BootstrapErrorKind, classifyBootstrapError} from '@app/features/app/components/BootstrapErrorKind';
import {type BootstrapAccounts, BootstrapErrorScreen} from '@app/features/app/components/BootstrapErrorScreen';
import {AppI18nProvider} from '@app/features/i18n/components/AppI18nProvider';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {i18n} from '@lingui/core';
import ReactDOM from 'react-dom/client';

const logger = new Logger('index');

async function loadBootstrapAccounts(): Promise<BootstrapAccounts | null> {
	try {
		const {default: SessionManager} = await import('@app/features/platform/state/AuthSession');
		const restoringAccountKey = SessionManager.restoringAccountKey;
		const accounts = SessionManager.accounts;
		return {
			restoring: accounts.find((account) => account.storageKey === restoringAccountKey) ?? null,
			others: accounts.filter(
				(account) => account.storageKey !== restoringAccountKey && account.isValid && account.instance !== undefined,
			),
			restoreOnNextStart: (accountKey) => SessionManager.restoreAccountOnNextStart(accountKey),
		};
	} catch (error) {
		logger.warn('Failed to load the stored accounts for the bootstrap error screen:', error);
		return null;
	}
}

export async function reportBootstrapError(error: unknown): Promise<void> {
	const normalized = error instanceof Error ? error : new Error(String(error));
	logger.error('Failed to bootstrap app:', normalized);
	const container = document.getElementById('root');
	if (!container) {
		throw new Error('Missing #root element');
	}
	const accounts =
		classifyBootstrapError(normalized, navigator.onLine) === BootstrapErrorKind.UNREACHABLE
			? await loadBootstrapAccounts()
			: null;
	ensureActiveLocale();
	ReactDOM.createRoot(container).render(
		<AppI18nProvider i18n={i18n}>
			<BootstrapErrorScreen error={normalized} accounts={accounts} data-flx="index.bootstrap-error-screen" />
		</AppI18nProvider>,
	);
	globalThis.window?.electron?.notifyFirstContentPainted?.();
	initI18n().catch((localeError: unknown) => {
		logger.warn('Failed to load the locale for the bootstrap error screen:', localeError);
	});
}
