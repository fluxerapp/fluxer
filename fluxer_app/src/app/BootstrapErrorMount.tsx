// SPDX-License-Identifier: AGPL-3.0-or-later

import {initI18n} from '@app/app/I18n';
import {BootstrapErrorScreen} from '@app/features/app/components/BootstrapErrorScreen';
import {AppI18nProvider} from '@app/features/i18n/components/AppI18nProvider';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {i18n} from '@lingui/core';
import ReactDOM from 'react-dom/client';

const logger = new Logger('index');

export async function reportBootstrapError(error: unknown): Promise<void> {
	const normalized = error instanceof Error ? error : new Error(String(error));
	logger.error('Failed to bootstrap app:', normalized);
	const container = document.getElementById('root');
	if (!container) {
		throw new Error('Missing #root element');
	}
	await initI18n();
	ReactDOM.createRoot(container).render(
		<AppI18nProvider i18n={i18n}>
			<BootstrapErrorScreen error={normalized} data-flx="index.bootstrap-error-screen" />
		</AppI18nProvider>,
	);
}
