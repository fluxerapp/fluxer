// SPDX-License-Identifier: AGPL-3.0-or-later

import '@app/app/BootShell.css';
import {startAppStorage} from '@app/features/platform/state/AppStorageBootstrap';
import {installBrowserStorageAccessProtection} from '@app/features/platform/state/ProtectedWebStorage';
import '@fluxer/fonts/css/fluxer-sans.css';
import '@fluxer/fonts/css/fluxer-mono.css';
import '@fluxer/fonts/css/variables.css';
import '@fluxer/fonts/css/locale-fallbacks.css';
import '@app/app/font-fallback.css';
import '@app/app/fonts/fallback/fallback-faces.css';
import '@app/app/globals.css';
import '@app/features/theme/styles/generated/color-system.css';
import '@app/features/theme/styles/generated/message-layout.css';
import '@app/features/theme/styles/preflight.css';
import {resolveDomainMigrationSide} from '@app/features/app/domain_migration/DomainMigrationCore';
import {runDomainMigrationPreMount} from '@app/features/app/domain_migration/DomainMigrationPreMount';
import {installRuntimeConfigEffects} from '@app/features/app/state/RuntimeConfigEffects';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import {installUnhandledRejectionReporter} from '@app/features/platform/utils/UnhandledRejectionReporter';
import {PASSKEY_BRIDGE_PATH} from '@fluxer/constants/src/PasskeyConstants';
import {configure} from 'mobx';

const logger = new Logger('index');

installBrowserStorageAccessProtection();
installUnhandledRejectionReporter();

configure({disableErrorBoundaries: true, enforceActions: 'observed'});

function loadAppBootstrap() {
	return loadLazyModule(() => import(/* webpackChunkName: "app-bootstrap" */ '@app/app/AppBootstrap'));
}

async function bootstrap(): Promise<void> {
	installRuntimeConfigEffects();
	const passkeyBridgeSide =
		window.location.pathname === PASSKEY_BRIDGE_PATH ? resolveDomainMigrationSide(window.location.origin) : null;
	if (passkeyBridgeSide !== null) {
		const {runPasskeyBridge} = await loadAppBootstrap();
		await runPasskeyBridge(passkeyBridgeSide);
		return;
	}
	if (await runDomainMigrationPreMount()) {
		return;
	}
	const storageBootstrap = await startAppStorage();
	const {runApp} = await loadAppBootstrap();
	await runApp(storageBootstrap);
	confirmDesktopLaunch();
}

function confirmDesktopLaunch(): void {
	const confirmLaunch = globalThis.window?.electron?.desktopModules?.confirmLaunch;
	if (confirmLaunch == null) {
		return;
	}
	confirmLaunch().catch((error: unknown) => {
		logger.warn('Failed to confirm the desktop launch:', error);
	});
}

async function showBootstrapError(error: unknown): Promise<void> {
	const {reportBootstrapError} = await loadLazyModule(() => import('@app/app/BootstrapErrorMount'));
	await reportBootstrapError(error);
}

function showPlainBootstrapError(): void {
	const container = document.getElementById('root');
	if (!container) {
		return;
	}
	const title = document.createElement('p');
	title.textContent = 'Failed to start';
	const retry = document.createElement('button');
	retry.type = 'button';
	retry.textContent = 'Try again';
	retry.addEventListener('click', () => window.location.reload());
	container.replaceChildren(title, retry);
}

bootstrap().catch((error: unknown) => {
	showBootstrapError(error).catch((renderError: unknown) => {
		logger.error('Failed to bootstrap app:', error);
		logger.error('Failed to show the bootstrap error screen:', renderError);
		showPlainBootstrapError();
	});
});
