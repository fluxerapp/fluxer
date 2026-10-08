// SPDX-License-Identifier: AGPL-3.0-or-later

import 'urlpattern-polyfill';
import {bootstrapSyntheticHistory} from '@app/app/HistoryBootstrap';
import reactiveI18n, {initI18n} from '@app/app/I18n';
import {Routes} from '@app/app/Routes';
import {AppErrorBoundary} from '@app/features/app/components/AppErrorBoundary';
import {ErrorFallback} from '@app/features/app/components/ErrorFallback';
import type {DomainMigrationSide} from '@app/features/app/domain_migration/DomainMigrationCore';
import {installSelfXssNotice} from '@app/features/devtools/utils/SelfXssNotice';
import {AppI18nProvider} from '@app/features/i18n/components/AppI18nProvider';
import {installLocaleSwitchWatchdog} from '@app/features/i18n/utils/LocaleSwitchWatchdog';
import {installTranslationDomGuard} from '@app/features/i18n/utils/TranslationDomGuard';
import {installScrollRestoration} from '@app/features/platform/components/router/ScrollRestoration';
import {isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';
import type {
	AppStorageBootstrapHandle,
	AppStorageSessionAccount,
} from '@app/features/platform/state/AppStorageBootstrap';
import type {Account} from '@app/features/platform/state/AuthSession';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {
	getFormattedClientInfo,
	getFormattedClientInfoSync,
	installFluxerConfigDebugApi,
	preloadClientInfo,
} from '@app/features/platform/utils/ClientInfo';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import {scheduleNonLatinScriptFaces} from '@app/features/theme/fonts/ScriptFontLoader';
import {installVoiceSubscriptionDebugApi} from '@app/features/voice/diagnostics/VoiceSubscriptionDebugApi';
import {PASSKEY_BRIDGE_PATH} from '@fluxer/constants/src/PasskeyConstants';
import {i18n} from '@lingui/core';
import {type ReactNode, startTransition} from 'react';
import ReactDOM from 'react-dom/client';

const logger = new Logger('index');

const SIGNED_OUT_INSTANCE_DISCOVERY_TIMEOUT_MS = 5000;

if (typeof window !== 'undefined' && window.history) {
	bootstrapSyntheticHistory();
	installScrollRestoration();
}

installFluxerConfigDebugApi();
installVoiceSubscriptionDebugApi();

function createRoot(): ReactDOM.Root {
	const container = document.getElementById('root');
	if (!container) {
		throw new Error('Missing #root element');
	}
	return ReactDOM.createRoot(container);
}

function mountRoot(content: ReactNode, dataFlxScope: string): void {
	installTranslationDomGuard();
	const root = createRoot();
	startTransition(() => {
		root.render(
			<AppErrorBoundary
				fallback={(error) => (
					<AppI18nProvider i18n={i18n}>
						<ErrorFallback error={error ?? undefined} data-flx={`${dataFlxScope}.error-fallback`} />
					</AppI18nProvider>
				)}
				data-flx={`${dataFlxScope}.app-error-boundary`}
			>
				{content}
			</AppErrorBoundary>,
		);
	});
	globalThis.window?.electron?.notifyFirstContentPainted?.();
}

async function logClientInfo(): Promise<void> {
	try {
		const info = await getFormattedClientInfo();
		logger.info(`[CLIENT INFO] ${info}`);
	} catch (error) {
		logger.warn('Failed to load full client info:', error);
		logger.info(`[CLIENT INFO] ${getFormattedClientInfoSync()}`);
	}
}

async function preloadMarkdownParser(): Promise<void> {
	try {
		const {preloadMarkdownParserWasm} = await loadLazyModule(
			() => import('@app/features/messaging/utils/markdown/parser/MarkdownParserWasm'),
		);
		await preloadMarkdownParserWasm();
	} catch (error) {
		logger.warn('Failed to preload markdown parser:', error);
	}
}

function storageSessionAccount(account: Account | null): AppStorageSessionAccount | null {
	if (account === null) {
		return null;
	}
	if (account.instance === undefined) {
		throw new Error(`Authenticated account ${account.storageKey} has no instance runtime`);
	}
	return {accountKey: account.storageKey, userId: account.userId, token: account.token, instance: account.instance};
}

async function bootstrapThemeStudio(storageBootstrap: AppStorageBootstrapHandle): Promise<void> {
	const markdownParserReady = preloadMarkdownParser();
	const [{ThemeStudioStandaloneApp}, {setupHttp}, {default: Accounts}] = await Promise.all([
		loadLazyModule(() => import('@app/features/theme_studio/ThemeStudioStandaloneApp')),
		loadLazyModule(() => import('@app/app/SetupHttp')),
		loadLazyModule(() => import('@app/features/auth/state/Accounts')),
	]);
	await Accounts.bootstrap();
	await storageBootstrap.finalizeAfterSessionResolution(storageSessionAccount(Accounts.currentAccount));
	setupHttp();
	await markdownParserReady;
	mountRoot(
		<AppI18nProvider i18n={i18n}>
			<ThemeStudioStandaloneApp data-flx="index.render-theme-studio.theme-studio-standalone-app" />
		</AppI18nProvider>,
		'index.render-theme-studio',
	);
}

export async function runPasskeyBridge(side: DomainMigrationSide): Promise<void> {
	const hash = window.location.hash;
	const opensInOwnTab = window.history.length === 1;
	window.history.replaceState(null, '', PASSKEY_BRIDGE_PATH);
	const [{PasskeyBridgePage}] = await Promise.all([
		loadLazyModule(() => import('@app/features/auth/passkey_migration/PasskeyBridgePage')),
		initI18n(),
	]);
	mountRoot(
		<AppI18nProvider i18n={i18n}>
			<PasskeyBridgePage
				side={side}
				hash={hash}
				opensInOwnTab={opensInOwnTab}
				data-flx="index.passkey-bridge.passkey-bridge-page"
			/>
		</AppI18nProvider>,
		'index.passkey-bridge',
	);
}

async function probeSignedOutDomainMigration(): Promise<void> {
	try {
		const {probeSignedOutDeviceEnrollment} = await loadLazyModule(
			() => import('@app/features/app/domain_migration/DomainMigrationTrigger'),
		);
		probeSignedOutDeviceEnrollment();
	} catch (error) {
		logger.warn('Failed to start the signed-out domain migration probe:', error);
	}
}

async function activateSignedOutDocumentInstance(): Promise<void> {
	if (isDesktopLocalAppDocument()) {
		return;
	}
	try {
		const {default: RuntimeConfig} = await loadLazyModule(() => import('@app/features/app/state/RuntimeConfig'));
		if (RuntimeConfig.getSnapshotOrNull() !== null) {
			return;
		}
		const resolution = await RuntimeConfig.resolveEndpoint({
			input: window.location.origin,
			signal: AbortSignal.timeout(SIGNED_OUT_INSTANCE_DISCOVERY_TIMEOUT_MS),
		});
		if (RuntimeConfig.getSnapshotOrNull() === null) {
			RuntimeConfig.applySnapshot(resolution.snapshot);
		}
	} catch (error) {
		logger.warn('Failed to activate the instance this page belongs to:', error);
	}
}

async function prepareSignedOutFirstScreen(): Promise<void> {
	try {
		const {prepareSignedOutFirstScreen: prepare} = await loadLazyModule(
			() => import('@app/features/auth/flow/AuthFirstScreen'),
		);
		await prepare();
	} catch (error) {
		logger.warn('Failed to prepare the signed-out first screen:', error);
	}
}

async function bootstrapApp(storageBootstrap: AppStorageBootstrapHandle): Promise<void> {
	const markdownParserReady = preloadMarkdownParser();
	const [
		{App},
		{setupHttp},
		{default: CaptchaInterceptor},
		{initializeEmojiParser},
		{registerServiceWorker},
		{default: Accounts},
		{default: ChannelDisplayName},
		_channelFrecency,
		_geoIp,
		{default: Keybind},
		{default: NewDeviceMonitoring},
		{default: Notification},
		{default: QuickSwitcher},
		_runtimeConfig,
		{default: StatusPage},
		{installMigratedDeviceRemap},
	] = await Promise.all([
		loadLazyModule(() => import(/* webpackChunkName: "boot-app" */ '@app/app/App')),
		loadLazyModule(() => import('@app/app/SetupHttp')),
		loadLazyModule(() => import('@app/features/auth/altcha/CaptchaInterceptor')),
		loadLazyModule(() => import('@app/features/messaging/utils/markdown/EmojiProviderSetup')),
		loadLazyModule(() => import('@app/features/platform/service_worker/Register')),
		loadLazyModule(() => import('@app/features/auth/state/Accounts')),
		loadLazyModule(() => import('@app/features/channel/state/ChannelDisplayName')),
		loadLazyModule(() => import('@app/features/channel/state/ChannelFrecency')),
		loadLazyModule(() => import('@app/features/app/state/GeoIP')),
		loadLazyModule(() => import('@app/features/input/state/InputKeybind')),
		loadLazyModule(() => import('@app/features/auth/state/NewDeviceMonitoring')),
		loadLazyModule(() => import('@app/features/ui/state/Notification')),
		loadLazyModule(() => import('@app/features/search/state/QuickSwitcher')),
		loadLazyModule(() => import('@app/features/app/state/RuntimeConfig')),
		loadLazyModule(() => import('@app/features/user/state/StatusPage')),
		loadLazyModule(() => import('@app/features/app/domain_migration/DomainMigrationDeviceRemap')),
	]);
	void preloadClientInfo();
	QuickSwitcher.setI18n(reactiveI18n);
	ChannelDisplayName.setI18n(reactiveI18n);
	Keybind.setI18n(reactiveI18n);
	NewDeviceMonitoring.setI18n(reactiveI18n);
	Notification.setI18n(reactiveI18n);
	CaptchaInterceptor.setI18n(reactiveI18n);
	void StatusPage.checkIncidents();
	StatusPage.startPolling();
	await Promise.all([Accounts.bootstrap(), installMigratedDeviceRemap()]);
	await storageBootstrap.finalizeAfterSessionResolution(storageSessionAccount(Accounts.currentAccount));
	if (Accounts.currentUserId === null) {
		void probeSignedOutDomainMigration();
	}
	if (Accounts.currentAccount === null) {
		await Promise.all([activateSignedOutDocumentInstance(), prepareSignedOutFirstScreen()]);
	}
	setupHttp();
	initializeEmojiParser();
	await markdownParserReady;
	mountRoot(<App data-flx="index.bootstrap.app" />, 'index.bootstrap');
	QuickSwitcher.preloadModal();
	registerServiceWorker();
}

export async function runApp(storageBootstrap: AppStorageBootstrapHandle): Promise<void> {
	scheduleNonLatinScriptFaces();
	await initI18n();
	installLocaleSwitchWatchdog();
	installSelfXssNotice();
	void logClientInfo();
	if (window.location.pathname === Routes.THEME_STUDIO) {
		await bootstrapThemeStudio(storageBootstrap);
	} else {
		await bootstrapApp(storageBootstrap);
	}
}
