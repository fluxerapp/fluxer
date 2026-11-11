// SPDX-License-Identifier: AGPL-3.0-or-later

import 'urlpattern-polyfill';
import {bootstrapSyntheticHistory} from '@app/app/HistoryBootstrap';
import reactiveI18n, {initI18n} from '@app/app/I18n';
import {AppErrorBoundary} from '@app/features/app/components/AppErrorBoundary';
import type {DomainMigrationSide} from '@app/features/app/domain_migration/DomainMigrationCore';
import {isAccountHandoffArrival} from '@app/features/auth/state/AccountHandoffCapture';
import {notifyFirstContentPainted, removeReloadCover} from '@app/features/boot_state/BootDomFrame';
import {afterBootReveal, releaseBootReveal} from '@app/features/boot_state/BootReveal';
import {startBootStateRead} from '@app/features/boot_state/BootStateBridge';
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
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import {scheduleNonLatinScriptFaces} from '@app/features/theme/fonts/ScriptFontLoader';
import {PASSKEY_BRIDGE_PATH} from '@fluxer/constants/src/PasskeyConstants';
import {i18n} from '@lingui/core';
import {Trans} from '@lingui/react/macro';
import {type ComponentType, lazy, type ReactNode, Suspense, startTransition} from 'react';
import ReactDOM from 'react-dom/client';

const logger = new Logger('index');

const SIGNED_OUT_INSTANCE_DISCOVERY_TIMEOUT_MS = 5000;

if (typeof window !== 'undefined' && window.history) {
	bootstrapSyntheticHistory();
	installScrollRestoration();
}

const ErrorFallback = lazy(() =>
	loadLazyModule(() => import('@app/features/app/components/ErrorFallback')).then(({ErrorFallback}) => ({
		default: ErrorFallback,
	})),
);

function createRoot(): ReactDOM.Root {
	const container = document.getElementById('root');
	if (!container) {
		throw new Error('Missing #root element');
	}
	return ReactDOM.createRoot(container);
}

type RootReveal = 'on-mount' | 'after-restored-paint';

function mountRoot(
	content: ReactNode,
	dataFlxScope: string,
	reveal: RootReveal = 'on-mount',
	RevealSignal?: ComponentType,
): void {
	installTranslationDomGuard();
	const root = createRoot();
	const render = (): void => {
		root.render(
			<>
				<AppErrorBoundary
					fallback={(error) => (
						<AppI18nProvider i18n={i18n}>
							<Suspense
								fallback={
									<button
										type="button"
										onClick={() => location.reload()}
										data-flx="app.app-bootstrap.mount-root.button.reload"
									>
										<Trans>Reload app</Trans>
									</button>
								}
							>
								<ErrorFallback error={error ?? undefined} data-flx={`${dataFlxScope}.error-fallback`} />
							</Suspense>
						</AppI18nProvider>
					)}
					data-flx={`${dataFlxScope}.app-error-boundary`}
				>
					{content}
				</AppErrorBoundary>
				{reveal === 'after-restored-paint' && RevealSignal !== undefined && (
					<RevealSignal data-flx="app.app-bootstrap.mount-root.reveal-signal" />
				)}
			</>,
		);
	};
	if (reveal === 'after-restored-paint') {
		render();
	} else {
		startTransition(render);
	}
	if (reveal === 'on-mount') {
		notifyFirstContentPainted('other');
		releaseBootReveal();
	}
}

async function installDiagnostics(): Promise<void> {
	try {
		const [clientInfo, {installVoiceSubscriptionDebugApi}] = await Promise.all([
			loadLazyModule(() => import('@app/features/platform/utils/ClientInfo')),
			loadLazyModule(() => import('@app/features/voice/diagnostics/VoiceSubscriptionDebugApi')),
		]);
		clientInfo.installFluxerConfigDebugApi();
		installVoiceSubscriptionDebugApi();
		try {
			logger.info(`[CLIENT INFO] ${await clientInfo.getFormattedClientInfo()}`);
		} catch (error) {
			logger.warn('Failed to load full client info:', error);
			logger.info(`[CLIENT INFO] ${clientInfo.getFormattedClientInfoSync()}`);
		}
	} catch (error) {
		logger.warn('Failed to install diagnostics:', error);
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

interface SignedInAppModules {
	readonly App: ComponentType;
	readonly RevealSignal: ComponentType;
	readonly initialize: () => void;
	readonly preloadAfterMount: () => void;
}

async function loadSignedInApp(): Promise<SignedInAppModules> {
	const [
		{App},
		{BootRevealSignal},
		{default: ChannelDisplayName},
		_channelFrecency,
		{default: Keybind},
		{default: NewDeviceMonitoring},
		{default: Notification},
		{default: QuickSwitcher},
	] = await Promise.all([
		loadLazyModule(() => import(/* webpackChunkName: "boot-app" */ '@app/app/App')),
		loadLazyModule(() => import('@app/features/boot_state/BootRevealSignal')),
		loadLazyModule(() => import('@app/features/channel/state/ChannelDisplayName')),
		loadLazyModule(() => import('@app/features/channel/state/ChannelFrecency')),
		loadLazyModule(() => import('@app/features/input/state/InputKeybind')),
		loadLazyModule(() => import('@app/features/auth/state/NewDeviceMonitoring')),
		loadLazyModule(() => import('@app/features/ui/state/Notification')),
		loadLazyModule(() => import('@app/features/search/state/QuickSwitcher')),
	]);
	return {
		App,
		RevealSignal: BootRevealSignal,
		initialize: () => {
			QuickSwitcher.setI18n(reactiveI18n);
			ChannelDisplayName.setI18n(reactiveI18n);
			Keybind.setI18n(reactiveI18n);
			NewDeviceMonitoring.setI18n(reactiveI18n);
			Notification.setI18n(reactiveI18n);
		},
		preloadAfterMount: () => QuickSwitcher.preloadModal(),
	};
}

async function loadSignedInAppAfterLogin(): Promise<ComponentType> {
	const {App, initialize, preloadAfterMount} = await loadSignedInApp();
	initialize();
	preloadAfterMount();
	return App;
}

async function bootstrapApp(storageBootstrap: AppStorageBootstrapHandle, i18nReady: Promise<void>): Promise<void> {
	const {default: SessionManager} = await loadLazyModule(() => import('@app/features/platform/state/AuthSession'));
	await SessionManager.initialize();
	const bootStateRead = startBootStateRead(SessionManager.currentAccount);
	const storageReady = storageBootstrap.finalizeAfterSessionResolution(
		storageSessionAccount(SessionManager.currentAccount),
	);
	const signedOut = SessionManager.currentAccount === null && !isAccountHandoffArrival(window.location.pathname);
	const signedInApp = signedOut
		? null
		: storageReady
				.then(() => Promise.all([loadSignedInApp(), i18nReady]))
				.then(([app]) => {
					app.initialize();
					return app;
				});
	const authRoot = signedOut ? storageReady.then(() => loadLazyModule(() => import('@app/app/AuthRoot'))) : null;
	const themeReady = storageReady.then(async () => {
		const {migrateThemeProfiles} = await loadLazyModule(
			() => import('@app/features/theme/state/ThemeProfileMigration'),
		);
		await migrateThemeProfiles(SessionManager.accounts.map((account) => account.storageKey));
	});
	const infrastructure = storageReady
		.then(() =>
			Promise.all([
				loadLazyModule(() => import('@app/app/SetupHttp')),
				loadLazyModule(() => import('@app/features/auth/altcha/CaptchaInterceptor')),
				loadLazyModule(() => import('@app/features/messaging/utils/markdown/EmojiProviderSetup')),
				loadLazyModule(() => import('@app/features/app/domain_migration/DomainMigrationDeviceRemap')),
			]),
		)
		.then(
			async ([{setupHttp}, {default: CaptchaInterceptor}, {initializeEmojiParser}, {installMigratedDeviceRemap}]) => {
				CaptchaInterceptor.setI18n(reactiveI18n);
				await installMigratedDeviceRemap();
				return {setupHttp, initializeEmojiParser};
			},
		);
	const preparedForMount = Promise.all([storageReady, i18nReady, infrastructure]).then(
		async ([, , {setupHttp, initializeEmojiParser}]) => {
			if (SessionManager.userId === null) {
				void probeSignedOutDomainMigration();
			}
			if (signedOut) {
				await Promise.all([activateSignedOutDocumentInstance(), prepareSignedOutFirstScreen()]);
			} else if (SessionManager.currentAccount === null) {
				await activateSignedOutDocumentInstance();
			}
			setupHttp();
			initializeEmojiParser();
		},
	);
	const bootStateRestored =
		signedInApp === null || bootStateRead === null
			? Promise.resolve(false)
			: storageReady
					.then(() => loadLazyModule(() => import('@app/features/boot_state/BootStateBoot')))
					.then(({restoreBootState}) => restoreBootState(bootStateRead, Promise.all([preparedForMount, signedInApp])))
					.catch((error: unknown) => {
						logger.warn('Failed to restore the boot state:', error);
						return false;
					});
	await preparedForMount;
	if (signedInApp !== null) {
		const [{App, RevealSignal, preloadAfterMount}, restored] = await Promise.all([
			signedInApp,
			bootStateRestored,
			themeReady,
		]);
		mountRoot(
			<App data-flx="index.bootstrap.app" />,
			'index.bootstrap',
			restored ? 'after-restored-paint' : 'on-mount',
			RevealSignal,
		);
		afterBootReveal(preloadAfterMount);
	} else if (authRoot !== null) {
		await themeReady;
		const {AuthRoot} = await authRoot;
		mountRoot(<AuthRoot loadApp={loadSignedInAppAfterLogin} data-flx="index.bootstrap.auth-root" />, 'index.bootstrap');
	}
	afterBootReveal(() => {
		void loadLazyModule(() => import('@app/features/platform/service_worker/Register'))
			.then(({registerServiceWorker}) => {
				registerServiceWorker();
			})
			.catch((error: unknown) => {
				logger.warn('Failed to initialize the service worker:', error);
			});
		void loadLazyModule(() => import('@app/features/app/state/GeoIP')).catch((error: unknown) => {
			logger.warn('Failed to initialize region lookup:', error);
		});
		void loadLazyModule(() => import('@app/features/user/state/StatusPage'))
			.then(({default: StatusPage}) => {
				void StatusPage.checkIncidents();
				StatusPage.startPolling();
			})
			.catch((error: unknown) => {
				logger.warn('Failed to initialize status polling:', error);
			});
	});
}

export async function runApp(storageBootstrap: AppStorageBootstrapHandle): Promise<void> {
	removeReloadCover();
	scheduleNonLatinScriptFaces();
	const i18nReady = initI18n().then(() => {
		afterBootReveal(() => {
			installLocaleSwitchWatchdog();
			installSelfXssNotice();
			void installDiagnostics();
		});
	});
	await bootstrapApp(storageBootstrap, i18nReady);
}
