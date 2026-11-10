// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {BUILD_CHANNEL} from '@electron/common/BuildChannel';
import {loadDesktopConfig} from '@electron/common/DesktopConfig';
import {WINDOWS_APP_USER_MODEL_ID, WINDOWS_TOAST_ACTIVATOR_CLSID} from '@electron/common/DesktopIdentity';
import {createChildLogger, writeLogFilesUnder} from '@electron/common/Logger';
import {resolveModulePollPlan} from '@electron/common/ModulePollPlan';
import {
	hasOfflineRenderer,
	ModuleSystemLaunchDecision,
	readBundledRendererVersion,
	resolveModuleSystemLaunch,
} from '@electron/common/ModuleSystem';
import {configureUserDataPath} from '@electron/common/UserDataPath';
import {isStartMinimizedLaunch} from '@electron/main/AutostartLaunch';
import {decryptBootStatePrewarm, prewarmBootState, whenBootStatePrewarmed} from '@electron/main/BootState';
import {appendWindowsGpuDriverWorkaroundSwitches} from '@electron/main/ChromiumRuntime';
import {isUpdateOnDemandLaunch} from '@electron/main/DesktopDebugInfo';
import {prewarmSystemCertificateStore} from '@electron/main/DesktopTrustedCertificates';
import {armDesktopUpdate, checkDesktopUpdateNow} from '@electron/main/DesktopUpdateGate';
import {armDesktopUpdatePoll, wakeDesktopUpdatePoll} from '@electron/main/DesktopUpdatePoll';
import {DesktopUpdateRun, type DesktopUpdateTakeover} from '@electron/main/DesktopUpdateRun';
import {isDesktopUpdateRequested, shouldDisableModuleUpdatesForLaunch} from '@electron/main/LaunchOptions';
import {relaunchStableLaunchPath} from '@electron/main/LinuxLaunchPath';
import {getDesktopLocalAppProtocol} from '@electron/main/LocalAppProtocol';
import {LocalNetworkSplashHint} from '@electron/main/LocalNetworkSplashHint';
import {MAIN_WINDOW_LIVE_FRAME_WAIT_MS} from '@electron/main/MainWindowReveal';
import {
	armOpenUrlForwarding,
	armSecondInstanceForwarding,
	getMainWindowFactory,
	hasQuitStarted,
	markModuleLaunchGranted,
	observeRendererLaunchConfirmed,
	onMainWindowCreated,
	onMainWindowReady,
	setCommittedModuleFiles,
	setOnDemandModuleInstaller,
	setSecondInstanceSink,
	settleStartupWindow,
} from '@electron/main/ModuleBootHandoff';
import {moduleNetworkFetch} from '@electron/main/ModuleNetworkFetch';
import {createOnDemandModuleInstaller} from '@electron/main/ModuleOnDemand';
import {getModuleStoreRoot, type ModuleLaunchAttempt, ModuleStore} from '@electron/main/ModuleStore';
import {
	ModuleUpdater,
	type ModuleUpdaterOutcome,
	type ModuleUpdaterSplashState,
	ModuleUpdaterStatus,
} from '@electron/main/ModuleUpdater';
import {armNativeProbeCache} from '@electron/main/NativeProbeCache';
import {applyPreReadyChromiumConfiguration} from '@electron/main/PreReadyChromium';
import {resolveDesktopPackageOrigin} from '@electron/main/ShellDownloadFormats';
import {resolveShellUpdatePlan, ShellUpdateCapability} from '@electron/main/ShellUpdateCapability';
import {armBlockedShellUpdate} from '@electron/main/ShellUpdateSplash';
import {buildSplashDiagnosticsText} from '@electron/main/SplashDiagnostics';
import {
	closeSplashWindow,
	focusSplashWindow,
	onSplashCopyDiagnostics,
	onSplashNetworkOnline,
	onSplashOpenLogs,
	onSplashQuit,
	onSplashRetry,
	openSplashWindow,
	releaseSplashAffordance,
	SplashAction,
	type SplashState,
	type SplashStatus,
	setSplashState,
} from '@electron/main/SplashWindow';
import {UpdateServerRetry} from '@electron/main/UpdateServerRetry';
import {createWindow, getMainWindow} from '@electron/main/Window';
import {repairWindowsShortcuts} from '@electron/main/WindowsShortcuts';
import {app, clipboard, session, shell} from 'electron';

declare const __FLUXER_MAIN_APP_OUTPUT_FILE__: string;

const requireModule = createRequire(import.meta.url);

if (process.platform === 'win32') {
	const {VelopackApp} = requireModule('velopack') as typeof import('velopack');
	VelopackApp.build()
		.onAfterInstallFastCallback(() => {
			repairWindowsShortcuts();
		})
		.onAfterUpdateFastCallback(() => {
			repairWindowsShortcuts();
		})
		.onFirstRun(() => {
			repairWindowsShortcuts();
		})
		.onRestarted(() => {
			repairWindowsShortcuts();
		})
		.run();
	repairWindowsShortcuts();
	app.setToastActivatorCLSID(WINDOWS_TOAST_ACTIVATOR_CLSID);
	app.setAppUserModelId(WINDOWS_APP_USER_MODEL_ID);
}

armOpenUrlForwarding();

const MAIN_PROCESS_ENTRY_URL = new URL(`./${__FLUXER_MAIN_APP_OUTPUT_FILE__}`, import.meta.url).href;

const DEV_MODULE_POLL_INTERVAL_ENV = 'FLUXER_DEV_MODULE_POLL_MS';
function readDevModulePollInterval(): number | null {
	const raw = Number.parseInt(process.env[DEV_MODULE_POLL_INTERVAL_ENV] ?? '', 10);
	return Number.isSafeInteger(raw) && raw > 0 ? raw : null;
}

const HAS_OFFLINE_RENDERER = hasOfflineRenderer(import.meta.url);
const BUNDLED_RENDERER_VERSION = HAS_OFFLINE_RENDERER ? readBundledRendererVersion(import.meta.url) : null;

const CAN_TURN_MODULE_SYSTEM_OFF = HAS_OFFLINE_RENDERER && BUILD_CHANNEL === 'development';

const MODULE_SYSTEM_LAUNCH = resolveModuleSystemLaunch({hasOfflineRenderer: CAN_TURN_MODULE_SYSTEM_OFF});

const MODULE_LAUNCH_PERMIT = Symbol('fluxer.desktop.moduleLaunchPermit');

const IN_PLACE_RELOAD_CONFIRM_TIMEOUT_MS = 45_000;
const NETWORK_SPLASH_GRACE_MS = 500;

const UPDATE_SERVER_UNREACHABLE_MESSAGE =
	"Fluxer couldn't reach its update server to download app files. Check your internet connection, proxy or firewall. Fluxer keeps retrying.";

interface ModuleLaunchPermit {
	readonly [MODULE_LAUNCH_PERMIT]: true;
	readonly unreachable: boolean;
	readonly rolledBack: boolean;
	readonly belowFloor: ReadonlyArray<string>;
	readonly launchAttempt: ModuleLaunchAttempt;
}

type BootstrapLogger = ReturnType<typeof createChildLogger>;

const SPLASH_STATUS_BY_UPDATER_STATUS: Readonly<Record<ModuleUpdaterStatus, SplashStatus>> = Object.freeze({
	checking: 'checking-for-updates',
	downloading: 'downloading-updates',
	installing: 'installing-updates',
	verifying: 'verifying',
	'retry-wait': 'update-failure',
	'blocked-update-required': 'blocked-update-required',
	'blocked-shell-update': 'blocked-shell-update',
	launching: 'launching',
	'unreachable-launch': 'unreachable-launch',
});

function toSplashState(state: ModuleUpdaterSplashState): SplashState {
	return {
		status: state.stalled ? 'download-stalled' : SPLASH_STATUS_BY_UPDATER_STATUS[state.status],
		current: state.current,
		total: state.total,
		progress: state.progress,
		seconds: state.seconds,
		receivedBytes: state.receivedBytes,
		totalBytes: state.totalBytes,
		bytesPerSecond: state.bytesPerSecond,
		message: state.detail,
	};
}

function unhandledModuleUpdaterOutcome(outcome: never): never {
	throw new Error(`Unhandled module updater outcome: ${JSON.stringify(outcome)}`);
}

function reportModuleLaunch(permit: ModuleLaunchPermit, logger: BootstrapLogger): void {
	logger.info('Module convergence granted a launch permit', {
		unreachable: permit.unreachable,
		rolledBack: permit.rolledBack,
		belowFloor: permit.belowFloor,
	});
	if (permit.belowFloor.length > 0) {
		logger.warn(
			'Launching a module set the persisted floor forbids, because the rollback path deliberately outranks the forced update',
			{modules: permit.belowFloor},
		);
	}
}

async function runBootstrap(): Promise<void> {
	const logger = createChildLogger('Bootstrap');
	const userDataConfig = configureProcessStorage();
	if (!app.requestSingleInstanceLock()) {
		logger.info('Another instance already holds the single instance lock, forwarding this launch and quitting');
		app.quit();
		return;
	}
	armSecondInstanceForwarding();
	prewarmBootState(userDataConfig.base);
	loadDesktopConfig(userDataConfig.base);
	applyPreReadyChromiumConfiguration(userDataConfig.channel, process.argv);
	getDesktopLocalAppProtocol().registerSchemes();
	const modulesEnabled =
		MODULE_SYSTEM_LAUNCH.kind === ModuleSystemLaunchDecision.ENABLED ||
		MODULE_SYSTEM_LAUNCH.kind === ModuleSystemLaunchDecision.ENABLED_WITH_IGNORED_DISABLE_REQUEST;
	const storeOpening = modulesEnabled
		? ModuleStore.open({
				root: getModuleStoreRoot(userDataConfig.base),
				shellVersion: app.getVersion(),
				releaseChannel: BUILD_CHANNEL,
			})
		: null;
	void storeOpening?.catch(() => undefined);
	const mainApp: typeof import('@electron/main/index') = await import(MAIN_PROCESS_ENTRY_URL);
	if (!mainApp.prepareMainProcess() || hasQuitStarted()) return;
	if (!modulesEnabled && !HAS_OFFLINE_RENDERER) {
		await refuseUnsupportedBuild('the module system is turned off and this build has no offline renderer');
		return;
	}
	let launchBlocked = false;
	const moduleReady =
		storeOpening === null
			? Promise.resolve()
			: runModuleBootstrap(userDataConfig, storeOpening, () => {
					launchBlocked = true;
					getMainWindow()?.destroy();
				});
	let resolveStorageReady!: (accountKey: Promise<string | null>) => void;
	const storageReady = new Promise<string | null>((resolve) => {
		resolveStorageReady = resolve;
	});
	const bootStateReady = whenBootStatePrewarmed(null);
	const localProtocol = getDesktopLocalAppProtocol();
	const windowCreated = new Promise<void>((resolve) => onMainWindowCreated(() => resolve()));
	const frameAdmission = localProtocol
		.prepareBootFrame(moduleReady, async (format) => {
			await Promise.all([bootStateReady, storageReady, windowCreated]);
			void mainApp.prepareStartupBootFrame(format, storageReady).catch((error: unknown) => {
				if (!hasQuitStarted()) logger.warn('Failed to prepare the native boot frame', {error});
			});
		})
		.catch((error: unknown) => {
			if (!hasQuitStarted()) logger.warn('Failed to admit the served renderer boot frame', {error});
		});
	await appendWindowsGpuDriverWorkaroundSwitches();
	await app.whenReady();
	const whenReadyMs = process.uptime() * 1000;
	if (hasQuitStarted()) return;
	mainApp.initializeMainWindowStartup();
	decryptBootStatePrewarm();
	prewarmSystemCertificateStore();

	let resolveRendererReady!: (bootSession: boolean | null) => void;
	let rejectRendererReady!: (error: unknown) => void;
	const rendererReadiness = new Promise<boolean | null>((resolve, reject) => {
		resolveRendererReady = resolve;
		rejectRendererReady = reject;
	});
	void rendererReadiness.catch(() => undefined);
	let startupIndexServed = false;
	localProtocol.register({
		rendererReadiness,
		bootStatePrewarmed: () => {
			if (startupIndexServed) return whenBootStatePrewarmed();
			startupIndexServed = true;
			return bootStateReady;
		},
		admitBootFrame: mainApp.admitStartupBootFrame,
		readBootSession: mainApp.readBootSession,
	});
	const createStartupWindow = (): void => {
		if (hasQuitStarted() || launchBlocked || getMainWindow() !== null) return;
		const startedAt = Date.now();
		try {
			createWindow({
				startHidden: isStartMinimizedLaunch(),
				rendererReadiness,
				liveFrameDeadlineMs: whenReadyMs + MAIN_WINDOW_LIVE_FRAME_WAIT_MS,
			});
		} finally {
			logger.info('[Startup] Phase completed', {phase: 'create-window', durationMs: Date.now() - startedAt});
		}
	};
	let initialWindowDispatched = false;
	void moduleReady
		.then(() => {
			launchBlocked = false;
			if (initialWindowDispatched) createStartupWindow();
		})
		.catch(rejectRendererReady);
	if (hasQuitStarted()) return;
	initialWindowDispatched = true;
	createStartupWindow();
	resolveStorageReady(mainApp.openStartupAppStorage());
	void mainApp.startMainApp(moduleReady, storageReady, frameAdmission).then(
		(bootSession) => {
			if (hasQuitStarted()) return;
			createStartupWindow();
			resolveRendererReady(bootSession);
			settleStartupWindow();
			void mainApp.finishMainAppStartup().catch((error: unknown) => {
				if (!hasQuitStarted()) logger.error('Deferred main initialization failed', error);
			});
		},
		(error: unknown) => {
			rejectRendererReady(error);
			if (hasQuitStarted()) return;
			logger.error('Failed to initialize the main app', error);
			getMainWindow()?.destroy();
			app.quit();
		},
	);
}

async function refuseUnsupportedBuild(reason: string): Promise<never> {
	const logger = createChildLogger('Bootstrap');
	logger.error('This build cannot start a renderer, refusing to launch', {reason});
	process.stderr.write(`Fluxer cannot start: ${reason}\n`);
	await app.whenReady();
	openSplashWindow();
	setSplashState({status: 'blocked-unsupported-build', action: SplashAction.QUIT});
	onSplashQuit(() => {
		app.quit();
	});
	return new Promise<never>(() => {});
}

function configureProcessStorage(): ReturnType<typeof configureUserDataPath> {
	const userDataConfig = configureUserDataPath();
	if (userDataConfig.portable) {
		writeLogFilesUnder(app.getPath('logs'));
	}
	armNativeProbeCache(userDataConfig.base);
	return userDataConfig;
}

async function runModuleBootstrap(
	userDataConfig: ReturnType<typeof configureUserDataPath>,
	storeOpening: Promise<ModuleStore>,
	onLaunchBlocked: () => void,
): Promise<void> {
	const logger = createChildLogger('Bootstrap');
	if (MODULE_SYSTEM_LAUNCH.kind === ModuleSystemLaunchDecision.ENABLED_WITH_IGNORED_DISABLE_REQUEST) {
		logger.warn(
			'Ignoring the --fluxer-no-module-system override, only a development build with a bundled renderer may turn the module system off',
		);
	}

	const startMinimized = isStartMinimizedLaunch();
	let networkSplashTimer: NodeJS.Timeout | null = null;
	let launchGranted = false;
	let splashOpenedAt: number | null = null;
	const openLaunchSplash = (): void => {
		splashOpenedAt ??= Date.now();
		if (app.isReady()) {
			if (!hasQuitStarted()) openSplashWindow();
		} else {
			void app.whenReady().then(() => {
				if (!hasQuitStarted() && !launchGranted) openSplashWindow();
			});
		}
	};
	const armNetworkSplash = (): void => {
		if (hasQuitStarted() || startMinimized || launchGranted || networkSplashTimer != null) return;
		networkSplashTimer = setTimeout(() => {
			networkSplashTimer = null;
			if (hasQuitStarted() || launchGranted) return;
			logger.info('The launch is waiting on the update server, opening the splash');
			openLaunchSplash();
		}, NETWORK_SPLASH_GRACE_MS);
	};
	const disarmNetworkSplash = (): void => {
		if (networkSplashTimer != null) {
			clearTimeout(networkSplashTimer);
			networkSplashTimer = null;
		}
	};

	const updateServerRetry = new UpdateServerRetry();

	const shellUpdatePlan = resolveShellUpdatePlan();

	onMainWindowReady(() => {
		void (async () => {
			if (shellUpdatePlan.capability === ShellUpdateCapability.SELF_UPDATE && shellUpdatePlan.updater === 'appimage') {
				const {sweepAbandonedAppImageUpdates} = await import('@electron/main/AppImageUpdate');
				try {
					const reclaimed = sweepAbandonedAppImageUpdates(shellUpdatePlan.target);
					if (reclaimed.length > 0) {
						logger.info('Reclaimed abandoned AppImage staging directories', {count: reclaimed.length});
					}
				} catch (error) {
					logger.warn('Failed to reclaim abandoned AppImage staging directories', error);
				}
			}
		})().catch((error: unknown) => logger.warn('Failed to reclaim abandoned AppImage updates', error));
	});

	let diagnosticsSource: {
		readonly store: ModuleStore;
		readonly updater: ModuleUpdater;
	} | null = null;
	const resolveLogsPath = (): string | null => {
		try {
			return app.getPath('logs');
		} catch {
			return null;
		}
	};
	const collectSplashDiagnostics = async (): Promise<string> => {
		const updaterDiagnostics = diagnosticsSource?.updater.diagnostics() ?? null;
		const packageOrigin = updaterDiagnostics?.packageOrigin ?? resolveDesktopPackageOrigin();
		let proxyRoute: string | null = null;
		try {
			proxyRoute = await session.defaultSession.resolveProxy(packageOrigin);
		} catch (error) {
			logger.warn('Failed to resolve the proxy route for diagnostics', error);
		}
		const state = updaterDiagnostics?.state ?? null;
		return buildSplashDiagnosticsText({
			generatedAt: Date.now(),
			splashOpenedAt,
			appVersion: app.getVersion(),
			channel: BUILD_CHANNEL,
			platform: process.platform,
			arch: process.arch,
			osVersion: process.getSystemVersion(),
			electronVersion: process.versions.electron ?? 'unknown',
			logsPath: resolveLogsPath(),
			userDataPath: userDataConfig.base,
			packageOrigin,
			proxyRoute,
			splashStatus: state == null ? null : (toSplashState(state).status ?? null),
			updaterStatus: state?.status ?? null,
			pendingModule: state?.moduleName ?? null,
			receivedBytes: state?.receivedBytes ?? null,
			totalBytes: state?.totalBytes ?? null,
			bytesPerSecond: state?.bytesPerSecond ?? null,
			committed: diagnosticsSource?.store.getCommitted() ?? {},
			lastError: updaterDiagnostics?.lastError ?? null,
			lastErrorAt: updaterDiagnostics?.lastErrorAt ?? null,
		});
	};
	const armSplashActions = (): void => {
		onSplashQuit(() => {
			logger.info('The splash requested a quit');
			app.quit();
		});
		onSplashRetry(() => {
			logger.info('The splash requested a retry, relaunching');
			relaunchStableLaunchPath();
			app.exit(0);
		});
		onSplashNetworkOnline(() => {
			logger.info('The splash reported the network is back');
			updateServerRetry.networkReturned();
		});
		onSplashOpenLogs(() => {
			const logsPath = resolveLogsPath();
			if (logsPath == null) return;
			void shell.openPath(logsPath).then((failure) => {
				if (failure.length > 0) {
					logger.warn('Failed to open the logs folder from the splash', {failure});
				}
			});
		});
		onSplashCopyDiagnostics(() => {
			void collectSplashDiagnostics()
				.then((text) => {
					clipboard.writeText(text);
					logger.info('Copied updater diagnostics from the splash');
				})
				.catch((error: unknown) => {
					logger.error('Failed to copy updater diagnostics', error);
				});
		});
	};
	armSplashActions();

	const runShellSelfUpdateOnSplash = async (): Promise<{readonly reason: string; readonly detail: string | null}> => {
		if (shellUpdatePlan.capability !== ShellUpdateCapability.SELF_UPDATE) {
			return {reason: 'unsupported', detail: null};
		}
		const {runShellSelfUpdate} = await import('@electron/main/ShellSelfUpdate');
		return await runShellSelfUpdate(shellUpdatePlan, {
			onDownloading: (progress) => {
				setSplashState({status: 'shell-update-downloading', progress});
			},
			onRestarting: () => {
				setSplashState({status: 'shell-update-restarting'});
			},
		});
	};

	const showBlockedSplash = (status: SplashStatus, message: string | null = null): void => {
		disarmNetworkSplash();
		onLaunchBlocked();
		openLaunchSplash();
		setSplashState({status, action: SplashAction.RETRY, message});
		setSecondInstanceSink(focusSplashWindow);
	};
	const blockUntilRelaunch = (status: SplashStatus, message: string | null = null): Promise<never> => {
		showBlockedSplash(status, message);
		return new Promise<never>(() => {});
	};

	try {
		let store: ModuleStore;
		try {
			store = await storeOpening;
		} catch (error) {
			logger.error('Failed to open the module store', error);
			await blockUntilRelaunch('blocked-update-required');
			return;
		}

		if (hasQuitStarted()) return;
		const localNetworkHint = new LocalNetworkSplashHint({
			apiBaseUrl: resolveDesktopPackageOrigin(),
			platform: process.platform,
			onHint: () => {
				logger.info('The module manifest host is on the local network and the fetch is still pending');
				setSplashState({status: 'waiting-local-network'});
			},
		});

		if (store.recoveredUnreadableState != null) {
			logger.warn('The module state was unreadable, set it aside and started from a clean state', {
				setAside: store.recoveredUnreadableState,
			});
		}
		const moduleUpdatesForcedOff = shouldDisableModuleUpdatesForLaunch();
		const updateOnDemand = isUpdateOnDemandLaunch();
		const updateWasRequested = isDesktopUpdateRequested();
		if (moduleUpdatesForcedOff) {
			logger.info('Module updates are disabled for this launch');
		} else if (updateWasRequested) {
			logger.info('The user asked for an update, installing it before launch');
		} else if (updateOnDemand) {
			logger.info('Updates install on demand, launching the installed modules');
		}
		const updater = new ModuleUpdater({
			store,
			shellVersion: app.getVersion(),
			hasOfflineRenderer: HAS_OFFLINE_RENDERER,
			bundledRendererVersion: BUNDLED_RENDERER_VERSION,
			preferUnversionedBundle: app.isPackaged,
			startupUpdateRequested: updateWasRequested,
			moduleUpdatesDisabled: moduleUpdatesForcedOff,
			updateOnDemand,
			selfUpdateShellFirst:
				shellUpdatePlan.capability === ShellUpdateCapability.SELF_UPDATE
					? async (latestVersion) => {
							logger.info('A newer shell is available, updating it before the modules', {latestVersion});
							const result = await runShellSelfUpdateOnSplash();
							logger.warn('The shell self update did not restart the app, converging the modules for this shell', {
								latestVersion,
								...result,
							});
							return result;
						}
					: undefined,
			fetch: async (input, init) => {
				await app.whenReady();
				return moduleNetworkFetch(input, init);
			},
			sleep: updateServerRetry.sleep,
			onState: (state) => {
				if (hasQuitStarted()) return;
				if (state.status !== ModuleUpdaterStatus.LAUNCHING && state.status !== ModuleUpdaterStatus.UNREACHABLE_LAUNCH) {
					armNetworkSplash();
				}
				localNetworkHint.armWhileChecking(state.status === 'checking');
				if (updateServerRetry.holding) {
					if (!updateServerRetry.endsHold(state.status)) {
						return;
					}
					releaseSplashAffordance();
				}
				setSplashState(toSplashState(state));
			},
			report: (report) => {
				logger.warn('Module updater report', {
					type: report.type,
					module: report.module,
					message: report.message,
					sampleRate: report.sampleRate,
					error: report.error,
				});
			},
		});
		diagnosticsSource = {store, updater};

		const runModuleUpdateAttempt = async (): Promise<ModuleLaunchPermit | null> => {
			let outcome: ModuleUpdaterOutcome;
			try {
				outcome = await updater.launchLocal();
			} catch (error) {
				logger.error('The module updater failed', error);
				return await blockUntilRelaunch('blocked-update-required');
			}
			switch (outcome.status) {
				case 'launching':
					return {
						[MODULE_LAUNCH_PERMIT]: true,
						unreachable: false,
						rolledBack: outcome.rolledBack,
						belowFloor: outcome.belowFloor,
						launchAttempt: outcome.launchAttempt,
					};
				case 'unreachable-launch':
					return {
						[MODULE_LAUNCH_PERMIT]: true,
						unreachable: true,
						rolledBack: false,
						belowFloor: [],
						launchAttempt: outcome.launchAttempt,
					};
				case 'blocked-shell-update': {
					logger.warn('A shell update is required before the modules can converge', {
						latestVersion: outcome.latestVersion,
						minimumVersion: outcome.minimumVersion,
						capability: shellUpdatePlan.capability,
					});
					disarmNetworkSplash();
					onLaunchBlocked();
					openLaunchSplash();
					armBlockedShellUpdate();
					setSecondInstanceSink(focusSplashWindow);
					return await new Promise<never>(() => {});
				}
				case 'blocked-update-required':
					logger.warn('The module update is blocked', {
						reason: outcome.reason,
						modules: outcome.modules,
						message: outcome.message,
						updateServerUnreachable: outcome.updateServerUnreachable,
					});
					if (outcome.updateServerUnreachable) {
						showBlockedSplash('blocked-update-unreachable', UPDATE_SERVER_UNREACHABLE_MESSAGE);
						await updateServerRetry.holdUntilNextAttempt();
						logger.info('Retrying the update server');
						return null;
					}
					return await blockUntilRelaunch('blocked-update-required');
				default:
					return unhandledModuleUpdaterOutcome(outcome);
			}
		};

		const runModuleUpdateLoop = async (): Promise<ModuleLaunchPermit> => {
			for (;;) {
				if (hasQuitStarted()) return await new Promise<never>(() => {});
				const permit = await runModuleUpdateAttempt();
				if (permit !== null) {
					return permit;
				}
			}
		};

		let servedModules: Readonly<Record<string, string>> = {};
		let servedModulesUpdate: Promise<void> = Promise.resolve();
		const serveModules = (resolveModules: () => Readonly<Record<string, string>>): Promise<void> => {
			const update = async (): Promise<void> => {
				const selection = await updater.selectServedModules(resolveModules());
				setCommittedModuleFiles(store.storeRoot, await store.buildModuleIndex(selection.modules));
				servedModules = selection.modules;
				logger.info(
					`Serving the renderer: source=${selection.renderer.source} version=${selection.renderer.version ?? 'unknown'} bundled=${selection.renderer.bundledVersion ?? 'none'}`,
				);
			};
			const started = servedModulesUpdate.then(update, update);
			servedModulesUpdate = started.catch(() => undefined);
			return started;
		};
		const refreshModuleRoots = (modules: Readonly<Record<string, string>>): Promise<void> =>
			serveModules(() => modules);
		const serveOnDemandModules = (): Promise<void> =>
			serveModules(() => {
				const added = Object.entries(store.getCommitted()).filter(([name]) => servedModules[name] === undefined);
				return {...servedModules, ...Object.fromEntries(added)};
			});

		const armMainWindowHandoff = (
			updaterInstance: ModuleUpdater,
			launchAttempt: ModuleLaunchAttempt,
			onLaunchSettled: () => void,
		): void => {
			onMainWindowReady(() => {
				closeSplashWindow();
			});
			const stopObservingLaunch = observeRendererLaunchConfirmed(() => {
				stopObservingLaunch();
				void updaterInstance
					.markLaunchSucceeded(launchAttempt)
					.catch((error: unknown) => {
						logger.error('Failed to record a successful module launch', error);
					})
					.finally(onLaunchSettled);
			});
			onMainWindowCreated((window) => {
				if (window == null || window.isDestroyed()) {
					closeSplashWindow();
					return;
				}
				const closeSplashWithWindow = (): void => {
					closeSplashWindow();
				};
				window.once('closed', closeSplashWithWindow);
				onMainWindowReady(() => {
					window.removeListener('closed', closeSplashWithWindow);
				});
			});
		};

		const reopenMainWindow = (): void => {
			const createMainWindow = getMainWindowFactory();
			if (createMainWindow == null) {
				logger.error('No main window factory is registered, relaunching to recover');
				relaunchStableLaunchPath();
				app.exit(0);
				return;
			}
			createMainWindow();
		};

		const updateTakeover: DesktopUpdateTakeover = {
			reloadInPlace: async (launchAttempt) => {
				const windows = await import('@electron/main/Window');
				let settled = false;
				const stopObservingLaunch = observeRendererLaunchConfirmed(() => {
					if (settled) return;
					settled = true;
					stopObservingLaunch();
					clearTimeout(watchdog);
					void updater.markLaunchSucceeded(launchAttempt).catch((error: unknown) => {
						logger.error('Failed to record a successful in place module update', error);
					});
				});
				const watchdog = setTimeout(() => {
					if (settled) return;
					settled = true;
					stopObservingLaunch();
					void (async () => {
						const reverted = await updater.revertLaunch(launchAttempt);
						if (reverted == null) return;
						logger.error('The updated renderer never confirmed it started, going back to the one before it', {
							timeoutMs: IN_PLACE_RELOAD_CONFIRM_TIMEOUT_MS,
						});
						await refreshModuleRoots(reverted);
						if (!(await windows.reloadMainWindowForUpdate())) {
							reopenMainWindow();
						}
					})().catch((error: unknown) => {
						logger.error('Failed to go back to the renderer before the update', error);
					});
				}, IN_PLACE_RELOAD_CONFIRM_TIMEOUT_MS);
				watchdog.unref();
				await refreshModuleRoots(launchAttempt.committed);
				const reloaded = await windows.reloadMainWindowForUpdate();
				logger.info('Reloaded the main window onto the updated renderer', {reloaded});
				if (!reloaded) {
					reopenMainWindow();
				}
			},
		};

		const desktopUpdate = moduleUpdatesForcedOff
			? null
			: new DesktopUpdateRun({
					probe: () => updater.checkForUpdate(),
					stageModules: () => updater.stageModules(),
					activateStagedModules: () => updater.activateStagedForReload(),
					shellUpdatePlan,
					takeover: updateTakeover,
					reportFailure: (failure) => {
						void import('@electron/main/DesktopUpdatePrompt')
							.then(({reportDesktopUpdateFailure}) => reportDesktopUpdateFailure(failure))
							.catch((error: unknown) => {
								logger.error('Failed to report the desktop update failure', error);
							});
					},
					openDownloadsPage: async () => {
						const {DOWNLOAD_PAGE_URL} = await import('@electron/main/UpdaterDownloads');
						await shell.openExternal(DOWNLOAD_PAGE_URL);
					},
					logger,
				});

		const permit = await runModuleUpdateLoop();
		if (hasQuitStarted()) return;
		launchGranted = true;
		disarmNetworkSplash();
		markModuleLaunchGranted();
		localNetworkHint.disarm();
		await refreshModuleRoots(permit.launchAttempt.committed);
		if (hasQuitStarted()) return;
		if (desktopUpdate != null) {
			setOnDemandModuleInstaller(
				createOnDemandModuleInstaller({
					ensure: (moduleName) => updater.ensureModule(moduleName),
					refresh: serveOnDemandModules,
					onFailure: (moduleName, error) => {
						logger.error('Failed to install a module on demand', {module: moduleName, error});
					},
				}),
			);
			armDesktopUpdate({
				check: (manual) => desktopUpdate.check({manual}),
				start: () => desktopUpdate.start(),
			});
		}
		armMainWindowHandoff(updater, permit.launchAttempt, () => {
			if (desktopUpdate == null) {
				return;
			}
			desktopUpdate.markLaunchSettled();
			armDesktopUpdatePoll({
				plan: resolveModulePollPlan(readDevModulePollInterval()),
				check: () => checkDesktopUpdateNow(),
				logger,
			});
			observeRendererLaunchConfirmed(wakeDesktopUpdatePoll);
		});
		app.once('before-quit', () => {
			closeSplashWindow();
		});
		if (hasQuitStarted()) {
			logger.info('The app started quitting during module convergence, not launching the main app');
			return;
		}
		reportModuleLaunch(permit, logger);
	} catch (error) {
		logger.error('The module bootstrap failed unexpectedly', error);
		await blockUntilRelaunch('blocked-update-required');
	}
}

void runBootstrap().catch((error: unknown) => {
	process.stderr.write(`The module bootstrap rejected before it could report: ${String(error)}\n`);
	process.exit(1);
});
