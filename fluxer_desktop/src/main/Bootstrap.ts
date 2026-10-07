// SPDX-License-Identifier: AGPL-3.0-or-later

import {createRequire} from 'node:module';
import {WINDOWS_APP_USER_MODEL_ID, WINDOWS_TOAST_ACTIVATOR_CLSID} from '@electron/common/DesktopIdentity';
import {type ModulePollPlan, nextModulePollDelay, resolveModulePollPlan} from '@electron/common/ModulePollPlan';
import {
	decideModuleSystemDisableRequest,
	hasOfflineRenderer,
	ModuleSystemDisableDecision,
	ModuleSystemLaunchDecision,
	resolveModuleSystemLaunch,
} from '@electron/common/ModuleSystem';
import {armOpenUrlForwarding, observeRendererLaunchConfirmed} from '@electron/main/ModuleBootHandoff';
import {watchModuleRendererReload} from '@electron/main/ModuleRendererReload';
import type {ModuleLaunchAttempt, ModuleStore as ModuleStoreInstance} from '@electron/main/ModuleStore';
import {offerPendingModuleUpdate} from '@electron/main/ModuleUpdateGate';
import type {
	ModuleUpdater as ModuleUpdaterInstance,
	ModuleUpdaterOutcome,
	ModuleUpdaterSplashState,
	ModuleUpdaterStatus,
} from '@electron/main/ModuleUpdater';
import type {SplashState, SplashStatus} from '@electron/main/SplashWindow';
import {repairWindowsShortcuts} from '@electron/main/WindowsShortcuts';
import {type BrowserWindow, app as electronApp} from 'electron';

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
	electronApp.setToastActivatorCLSID(WINDOWS_TOAST_ACTIVATOR_CLSID);
	electronApp.setAppUserModelId(WINDOWS_APP_USER_MODEL_ID);
}

armOpenUrlForwarding();

const MAIN_PROCESS_ENTRY_URL = new URL(`./${__FLUXER_MAIN_APP_OUTPUT_FILE__}`, import.meta.url).href;

const DEV_MODULE_POLL_INTERVAL_ENV = 'FLUXER_DEV_MODULE_POLL_MS';
function readDevModulePollInterval(): number | null {
	const raw = Number.parseInt(process.env[DEV_MODULE_POLL_INTERVAL_ENV] ?? '', 10);
	return Number.isSafeInteger(raw) && raw > 0 ? raw : null;
}

const HAS_OFFLINE_RENDERER = hasOfflineRenderer(import.meta.url);

const MODULE_SYSTEM_LAUNCH = resolveModuleSystemLaunch({hasOfflineRenderer: HAS_OFFLINE_RENDERER});

const MODULE_LAUNCH_PERMIT = Symbol('fluxer.desktop.moduleLaunchPermit');

const UPDATE_SERVER_UNREACHABLE_MESSAGE =
	"Fluxer couldn't reach its update server to download app files. Check your internet connection, proxy or firewall. Fluxer keeps retrying.";

interface ModuleLaunchPermit {
	readonly [MODULE_LAUNCH_PERMIT]: true;
	readonly unreachable: boolean;
	readonly rolledBack: boolean;
	readonly belowFloor: ReadonlyArray<string>;
	readonly launchAttempt: ModuleLaunchAttempt;
}

type BootstrapLogger = typeof import('@electron/common/Logger').Logger;

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
		status: SPLASH_STATUS_BY_UPDATER_STATUS[state.status],
		requiredSecurityUpdate: state.requiredSecurityUpdate,
		current: state.current,
		total: state.total,
		progress: state.progress,
		seconds: state.seconds,
		message: state.detail,
	};
}

function unhandledModuleUpdaterOutcome(outcome: never): never {
	throw new Error(`Unhandled module updater outcome: ${JSON.stringify(outcome)}`);
}

async function launchMainApp(permit: ModuleLaunchPermit, logger: BootstrapLogger): Promise<void> {
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
	await import(MAIN_PROCESS_ENTRY_URL);
}

function armModulePoll(
	plan: ModulePollPlan,
	updater: ModuleUpdaterInstance,
	refreshModuleRoots: (modules: Readonly<Record<string, string>>) => Promise<void>,
	logger: BootstrapLogger,
): void {
	let mainWindow: BrowserWindow | null = null;
	let inFlight = false;
	let failing = false;
	void import('@electron/main/ModuleBootHandoff').then(({observeMainWindow, onMainWindowReady}) => {
		observeMainWindow((window) => {
			mainWindow = window;
		});
		onMainWindowReady(() => {
			logger.info('Polling the module manifest', {
				intervalMs: plan.intervalMs,
				jitterRatio: plan.jitterRatio,
			});
			const schedule = (): void => {
				const timer = setTimeout(tick, nextModulePollDelay(plan));
				timer.unref();
			};
			const tick = (): void => {
				const window = mainWindow;
				if (inFlight || window == null || window.isDestroyed()) {
					schedule();
					return;
				}
				inFlight = true;
				void updater
					.refreshInstalledModules()
					.then(async (refresh) => {
						if (failing) {
							failing = false;
							logger.info('The module poll recovered');
						}
						if (refresh.status !== 'activated') {
							return;
						}
						logger.info('A new module set landed, offering it to the renderer', {modules: refresh.modules});
						try {
							if (window.isDestroyed()) {
								throw new Error('the main window closed before the activated module set could be offered');
							}
							const releasePendingLaunch = (): void => {
								updater.abandonPendingLaunch(refresh.launchAttempt);
							};
							const reloadRenderer = (): void => {
								if (window.isDestroyed()) {
									logger.warn('The main window closed before the activated module set could reload');
									releasePendingLaunch();
									return;
								}
								const webContents = window.webContents;
								const markReloadSucceeded = (): void => {
									void updater.markLaunchSucceeded(refresh.launchAttempt).catch((error: unknown) => {
										logger.error('Failed to record a successful module reload', error);
									});
								};
								void refreshModuleRoots(refresh.launchAttempt.committed).then(
									() => {
										if (window.isDestroyed()) {
											logger.warn('The main window closed before the activated module set could reload');
											releasePendingLaunch();
											return;
										}
										const stopWatchingReload = watchModuleRendererReload({
											target: webContents,
											observeLaunchConfirmed: observeRendererLaunchConfirmed,
											onLoaded: markReloadSucceeded,
											onAbandoned: (reason) => {
												logger.warn('The renderer never confirmed the module reload, releasing the pending launch', {
													reason,
												});
												releasePendingLaunch();
											},
										});
										try {
											webContents.reloadIgnoringCache();
										} catch (error) {
											stopWatchingReload();
											releasePendingLaunch();
											logger.error('The activated module set could not reload the renderer', error);
										}
									},
									(error: unknown) => {
										releasePendingLaunch();
										logger.error('The activated module set could not be served to the renderer', error);
									},
								);
							};
							offerPendingModuleUpdate({
								update: {modules: refresh.modules},
								apply: reloadRenderer,
								discard: releasePendingLaunch,
							});
						} catch (error) {
							updater.abandonPendingLaunch(refresh.launchAttempt);
							throw error;
						}
					})
					.catch((error: unknown) => {
						if (!failing) {
							failing = true;
							logger.warn('The module poll failed, staying quiet until it recovers', error);
						}
					})
					.finally(() => {
						inFlight = false;
						schedule();
					});
			};
			if (process.platform === 'linux') {
				tick();
			} else {
				schedule();
			}
		});
	});
}

async function refuseUnsupportedBuild(reason: string): Promise<never> {
	const {app} = await import('electron');
	const {createChildLogger} = await import('@electron/common/Logger');
	const {onSplashQuit, openSplashWindow, setSplashState, SplashAction} = await import('@electron/main/SplashWindow');
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

async function runModuleBootstrap(): Promise<void> {
	const {app} = await import('electron');
	const {BUILD_CHANNEL} = await import('@electron/common/BuildChannel');
	const {createChildLogger} = await import('@electron/common/Logger');
	const {loadDesktopConfig} = await import('@electron/common/DesktopConfig');
	const {appendWindowsGpuDriverWorkaroundSwitches} = await import('@electron/main/ChromiumRuntime');
	const {applyPreReadyChromiumConfiguration} = await import('@electron/main/PreReadyChromium');
	const {configureUserDataPath} = await import('@electron/common/UserDataPath');
	const {armNativeProbeCache} = await import('@electron/main/NativeProbeCache');
	const {isStartMinimizedLaunch} = await import('@electron/main/AutostartLaunch');
	const {relaunchStableLaunchPath} = await import('@electron/main/LinuxLaunchPath');
	const {getDesktopLocalAppProtocol} = await import('@electron/main/LocalAppProtocol');
	const {
		armSecondInstanceForwarding,
		onMainWindowCreated,
		onMainWindowReady,
		setCommittedModuleFiles,
		setOnDemandModuleInstaller,
		setSecondInstanceSink,
	} = await import('@electron/main/ModuleBootHandoff');
	const {instanceTurnedModulesOff} = await import('@electron/main/InstanceModulePreference');
	const {createOnDemandModuleInstaller} = await import('@electron/main/ModuleOnDemand');
	const {getModuleStoreRoot, ModuleStore} = await import('@electron/main/ModuleStore');
	const {ModuleUpdater} = await import('@electron/main/ModuleUpdater');
	const {moduleNetworkFetch} = await import('@electron/main/ModuleNetworkFetch');
	const {resolveDesktopPackageOrigin} = await import('@electron/main/ShellDownloadFormats');
	const {LocalNetworkSplashHint} = await import('@electron/main/LocalNetworkSplashHint');
	const {resolveShellUpdatePlan, ShellUpdateCapability} = await import('@electron/main/ShellUpdateCapability');
	const {armBlockedShellUpdate} = await import('@electron/main/ShellUpdateSplash');
	const {UpdateServerRetry} = await import('@electron/main/UpdateServerRetry');
	const {
		closeSplashWindow,
		focusSplashWindow,
		markSplashLaunching,
		onSplashNetworkOnline,
		onSplashQuit,
		onSplashRetry,
		openSplashWindow,
		releaseSplashAffordance,
		setSplashState,
		SplashAction,
	} = await import('@electron/main/SplashWindow');

	const logger = createChildLogger('Bootstrap');
	if (MODULE_SYSTEM_LAUNCH.kind === ModuleSystemLaunchDecision.ENABLED_WITH_IGNORED_DISABLE_REQUEST) {
		logger.warn(
			'Ignoring the --fluxer-no-module-system override because this build carries no offline renderer, so turning the module system off would leave nothing to render',
		);
	}
	const userDataConfig = configureUserDataPath();
	armNativeProbeCache(userDataConfig.base);

	if (!app.requestSingleInstanceLock()) {
		logger.info('Another instance already holds the single instance lock, forwarding this launch and quitting');
		app.quit();
		return;
	}
	armSecondInstanceForwarding();

	const instancePreference = decideModuleSystemDisableRequest(
		instanceTurnedModulesOff(userDataConfig.base),
		HAS_OFFLINE_RENDERER,
	);
	switch (instancePreference.kind) {
		case ModuleSystemDisableDecision.NOT_REQUESTED:
			break;
		case ModuleSystemDisableDecision.IGNORED_WITHOUT_OFFLINE_RENDERER:
			logger.warn(
				'Ignoring the instance desktop module preference because this build carries no offline renderer, so turning the module system off would leave nothing to render',
			);
			break;
		case ModuleSystemDisableDecision.HONOURED:
			logger.warn(
				'The instance turned the desktop module system off on an earlier boot, launching the offline renderer instead',
			);
			await import(MAIN_PROCESS_ENTRY_URL);
			return;
	}

	loadDesktopConfig(userDataConfig.base);
	applyPreReadyChromiumConfiguration(userDataConfig.channel, process.argv);

	try {
		getDesktopLocalAppProtocol().registerSchemes();
	} catch (error) {
		logger.error('Failed to register the local app scheme privileges', error);
		app.exit(1);
		process.exit(1);
	}

	await appendWindowsGpuDriverWorkaroundSwitches();
	await app.whenReady();

	if (isStartMinimizedLaunch()) {
		logger.info('Launched at login with start minimized, converging the modules without the splash');
	} else {
		openSplashWindow();
	}
	setSplashState({status: 'checking-for-updates'});
	onSplashQuit(() => {
		logger.info('The splash requested a quit');
		app.quit();
	});
	onSplashRetry(() => {
		logger.info('The splash requested a retry, relaunching');
		relaunchStableLaunchPath();
		app.exit(0);
	});

	const updateServerRetry = new UpdateServerRetry();
	onSplashNetworkOnline(() => {
		logger.info('The splash reported the network is back');
		updateServerRetry.networkReturned();
	});

	const showBlockedSplash = (status: SplashStatus, message: string | null = null): void => {
		openSplashWindow();
		setSplashState({status, action: SplashAction.RETRY, message});
		setSecondInstanceSink(focusSplashWindow);
	};
	const blockUntilRelaunch = (status: SplashStatus, message: string | null = null): Promise<never> => {
		showBlockedSplash(status, message);
		return new Promise<never>(() => {});
	};

	try {
		let store: ModuleStoreInstance;
		try {
			store = await ModuleStore.open({
				root: getModuleStoreRoot(userDataConfig.base),
				shellVersion: app.getVersion(),
				releaseChannel: BUILD_CHANNEL,
			});
		} catch (error) {
			logger.error('Failed to open the module store', error);
			await blockUntilRelaunch('blocked-update-required');
			return;
		}

		const localNetworkHint = new LocalNetworkSplashHint({
			apiBaseUrl: resolveDesktopPackageOrigin(),
			platform: process.platform,
			onHint: () => {
				logger.info('The module manifest host is on the local network and the fetch is still pending');
				setSplashState({status: 'waiting-local-network'});
			},
		});

		const updater = new ModuleUpdater({
			store,
			shellVersion: app.getVersion(),
			hasOfflineRenderer: HAS_OFFLINE_RENDERER,
			fetch: moduleNetworkFetch,
			sleep: updateServerRetry.sleep,
			onState: (state) => {
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

		const runModuleUpdateAttempt = async (): Promise<ModuleLaunchPermit | null> => {
			let outcome: ModuleUpdaterOutcome;
			try {
				outcome = await updater.run();
			} catch (error) {
				logger.error('The module updater failed', error);
				return await blockUntilRelaunch(
					updater.getLastState().requiredSecurityUpdate
						? 'blocked-security-update-required'
						: 'blocked-update-required',
				);
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
					const plan = resolveShellUpdatePlan();
					logger.warn('A shell update is required before the modules can converge', {
						latestVersion: outcome.latestVersion,
						minimumVersion: outcome.minimumVersion,
						capability: plan.capability,
					});
					if (plan.capability === ShellUpdateCapability.SELF_UPDATE) {
						const {runShellSelfUpdate} = await import('@electron/main/ShellSelfUpdate');
						const failure = await runShellSelfUpdate(plan, {
							onDownloading: (progress) => {
								setSplashState({
									status: 'shell-update-downloading',
									requiredSecurityUpdate: outcome.requiredSecurityUpdate,
									progress,
								});
							},
							onRestarting: () => {
								setSplashState({
									status: 'shell-update-restarting',
									requiredSecurityUpdate: outcome.requiredSecurityUpdate,
								});
							},
						});
						logger.error('The shell self update failed, falling back to a manual download', failure);
					}
					openSplashWindow();
					armBlockedShellUpdate(plan, outcome.latestVersion, outcome.requiredSecurityUpdate);
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
						showBlockedSplash(
							outcome.requiredSecurityUpdate ? 'blocked-security-update-required' : 'blocked-update-unreachable',
							UPDATE_SERVER_UNREACHABLE_MESSAGE,
						);
						await updateServerRetry.holdUntilNextAttempt();
						logger.info('Retrying the update server');
						return null;
					}
					return await blockUntilRelaunch(
						outcome.requiredSecurityUpdate ? 'blocked-security-update-required' : 'blocked-update-required',
					);
				default:
					return unhandledModuleUpdaterOutcome(outcome);
			}
		};

		const runModuleUpdateLoop = async (): Promise<ModuleLaunchPermit> => {
			for (;;) {
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
				const modules = resolveModules();
				setCommittedModuleFiles(store.storeRoot, await store.buildModuleIndex(modules));
				servedModules = modules;
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
			updaterInstance: ModuleUpdaterInstance,
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
				window.once('closed', closeSplashWindow);
			});
		};

		const permit = await runModuleUpdateLoop();
		localNetworkHint.disarm();
		await refreshModuleRoots(permit.launchAttempt.committed);
		setOnDemandModuleInstaller(
			createOnDemandModuleInstaller({
				ensure: (moduleName) => updater.ensureModule(moduleName),
				refresh: serveOnDemandModules,
				onFailure: (moduleName, error) => {
					logger.error('Failed to install a module on demand', {module: moduleName, error});
				},
			}),
		);
		markSplashLaunching();
		armMainWindowHandoff(updater, permit.launchAttempt, () => {
			armModulePoll(resolveModulePollPlan(readDevModulePollInterval()), updater, refreshModuleRoots, logger);
		});
		app.once('before-quit', () => {
			closeSplashWindow();
		});
		await launchMainApp(permit, logger);
	} catch (error) {
		logger.error('The module bootstrap failed unexpectedly', error);
		await blockUntilRelaunch('blocked-update-required');
	}
}

switch (MODULE_SYSTEM_LAUNCH.kind) {
	case ModuleSystemLaunchDecision.ENABLED:
	case ModuleSystemLaunchDecision.ENABLED_WITH_IGNORED_DISABLE_REQUEST:
		void runModuleBootstrap().catch((error: unknown) => {
			process.stderr.write(`The module bootstrap rejected before it could report: ${String(error)}\n`);
			process.exit(1);
		});
		break;
	case ModuleSystemLaunchDecision.DISABLED_BY_BUILD:
	case ModuleSystemLaunchDecision.DISABLED_BY_REQUEST:
		if (HAS_OFFLINE_RENDERER) {
			await import(MAIN_PROCESS_ENTRY_URL);
		} else {
			void refuseUnsupportedBuild('the module system is turned off and this build carries no offline renderer').catch(
				(error: unknown) => {
					process.stderr.write(`The module bootstrap rejected before it could report: ${String(error)}\n`);
					process.exit(1);
				},
			);
		}
		break;
}
