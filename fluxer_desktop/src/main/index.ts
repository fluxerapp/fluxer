// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {getDesktopWindowBehaviorSettings, loadDesktopConfig} from '@electron/common/DesktopConfig';
import {
	DESKTOP_APP_NAME,
	LINUX_DESKTOP_ENTRY_ID,
	WINDOWS_APP_USER_MODEL_ID,
	WINDOWS_TOAST_ACTIVATOR_CLSID,
} from '@electron/common/DesktopIdentity';
import {configureUserDataPath} from '@electron/common/UserDataPath';
import {
	APP_STORE_ADDON_PACKAGE,
	createAppStoreBoundary,
	getAppStoreLoadFailure,
	loadAppStoreBinding,
} from '@electron/main/AppStoreNativeBoundary';
import {registerAutostartHandlers} from '@electron/main/Autostart';
import {isStartMinimizedLaunch} from '@electron/main/AutostartLaunch';
import {
	appendWindowsGpuDriverWorkaroundSwitches,
	purgeChromiumRuntimeCachesIfNeeded,
} from '@electron/main/ChromiumRuntime';
import {
	consumeInitialJumpListTask,
	handleOpenUrl,
	handleSecondInstance,
	initializeDeepLinks,
} from '@electron/main/DeepLinks';
import {
	DesktopAccountStoreEvent,
	DesktopStorePermissionState,
	probeDesktopStorePermissions,
	recordDesktopAccountStoreEvent,
} from '@electron/main/DesktopAccountStoreTelemetry';
import {
	closeDesktopAppStorage,
	getDesktopAppStorage,
	openDesktopAppStorage,
	recoverDesktopAppStorage,
} from '@electron/main/DesktopAppStorage';
import {
	DesktopAppStoreHealth,
	desktopAppStoreStateFile,
	evaluateDesktopAppStoreHealth,
	readDesktopAppStoreState,
	recordDesktopAppStoreSuccess,
} from '@electron/main/DesktopAppStoreHealth';
import {
	formatDesktopDebugInfo,
	getDesktopDebugInfo,
	getLaunchNetLogPath,
	hasDesktopDebugInfoArg,
	logDesktopDebugInfo,
	shouldResetWindowStateOnLaunch,
} from '@electron/main/DesktopDebugInfo';
import {recordDesktopLastRoute} from '@electron/main/DesktopLastRoute';
import {cleanupDesktopOutboundHTTP} from '@electron/main/DesktopOutboundHTTP';
import {registerDesktopRuntimeConfigHandlers} from '@electron/main/DesktopRuntimeConfigIpc';
import {destroyDesktopTray, hasActiveDesktopTray, initializeDesktopTray} from '@electron/main/DesktopTray';
import {registerDisplayMediaHandlers} from '@electron/main/DisplayMedia';
import {initializeDockMenu} from '@electron/main/DockMenu';
import {GATEWAY_SOCKET_ADDON_PACKAGE, loadGatewaySocketBinding} from '@electron/main/GatewaySocketNativeBoundary';
import {cleanupGlobalShortcuts, initializeGlobalShortcuts} from '@electron/main/GlobalShortcutsIpc';
import {
	cleanupGatewayTransportHandlers,
	cleanupIpcHandlers,
	registerDesktopAppStorageHandlers,
	registerGatewayTransportHandlers,
	registerIpcHandlers,
} from '@electron/main/IpcHandlers';
import {initializeJumpList} from '@electron/main/JumpList';
import {describeLaunchDiagnosticOptions} from '@electron/main/LaunchOptions';
import {cleanupLegacyHarvestHandlers, registerLegacyHarvestHandlers} from '@electron/main/LegacyHarvestIpc';
import {initializeLegacyOriginHarvest} from '@electron/main/LegacyOriginHarvest';
import {cleanupVirtmic, registerVirtmicHandlers} from '@electron/main/LinuxAudioCapture';
import {ensureLinuxDesktopEntry} from '@electron/main/LinuxDesktopEntry';
import {cleanupDesktopLocalAppProtocol, getDesktopLocalAppProtocol} from '@electron/main/LocalAppProtocol';
import {initializeMainI18n, t} from '@electron/main/MainI18n';
import {createApplicationMenu} from '@electron/main/Menu';
import {
	armOpenUrlForwarding,
	armSecondInstanceForwarding,
	setOpenUrlSink,
	setSecondInstanceSink,
} from '@electron/main/ModuleBootHandoff';
import {cleanupNativeAudio, registerNativeAudioHandlers} from '@electron/main/NativeAudio';
import {
	cleanupNativeHardwareEncoderHandlers,
	registerNativeHardwareEncoderHandlers,
} from '@electron/main/NativeHardwareEncoder';
import {type NativeModulePreflightResult, runNativeModulePreflight} from '@electron/main/NativeModulePreflight';
import {armNativeProbeCache} from '@electron/main/NativeProbeCache';
import {cleanupNativeScreenCapture, registerNativeScreenCaptureHandlers} from '@electron/main/NativeScreenCapture';
import {applyPreReadyChromiumConfiguration} from '@electron/main/PreReadyChromium';
import {cleanupLinuxChromiumSpellcheckDictionaries} from '@electron/main/Spellcheck';
import {
	clearSavedWindowBounds,
	createWindow,
	getMainWindow,
	hideWindow,
	setQuitting,
	showWindow,
} from '@electron/main/Window';
import {removeLegacySquirrelUninstallEntry} from '@electron/main/WindowsLegacyUninstallEntry';
import {removeFluxerVulkanLayerRegistrations} from '@electron/main/WindowsVulkanLayerCleanup';
import {
	DESKTOP_CAPABILITY_MANIFEST_CHANNEL,
	type DesktopCapabilityManifest,
} from '@fluxer/desktop_ipc/src/CapabilityManifest';
import {DESKTOP_LAST_ROUTE_CHANNEL} from '@fluxer/desktop_ipc/src/LastRouteContract';
import {
	DESKTOP_LEGACY_IMPORT_FAILURE_MARKER_KEY,
	DESKTOP_LEGACY_IMPORT_MARKER_KEY,
	DESKTOP_LEGACY_SESSION_MARKER_KEY,
	DesktopLegacyImportPhase,
	readDesktopLegacyImportPhase,
} from '@fluxer/desktop_ipc/src/StorageContract';
import {app, dialog, ipcMain, netLog, shell} from 'electron';
import log from 'electron-log';

log.transports.file.level = 'info';

log.transports.file.sync = false;

log.transports.console.level = 'debug';

if (process.platform === 'linux' && process.env.PULSE_LATENCY_MSEC === undefined) {
	process.env.PULSE_LATENCY_MSEC = '30';
}

if (process.platform === 'linux') {
	process.env['PULSE_PROP_OVERRIDE_application.name'] = DESKTOP_APP_NAME;
}

process.on('uncaughtException', (error) => {
	try {
		log.error('Uncaught exception (early):', error);
	} catch {}
});

process.on('unhandledRejection', (reason, promise) => {
	try {
		log.error('Unhandled rejection (early) at:', promise, 'reason:', reason);
	} catch {}
});

const userDataConfig = configureUserDataPath();
if (userDataConfig.portable) {
	const portableLogsPath = app.getPath('logs');
	log.transports.file.resolvePathFn = (variables) => path.join(portableLogsPath, variables.fileName ?? 'main.log');
}
armNativeProbeCache(userDataConfig.base);
const processConfiguredAt = Date.now();

log.info('Configured user data storage', {
	channel: userDataConfig.channel,
	directory: userDataConfig.directoryName,
	path: userDataConfig.base,
	portable: userDataConfig.portable,
});

cleanupLinuxChromiumSpellcheckDictionaries(userDataConfig.base);

loadDesktopConfig(userDataConfig.base);

function exitCli(code: number): void {
	process.exitCode = code;
	setImmediate(() => {
		process.exit(code);
	});
}

function reportUnreadableDesktopAppStore(storeFile: string, reason: string | null): void {
	log.error('[AppStore] The desktop app store worked here before and cannot be opened now', {storeFile, reason});
	let choice = 0;
	try {
		choice = dialog.showMessageBoxSync({
			type: 'error',
			title: t('desktop.appStore.unreadableTitle'),
			message: t('desktop.appStore.unreadableMessage'),
			detail: [
				t('desktop.appStore.unreadableUnchanged'),
				t('desktop.appStore.unreadableAdvice'),
				reason == null ? '' : t('desktop.appStore.unreadableDetails', {reason}),
			]
				.filter(Boolean)
				.join('\n\n'),
			buttons: [t('desktop.appMenu.quit'), t('desktop.appStore.showStoreFolder')],
			defaultId: 0,
			cancelId: 0,
			noLink: true,
		});
	} catch (error) {
		log.error('[AppStore] Failed to present the unreadable store notice:', error);
	}
	if (choice === 1) {
		try {
			shell.showItemInFolder(storeFile);
		} catch (error) {
			log.error('[AppStore] Failed to reveal the store folder:', error);
		}
	}
	app.exit(1);
}

async function recordDesktopAppStoreImportState(schemaVersion: number): Promise<void> {
	const storage = getDesktopAppStorage();
	if (storage == null) {
		return;
	}
	try {
		const marker = await storage.getMarker(DESKTOP_LEGACY_IMPORT_MARKER_KEY);
		const phase = readDesktopLegacyImportPhase(marker);
		log.info('[AppStore] Legacy import state', {
			phase,
			failures: await storage.getMarker(DESKTOP_LEGACY_IMPORT_FAILURE_MARKER_KEY),
			reconciledAt: await storage.getMarker(DESKTOP_LEGACY_SESSION_MARKER_KEY),
			stateFile: desktopAppStoreStateFile(userDataConfig.base),
		});
		if (phase === DesktopLegacyImportPhase.DONE) {
			recordDesktopAppStoreSuccess({userDataPath: userDataConfig.base, now: Date.now(), schemaVersion});
			recordDesktopAccountStoreEvent(userDataConfig.base, DesktopAccountStoreEvent.MIGRATED);
		}
	} catch (error) {
		log.warn('[AppStore] Failed to read the legacy import state:', error);
	}
}

function writeCliAndExit(stream: NodeJS.WriteStream, message: string, code: number): void {
	let done = false;
	const finish = (): void => {
		if (done) return;
		done = true;
		stream.off('error', finish);
		exitCli(code);
	};
	stream.once('error', finish);
	stream.write(`${message}\n`, finish);
}

let launchConfigurationError: Error | null = null;
let launchDiagnosticOptions: Record<string, unknown> = {};

try {
	launchDiagnosticOptions = describeLaunchDiagnosticOptions(process.argv);
} catch (error) {
	launchConfigurationError = error instanceof Error ? error : new Error(String(error));
}

if (launchConfigurationError) {
	console.error(`Fluxer desktop launch configuration error: ${launchConfigurationError.message}`);
	log.error('Launch configuration error:', launchConfigurationError);
	app.exit(1);
} else if (hasDesktopDebugInfoArg(process.argv)) {
	void getDesktopDebugInfo(userDataConfig.base, {nativeProbes: false})
		.then((info) => {
			try {
				logDesktopDebugInfo(info);
			} catch {}
			writeCliAndExit(process.stdout, formatDesktopDebugInfo(info), 0);
		})
		.catch((error: unknown) => {
			const message = error instanceof Error ? error.message : String(error);
			log.error('Failed to collect desktop debug info:', error);
			writeCliAndExit(process.stderr, `Failed to collect Fluxer desktop debug info: ${message}`, 1);
		});
} else {
	if (shouldResetWindowStateOnLaunch(process.argv)) {
		clearSavedWindowBounds();
	}
	applyPreReadyChromiumConfiguration(userDataConfig.channel, process.argv);
	log.info('Launch diagnostic modes', launchDiagnosticOptions);
	const CHANNEL_APP_NAME = DESKTOP_APP_NAME;
	app.setName(CHANNEL_APP_NAME);
	function recordStartupPhase(phase: string, phaseStartedAt: number): void {
		log.info('[Startup] Phase completed', {
			phase,
			durationMs: Date.now() - phaseStartedAt,
			sinceConfiguredMs: Date.now() - processConfiguredAt,
		});
	}
	function runStartupPhase<T>(phase: string, fn: () => T): T {
		const phaseStartedAt = Date.now();
		try {
			return fn();
		} finally {
			recordStartupPhase(phase, phaseStartedAt);
		}
	}
	async function runStartupPhaseAsync<T>(phase: string, fn: () => Promise<T>): Promise<T> {
		const phaseStartedAt = Date.now();
		try {
			return await fn();
		} finally {
			recordStartupPhase(phase, phaseStartedAt);
		}
	}
	async function startLaunchNetLog(): Promise<void> {
		const netLogPath = getLaunchNetLogPath(userDataConfig.base, process.argv);
		if (!netLogPath) {
			return;
		}
		try {
			fs.mkdirSync(path.dirname(netLogPath), {recursive: true});
			await netLog.startLogging(netLogPath, {captureMode: 'default'});
			log.info('[DebugInfo] Chromium net log started', {path: netLogPath});
		} catch (error) {
			log.error('[DebugInfo] Failed to start Chromium net log:', error);
		}
	}
	const isLegacySquirrelStartupArg = (arg: string): boolean =>
		arg === '--squirrel-install' ||
		arg === '--squirrel-updated' ||
		arg === '--squirrel-uninstall' ||
		arg === '--squirrel-obsolete' ||
		arg === '--squirrel-firstrun';
	const hasLegacySquirrelArg = process.platform === 'win32' && process.argv.some(isLegacySquirrelStartupArg);
	if (hasLegacySquirrelArg) {
		app.exit(0);
	}
	try {
		runStartupPhase('main-i18n', initializeMainI18n);
	} catch (error) {
		log.error('[Init] Failed to initialize native i18n:', error);
	}
	let nativeModulePreflight: NativeModulePreflightResult = {degraded: []};
	try {
		nativeModulePreflight = runStartupPhase('native-module-preflight', runNativeModulePreflight);
	} catch (error) {
		const message = error instanceof Error ? error.message : String(error);
		log.error('[NativeModulePreflight] Fatal native module preflight failure:', error);
		console.error(message);
		try {
			dialog.showErrorBox(t('desktop.startup.failedTitle'), message);
		} catch {}
		app.exit(1);
		process.exit(1);
	}
	const degradedNativeModules = new Set(nativeModulePreflight.degraded);
	const desktopCapabilityManifest: DesktopCapabilityManifest = {
		appStore: !degradedNativeModules.has(APP_STORE_ADDON_PACKAGE) && loadAppStoreBinding() !== null,
		gatewaySocket: !degradedNativeModules.has(GATEWAY_SOCKET_ADDON_PACKAGE) && loadGatewaySocketBinding() !== null,
	};
	log.info('[Capabilities] Desktop capability manifest', desktopCapabilityManifest);
	ipcMain.on(DESKTOP_CAPABILITY_MANIFEST_CHANNEL, (event) => {
		event.returnValue = desktopCapabilityManifest;
	});
	ipcMain.on(DESKTOP_LAST_ROUTE_CHANNEL, (_event, routePath: unknown) => {
		recordDesktopLastRoute(app.getPath('userData'), routePath);
	});
	if (process.platform === 'win32') {
		app.setToastActivatorCLSID(WINDOWS_TOAST_ACTIVATOR_CLSID);
		app.setAppUserModelId(WINDOWS_APP_USER_MODEL_ID);
	}
	try {
		runStartupPhase('local-app-scheme', () => {
			getDesktopLocalAppProtocol().registerSchemes();
		});
	} catch (error) {
		log.error('[Init] Failed to register the local app scheme privileges:', error);
		app.exit(1);
		process.exit(1);
	}
	const startupController = new AbortController();
	let startupWindowsPending = true;
	const gotTheLock = app.hasSingleInstanceLock() || app.requestSingleInstanceLock();
	if (!gotTheLock) {
		app.quit();
	} else {
		runStartupPhase('runtime-cache-guard', () => {
			purgeChromiumRuntimeCachesIfNeeded(userDataConfig.base);
		});
		armSecondInstanceForwarding();
		setSecondInstanceSink(handleSecondInstance);
		app.on('child-process-gone', (_event, details) => {
			log.error('Child process gone', details);
		});
		armOpenUrlForwarding();
		setOpenUrlSink(handleOpenUrl);
		(app.isReady()
			? Promise.resolve()
			: runStartupPhaseAsync('gpu-driver-workarounds', appendWindowsGpuDriverWorkaroundSwitches)
		)
			.then(() => app.whenReady())
			.then(async () => {
				log.info('App ready, initializing...');
				try {
					runStartupPhase('host-resolver', () => app.configureHostResolver({enableAdditionalDnsQueryTypes: false}));
				} catch (error) {
					log.error('[Init] Failed to configure the host resolver:', error);
				}
				await runStartupPhaseAsync('launch-net-log', startLaunchNetLog);
				try {
					await runStartupPhaseAsync('desktop-debug-info', async () => {
						logDesktopDebugInfo(await getDesktopDebugInfo(userDataConfig.base, {nativeProbes: false}));
					});
				} catch (error) {
					log.error('[DebugInfo] Failed to collect desktop debug info:', error);
				}
				try {
					runStartupPhase('linux-desktop-entry', () => {
						if (ensureLinuxDesktopEntry()) {
							process.env.FLUXER_LINUX_PORTAL_APP_ID = LINUX_DESKTOP_ENTRY_ID;
						}
					});
				} catch (error) {
					log.error('[Init] Failed to ensure the Linux desktop entry:', error);
				}
				try {
					runStartupPhase('deep-links', initializeDeepLinks);
				} catch (error) {
					log.error('[Init] Failed to initialize deep links:', error);
				}
				try {
					runStartupPhase('jump-list', initializeJumpList);
				} catch (error) {
					log.error('[Init] Failed to initialize JumpList:', error);
				}
				try {
					runStartupPhase('dock-menu', initializeDockMenu);
				} catch (error) {
					log.error('[Init] Failed to initialize macOS dock menu:', error);
				}
				let appStoreSchemaVersion = 0;
				try {
					const appStore = runStartupPhase('app-store-open', () =>
						openDesktopAppStorage({
							userDataPath: userDataConfig.base,
							createBoundary: createAppStoreBoundary,
							describeLoadFailure: getAppStoreLoadFailure,
						}),
					);
					const health = evaluateDesktopAppStoreHealth({
						available: appStore.status.available,
						state: readDesktopAppStoreState(userDataConfig.base),
					});
					log.info('[AppStore] Desktop app store opened', {
						available: appStore.status.available,
						schemaVersion: appStore.status.schemaVersion,
						storeFile: appStore.storeFile,
						quarantineReason: appStore.status.quarantineReason,
						quarantinedFile: appStore.quarantinedFile,
						unavailableReason: appStore.status.unavailableReason,
						health,
					});
					if (!appStore.status.available) {
						recordDesktopAccountStoreEvent(userDataConfig.base, DesktopAccountStoreEvent.FALLBACK_TO_WEB);
						if (health === DesktopAppStoreHealth.REGRESSED) {
							reportUnreadableDesktopAppStore(appStore.storeFile, appStore.status.unavailableReason);
						}
					}
					if (appStore.status.quarantined) {
						recordDesktopAccountStoreEvent(userDataConfig.base, DesktopAccountStoreEvent.QUARANTINED);
					}
					const permissions = probeDesktopStorePermissions(appStore.storeFile);
					if (permissions === DesktopStorePermissionState.WIDENED) {
						recordDesktopAccountStoreEvent(userDataConfig.base, DesktopAccountStoreEvent.PERMISSION_HARDENING_FAILED);
						log.warn(
							'[AppStore] The account store is readable beyond this user and permission hardening did not hold',
							{
								storeFile: appStore.storeFile,
							},
						);
					}
					appStoreSchemaVersion = appStore.status.schemaVersion;
				} catch (error) {
					log.error('[AppStore] Failed to open the desktop app store:', error);
				}
				try {
					const recovery = await runStartupPhaseAsync('app-store-recovery', recoverDesktopAppStorage);
					if (recovery.reseedRequired) {
						recordDesktopAccountStoreEvent(userDataConfig.base, DesktopAccountStoreEvent.QUARANTINED);
						log.warn('[AppStore] The previous store was quarantined and the renderer re-seeds the fresh one', {
							quarantineReason: recovery.status.quarantineReason,
						});
					}
					await recordDesktopAppStoreImportState(appStoreSchemaVersion);
				} catch (error) {
					log.error('[AppStore] Desktop app store recovery failed:', error);
				}
				try {
					runStartupPhase('app-store-ipc', registerDesktopAppStorageHandlers);
				} catch (error) {
					log.error('[AppStore] Failed to register desktop app store IPC handlers:', error);
				}
				try {
					runStartupPhase('gateway-transport-ipc', registerGatewayTransportHandlers);
				} catch (error) {
					log.error('[NativeGateway] Failed to register native gateway transport IPC handlers:', error);
				}
				try {
					runStartupPhase('ipc-handlers', registerIpcHandlers);
				} catch (error) {
					log.error('[Init] Failed to register IPC handlers:', error);
				}
				try {
					runStartupPhase('autostart-handlers', registerAutostartHandlers);
				} catch (error) {
					log.error('[Init] Failed to register autostart handlers:', error);
				}
				try {
					runStartupPhase('global-shortcuts', initializeGlobalShortcuts);
				} catch (error) {
					log.error('[Init] Failed to initialize global shortcuts:', error);
				}
				try {
					runStartupPhase('display-media-handlers', registerDisplayMediaHandlers);
				} catch (error: unknown) {
					log.error('[Init] Failed to register display media handlers:', error);
				}
				try {
					runStartupPhase('virtmic-handlers', registerVirtmicHandlers);
				} catch (error: unknown) {
					log.error('[Init] Failed to register virtmic handlers:', error);
				}
				try {
					runStartupPhase('native-audio-handlers', registerNativeAudioHandlers);
				} catch (error: unknown) {
					log.error('[Init] Failed to register native audio handlers:', error);
				}
				try {
					runStartupPhase('vulkan-layer-cleanup', removeFluxerVulkanLayerRegistrations);
				} catch (error: unknown) {
					log.error('[Init] Failed to remove stale Vulkan layer registrations:', error);
				}
				try {
					runStartupPhase('legacy-uninstall-entry-cleanup', removeLegacySquirrelUninstallEntry);
				} catch (error: unknown) {
					log.error('[Init] Failed to remove the legacy Squirrel uninstall entry:', error);
				}
				try {
					runStartupPhase('native-screen-capture-handlers', registerNativeScreenCaptureHandlers);
				} catch (error: unknown) {
					log.error('[Init] Failed to register native screen capture handlers:', error);
				}
				try {
					runStartupPhase('native-hardware-encoder-handlers', registerNativeHardwareEncoderHandlers);
				} catch (error: unknown) {
					log.error('[Init] Failed to register native hardware encoder handlers:', error);
				}
				try {
					runStartupPhase('legacy-harvest-ipc', registerLegacyHarvestHandlers);
				} catch (error: unknown) {
					log.error('[LegacyHarvest] Failed to register legacy harvest IPC handlers:', error);
				}
				try {
					runStartupPhase('local-app-protocol', () => {
						getDesktopLocalAppProtocol().register();
					});
				} catch (error: unknown) {
					log.error('[LocalApp] Failed to register the local app protocol handler:', error);
				}
				try {
					await runStartupPhaseAsync('legacy-origin-harvest', () =>
						initializeLegacyOriginHarvest(startupController.signal),
					);
				} catch (error: unknown) {
					log.error('[LegacyHarvest] Legacy origin harvest failed:', error);
				}
				try {
					runStartupPhase('runtime-config-ipc', registerDesktopRuntimeConfigHandlers);
				} catch (error: unknown) {
					log.error('[LocalApp] Failed to register runtime config IPC handlers:', error);
				}
				try {
					runStartupPhase('application-menu', createApplicationMenu);
				} catch (error: unknown) {
					log.error('[Init] Failed to create application menu:', error);
				}
				runStartupPhase('create-window', () => {
					try {
						createWindow({startHidden: isStartMinimizedLaunch()});
					} finally {
						startupWindowsPending = false;
					}
				});
				const initialTask = consumeInitialJumpListTask();
				if (initialTask) {
					const mainWindow = getMainWindow();
					mainWindow?.webContents.once('did-finish-load', () => {
						if (initialTask === 'open-settings') {
							mainWindow.webContents.send('open-settings');
						} else {
							mainWindow.webContents.send('jump-list-new-dm');
						}
					});
				}
				runStartupPhase('desktop-tray', () => {
					initializeDesktopTray({
						createWindow,
						getMainWindow,
						hideWindow,
						setQuitting,
						showWindow,
					});
				});
				if (process.env.FLUXER_OFFLINE !== '1') {
					const {registerUpdater} = await import('@electron/main/Updater');
					registerUpdater(getMainWindow);
				}
				app.on('activate', () => {
					const mainWindow = getMainWindow();
					if (mainWindow === null || mainWindow.isDestroyed()) {
						createWindow();
					} else {
						showWindow();
					}
				});
				log.info('App initialized successfully');
			})
			.catch((error: unknown) => {
				startupWindowsPending = false;
				log.error('[Startup] whenReady chain rejected:', error);
			});
		app.on('window-all-closed', () => {
			if (startupWindowsPending) {
				log.info('[Shutdown] All windows closed before startup created the main window, keeping app alive');
				return;
			}
			const settings = getDesktopWindowBehaviorSettings();
			if (process.platform !== 'darwin' && !(hasActiveDesktopTray() && settings.showTrayIcon && settings.closeToTray)) {
				app.quit();
			} else if (process.platform !== 'darwin') {
				log.info('[Shutdown] All windows closed; keeping app alive because close-to-tray is enabled');
			}
		});
		const QUIT_WATCHDOG_MS = 15000;
		let quitWatchdog: NodeJS.Timeout | null = null;
		function armQuitWatchdog(reason: string): void {
			if (quitWatchdog) return;
			quitWatchdog = setTimeout(() => {
				log.warn('[Shutdown] Process did not exit before watchdog timeout; forcing exit', {
					reason,
					timeoutMs: QUIT_WATCHDOG_MS,
				});
				process.exit(0);
			}, QUIT_WATCHDOG_MS);
			quitWatchdog.unref?.();
		}
		app.on('before-quit', () => {
			log.info('[Shutdown] before-quit received');
			setQuitting(true);
			startupController.abort();
			armQuitWatchdog('before-quit');
		});
		let quitCleanupStarted = false;
		app.on('will-quit', (event) => {
			if (quitCleanupStarted) return;
			quitCleanupStarted = true;
			log.info('[Shutdown] will-quit cleanup started');
			armQuitWatchdog('will-quit');
			event.preventDefault();
			startupController.abort();
			try {
				cleanupLegacyHarvestHandlers();
			} catch (error) {
				log.error('[Shutdown] Failed to remove the legacy harvest IPC handlers:', error);
			}
			try {
				closeDesktopAppStorage();
				log.info('[Shutdown] Desktop app store closed and checkpointed');
			} catch (error) {
				log.error('[Shutdown] Failed to close the desktop app store:', error);
			}
			try {
				cleanupGatewayTransportHandlers();
			} catch (error) {
				log.error('[Shutdown] Failed to close the native gateway transport:', error);
			}
			try {
				cleanupDesktopOutboundHTTP();
			} catch (error) {
				log.error('[Shutdown] Failed to close the desktop outbound HTTP client:', error);
			}
			cleanupIpcHandlers({quitting: true});
			cleanupGlobalShortcuts();
			cleanupNativeAudio();
			cleanupNativeScreenCapture();
			cleanupNativeHardwareEncoderHandlers();
			cleanupVirtmic();
			destroyDesktopTray();
			const asyncCleanups: Array<Promise<unknown>> = [
				cleanupDesktopLocalAppProtocol().catch((error: unknown) => {
					log.error('[Shutdown] Failed to shut down the local app protocol:', error);
				}),
			];
			if (netLog.currentlyLogging) {
				asyncCleanups.push(
					netLog.stopLogging().catch((error) => {
						log.warn('[DebugInfo] Failed to stop Chromium net log:', error);
					}),
				);
			}
			void Promise.allSettled(asyncCleanups).finally(() => {
				log.info('[Shutdown] will-quit cleanup complete; exiting');
				app.exit(0);
			});
		});
		app.on('quit', (_event, exitCode) => {
			log.info('[Shutdown] quit received', {exitCode});
		});
	}
}
