// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs';
import path from 'node:path';
import {
	getConfiguredChromiumSwitches,
	getDesktopTroubleshootingSettings,
	getDesktopWindowBehaviorSettings,
	loadDesktopConfig,
	setRuntimeAppUrlOverride,
} from '@electron/common/DesktopConfig';
import {
	DESKTOP_APP_NAME,
	LINUX_DESKTOP_ENTRY_ID,
	WINDOWS_APP_USER_MODEL_ID,
	WINDOWS_TOAST_ACTIVATOR_CLSID,
} from '@electron/common/DesktopIdentity';
import {configureUserDataPath} from '@electron/common/UserDataPath';
import {registerAutostartHandlers} from '@electron/main/Autostart';
import {
	addLinuxHardwareVideoEncodeFeatures,
	addLinuxScreenCapturePipeWireFeature,
	addMacosPreSequoiaScreenCaptureDisabledFeatures,
	addWindowsHardwareVideoEncodeFeatures,
	addWindowsWebRtcWgcDisabledFeatures,
	appendConfiguredChromiumSwitches,
	appendDisabledChromiumFeatures,
	appendEnabledBlinkFeature,
	appendEnabledChromiumFeatures,
	appendLinuxChromiumFlagsConfig,
	appendLinuxOzonePlatformHint,
	appendWindowsGpuDriverWorkaroundSwitches,
	BASE_DISABLED_CHROMIUM_FEATURES,
	MIDDLE_CLICK_AUTOSCROLL_BLINK_FEATURE,
	purgeChromiumRuntimeCachesIfNeeded,
} from '@electron/main/ChromiumRuntime';
import {
	consumeInitialJumpListTask,
	handleOpenUrl,
	handleSecondInstance,
	initializeDeepLinks,
} from '@electron/main/DeepLinks';
import {
	formatDesktopDebugInfo,
	getDesktopDebugInfo,
	getLaunchAppUrlOverride,
	getLaunchNetLogPath,
	hasDesktopDebugInfoArg,
	logDesktopDebugInfo,
	shouldDisableHardwareAccelerationForLaunch,
	shouldResetWindowStateOnLaunch,
} from '@electron/main/DesktopDebugInfo';
import {destroyDesktopTray, hasActiveDesktopTray, initializeDesktopTray} from '@electron/main/DesktopTray';
import {registerDisplayMediaHandlers} from '@electron/main/DisplayMedia';
import {initializeDockMenu} from '@electron/main/DockMenu';
import {cleanupGlobalKeyHook, registerGlobalKeyHookHandlers} from '@electron/main/GlobalKeyHook';
import {cleanupIpcHandlers, registerIpcHandlers} from '@electron/main/IpcHandlers';
import {initializeJumpList} from '@electron/main/JumpList';
import {describeLaunchDiagnosticOptions, shouldStartHiddenAtLogin} from '@electron/main/LaunchOptions';
import {cleanupVirtmic, registerVirtmicHandlers} from '@electron/main/LinuxAudioCapture';
import {initializeMainI18n} from '@electron/main/MainI18n';
import {createApplicationMenu} from '@electron/main/Menu';
import {cleanupNativeAudio, registerNativeAudioHandlers} from '@electron/main/NativeAudio';
import {
	cleanupNativeHardwareEncoderHandlers,
	registerNativeHardwareEncoderHandlers,
} from '@electron/main/NativeHardwareEncoder';
import {runNativeModulePreflight} from '@electron/main/NativeModulePreflight';
import {cleanupNativeScreenCapture, registerNativeScreenCaptureHandlers} from '@electron/main/NativeScreenCapture';
import {appendOpenH264Switches} from '@electron/main/OpenH264Manager';
import {startRpcServer, stopRpcServer} from '@electron/main/RpcServer';
import {cleanupLinuxChromiumSpellcheckDictionaries} from '@electron/main/Spellcheck';
import {registerUpdater} from '@electron/main/Updater';
import {
	clearSavedWindowBounds,
	createWindow,
	getMainWindow,
	hideWindow,
	setQuitting,
	showWindow,
} from '@electron/main/Window';
import {initializeWindowsVulkanGameCaptureLayer} from '@electron/main/WindowsVulkanGameCaptureLayer';
import {app, dialog, netLog} from 'electron';
import log from 'electron-log';

log.transports.file.level = 'info';

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
	const appUrlOverride = getLaunchAppUrlOverride(process.argv);
	if (appUrlOverride) {
		setRuntimeAppUrlOverride(appUrlOverride);
		log.info('Using one-shot app URL override from command line', {appUrl: appUrlOverride});
	}
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
	const disableHardwareAccelerationRequested =
		shouldDisableHardwareAccelerationForLaunch(process.argv) ||
		getDesktopTroubleshootingSettings().disableHardwareAcceleration;
	if (process.platform !== 'darwin' && disableHardwareAccelerationRequested) {
		app.disableHardwareAcceleration();
		log.info('Hardware acceleration disabled for this launch', {
			commandLine: shouldDisableHardwareAccelerationForLaunch(process.argv),
			persistentSetting: getDesktopTroubleshootingSettings().disableHardwareAcceleration,
		});
	} else if (process.platform === 'darwin' && disableHardwareAccelerationRequested) {
		log.info('Hardware acceleration disable request ignored on macOS');
	}
	log.info('Launch diagnostic modes', launchDiagnosticOptions);
	const CHANNEL_APP_NAME = DESKTOP_APP_NAME;
	app.setName(CHANNEL_APP_NAME);
	if (process.platform === 'linux') {
		process.env.FLUXER_LINUX_DESKTOP_ENTRY_ID = LINUX_DESKTOP_ENTRY_ID;
	}
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
		runStartupPhase('native-module-preflight', runNativeModulePreflight);
	} catch (error) {
		const message = error instanceof Error ? error.message : String(error);
		log.error('[NativeModulePreflight] Fatal native module preflight failure:', error);
		console.error(message);
		try {
			dialog.showErrorBox('Fluxer failed to start', message);
		} catch {}
		app.exit(1);
		process.exit(1);
	}
	app.commandLine.appendSwitch('autoplay-policy', 'no-user-gesture-required');
	const windowBehaviorSettings = getDesktopWindowBehaviorSettings();
	app.commandLine.appendSwitch(
		windowBehaviorSettings.smoothScrolling ? 'enable-smooth-scrolling' : 'disable-smooth-scrolling',
	);
	if (process.platform === 'linux' && windowBehaviorSettings.middleClickAutoscroll) {
		appendEnabledBlinkFeature(MIDDLE_CLICK_AUTOSCROLL_BLINK_FEATURE);
	}
	const disabledChromiumFeatures = new Set(BASE_DISABLED_CHROMIUM_FEATURES);
	const enabledChromiumFeatures = new Set<string>();
	if (!disableHardwareAccelerationRequested) {
		addLinuxHardwareVideoEncodeFeatures(enabledChromiumFeatures);
		addWindowsHardwareVideoEncodeFeatures(enabledChromiumFeatures);
	}
	addLinuxScreenCapturePipeWireFeature(enabledChromiumFeatures);
	if (process.platform === 'darwin') {
		addMacosPreSequoiaScreenCaptureDisabledFeatures(disabledChromiumFeatures);
	}
	addWindowsWebRtcWgcDisabledFeatures(disabledChromiumFeatures);
	appendDisabledChromiumFeatures(disabledChromiumFeatures);
	if (enabledChromiumFeatures.size > 0) {
		appendEnabledChromiumFeatures(enabledChromiumFeatures);
	}
	appendConfiguredChromiumSwitches(getConfiguredChromiumSwitches());
	if (launchDiagnosticOptions.safeMode !== true) {
		appendLinuxChromiumFlagsConfig(userDataConfig.channel);
	}
	appendLinuxOzonePlatformHint();
	if (process.platform === 'win32') {
		app.commandLine.appendSwitch('enable-h264-mf');
		app.commandLine.appendSwitch('enable-h264-mf-zero-copy');
		app.setToastActivatorCLSID(WINDOWS_TOAST_ACTIVATOR_CLSID);
		app.setAppUserModelId(WINDOWS_APP_USER_MODEL_ID);
	}
	appendOpenH264Switches();
	const gotTheLock = app.requestSingleInstanceLock();
	if (!gotTheLock) {
		app.quit();
	} else {
		runStartupPhase('runtime-cache-guard', () => {
			purgeChromiumRuntimeCachesIfNeeded(userDataConfig.base);
		});
		app.on('second-instance', (_event, argv, _workingDirectory) => {
			handleSecondInstance(argv);
		});
		app.on('child-process-gone', (_event, details) => {
			log.error('Child process gone', details);
		});
		app.on('open-url', (event, url) => {
			event.preventDefault();
			handleOpenUrl(url);
		});
		app.on('ready', (_event, launchInfo) => {
			const fallbackDeepLink = (launchInfo as {userInfo?: {fallbackDeepLink?: unknown}} | undefined)?.userInfo
				?.fallbackDeepLink;
			if (typeof fallbackDeepLink === 'string') {
				handleOpenUrl(fallbackDeepLink);
			}
		});
		runStartupPhaseAsync('gpu-driver-workarounds', appendWindowsGpuDriverWorkaroundSwitches)
			.then(() => app.whenReady())
			.then(async () => {
				log.info('App ready, initializing...');
				await runStartupPhaseAsync('launch-net-log', startLaunchNetLog);
				try {
					await runStartupPhaseAsync('desktop-debug-info', async () => {
						logDesktopDebugInfo(await getDesktopDebugInfo(userDataConfig.base, {nativeProbes: false}));
					});
				} catch (error) {
					log.error('[DebugInfo] Failed to collect desktop debug info:', error);
				}
				try {
					runStartupPhase('main-i18n', initializeMainI18n);
				} catch (error) {
					log.error('[Init] Failed to initialize native i18n:', error);
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
				try {
					runStartupPhase('ipc-handlers', registerIpcHandlers);
				} catch (error) {
					log.error('[Init] Failed to register IPC handlers:', error);
				}
				try {
					const {initOpenH264} = await import('@electron/main/OpenH264Manager');
					initOpenH264();
				} catch (error) {
					log.warn('[Init] OpenH264 initialization skipped:', error);
				}
				try {
					runStartupPhase('autostart-handlers', registerAutostartHandlers);
				} catch (error) {
					log.error('[Init] Failed to register autostart handlers:', error);
				}
				try {
					runStartupPhase('global-key-hook-handlers', registerGlobalKeyHookHandlers);
				} catch (error) {
					log.error('[Init] Failed to register global key hook handlers:', error);
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
					runStartupPhase('vulkan-game-capture-layer', initializeWindowsVulkanGameCaptureLayer);
				} catch (error: unknown) {
					log.error('[Init] Failed to initialize Vulkan game capture layer:', error);
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
					runStartupPhase('application-menu', createApplicationMenu);
				} catch (error: unknown) {
					log.error('[Init] Failed to create application menu:', error);
				}
				runStartupPhase('create-window', () => {
					createWindow({startHidden: shouldStartHiddenAtLogin()});
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
				registerUpdater(getMainWindow);
				app.on('activate', () => {
					const mainWindow = getMainWindow();
					if (mainWindow === null || mainWindow.isDestroyed()) {
						createWindow();
					} else {
						showWindow();
					}
				});
				void startRpcServer().catch((error: unknown) => {
					log.error('[RPC] Failed to start RPC server:', error);
				});
				log.info('App initialized successfully');
			})
			.catch((error: unknown) => {
				log.error('[Startup] whenReady chain rejected:', error);
			});
		app.on('window-all-closed', () => {
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
			armQuitWatchdog('before-quit');
		});
		let quitCleanupStarted = false;
		app.on('will-quit', (event) => {
			if (quitCleanupStarted) return;
			quitCleanupStarted = true;
			log.info('[Shutdown] will-quit cleanup started');
			armQuitWatchdog('will-quit');
			event.preventDefault();
			cleanupIpcHandlers({quitting: true});
			cleanupGlobalKeyHook();
			cleanupNativeAudio();
			cleanupNativeScreenCapture();
			cleanupNativeHardwareEncoderHandlers();
			cleanupVirtmic();
			destroyDesktopTray();
			const asyncCleanups: Array<Promise<unknown>> = [stopRpcServer()];
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
