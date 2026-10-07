// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import path from 'node:path';
import {describe, test} from 'node:test';
import {fileURLToPath} from 'node:url';

const source = readFileSync(path.join(path.dirname(fileURLToPath(import.meta.url)), 'Bootstrap.ts'), 'utf8');

describe('Bootstrap entry point', () => {
	test('never top level awaits the module bootstrap', () => {
		assert.doesNotMatch(
			source,
			/^\t*await runModuleBootstrap\(\)/m,
			'Electron emits ready only after the entry module finishes evaluating. Top level awaiting runModuleBootstrap, which itself awaits app.whenReady(), deadlocks: ready cannot fire until evaluation completes and evaluation cannot complete until ready fires. The splash never opens and no module is ever fetched. Keep it detached.',
		);
	});

	test('detaches the module bootstrap and still handles its rejection', () => {
		assert.match(source, /void runModuleBootstrap\(\)\.catch\(/);
	});

	test('never awaits app.whenReady at the top level either', () => {
		assert.doesNotMatch(
			source,
			/^await app\.whenReady\(\)/m,
			'Awaiting ready at module scope deadlocks for the same reason. Inside an async function that is not top level awaited it is correct.',
		);
	});

	test('the flag off branch imports the main app directly only when the asar still carries a renderer', () => {
		assert.match(
			source,
			/case ModuleSystemLaunchDecision\.DISABLED_BY_REQUEST:\n\t\tif \(HAS_OFFLINE_RENDERER\) \{\n\t\t\tawait import\(MAIN_PROCESS_ENTRY_URL\);/,
		);
	});

	test('every MainApp import site is gated on a launch permit or on an offline renderer', () => {
		const sites = source.split('\n').filter((line) => line.includes('import(MAIN_PROCESS_ENTRY_URL)'));
		assert.equal(
			sites.length,
			3,
			'a new MainApp import site must be justified here before it is added. The three are: the launch permit, the flag off branch, and the instance module preference, which only fires once decideModuleSystemDisableRequest has confirmed an offline renderer can take over.',
		);
	});

	test('the instance module preference can only be honoured through the offline renderer guard', () => {
		assert.match(
			source,
			/const instancePreference = decideModuleSystemDisableRequest\(\n\t\tinstanceTurnedModulesOff\(userDataConfig\.base\),\n\t\tHAS_OFFLINE_RENDERER,\n\t\);/,
			'A server side switch that could turn the module system off on a build with no renderer would brick the install, so it goes through the same guard the launch flag does.',
		);
		assert.match(source, /case ModuleSystemDisableDecision\.HONOURED:\n\t\t\tlogger\.warn\(/);
	});

	test('the instance module preference is read from the module store root, never from the app store database', () => {
		assert.doesNotMatch(source, /getDesktopAppStorage/);
		assert.match(source, /instanceTurnedModulesOff\(userDataConfig\.base\)/);
	});

	test('the instance module preference is read once during the boot, so flipping it mid session only reaches the next boot', () => {
		assert.equal(
			source.split('instanceTurnedModulesOff(').length - 1,
			1,
			'instanceTurnedModulesOff re-reads the file off disk on every call, so a second read from the module poll or from any later re-check would tear the module system out from under a running session. The switch is a boot decision.',
		);
		const bootstrap = source.slice(source.indexOf('async function runModuleBootstrap()'));
		assert.ok(bootstrap.indexOf('instanceTurnedModulesOff(') < bootstrap.indexOf('await app.whenReady()'));
	});

	test('an ignored disable override is logged rather than silently swallowed', () => {
		assert.match(
			source,
			/if \(MODULE_SYSTEM_LAUNCH\.kind === ModuleSystemLaunchDecision\.ENABLED_WITH_IGNORED_DISABLE_REQUEST\) \{/,
		);
		assert.match(source, /case ModuleSystemDisableDecision\.IGNORED_WITHOUT_OFFLINE_RENDERER:\n\t\t\tlogger\.warn\(/);
	});

	test('the deliberate floor bypass on the rollback path reaches the log', () => {
		assert.match(source, /if \(permit\.belowFloor\.length > 0\) \{\n\t+logger\.warn\(/);
		assert.match(
			source,
			/logger\.warn\(\n\t+'Launching a module set the persisted floor forbids[^']*',\n\t+\{modules: permit\.belowFloor\},\n\t+\);/,
			'The launch permit only carries belowFloor on the rollback path, so the module names are the whole point of the warning. Without them the log says a floor was bypassed and never says which modules did it.',
		);
	});

	test('a required shell update never blocks on a retry the platform cannot satisfy', () => {
		assert.doesNotMatch(
			source,
			/blockUntilRelaunch\('blocked-shell-update'\)/,
			'Relaunching runs the same binary, refetches the same manifest and blocks again. On Linux, portable and Flatpak the shell cannot self update at all, so a retry is an unescapable loop.',
		);
		assert.match(source, /const plan = resolveShellUpdatePlan\(\);/);
		assert.match(source, /armBlockedShellUpdate\(plan, outcome\.latestVersion, outcome\.requiredSecurityUpdate\);/);
	});

	test('a shell that can update itself tries to, and still falls through to the manual screen when it cannot', () => {
		assert.match(source, /if \(plan\.capability === ShellUpdateCapability\.SELF_UPDATE\) \{/);
		assert.match(source, /const failure = await runShellSelfUpdate\(plan, \{/);
		assert.match(
			source,
			/logger\.error\('The shell self update failed, falling back to a manual download', failure\);\n\t{5}\}\n\t{5}openSplashWindow\(\);\n\t{5}armBlockedShellUpdate\(plan, outcome\.latestVersion, outcome\.requiredSecurityUpdate\);/,
			'The manual screen is the fallback, so arming it must sit after the self update attempt rather than inside its else branch.',
		);
	});

	test('the self update is imported inside the update loop, never at the top level', () => {
		assert.doesNotMatch(
			source,
			/^(const \{runShellSelfUpdate\}|await import\('@electron\/main\/ShellSelfUpdate'\))/m,
			'Importing the self update at module scope would pull electron and velopack into the entry evaluation, which is exactly what deadlocks ready.',
		);
		assert.match(source, /\t+const \{runShellSelfUpdate\} = await import\('@electron\/main\/ShellSelfUpdate'\);/);
	});

	test('the self update paints its own progress rather than leaving the checking screen up', () => {
		assert.match(
			source,
			/status: 'shell-update-downloading',\n\t+requiredSecurityUpdate: outcome\.requiredSecurityUpdate,\n\t+progress,/,
		);
		assert.match(
			source,
			/status: 'shell-update-restarting',\n\t+requiredSecurityUpdate: outcome\.requiredSecurityUpdate,/,
		);
	});

	test('the retry affordance still belongs to the module update block', () => {
		assert.match(source, /setSplashState\(\{status, action: SplashAction\.RETRY, message\}\);/);
		assert.match(
			source,
			/return await blockUntilRelaunch\(\n\t+outcome\.requiredSecurityUpdate \? 'blocked-security-update-required' : 'blocked-update-required',\n\t+\);/,
		);
	});

	test('the retry listener is registered once, beside the quit listener, never inside the block helper', () => {
		assert.equal(source.split('\n').filter((line) => line.includes('onSplashRetry(')).length, 1);
		assert.match(
			source,
			/onSplashQuit\(\(\) => \{\n\t\tlogger\.info\('The splash requested a quit'\);\n\t\tapp\.quit\(\);\n\t\}\);\n\tonSplashRetry\(/,
			'Two reachable blockUntilRelaunch calls would otherwise register two listeners and one click would relaunch twice.',
		);
	});

	test('refusing to start never top level awaits either', () => {
		assert.doesNotMatch(source, /^await refuseUnsupportedBuild\(/m);
		assert.match(source, /void refuseUnsupportedBuild\(/);
	});

	test('the single instance lock is taken before the bootstrap touches anything a second process could corrupt', () => {
		const bootstrap = source.slice(source.indexOf('async function runModuleBootstrap()'));
		const lock = bootstrap.indexOf('app.requestSingleInstanceLock()');
		assert.notEqual(
			lock,
			-1,
			'index.ts only requests the lock once the whole bootstrap has finished, and the blocked update paths never reach index.ts at all. Until the lock moves up here two processes race ModuleStore.open, each burn a boot attempt against the shared state and two of those roll the next cold start back to the older module set.',
		);
		assert.ok(
			lock > bootstrap.indexOf('configureUserDataPath()'),
			'The lock is keyed on the user data directory, so requesting it before that directory is configured would lock the wrong profile.',
		);
		assert.ok(lock < bootstrap.indexOf('await app.whenReady()'));
		assert.ok(lock < bootstrap.indexOf('openSplashWindow()'));
		assert.ok(lock < bootstrap.indexOf('ModuleStore.open('));
	});

	test('a second instance quits instead of stranding its launch on a splash that never resolves', () => {
		assert.match(
			source,
			/if \(!app\.requestSingleInstanceLock\(\)\) \{\n\t+logger\.info\([^\n]*\);\n\t+app\.quit\(\);\n\t+return;\n\t\}/,
			'blockUntilRelaunch and the blocked-shell-update branch both return a promise that never settles, so a second launch that meets a required update manifest would sit on a splash forever with its argv and its deep link never forwarded.',
		);
	});

	test('the lock that keeps the second process out also arms the forwarding of its launch', () => {
		const bootstrap = source.slice(source.indexOf('async function runModuleBootstrap()'));
		const lock = bootstrap.indexOf('app.requestSingleInstanceLock()');
		const arm = bootstrap.indexOf('armSecondInstanceForwarding()');
		assert.notEqual(
			arm,
			-1,
			'Electron discards a forwarded second-instance launch when nothing is listening, and index.ts only attaches the real handler once the update loop has resolved. Without a listener armed here every deep link, jump list task and plain focus that arrives during the splash is dropped, permanently so on the blocked paths that never reach index.ts at all.',
		);
		assert.ok(
			arm > lock,
			'Arming before the lock is granted would attach a listener in the process that is about to quit.',
		);
		assert.ok(arm < bootstrap.indexOf('await app.whenReady()'));
		assert.ok(arm < bootstrap.indexOf('openSplashWindow()'));
	});

	test('a splash that will never launch still raises itself when a second launch is forwarded', () => {
		assert.match(
			source,
			/setSplashState\(\{status, action: SplashAction\.RETRY, message\}\);\n\t+setSecondInstanceSink\(focusSplashWindow\);/,
			'blockUntilRelaunch never settles, so index.ts never installs handleSecondInstance and the buffered launches would sit there forever. Pointing the sink at the splash is the only response left.',
		);
		assert.match(
			source,
			/armBlockedShellUpdate\(plan, outcome\.latestVersion, outcome\.requiredSecurityUpdate\);\n\t+setSecondInstanceSink\(focusSplashWindow\);/,
		);
	});

	test('an unreachable update server is retried in process instead of waiting for a click', () => {
		const bootstrap = source.slice(source.indexOf('async function runModuleBootstrap()'));
		assert.match(
			bootstrap,
			/if \(outcome\.updateServerUnreachable\) \{\n\t+showBlockedSplash\([^;]*UPDATE_SERVER_UNREACHABLE_MESSAGE,\n\t+\);\n\t+await updateServerRetry\.holdUntilNextAttempt\(\);\n[^\n]*\n\t+return null;/,
			'blockUntilRelaunch never settles, so a splash that reached it while the network was down stayed there after the network came back until the user pressed Retry now.',
		);
		assert.match(bootstrap, /onSplashNetworkOnline\(\(\) => \{\n[^\n]*\n\t+updateServerRetry\.networkReturned\(\);/);
		assert.match(
			bootstrap,
			/sleep: updateServerRetry\.sleep,/,
			'The updater sleeps between its own attempts, and a network that returns during that sleep has to cut it short.',
		);
		assert.match(
			bootstrap,
			/if \(updateServerRetry\.holding\) \{\n\t+if \(!updateServerRetry\.endsHold\(state\.status\)\) \{\n\t+return;\n\t+\}\n\t+releaseSplashAffordance\(\);/,
			'The splash latches the unreachable screen, so the retry has to release it once the server answers or the download would run behind a screen that says the server cannot be reached.',
		);
	});

	test('the module poll tracks the live main window rather than the one the boot handoff latched', () => {
		assert.doesNotMatch(
			source,
			/onMainWindowCreated\(\(window\) => \{\n\t\t\tmainWindow = window;/,
			'onMainWindowCreated is a one shot boot handoff. Closing the window and reopening it from the dock or the tray would leave the poll holding a destroyed window forever, so every later tick would short circuit and no module version would ever land again.',
		);
		assert.match(source, /observeMainWindow\(\(window\) => \{\n\t\t\tmainWindow = window;\n\t\t\}\);/);
	});

	test('a module poll that went quiet says so when it comes back', () => {
		assert.match(
			source,
			/if \(failing\) \{\n\t+failing = false;\n\t+logger\.info\('The module poll recovered'\);\n\t+\}/,
			'The failure is logged once and then suppressed until recovery. Without a line on the way back, a reader of the log cannot tell a poll that recovered from one that is still failing, which is the whole question that warning raises.',
		);
	});

	test('an activated module set that cannot reach the renderer releases the pending launch', () => {
		assert.match(
			source,
			/\} catch \(error\) \{\n\t+updater\.abandonPendingLaunch\(refresh\.launchAttempt\);\n\t+throw error;/,
			'refreshInstalledModules holds a pending launch from the moment it activates. If refreshing the module roots, the closed window check or reloadIgnoringCache throws, the poll only logs, and every later poll returns awaiting-renderer for the rest of the session.',
		);
	});

	test('a reload the renderer never confirms releases the pending launch too', () => {
		assert.doesNotMatch(
			source,
			/webContents\.once\('did-finish-load'/,
			'A bare one shot only settles when the load lands. Nothing throws when the window is destroyed mid reload, so the pending launch would stay held and every later poll would return awaiting-renderer for the rest of the session.',
		);
		assert.match(
			source,
			/const stopWatchingReload = watchModuleRendererReload\(\{\n\t+target: webContents,\n\t+observeLaunchConfirmed: observeRendererLaunchConfirmed,\n\t+onLoaded: markReloadSucceeded,\n\t+onAbandoned: \(reason\) => \{/,
		);
		assert.match(source, /releasePendingLaunch\(\);\n\t+\},\n\t+\}\);/);
		assert.match(
			source,
			/\} catch \(error\) \{\n\t+stopWatchingReload\(\);\n\t+releasePendingLaunch\(\);/,
			'The reload runs from a user click, not from the poll, so nothing upstream is left to catch a rethrow. It has to release the pending launch itself or every later poll returns awaiting-renderer for the rest of the session.',
		);
	});

	test('an activated module set is offered to the renderer rather than reloaded underneath it', () => {
		assert.match(
			source,
			/offerPendingModuleUpdate\(\{\n\t+update: \{modules: refresh\.modules\},\n\t+apply: reloadRenderer,\n\t+discard: releasePendingLaunch,\n\t+\}\);/,
			'A silent reloadIgnoringCache throws away whatever the user was typing. The poll hands the activated set to the gate and the renderer reloads when the user asks for it.',
		);
		assert.doesNotMatch(
			source,
			/\}\);\n\t+webContents\.reloadIgnoringCache\(\);/,
			'reloadIgnoringCache belongs inside the applied callback, never on the poll path.',
		);
	});

	test('a superseded or discarded module update releases the pending launch', () => {
		assert.match(
			source,
			/discard: releasePendingLaunch,/,
			'The gate holds at most one offer. Dropping one without releasing its launch attempt would leave refreshInstalledModules returning awaiting-renderer for the rest of the session.',
		);
	});

	test('a window that closed before the user applied the update does not reload a destroyed target', () => {
		assert.match(
			source,
			/const reloadRenderer = \(\): void => \{\n\t+if \(window\.isDestroyed\(\)\) \{/,
			'The offer can sit unapplied for as long as the user leaves it. The window it captured may be gone by the time they click.',
		);
	});
	test('a minimized login launch converges the modules without flashing the splash', () => {
		const bootstrap = source.slice(source.indexOf('async function runModuleBootstrap()'));
		assert.match(
			bootstrap,
			/await app\.whenReady\(\);\n\n\tif \(isStartMinimizedLaunch\(\)\) \{\n\t\tlogger\.info\([^\n]*\);\n\t\} else \{\n\t\topenSplashWindow\(\);\n\t\}\n\tsetSplashState\(\{status: 'checking-for-updates'\}\);/,
			'index.ts starts the main window hidden on an autostart launch with start minimized, so a splash opened here unconditionally flashes on every login anyway.',
		);
		assert.ok(bootstrap.indexOf('loadDesktopConfig(userDataConfig.base)') < bootstrap.indexOf('await app.whenReady()'));
		assert.match(source, /const \{isStartMinimizedLaunch\} = await import\('@electron\/main\/AutostartLaunch'\);/);
		assert.doesNotMatch(
			source,
			/import\('@electron\/main\/Autostart'\)/,
			'Autostart.ts pulls the main process i18n and the Linux portals in. The bootstrap only needs the launch check, which lives in the light AutostartLaunch module.',
		);
	});

	test('a quiet launch still surfaces the splash for every state that needs the user', () => {
		assert.match(
			source,
			/const showBlockedSplash = \(status: SplashStatus, message: string \| null = null\): void => \{\n\t\topenSplashWindow\(\);\n\t\tsetSplashState\(\{status, action: SplashAction\.RETRY, message\}\);/,
			'A blocked or failed boot never reaches the main window, so a minimized login launch would otherwise sit invisible forever with nothing in the tray to bring it back.',
		);
		assert.match(
			source,
			/openSplashWindow\(\);\n\t+armBlockedShellUpdate\(plan, outcome\.latestVersion, outcome\.requiredSecurityUpdate\);/,
		);
	});

	test('the pre-ready Chromium configuration runs before the bootstrap waits for ready', () => {
		const configure = source.indexOf('applyPreReadyChromiumConfiguration(userDataConfig.channel, process.argv);');
		const gpuWorkaround = source.indexOf('await appendWindowsGpuDriverWorkaroundSwitches();');
		const ready = source.indexOf('await app.whenReady();\n\n\tif (isStartMinimizedLaunch())');
		assert.ok(configure > 0 && ready > 0 && gpuWorkaround > 0);
		assert.ok(
			configure < ready && gpuWorkaround < ready,
			'disableHardwareAcceleration throws after ready, and switches or feature flags appended after ready never reach the GPU process or the FeatureList',
		);
		assert.ok(source.indexOf('loadDesktopConfig(userDataConfig.base);') < configure);
	});

	test('open-url is claimed at module evaluation, before any await can let ready pass', () => {
		assert.match(source, /^armOpenUrlForwarding\(\);$/m);
		assert.ok(source.indexOf('armOpenUrlForwarding();') < source.indexOf('switch (MODULE_SYSTEM_LAUNCH.kind)'));
	});

	test('the Windows taskbar identity is set before the splash or any other window exists', () => {
		assert.match(
			source,
			/repairWindowsShortcuts\(\);\n\telectronApp\.setToastActivatorCLSID\(WINDOWS_TOAST_ACTIVATOR_CLSID\);\n\telectronApp\.setAppUserModelId\(WINDOWS_APP_USER_MODEL_ID\);\n\}/,
		);
	});
});
