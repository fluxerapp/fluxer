// SPDX-License-Identifier: AGPL-3.0-or-later

import {errorMessage as describeFailure} from '@electron/common/ErrorMessage';
import {type DesktopUpdateCheck, publishDesktopUpdateCheck} from '@electron/main/DesktopUpdateGate';
import type {ModuleLaunchAttempt} from '@electron/main/ModuleStore';
import type {ShellUpdateCapability, ShellUpdatePlan} from '@electron/main/ShellUpdateCapability';

export interface DesktopUpdateProbe {
	readonly shellLatestVersion: string;
	readonly shellNewer: boolean;
	readonly modulesChanged: boolean;
}

interface ShellSelfUpdateResult {
	readonly reason: string;
	readonly detail: string | null;
}

export interface DesktopUpdateTakeover {
	readonly reloadInPlace: (launchAttempt: ModuleLaunchAttempt) => Promise<void>;
}

interface DesktopUpdateLogger {
	readonly info: (message: string, details?: unknown) => void;
	readonly warn: (message: string, details?: unknown) => void;
	readonly error: (message: string, details?: unknown) => void;
}

interface DesktopUpdateRunOptions {
	readonly probe: () => Promise<DesktopUpdateProbe>;
	readonly stageModules: () => Promise<boolean>;
	readonly activateStagedModules: () => Promise<ModuleLaunchAttempt | null>;
	readonly shellUpdatePlan: ShellUpdatePlan;
	readonly takeover: DesktopUpdateTakeover;
	readonly openDownloadsPage: () => Promise<void>;
	readonly reportFailure?: (failure: ShellSelfUpdateResult) => void;
	readonly logger: DesktopUpdateLogger;
	readonly now?: () => number;
}

const SHELL_FEED_BEHIND_RETRY_MS = 10 * 60_000;
const BOOT_STATE_FLUSH_TIMEOUT_MS = 1500;

type ShellSelfUpdatePlan = Extract<ShellUpdatePlan, {capability: typeof ShellUpdateCapability.SELF_UPDATE}>;

export class DesktopUpdateRun {
	private readonly options: DesktopUpdateRunOptions;
	private readonly now: () => number;
	private readonly shellPlan: ShellSelfUpdatePlan | null;
	private launchSettled = false;
	private shellFeedBehind: {readonly version: string; readonly at: number} | null = null;
	private shellInstallFailed = false;
	private lastCheck: DesktopUpdateCheck = {shellReady: false, modulesReady: false};

	public constructor(options: DesktopUpdateRunOptions) {
		this.options = options;
		this.now = options.now ?? Date.now;
		const plan = options.shellUpdatePlan;
		this.shellPlan = 'updater' in plan ? plan : null;
	}

	public markLaunchSettled(): void {
		this.launchSettled = true;
	}

	public async check({manual = false}: {readonly manual?: boolean} = {}): Promise<DesktopUpdateCheck> {
		const {logger} = this.options;
		if (manual) {
			this.shellFeedBehind = null;
		}
		const startedAt = this.now();
		const probe = await this.options.probe();
		const modulesReady = probe.modulesChanged && (await this.options.stageModules());
		const shellReady = await this.stageShell(probe);
		const check = {shellReady, modulesReady};
		if (shellReady || modulesReady) {
			logger.info('Staged the desktop update in the background', {...check, elapsedMs: this.now() - startedAt});
		}
		this.lastCheck = check;
		return check;
	}

	public async start(): Promise<void> {
		const {logger} = this.options;
		const startedAt = this.now();
		if (!this.launchSettled) {
			logger.warn('Ignoring a desktop update start before the renderer confirmed its launch');
			return;
		}
		const check = this.lastCheck;
		if (check.shellReady) {
			if (this.shellInstallFailed && !check.modulesReady) {
				logger.warn('The shell update failed to install this session, opening the downloads page instead');
				await this.options.openDownloadsPage();
				return;
			}
			await this.applyShell(check, startedAt);
			return;
		}
		if (check.modulesReady) {
			await this.applyModulesInPlace(check, startedAt);
		}
	}

	private async stageShell(probe: DesktopUpdateProbe): Promise<boolean> {
		const plan = this.shellPlan;
		if (plan == null || !probe.shellNewer || this.isShellFeedBehind(probe.shellLatestVersion)) {
			return false;
		}
		const {stageShellUpdate} = await import('@electron/main/ShellSelfUpdate');
		const result = await stageShellUpdate(plan);
		if (result.status === 'staged') {
			return true;
		}
		if (result.failure.reason === 'no-update') {
			this.shellFeedBehind = {version: probe.shellLatestVersion, at: this.now()};
		}
		return false;
	}

	private async applyShell(check: DesktopUpdateCheck, startedAt: number): Promise<void> {
		const {logger} = this.options;
		await this.flushBootState();
		const {applyStagedShellUpdate} = await import('@electron/main/ShellSelfUpdate');
		const failure = applyStagedShellUpdate();
		if (failure == null) {
			logger.info('Applying the staged shell update, the app restarts now', {elapsedMs: this.now() - startedAt});
			return;
		}
		logger.error('The staged shell update failed to apply', failure);
		if (failure.reason === 'install-failed') {
			this.shellInstallFailed = true;
		}
		this.publish({shellReady: false, modulesReady: check.modulesReady});
		this.options.reportFailure?.(failure);
	}

	private async applyModulesInPlace(check: DesktopUpdateCheck, startedAt: number): Promise<void> {
		const {logger, takeover} = this.options;
		await this.flushBootState();
		let launchAttempt: ModuleLaunchAttempt | null;
		try {
			launchAttempt = await this.options.activateStagedModules();
		} catch (error) {
			logger.error(
				'The staged module update failed to activate, the app keeps running on the installed modules',
				error,
			);
			this.publish({shellReady: check.shellReady, modulesReady: false});
			this.options.reportFailure?.({reason: 'install-failed', detail: describeFailure(error)});
			return;
		}
		this.publish({shellReady: check.shellReady, modulesReady: false});
		logger.info('Activated the staged module update in place', {
			activated: launchAttempt != null,
			elapsedMs: this.now() - startedAt,
		});
		if (launchAttempt != null) {
			const {prepareBootStateReload} = await import('@electron/main/BootState');
			await prepareBootStateReload();
			await takeover.reloadInPlace(launchAttempt);
		}
	}

	private async flushBootState(): Promise<void> {
		const [{requestBootStateFlush}, {getMainWindow}] = await Promise.all([
			import('@electron/main/BootState'),
			import('@electron/main/Window'),
		]);
		const result = await requestBootStateFlush(getMainWindow()?.webContents ?? null, BOOT_STATE_FLUSH_TIMEOUT_MS);
		this.options.logger.info('Flushed the boot state before applying the update', {result});
	}

	private isShellFeedBehind(version: string): boolean {
		const behind = this.shellFeedBehind;
		return behind != null && behind.version === version && this.now() - behind.at < SHELL_FEED_BEHIND_RETRY_MS;
	}

	private publish(check: DesktopUpdateCheck): void {
		this.lastCheck = check;
		publishDesktopUpdateCheck(check);
	}
}
