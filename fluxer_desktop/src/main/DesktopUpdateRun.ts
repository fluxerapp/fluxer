// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DesktopUpdateCheck} from '@electron/main/DesktopUpdateGate';
import type {ModuleLaunchAttempt} from '@electron/main/ModuleStore';

export interface DesktopUpdateProbe {
	readonly shellLatestVersion: string;
	readonly shellNewer: boolean;
	readonly modulesChanged: boolean;
}

export interface ShellSelfUpdateResult {
	readonly reason: string;
	readonly detail: string | null;
}

export interface DesktopUpdateTakeover {
	readonly begin: () => Promise<void>;
	readonly restore: () => void;
	readonly closeApp: () => Promise<void>;
	readonly reopen: (launchAttempt: ModuleLaunchAttempt | null) => void;
}

interface DesktopUpdateLogger {
	readonly info: (message: string, details?: unknown) => void;
	readonly warn: (message: string, details?: unknown) => void;
	readonly error: (message: string, details?: unknown) => void;
}

interface DesktopUpdateRunOptions {
	readonly probe: () => Promise<DesktopUpdateProbe>;
	readonly canSelfUpdateShell: boolean;
	readonly runShellSelfUpdate: () => Promise<ShellSelfUpdateResult>;
	readonly installModules: () => Promise<ModuleLaunchAttempt | null>;
	readonly takeover: DesktopUpdateTakeover;
	readonly publish: (check: DesktopUpdateCheck) => void;
	readonly openDownloadsPage: () => Promise<void>;
	readonly logger: DesktopUpdateLogger;
	readonly now?: () => number;
}

interface FilteredProbe {
	readonly check: DesktopUpdateCheck;
	readonly shellLatestVersion: string;
}

export class DesktopUpdateRun {
	private readonly options: DesktopUpdateRunOptions;
	private readonly now: () => number;
	private launchSettled = false;
	private shellFeedBehindVersion: string | null = null;
	private shellInstallFailed = false;
	private lastCheck: DesktopUpdateCheck | null = null;

	public constructor(options: DesktopUpdateRunOptions) {
		this.options = options;
		this.now = options.now ?? Date.now;
	}

	public markLaunchSettled(): void {
		this.launchSettled = true;
	}

	public async check(): Promise<DesktopUpdateCheck> {
		return (await this.probe()).check;
	}

	public async start(): Promise<void> {
		const {logger, takeover} = this.options;
		const startedAt = this.now();
		if (!this.launchSettled) {
			logger.warn('Ignoring a desktop update start before the renderer confirmed its launch');
			return;
		}
		if (this.shellInstallFailed && this.lastCheck?.modulesChanged !== true) {
			logger.warn('The shell update failed to install this session, opening the downloads page instead');
			await this.options.openDownloadsPage();
			return;
		}
		logger.info('Starting the desktop update');
		const probing = this.probe();
		probing.catch(() => {});
		await takeover.begin();
		logger.info('The update splash took the app windows over', {elapsedMs: this.now() - startedAt});
		let probe: FilteredProbe;
		try {
			probe = await probing;
		} catch (error) {
			logger.warn('The desktop update check failed, giving the app windows back', error);
			takeover.restore();
			return;
		}
		const {check} = probe;
		logger.info('Checked for the desktop update', {...check, elapsedMs: this.now() - startedAt});
		if (!check.shellNewer && !check.modulesChanged) {
			this.publish(check);
			takeover.restore();
			return;
		}
		await takeover.closeApp();
		logger.info('Closed the app windows for the update', {elapsedMs: this.now() - startedAt});
		let launchAttempt: ModuleLaunchAttempt | null = null;
		let remaining = check;
		try {
			if (check.shellNewer) {
				remaining = await this.updateShell(probe);
			}
			if (check.modulesChanged) {
				launchAttempt = await this.options.installModules();
				remaining = {shellNewer: remaining.shellNewer, modulesChanged: false};
				logger.info('Installed the module update', {
					activated: launchAttempt != null,
					elapsedMs: this.now() - startedAt,
				});
			}
		} catch (error) {
			logger.error('The desktop update failed, reopening the app on the installed modules', error);
		} finally {
			this.publish(remaining);
			takeover.reopen(launchAttempt);
		}
	}

	private async updateShell(probe: FilteredProbe): Promise<DesktopUpdateCheck> {
		const {logger} = this.options;
		const result = await this.options.runShellSelfUpdate();
		logger.warn('The shell self update did not restart the app', result);
		if (result.reason === 'no-update') {
			this.shellFeedBehindVersion = probe.shellLatestVersion;
			return {shellNewer: false, modulesChanged: probe.check.modulesChanged};
		}
		if (result.reason === 'install-failed') {
			this.shellInstallFailed = true;
		}
		return probe.check;
	}

	private async probe(): Promise<FilteredProbe> {
		const probe = await this.options.probe();
		const check = {
			shellNewer:
				this.options.canSelfUpdateShell && probe.shellNewer && probe.shellLatestVersion !== this.shellFeedBehindVersion,
			modulesChanged: probe.modulesChanged,
		};
		this.lastCheck = check;
		return {check, shellLatestVersion: probe.shellLatestVersion};
	}

	private publish(check: DesktopUpdateCheck): void {
		this.lastCheck = check;
		this.options.publish(check);
	}
}
