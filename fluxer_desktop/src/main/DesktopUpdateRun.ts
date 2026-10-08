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
	readonly reloadInPlace: (launchAttempt: ModuleLaunchAttempt) => Promise<void>;
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
	readonly reportFailure?: (failure: ShellSelfUpdateResult) => void;
	readonly logger: DesktopUpdateLogger;
	readonly now?: () => number;
}

interface FilteredProbe {
	readonly check: DesktopUpdateCheck;
	readonly shellLatestVersion: string;
}

function describeFailure(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
}

export class DesktopUpdateRun {
	private readonly options: DesktopUpdateRunOptions;
	private readonly now: () => number;
	private launchSettled = false;
	private shellFeedBehindVersion: string | null = null;
	private shellInstallFailed = false;
	private lastShellFailure: ShellSelfUpdateResult | null = null;
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
		const {logger} = this.options;
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
		if (this.lastCheck != null && !this.lastCheck.shellNewer && this.lastCheck.modulesChanged) {
			await this.startInPlace(startedAt);
			return;
		}
		logger.info('Starting the desktop update');
		const probing = this.probe();
		probing.catch(() => {});
		await this.startWithTakeover(startedAt, probing);
	}

	private async startInPlace(startedAt: number): Promise<void> {
		const {logger, takeover} = this.options;
		logger.info('Starting the module update in place, the shell stays as it is');
		let probe: FilteredProbe;
		try {
			probe = await this.probe();
		} catch (error) {
			logger.warn('The desktop update check failed, leaving the app as it is', error);
			this.options.reportFailure?.({reason: 'check-failed', detail: describeFailure(error)});
			return;
		}
		const {check} = probe;
		if (check.shellNewer) {
			logger.info('A newer shell appeared since the last check, updating it behind the splash');
			await this.startWithTakeover(startedAt, Promise.resolve(probe));
			return;
		}
		if (!check.modulesChanged) {
			this.publish(check);
			return;
		}
		let launchAttempt: ModuleLaunchAttempt | null;
		try {
			launchAttempt = await this.options.installModules();
		} catch (error) {
			logger.error('The module update failed, the app keeps running on the installed modules', error);
			this.publish(check);
			this.options.reportFailure?.({reason: 'download-failed', detail: describeFailure(error)});
			return;
		}
		this.publish({shellNewer: false, modulesChanged: false});
		logger.info('Installed the module update in place', {
			activated: launchAttempt != null,
			elapsedMs: this.now() - startedAt,
		});
		if (launchAttempt != null) {
			await takeover.reloadInPlace(launchAttempt);
		}
	}

	private async startWithTakeover(startedAt: number, probing: Promise<FilteredProbe>): Promise<void> {
		const {logger, takeover} = this.options;
		await takeover.begin();
		logger.info('The update splash took the app windows over', {elapsedMs: this.now() - startedAt});
		let probe: FilteredProbe;
		try {
			probe = await probing;
		} catch (error) {
			logger.warn('The desktop update check failed, giving the app windows back', error);
			takeover.restore();
			this.options.reportFailure?.({reason: 'check-failed', detail: describeFailure(error)});
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
		let failure: ShellSelfUpdateResult | null = null;
		try {
			if (check.shellNewer) {
				remaining = await this.updateShell(probe);
				failure = this.lastShellFailure;
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
			failure = {reason: 'download-failed', detail: describeFailure(error)};
		} finally {
			this.publish(remaining);
			takeover.reopen(launchAttempt);
		}
		if (failure != null) {
			this.options.reportFailure?.(failure);
		}
	}

	private async updateShell(probe: FilteredProbe): Promise<DesktopUpdateCheck> {
		const {logger} = this.options;
		const result = await this.options.runShellSelfUpdate();
		logger.warn('The shell self update did not restart the app', result);
		this.lastShellFailure = result.reason === 'no-update' ? null : result;
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
