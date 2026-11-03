// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {BUILD_CHANNEL} from '@electron/common/BuildChannel';
import {errorMessage} from '@electron/common/ErrorMessage';
import {jitteredExponentialDelay} from '@electron/common/ExponentialBackoff';
import type {DesktopUpdateProbe} from '@electron/main/DesktopUpdateRun';
import {ModuleDownloadRate} from '@electron/main/ModuleDownloadRate';
import {type DesktopModuleUpdateManifest, resolveDesktopModuleArchitecture} from '@electron/main/ModuleManifest';
import {
	type ModuleManifestDocument,
	ModuleManifestFetchError,
	ModuleManifestRepository,
} from '@electron/main/ModuleManifestRepository';
import {
	describeErrorChain,
	isDeterministicPackageFailure,
	isModulePackageStall,
	isTransientServerStatus,
	ModulePackageFetchError,
	ModulePackageInstaller,
	type ModulePackageInstallerReport,
	ModulePackageInstallerReportType,
	ModulePackageInstallPhase,
} from '@electron/main/ModulePackageInstaller';
import {
	MODULE_BOOT_ATTEMPT_ROLLBACK_THRESHOLD,
	type ModuleLaunchAttempt,
	ModuleManifestEquivocationError,
	type ModuleManifestFeedIdentity,
	type ModuleManifestFeedObservation,
	ModuleManifestRollbackError,
	type ModuleStore,
	sameModuleMap,
} from '@electron/main/ModuleStore';
import {
	type ModulePlan,
	type ModulePlanItem,
	ModulePlanItemRequirement,
	ModuleUnreachableLaunchDecision,
	ModuleUpdatePlanner,
	ModuleUpdaterBlockReason,
	type ServedModuleSelection,
} from '@electron/main/ModuleUpdatePlanner';
import {compareModuleVersions, type ModuleVersion, parseModuleVersion} from '@electron/main/ModuleVersion';
import {resolveDesktopPackageOrigin} from '@electron/main/ShellDownloadFormats';
import type {DesktopModuleEnsureResult} from '@fluxer/desktop_ipc/src/ModuleContract';
import {DESKTOP_RENDERER_MODULE_NAME, DesktopModuleEnsureStatus} from '@fluxer/desktop_ipc/src/ModuleContract';

const MODULE_UPDATER_BACKOFF_BASE_MS = 1000;
const MODULE_UPDATER_BACKOFF_CAP_MS = 30000;
const MODULE_UPDATER_NETWORK_ERROR_SAMPLE_RATE = 0.01;

const MODULE_ENSURE_MANIFEST_BACKOFF_MS = 30000;
const CAUSE_CHAIN_LIMIT = 8;
const STORAGE_FULL_CODES = new Set(['ENOSPC', 'EDQUOT', 'EROFS', 'EFBIG']);

export const ModuleUpdaterStatus = Object.freeze({
	CHECKING: 'checking',
	DOWNLOADING: 'downloading',
	INSTALLING: 'installing',
	VERIFYING: 'verifying',
	RETRY_WAIT: 'retry-wait',
	BLOCKED_UPDATE_REQUIRED: 'blocked-update-required',
	BLOCKED_SHELL_UPDATE: 'blocked-shell-update',
	LAUNCHING: 'launching',
	UNREACHABLE_LAUNCH: 'unreachable-launch',
} as const);

export type ModuleUpdaterStatus = (typeof ModuleUpdaterStatus)[keyof typeof ModuleUpdaterStatus];

const ModuleUpdaterReportType = Object.freeze({
	NETWORK_ERROR: 'network-error',
	MANIFEST_ROLLBACK: 'manifest-rollback',
	MANIFEST_EQUIVOCATION: 'manifest-equivocation',
	PACKAGE_HASH_MISMATCH: 'package-hash-mismatch',
	PACKAGE_MISSING: 'package-missing',
	PACKAGE_REJECTED: 'package-rejected',
	STORAGE_ERROR: 'storage-error',
	ROLLBACK: 'rollback',
	ROLLBACK_FLOOR_BYPASS: 'rollback-floor-bypass',
	BUNDLED_RENDERER_PREFERRED: 'bundled-renderer-preferred',
	SHELL_INCOMPATIBLE_RETIRED: 'shell-incompatible-retired',
	BLOCKED: 'blocked',
} as const);

type ModuleUpdaterReportType = (typeof ModuleUpdaterReportType)[keyof typeof ModuleUpdaterReportType];

const MODULE_UPDATER_REPORT_SAMPLE_RATES: Readonly<Record<ModuleUpdaterReportType, number>> = Object.freeze({
	[ModuleUpdaterReportType.NETWORK_ERROR]: MODULE_UPDATER_NETWORK_ERROR_SAMPLE_RATE,
	[ModuleUpdaterReportType.MANIFEST_ROLLBACK]: 1,
	[ModuleUpdaterReportType.MANIFEST_EQUIVOCATION]: 1,
	[ModuleUpdaterReportType.PACKAGE_HASH_MISMATCH]: 1,
	[ModuleUpdaterReportType.PACKAGE_MISSING]: 1,
	[ModuleUpdaterReportType.PACKAGE_REJECTED]: 1,
	[ModuleUpdaterReportType.STORAGE_ERROR]: 1,
	[ModuleUpdaterReportType.ROLLBACK]: 1,
	[ModuleUpdaterReportType.ROLLBACK_FLOOR_BYPASS]: 1,
	[ModuleUpdaterReportType.BUNDLED_RENDERER_PREFERRED]: 1,
	[ModuleUpdaterReportType.SHELL_INCOMPATIBLE_RETIRED]: 1,
	[ModuleUpdaterReportType.BLOCKED]: 1,
});

export {ModuleUpdaterBlockReason};

export interface ModuleUpdaterSplashState {
	readonly status: ModuleUpdaterStatus;
	readonly current: number | null;
	readonly total: number | null;
	readonly progress: number | null;
	readonly seconds: number | null;
	readonly moduleName: string | null;
	readonly detail: string | null;
	readonly receivedBytes: number | null;
	readonly totalBytes: number | null;
	readonly bytesPerSecond: number | null;
	readonly stalled: boolean;
}

export interface ModuleUpdaterDiagnostics {
	readonly state: ModuleUpdaterSplashState;
	readonly packageOrigin: string;
	readonly lastError: string | null;
	readonly lastErrorAt: number | null;
}

interface ModuleUpdaterReport {
	readonly type: ModuleUpdaterReportType;
	readonly module: string | null;
	readonly message: string;
	readonly sampleRate: number;
	readonly error: unknown;
}

export type ModuleUpdaterOutcome =
	| {
			readonly status: typeof ModuleUpdaterStatus.LAUNCHING;
			readonly committed: Readonly<Record<string, string>>;
			readonly rolledBack: boolean;
			readonly belowFloor: ReadonlyArray<string>;
			readonly launchAttempt: ModuleLaunchAttempt;
	  }
	| {
			readonly status: typeof ModuleUpdaterStatus.UNREACHABLE_LAUNCH;
			readonly committed: Readonly<Record<string, string>>;
			readonly launchAttempt: ModuleLaunchAttempt;
	  }
	| {
			readonly status: typeof ModuleUpdaterStatus.BLOCKED_UPDATE_REQUIRED;
			readonly reason: ModuleUpdaterBlockReason;
			readonly modules: ReadonlyArray<string>;
			readonly message: string;
			readonly updateServerUnreachable: boolean;
	  }
	| {
			readonly status: typeof ModuleUpdaterStatus.BLOCKED_SHELL_UPDATE;
			readonly latestVersion: string;
			readonly minimumVersion: string;
	  };

class ModuleUpdaterStorageFullError extends Error {
	public constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'ModuleUpdaterStorageFullError';
	}
}

function hasErrorCode(error: unknown, codes: ReadonlySet<string>): boolean {
	let cursor: unknown = error;
	for (let depth = 0; depth < CAUSE_CHAIN_LIMIT && cursor != null; depth += 1) {
		const code = (cursor as {code?: unknown}).code;
		if (typeof code === 'string' && codes.has(code)) {
			return true;
		}
		cursor = (cursor as {cause?: unknown}).cause;
	}
	return false;
}

function isStorageFullError(error: unknown): boolean {
	return hasErrorCode(error, STORAGE_FULL_CODES);
}

function isUpdateServerUnreachable(error: unknown): boolean {
	let cursor: unknown = error;
	for (let depth = 0; depth < CAUSE_CHAIN_LIMIT && cursor != null; depth += 1) {
		if (cursor instanceof ModuleManifestFetchError || cursor instanceof ModulePackageFetchError) {
			return isTransientServerStatus(cursor.status);
		}
		cursor = (cursor as {cause?: unknown}).cause;
	}
	return false;
}

function parseBundledRendererVersion(source: string | null): ModuleVersion | null {
	if (source == null) {
		return null;
	}
	try {
		return parseModuleVersion(source, 'bundled renderer version');
	} catch {
		return null;
	}
}

function defaultSleep(ms: number): Promise<void> {
	return new Promise((resolve) => {
		setTimeout(resolve, ms);
	});
}

class ModuleUpdaterBackoff {
	private readonly random: () => number;
	private attempt = 0;
	private ceiling = false;

	public constructor(random: () => number) {
		this.random = random;
	}

	public get reachedCeiling(): boolean {
		return this.ceiling;
	}

	public fail(): number {
		const exponential = MODULE_UPDATER_BACKOFF_BASE_MS * 2 ** this.attempt;
		this.attempt += 1;
		if (exponential >= MODULE_UPDATER_BACKOFF_CAP_MS) {
			this.ceiling = true;
		}
		return jitteredExponentialDelay(
			MODULE_UPDATER_BACKOFF_BASE_MS,
			MODULE_UPDATER_BACKOFF_CAP_MS,
			this.attempt - 1,
			this.random,
		);
	}

	public reset(): void {
		this.attempt = 0;
		this.ceiling = false;
	}
}

interface ModulePlanResult {
	readonly desired: Readonly<Record<string, string>>;
	readonly installed: ReadonlyArray<string>;
	readonly blocked: ReadonlyArray<string>;
	readonly blockedError: unknown;
}

const ModuleManifestObservationMode = Object.freeze({
	READ_ONLY: 'read-only',
	RECORD_FETCH: 'record-fetch',
} as const);

type ModuleManifestObservationMode = (typeof ModuleManifestObservationMode)[keyof typeof ModuleManifestObservationMode];

interface ModuleUpdaterOptions {
	readonly store: ModuleStore;
	readonly shellVersion: string;
	readonly releaseChannel?: string;
	readonly platform?: string;
	readonly arch?: NodeJS.Architecture;
	readonly packageOrigin?: string;
	readonly hasOfflineRenderer?: boolean;
	readonly bundledRendererVersion?: string | null;
	readonly preferUnversionedBundle?: boolean;
	readonly forceStartupUpdate?: boolean;
	readonly startupUpdateRequested?: boolean;
	readonly moduleUpdatesDisabled?: boolean;
	readonly updateOnDemand?: boolean;
	readonly selfUpdateShellFirst?: (latestVersion: string) => Promise<{readonly reason: string}>;
	readonly onState?: (state: ModuleUpdaterSplashState) => void;
	readonly report?: (report: ModuleUpdaterReport) => void;
	readonly fetch: typeof globalThis.fetch;
	readonly sleep?: (ms: number) => Promise<void>;
	readonly random?: () => number;
	readonly now?: () => number;
}

function isRejectedManifestError(error: unknown): boolean {
	return error instanceof ModuleManifestRollbackError || error instanceof ModuleManifestEquivocationError;
}

export class ModuleUpdater {
	private readonly store: ModuleStore;
	private readonly platform: string;
	private readonly shellVersion: ModuleVersion;
	private readonly hasOfflineRenderer: boolean;
	private readonly forceStartupUpdate: boolean;
	private readonly moduleUpdatesDisabled: boolean;
	private readonly launchInstalledOnDemand: boolean;
	private readonly selfUpdateShellFirst: ((latestVersion: string) => Promise<{readonly reason: string}>) | null;
	private readonly shellFirstAttempts = new Map<string, {readonly reason: string} | null>();
	private readonly manifestFeed: ModuleManifestFeedIdentity;
	private readonly manifestRepository: ModuleManifestRepository;
	private readonly planner: ModuleUpdatePlanner;
	private readonly packageInstaller: ModulePackageInstaller;
	private readonly onState: (state: ModuleUpdaterSplashState) => void;
	private readonly reporter: (report: ModuleUpdaterReport) => void;
	private readonly sleep: (ms: number) => Promise<void>;
	private readonly random: () => number;
	private readonly now: () => number;
	private lastState: ModuleUpdaterSplashState;
	private readonly packageOrigin: string;
	private readonly downloadRate = new ModuleDownloadRate();
	private downloadProgressed = false;
	private lastError: string | null = null;
	private lastErrorAt: number | null = null;
	private ensureBlockedUntil = 0;
	private readonly startupUpdateEnforced: boolean;
	private manifestObservationTail: Promise<void> = Promise.resolve();
	private recordedManifestSha256: string | null = null;
	private lastAccepted: {readonly document: ModuleManifestDocument; readonly metadataVersion: number} | null = null;

	public constructor(options: ModuleUpdaterOptions) {
		this.store = options.store;
		this.shellVersion = parseModuleVersion(options.shellVersion, 'desktop shell version');
		this.hasOfflineRenderer = options.hasOfflineRenderer ?? false;
		const startupUpdateRequested = options.startupUpdateRequested ?? false;
		this.forceStartupUpdate = (options.forceStartupUpdate ?? false) || startupUpdateRequested;
		this.moduleUpdatesDisabled = options.moduleUpdatesDisabled ?? false;
		this.launchInstalledOnDemand = (options.updateOnDemand ?? false) && !startupUpdateRequested;
		this.selfUpdateShellFirst = options.selfUpdateShellFirst ?? null;
		const releaseChannel = options.releaseChannel ?? BUILD_CHANNEL;
		this.platform = options.platform ?? process.platform;
		const arch = resolveDesktopModuleArchitecture(options.arch ?? process.arch);
		this.manifestFeed = {releaseChannel, platform: this.platform, arch};
		const packageOrigin = (options.packageOrigin ?? resolveDesktopPackageOrigin()).replace(/\/+$/u, '');
		this.packageOrigin = packageOrigin;
		this.planner = new ModuleUpdatePlanner(
			this.store,
			this.shellVersion,
			this.hasOfflineRenderer,
			parseBundledRendererVersion(options.bundledRendererVersion ?? null),
			options.preferUnversionedBundle ?? false,
		);
		this.onState = options.onState ?? (() => {});
		this.reporter = options.report ?? (() => {});
		this.sleep = options.sleep ?? defaultSleep;
		this.random = options.random ?? Math.random;
		this.now = options.now ?? Date.now;
		this.manifestRepository = new ModuleManifestRepository({
			store: this.store,
			target: {releaseChannel, platform: this.platform, arch},
			packageOrigin,
			fetch: options.fetch,
			now: this.now,
			onResponseCancellationError: (message, error) => {
				this.report({type: ModuleUpdaterReportType.NETWORK_ERROR, message, error});
			},
		});
		this.packageInstaller = new ModulePackageInstaller({
			store: this.store,
			packageOrigin,
			fetch: options.fetch,
			now: () => this.now(),
			onProgress: (progress) => {
				switch (progress.phase) {
					case ModulePackageInstallPhase.DOWNLOADING:
						this.recordDownloadProgress(progress.receivedBytes);
						this.emit(ModuleUpdaterStatus.DOWNLOADING, {
							...progress,
							bytesPerSecond: this.downloadRate.sample(progress.receivedBytes, this.now()) ?? undefined,
						});
						break;
					case ModulePackageInstallPhase.INSTALLING:
						this.emit(ModuleUpdaterStatus.INSTALLING, progress);
						break;
				}
			},
			report: (report) => this.handlePackageInstallerReport(report),
		});
		this.startupUpdateEnforced = this.platform !== 'linux';
		this.lastState = {
			status: ModuleUpdaterStatus.CHECKING,
			current: null,
			total: null,
			progress: null,
			seconds: null,
			moduleName: null,
			detail: null,
			receivedBytes: null,
			totalBytes: null,
			bytesPerSecond: null,
			stalled: false,
		};
	}

	public diagnostics(): ModuleUpdaterDiagnostics {
		return {
			state: this.lastState,
			packageOrigin: this.packageOrigin,
			lastError: this.lastError,
			lastErrorAt: this.lastErrorAt,
		};
	}

	private recordDownloadProgress(receivedBytes: number): void {
		const previous = this.lastState.status === ModuleUpdaterStatus.DOWNLOADING ? this.lastState.receivedBytes : null;
		if (previous != null && receivedBytes > previous) {
			this.downloadProgressed = true;
		}
	}

	public getLastState(): ModuleUpdaterSplashState {
		return this.lastState;
	}

	public async run(): Promise<ModuleUpdaterOutcome> {
		let attempt = await this.store.beginBootAttempt({bundled: this.planner.bundledModules()});
		const retiredRenderer = await this.retireRendererCoveredByBundle();
		const retiredMissing = await this.retireUninstalledModules();
		const retiredIncompatible = await this.retireIncompatibleModules();
		if (retiredRenderer || retiredMissing || retiredIncompatible) {
			attempt = {...attempt, committed: this.store.getCommitted()};
		}
		if (attempt.rolledBack) {
			this.report({
				type: ModuleUpdaterReportType.ROLLBACK,
				message: `reverted to the previous module set after ${MODULE_BOOT_ATTEMPT_ROLLBACK_THRESHOLD} launches without a ready window`,
			});
			const reverted = await this.launchRevertedSetBypassingFloor(attempt.committed);
			if (reverted != null) {
				return reverted;
			}
		}
		if (this.moduleUpdatesDisabled || this.launchInstalledOnDemand) {
			const decision = await this.planner.evaluateUnreachableLaunch(this.startupUpdateEnforced);
			if (decision.kind === ModuleUnreachableLaunchDecision.LAUNCH) {
				return await this.launchUnreachable(
					this.moduleUpdatesDisabled ? ModuleUpdaterStatus.UNREACHABLE_LAUNCH : ModuleUpdaterStatus.LAUNCHING,
				);
			}
			if (this.moduleUpdatesDisabled) {
				return this.blockUpdateRequired(decision.reason, [], 'module updates are disabled', {
					updateServerUnreachable: true,
				});
			}
		}
		if (this.platform === 'linux' && !(await this.startupUpdateRequired())) {
			return await this.launchIfAtFloor();
		}
		const backoff = new ModuleUpdaterBackoff(this.random);
		for (;;) {
			this.emit(ModuleUpdaterStatus.CHECKING);
			this.downloadRate.reset();
			this.downloadProgressed = false;
			let fetched = false;
			try {
				const document = await this.manifestRepository.fetchLatest();
				fetched = true;
				const manifest = await this.acceptManifest(document);
				if (!(await this.startupUpdateRequired())) {
					return await this.launchIfAtFloor();
				}
				const shellFirst = await this.updateShellFirst(manifest);
				if (
					this.startupUpdateEnforced &&
					this.planner.shellUpdateRequired(manifest) &&
					shellFirst?.reason !== 'no-update'
				) {
					this.emit(ModuleUpdaterStatus.BLOCKED_SHELL_UPDATE);
					return {
						status: ModuleUpdaterStatus.BLOCKED_SHELL_UPDATE,
						latestVersion: manifest.shell.latestVersion.source,
						minimumVersion: manifest.shell.minimumVersion.source,
					};
				}
				const plan = await this.planner.plan(manifest);
				if (plan.items.length === 0) {
					await this.commitIfChanged(plan.base);
					return await this.launchIfAtFloor();
				}
				const executed = await this.execute(plan);
				if (executed.blocked.length > 0) {
					return this.blockUpdateRequired(
						ModuleUpdaterBlockReason.REQUIRED_MODULE_UNAVAILABLE,
						executed.blocked,
						`required module ${executed.blocked.join(', ')} could not be installed: ${describeErrorChain(executed.blockedError)}`,
						{error: executed.blockedError},
					);
				}
				if (executed.installed.length > 0) {
					this.emit(ModuleUpdaterStatus.VERIFYING, {current: plan.items.length, total: plan.items.length});
				}
				await this.commitIfChanged(executed.desired);
				if (executed.installed.length === 0) {
					return await this.launchIfAtFloor();
				}
				backoff.reset();
			} catch (error) {
				this.reportFailure(error);
				this.lastError = describeErrorChain(error);
				this.lastErrorAt = this.now();
				const detail = isStorageFullError(error) ? 'Not enough disk space to install the update' : null;
				const updateServerUnreachable = !isRejectedManifestError(error) && isUpdateServerUnreachable(error);
				if (this.downloadProgressed) {
					backoff.reset();
				}
				const delayMs = backoff.fail();
				const exhausted =
					backoff.reachedCeiling ||
					error instanceof ModuleManifestEquivocationError ||
					(await this.forcedButLaunchable());
				if (exhausted || !fetched) {
					const decision = await this.planner.evaluateUnreachableLaunch(this.startupUpdateEnforced);
					if (decision.kind === ModuleUnreachableLaunchDecision.LAUNCH) {
						return await this.launchUnreachable();
					}
					if (exhausted) {
						return this.blockUpdateRequired(decision.reason, [], describeErrorChain(error), {
							detail,
							error,
							updateServerUnreachable,
						});
					}
				}
				const stalled = isModulePackageStall(error);
				const position = stalled && this.lastState.status === ModuleUpdaterStatus.DOWNLOADING ? this.lastState : null;
				this.emit(ModuleUpdaterStatus.RETRY_WAIT, {
					seconds: Math.max(1, Math.round(delayMs / 1000)),
					detail,
					stalled,
					current: position?.current ?? undefined,
					total: position?.total ?? undefined,
					progress: position?.progress ?? undefined,
					moduleName: position?.moduleName ?? undefined,
					receivedBytes: position?.receivedBytes ?? undefined,
					totalBytes: position?.totalBytes ?? undefined,
				});
				await this.sleep(delayMs);
			}
		}
	}

	public async ensureModule(moduleName: string): Promise<DesktopModuleEnsureResult> {
		if (this.ensureBlockedUntil > this.now()) {
			return {module: moduleName, status: DesktopModuleEnsureStatus.UNAVAILABLE};
		}
		let manifest: DesktopModuleUpdateManifest;
		try {
			manifest = await this.resolveManifestForEnsure();
		} catch (error) {
			this.ensureBlockedUntil = this.now() + MODULE_ENSURE_MANIFEST_BACKOFF_MS;
			this.reportFailure(error);
			return {module: moduleName, status: DesktopModuleEnsureStatus.UNAVAILABLE};
		}
		const committed = this.store.getCommitted()[moduleName];
		if (committed != null && (await this.planner.isInstalled(moduleName, committed))) {
			return {module: moduleName, status: DesktopModuleEnsureStatus.ALREADY_INSTALLED};
		}
		const entry = this.planner.compatibleEntry(manifest, moduleName);
		if (
			entry == null ||
			(this.planner.isRendererAssetModule(moduleName) && !this.planner.rendererFollowsFeed(manifest))
		) {
			return {module: moduleName, status: DesktopModuleEnsureStatus.UNAVAILABLE};
		}
		if (this.store.isRejected(moduleName, entry.sha256)) {
			return {module: moduleName, status: DesktopModuleEnsureStatus.UNAVAILABLE};
		}
		const item: ModulePlanItem = {module: moduleName, entry, requirement: ModulePlanItemRequirement.OPTIONAL};
		try {
			await this.packageInstaller.install(item, 1, 1);
		} catch (error) {
			if (isStorageFullError(error) || !isDeterministicPackageFailure(error)) {
				throw error;
			}
			this.report({
				type: ModuleUpdaterReportType.PACKAGE_MISSING,
				module: moduleName,
				message: errorMessage(error),
				error,
			});
			return {module: moduleName, status: DesktopModuleEnsureStatus.UNAVAILABLE};
		}
		await this.store.mergeCommitted({[moduleName]: entry.sha256});
		return {module: moduleName, status: DesktopModuleEnsureStatus.INSTALLED};
	}

	private async retireRendererCoveredByBundle(): Promise<boolean> {
		const stale = await this.planner.staleRendererModule();
		if (stale == null) {
			return false;
		}
		if (!(await this.store.retireModule(DESKTOP_RENDERER_MODULE_NAME))) {
			return false;
		}
		this.report({
			type: ModuleUpdaterReportType.BUNDLED_RENDERER_PREFERRED,
			module: DESKTOP_RENDERER_MODULE_NAME,
			message: `retired installed renderer ${stale} because the renderer bundled with this shell is at least as new`,
		});
		return true;
	}

	private async retireUninstalledModules(): Promise<boolean> {
		let retired = false;
		for (const moduleName of await this.planner.uninstalledCommittedModules()) {
			if (await this.store.retireModule(moduleName)) {
				retired = true;
				this.report({
					type: ModuleUpdaterReportType.PACKAGE_MISSING,
					module: moduleName,
					message: 'dropped a committed module whose files are gone, the bundled renderer does not need it to launch',
				});
			}
		}
		return retired;
	}

	private async retireIncompatibleModules(): Promise<boolean> {
		const cached = await this.readCachedManifest();
		const retired = await this.store.retireIncompatible((moduleName, sha256) =>
			this.planner.isRunnable(moduleName, sha256, cached),
		);
		for (const moduleName of retired) {
			this.report({
				type: ModuleUpdaterReportType.SHELL_INCOMPATIBLE_RETIRED,
				module: moduleName,
				message: `retired module ${moduleName} because shell ${this.shellVersion.source} cannot run it`,
			});
		}
		return retired.length > 0;
	}

	private async readCachedManifest(): Promise<DesktopModuleUpdateManifest | null> {
		try {
			const document = await this.manifestRepository.readCached();
			return document == null ? null : this.manifestRepository.parse(document);
		} catch {
			return null;
		}
	}

	private async resolveManifestForEnsure(): Promise<DesktopModuleUpdateManifest> {
		return this.acceptManifest(await this.manifestRepository.resolveMemoizedDocument());
	}

	private async updateShellFirst(manifest: DesktopModuleUpdateManifest): Promise<{readonly reason: string} | null> {
		const latestVersion = manifest.shell.latestVersion.source;
		if (
			this.selfUpdateShellFirst == null ||
			compareModuleVersions(manifest.shell.latestVersion, this.shellVersion) <= 0
		) {
			return null;
		}
		if (this.shellFirstAttempts.has(latestVersion)) {
			return this.shellFirstAttempts.get(latestVersion) ?? null;
		}
		this.shellFirstAttempts.set(latestVersion, null);
		const result = await this.selfUpdateShellFirst(latestVersion);
		this.shellFirstAttempts.set(latestVersion, result);
		this.emit(ModuleUpdaterStatus.CHECKING);
		return result;
	}

	public async checkForUpdate(): Promise<DesktopUpdateProbe> {
		const [manifest, feedLatest] = await Promise.all([
			this.fetchFreshManifest(ModuleManifestObservationMode.READ_ONLY),
			this.manifestRepository.fetchShellLatestVersion().catch((error: unknown) => {
				this.reportFailure(error);
				return null;
			}),
		]);
		const shellLatest =
			feedLatest != null && compareModuleVersions(feedLatest, manifest.shell.latestVersion) > 0
				? feedLatest
				: manifest.shell.latestVersion;
		const shellLatestVersion = shellLatest.source;
		const shellNewer = compareModuleVersions(shellLatest, this.shellVersion) > 0;
		return {
			shellLatestVersion,
			shellNewer,
			modulesChanged: (await this.collectPendingItems(manifest, this.planner)).length > 0,
		};
	}

	public async installPending(): Promise<ModuleLaunchAttempt | null> {
		this.emit(ModuleUpdaterStatus.CHECKING);
		this.downloadRate.reset();
		const manifest = await this.fetchFreshManifest(ModuleManifestObservationMode.RECORD_FETCH);
		const items = await this.collectPendingItems(manifest, this.planner);
		const {installed, unresolved} = await this.installItems(items);
		if (Object.keys(installed).length > 0) {
			this.emit(ModuleUpdaterStatus.VERIFYING, {current: items.length, total: items.length});
		}
		return await this.store.activateMergedForRendererReload(installed, unresolved);
	}

	private async fetchFreshManifest(mode: ModuleManifestObservationMode): Promise<DesktopModuleUpdateManifest> {
		this.manifestRepository.clearMemo();
		return await this.acceptManifest(await this.manifestRepository.fetchLatest(), mode);
	}

	private async acceptManifest(
		document: ModuleManifestDocument,
		mode: ModuleManifestObservationMode = ModuleManifestObservationMode.RECORD_FETCH,
	): Promise<DesktopModuleUpdateManifest> {
		try {
			return await this.observeManifest(document, mode);
		} catch (error) {
			if (!(error instanceof ModuleManifestRollbackError)) {
				throw error;
			}
			const fallback = this.lastAccepted?.document ?? (await this.manifestRepository.readCached());
			if (fallback == null || fallback === document) {
				throw error;
			}
			return await this.observeManifest(fallback, mode);
		}
	}

	private async collectPendingItems(
		manifest: DesktopModuleUpdateManifest,
		planner: ModuleUpdatePlanner,
	): Promise<Array<ModulePlanItem>> {
		const committed = this.store.getCommitted();
		const names = new Set<string>([...Object.keys(committed), ...manifest.requiredModules]);
		const wanted: Array<ModulePlanItem> = [];
		for (const moduleName of Array.from(names).sort()) {
			const entry = manifest.modules[moduleName];
			if (entry == null || planner.skipsFeedEntry(manifest, moduleName)) {
				continue;
			}
			if (committed[moduleName] === entry.sha256 && (await planner.isInstalled(moduleName, entry.sha256))) {
				continue;
			}
			if (this.store.isRejected(moduleName, entry.sha256)) {
				continue;
			}
			wanted.push({module: moduleName, entry, requirement: ModulePlanItemRequirement.OPTIONAL});
		}
		return wanted;
	}

	private async installItems(
		items: ReadonlyArray<ModulePlanItem>,
	): Promise<{readonly installed: Readonly<Record<string, string>>; readonly unresolved: ReadonlySet<string>}> {
		const committed = this.store.getCommitted();
		const installed: Record<string, string> = {};
		const unresolved = new Set<string>();
		for (let index = 0; index < items.length; index += 1) {
			const item = items[index];
			try {
				await this.packageInstaller.install(item, index + 1, items.length);
			} catch (error) {
				if (isStorageFullError(error) || !isDeterministicPackageFailure(error)) {
					throw error;
				}
				this.report({
					type: ModuleUpdaterReportType.PACKAGE_MISSING,
					module: item.module,
					message: errorMessage(error),
					error,
				});
				const committedSha = committed[item.module];
				if (committedSha === undefined || !(await this.planner.isInstalled(item.module, committedSha))) {
					unresolved.add(item.module);
				}
				continue;
			}
			installed[item.module] = item.entry.sha256;
		}
		return {installed, unresolved};
	}

	public async selectServedModules(modules: Readonly<Record<string, string>>): Promise<ServedModuleSelection> {
		return await this.planner.selectServedModules(modules);
	}

	public async revertLaunch(attempt: ModuleLaunchAttempt): Promise<Readonly<Record<string, string>> | null> {
		const reverted = await this.store.revertLaunchAttempt(attempt, this.planner.bundledModules());
		if (reverted != null) {
			this.report({
				type: ModuleUpdaterReportType.ROLLBACK,
				message: 'reverted an in place module update whose renderer never confirmed it started',
			});
		}
		return reverted;
	}

	public async markLaunchSucceeded(attempt: ModuleLaunchAttempt): Promise<void> {
		if (!(await this.store.markLaunchAttemptSucceeded(attempt))) {
			return;
		}
		await this.store.collectGarbage({now: this.now()});
	}

	private async launchIfAtFloor(): Promise<ModuleUpdaterOutcome> {
		const below = await this.planner.modulesBelowFloor(this.startupUpdateEnforced);
		if (below.length > 0) {
			return this.blockUpdateRequired(
				ModuleUpdaterBlockReason.BELOW_FLOOR,
				below,
				`the installed set is below the required floor for ${below.join(', ')}`,
			);
		}
		if (!this.planner.hasSomethingToRender(this.store.getCommitted())) {
			return this.blockUpdateRequired(
				ModuleUpdaterBlockReason.NOTHING_INSTALLED,
				[],
				'no module is committed and this build carries no offline renderer',
			);
		}
		await this.store.recordShellVersionConverged();
		const launchAttempt = await this.store.recordLaunchAttempt();
		this.emit(ModuleUpdaterStatus.LAUNCHING);
		return {
			status: ModuleUpdaterStatus.LAUNCHING,
			committed: this.store.getCommitted(),
			rolledBack: false,
			belowFloor: [],
			launchAttempt,
		};
	}

	private async launchUnreachable(
		splashStatus: ModuleUpdaterStatus = ModuleUpdaterStatus.UNREACHABLE_LAUNCH,
	): Promise<ModuleUpdaterOutcome> {
		const launchAttempt = await this.store.recordLaunchAttempt();
		this.emit(splashStatus);
		return {
			status: ModuleUpdaterStatus.UNREACHABLE_LAUNCH,
			committed: this.store.getCommitted(),
			launchAttempt,
		};
	}

	private async launchRevertedSetBypassingFloor(
		committed: Readonly<Record<string, string>>,
	): Promise<ModuleUpdaterOutcome | null> {
		const names = Object.keys(committed);
		if (names.length === 0) {
			return null;
		}
		for (const moduleName of names) {
			if (!(await this.planner.isInstalled(moduleName, committed[moduleName]))) {
				return null;
			}
		}
		const belowFloor = await this.planner.modulesBelowFloor(this.startupUpdateEnforced);
		if (belowFloor.length > 0) {
			this.report({
				type: ModuleUpdaterReportType.ROLLBACK_FLOOR_BYPASS,
				message: `launching the reverted set below the persisted floor for ${belowFloor.join(', ')}, because recovering a bricked install outranks enforcing the forced update`,
			});
		}
		const launchAttempt = await this.store.recordLaunchAttempt();
		this.emit(ModuleUpdaterStatus.LAUNCHING);
		return {
			status: ModuleUpdaterStatus.LAUNCHING,
			committed,
			rolledBack: true,
			belowFloor,
			launchAttempt,
		};
	}

	private blockUpdateRequired(
		reason: ModuleUpdaterBlockReason,
		modules: ReadonlyArray<string>,
		message: string,
		{
			detail = null,
			error = null,
			updateServerUnreachable = false,
		}: {readonly detail?: string | null; readonly error?: unknown; readonly updateServerUnreachable?: boolean} = {},
	): ModuleUpdaterOutcome {
		this.report({type: ModuleUpdaterReportType.BLOCKED, message: `${reason}: ${message}`, error});
		this.emit(ModuleUpdaterStatus.BLOCKED_UPDATE_REQUIRED, {detail});
		return {
			status: ModuleUpdaterStatus.BLOCKED_UPDATE_REQUIRED,
			reason,
			modules,
			message,
			updateServerUnreachable,
		};
	}

	private observeManifest(
		document: ModuleManifestDocument,
		mode: ModuleManifestObservationMode = ModuleManifestObservationMode.RECORD_FETCH,
	): Promise<DesktopModuleUpdateManifest> {
		const observe = async (): Promise<DesktopModuleUpdateManifest> => {
			const manifest = this.manifestRepository.parse(document);
			const accepted = this.lastAccepted;
			if (accepted != null && manifest.metadataVersion < accepted.metadataVersion) {
				throw new ModuleManifestRollbackError(this.manifestFeed, manifest.metadataVersion, accepted.metadataVersion);
			}
			const observation: ModuleManifestFeedObservation = {
				feed: this.manifestFeed,
				metadataVersion: manifest.metadataVersion,
				manifestSha256: createHash('sha256').update(document.bytes).digest('hex'),
			};
			this.store.requireManifestFresh(observation);
			await this.backfillShellRanges(manifest);
			if (
				mode === ModuleManifestObservationMode.RECORD_FETCH &&
				this.recordedManifestSha256 !== observation.manifestSha256
			) {
				await this.persistManifest(document, manifest, observation);
				this.recordedManifestSha256 = observation.manifestSha256;
			}
			this.lastAccepted = {document, metadataVersion: manifest.metadataVersion};
			return manifest;
		};
		const observed = this.manifestObservationTail.then(observe, observe);
		this.manifestObservationTail = observed.then(
			() => undefined,
			() => undefined,
		);
		return observed;
	}

	private async backfillShellRanges(manifest: DesktopModuleUpdateManifest): Promise<void> {
		const ranges = [];
		for (const [moduleName, sha256] of Object.entries(this.store.getCommitted())) {
			const entry = manifest.modules[moduleName];
			if (entry?.sha256 !== sha256) {
				continue;
			}
			ranges.push({
				module: moduleName,
				sha256,
				minimum: entry.minimumShellVersion?.source ?? null,
				maximum: entry.maximumShellVersion?.source ?? null,
			});
		}
		await this.store.recordShellRanges(ranges);
	}

	private async forcedButLaunchable(): Promise<boolean> {
		return this.forceStartupUpdate && !this.startupUpdateEnforced && (await this.planner.canLaunchCommittedModules());
	}

	private async startupUpdateRequired(): Promise<boolean> {
		return this.forceStartupUpdate || this.startupUpdateEnforced || !(await this.planner.canLaunchCommittedModules());
	}

	private async execute(plan: ModulePlan): Promise<ModulePlanResult> {
		const desired: Record<string, string> = {...plan.base};
		const installed: Array<string> = [];
		const blocked: Array<string> = [];
		let blockedError: unknown = null;
		const total = plan.items.length;
		for (let index = 0; index < total; index += 1) {
			const item = plan.items[index];
			try {
				await this.packageInstaller.install(item, index + 1, total);
				desired[item.module] = item.entry.sha256;
				installed.push(item.module);
			} catch (error) {
				if (isStorageFullError(error)) {
					this.report({
						type: ModuleUpdaterReportType.STORAGE_ERROR,
						module: item.module,
						message: errorMessage(error),
						error,
					});
					throw new ModuleUpdaterStorageFullError(`no space left to install ${item.module}`, {cause: error});
				}
				if (!isDeterministicPackageFailure(error)) {
					throw error;
				}
				if (item.requirement === ModulePlanItemRequirement.REQUIRED) {
					blocked.push(item.module);
					blockedError = error;
					break;
				}
				await this.planner.retainCommitted(desired, item.module);
			}
		}
		return {desired, installed, blocked, blockedError};
	}

	private async persistManifest(
		document: ModuleManifestDocument,
		manifest: DesktopModuleUpdateManifest,
		observation: ModuleManifestFeedObservation,
	): Promise<void> {
		const floor: Record<string, string> = {};
		const advertised: Record<string, string> = {};
		for (const [moduleName, entry] of Object.entries(manifest.modules)) {
			advertised[moduleName] = entry.sha256;
		}
		if (this.startupUpdateEnforced) {
			const enforced = this.store.getState().floor;
			for (const moduleName of manifest.requiredModules) {
				if (this.planner.bundleCoversModule(moduleName, manifest)) {
					continue;
				}
				const sha256 = this.planner.compatibleEntry(manifest, moduleName)?.sha256 ?? enforced[moduleName];
				if (sha256 != null) {
					floor[moduleName] = sha256;
				}
			}
		}
		await this.manifestRepository.persist(document);
		await this.store.recordManifestFetch({
			etag: document.etag,
			fetchedAt: new Date(this.now()).toISOString(),
			manifest: observation,
			floor,
			advertised,
		});
	}

	private async commitIfChanged(desired: Readonly<Record<string, string>>): Promise<void> {
		if (sameModuleMap(this.store.getCommitted(), desired)) {
			return;
		}
		await this.store.commit(desired);
	}

	private reportFailure(error: unknown): void {
		if (error instanceof ModuleManifestRollbackError) {
			this.report({type: ModuleUpdaterReportType.MANIFEST_ROLLBACK, message: error.message, error});
			return;
		}
		if (error instanceof ModuleManifestEquivocationError) {
			this.report({type: ModuleUpdaterReportType.MANIFEST_EQUIVOCATION, message: error.message, error});
			return;
		}
		if (isStorageFullError(error)) {
			this.report({type: ModuleUpdaterReportType.STORAGE_ERROR, message: errorMessage(error), error});
			return;
		}
		this.report({type: ModuleUpdaterReportType.NETWORK_ERROR, message: errorMessage(error), error});
	}

	private handlePackageInstallerReport(report: ModulePackageInstallerReport): void {
		switch (report.type) {
			case ModulePackageInstallerReportType.HASH_MISMATCH:
				this.report({...report, type: ModuleUpdaterReportType.PACKAGE_HASH_MISMATCH});
				break;
			case ModulePackageInstallerReportType.PACKAGE_MISSING:
				this.report({...report, type: ModuleUpdaterReportType.PACKAGE_MISSING});
				break;
			case ModulePackageInstallerReportType.PACKAGE_REJECTED:
				this.report({...report, type: ModuleUpdaterReportType.PACKAGE_REJECTED});
				break;
			case ModulePackageInstallerReportType.NETWORK_ERROR:
				this.report({...report, type: ModuleUpdaterReportType.NETWORK_ERROR});
				break;
		}
	}

	private report({
		type,
		module: moduleName = null,
		message,
		error = null,
	}: {
		readonly type: ModuleUpdaterReportType;
		readonly module?: string | null;
		readonly message: string;
		readonly error?: unknown;
	}): void {
		const sampleRate = MODULE_UPDATER_REPORT_SAMPLE_RATES[type];
		if (sampleRate < 1 && this.random() >= sampleRate) {
			return;
		}
		this.reporter({type, module: moduleName, message, sampleRate, error});
	}

	private emit(
		status: ModuleUpdaterStatus,
		patch: {
			readonly current?: number;
			readonly total?: number;
			readonly progress?: number;
			readonly seconds?: number;
			readonly moduleName?: string;
			readonly detail?: string | null;
			readonly receivedBytes?: number;
			readonly totalBytes?: number;
			readonly bytesPerSecond?: number;
			readonly stalled?: boolean;
		} = {},
	): void {
		this.lastState = {
			status,
			current: patch.current ?? null,
			total: patch.total ?? null,
			progress: patch.progress ?? null,
			seconds: patch.seconds ?? null,
			moduleName: patch.moduleName ?? null,
			detail: patch.detail ?? null,
			receivedBytes: patch.receivedBytes ?? null,
			totalBytes: patch.totalBytes ?? null,
			bytesPerSecond: patch.bytesPerSecond ?? null,
			stalled: patch.stalled === true,
		};
		this.onState(this.lastState);
	}
}
