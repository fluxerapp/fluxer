// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {BUILD_CHANNEL} from '@electron/common/BuildChannel';
import type {DesktopUpdateCheck} from '@electron/main/DesktopUpdateGate';
import {ModuleDownloadRate} from '@electron/main/ModuleDownloadRate';
import {
	blockingModuleNames,
	type DesktopLinuxSecurityMinimum,
	type DesktopModuleUpdateManifest,
	resolveDesktopModuleArchitecture,
} from '@electron/main/ModuleManifest';
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
	type LinuxModuleSecurityMinimum,
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
} from '@electron/main/ModuleUpdatePlanner';
import {compareModuleVersions, type ModuleVersion, parseModuleVersion} from '@electron/main/ModuleVersion';
import {resolveDesktopPackageOrigin} from '@electron/main/ShellDownloadFormats';
import type {DesktopModuleEnsureResult} from '@fluxer/desktop_ipc/src/ModuleContract';
import {DesktopModuleEnsureStatus} from '@fluxer/desktop_ipc/src/ModuleContract';

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
	[ModuleUpdaterReportType.BLOCKED]: 1,
});

export {ModuleUpdaterBlockReason};

export interface ModuleUpdaterSplashState {
	readonly status: ModuleUpdaterStatus;
	readonly requiredSecurityUpdate: boolean;
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
			readonly requiredSecurityUpdate: boolean;
			readonly updateServerUnreachable: boolean;
	  }
	| {
			readonly status: typeof ModuleUpdaterStatus.BLOCKED_SHELL_UPDATE;
			readonly latestVersion: string;
			readonly minimumVersion: string;
			readonly requiredSecurityUpdate: boolean;
	  };

class ModuleUpdaterStorageFullError extends Error {
	public constructor(message: string, options?: ErrorOptions) {
		super(message, options);
		this.name = 'ModuleUpdaterStorageFullError';
	}
}

function errorMessage(error: unknown): string {
	return error instanceof Error ? error.message : String(error);
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
		const capped = Math.min(exponential, MODULE_UPDATER_BACKOFF_CAP_MS);
		return Math.round(capped * (0.5 + this.random() * 0.5));
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

const ModuleStartupUpdatePolicy = Object.freeze({
	REQUIRED: 'required',
	OPTIONAL: 'optional',
	REQUIRED_SECURITY: 'required-security',
} as const);

type ModuleStartupUpdatePolicy = (typeof ModuleStartupUpdatePolicy)[keyof typeof ModuleStartupUpdatePolicy];

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
	readonly forceStartupUpdate?: boolean;
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
	public readonly store: ModuleStore;
	private readonly platform: string;
	private readonly shellVersion: ModuleVersion;
	private readonly hasOfflineRenderer: boolean;
	private readonly forceStartupUpdate: boolean;
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
	private startupUpdatePolicy: ModuleStartupUpdatePolicy;
	private manifestObservationTail: Promise<void> = Promise.resolve();
	private recordedManifestSha256: string | null = null;

	public constructor(options: ModuleUpdaterOptions) {
		this.store = options.store;
		this.shellVersion = parseModuleVersion(options.shellVersion, 'desktop shell version');
		this.hasOfflineRenderer = options.hasOfflineRenderer ?? false;
		this.forceStartupUpdate = options.forceStartupUpdate ?? false;
		const releaseChannel = options.releaseChannel ?? BUILD_CHANNEL;
		this.platform = options.platform ?? process.platform;
		const arch = resolveDesktopModuleArchitecture(options.arch ?? process.arch);
		this.manifestFeed = {releaseChannel, platform: this.platform, arch};
		const packageOrigin = (options.packageOrigin ?? resolveDesktopPackageOrigin()).replace(/\/+$/u, '');
		this.packageOrigin = packageOrigin;
		this.planner = new ModuleUpdatePlanner(this.store, this.shellVersion, this.hasOfflineRenderer);
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
		this.startupUpdatePolicy =
			this.platform === 'linux' ? ModuleStartupUpdatePolicy.OPTIONAL : ModuleStartupUpdatePolicy.REQUIRED;
		this.lastState = {
			status: ModuleUpdaterStatus.CHECKING,
			requiredSecurityUpdate: false,
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
		const attempt = await this.store.beginBootAttempt();
		await this.restoreCachedUpdatePolicy();
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
				const manifest = await this.observeManifest(document);
				const startupUpdateRequired = await this.startupUpdateRequired();
				if (!startupUpdateRequired) {
					return await this.launchIfAtFloor();
				}
				if (this.planner.shellUpdateRequired(manifest)) {
					this.emit(ModuleUpdaterStatus.BLOCKED_SHELL_UPDATE);
					return {
						status: ModuleUpdaterStatus.BLOCKED_SHELL_UPDATE,
						latestVersion: manifest.shell.latestVersion.source,
						minimumVersion: manifest.shell.minimumVersion.source,
						requiredSecurityUpdate: this.isSecurityUpdateRequired(),
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
				await this.applyUpdatePolicy(manifest);
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
				const exhausted = backoff.reachedCeiling || isRejectedManifestError(error);
				if (exhausted || !fetched) {
					const decision = await this.planner.evaluateUnreachableLaunch(
						this.isStartupUpdateEnforced(),
						this.isSecurityUpdateRequired(),
					);
					if (decision.kind === ModuleUnreachableLaunchDecision.LAUNCH) {
						const launchAttempt = await this.store.recordLaunchAttempt();
						this.emit(ModuleUpdaterStatus.UNREACHABLE_LAUNCH);
						return {
							status: ModuleUpdaterStatus.UNREACHABLE_LAUNCH,
							committed: this.store.getCommitted(),
							launchAttempt,
						};
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
		if (this.planner.shellUpdateRequired(manifest)) {
			return {module: moduleName, status: DesktopModuleEnsureStatus.UNAVAILABLE};
		}
		if (!Object.hasOwn(manifest.modules, moduleName)) {
			return {module: moduleName, status: DesktopModuleEnsureStatus.UNAVAILABLE};
		}
		const entry = manifest.modules[moduleName];
		if (!this.planner.isShellCompatible(entry)) {
			return {module: moduleName, status: DesktopModuleEnsureStatus.UNAVAILABLE};
		}
		const committed = this.store.getCommitted()[moduleName];
		if (committed != null && (await this.planner.isInstalled(moduleName, committed))) {
			return {module: moduleName, status: DesktopModuleEnsureStatus.ALREADY_INSTALLED};
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

	private async resolveManifestForEnsure(): Promise<DesktopModuleUpdateManifest> {
		return this.observeManifest(await this.manifestRepository.resolveMemoizedDocument());
	}

	public async checkForUpdate(): Promise<DesktopUpdateCheck> {
		const manifest = await this.fetchFreshManifest(ModuleManifestObservationMode.READ_ONLY);
		const shellNewer = compareModuleVersions(manifest.shell.latestVersion, this.shellVersion) > 0;
		if (this.planner.shellUpdateRequired(manifest)) {
			return {shellNewer, modulesChanged: false};
		}
		return {shellNewer, modulesChanged: (await this.collectPendingItems(manifest, this.planner)).length > 0};
	}

	public async installPending(): Promise<ModuleLaunchAttempt | null> {
		this.emit(ModuleUpdaterStatus.CHECKING);
		this.downloadRate.reset();
		const manifest = await this.fetchFreshManifest(ModuleManifestObservationMode.RECORD_FETCH);
		if (this.planner.shellUpdateRequired(manifest)) {
			return null;
		}
		const items = await this.collectPendingItems(manifest, this.planner);
		const {installed, unresolved} = await this.installItems(items);
		if (Object.keys(installed).length > 0) {
			this.emit(ModuleUpdaterStatus.VERIFYING, {current: items.length, total: items.length});
		}
		return await this.store.activateMergedForRendererReload(installed, unresolved);
	}

	public async prefetch(): Promise<void> {
		this.emit(ModuleUpdaterStatus.CHECKING);
		this.downloadRate.reset();
		const manifest = await this.fetchFreshManifest(ModuleManifestObservationMode.READ_ONLY);
		const nextShellPlanner = new ModuleUpdatePlanner(this.store, manifest.shell.latestVersion, this.hasOfflineRenderer);
		await this.installItems(await this.collectPendingItems(manifest, nextShellPlanner));
	}

	private async fetchFreshManifest(mode: ModuleManifestObservationMode): Promise<DesktopModuleUpdateManifest> {
		this.manifestRepository.clearMemo();
		return await this.observeManifest(await this.manifestRepository.fetchLatest(), mode);
	}

	private async collectPendingItems(
		manifest: DesktopModuleUpdateManifest,
		planner: ModuleUpdatePlanner,
	): Promise<Array<ModulePlanItem>> {
		const committed = this.store.getCommitted();
		const names = new Set<string>([...Object.keys(committed), ...blockingModuleNames(manifest)]);
		const wanted: Array<ModulePlanItem> = [];
		for (const moduleName of Array.from(names).sort()) {
			const entry = manifest.modules[moduleName];
			if (entry == null || !planner.isShellCompatible(entry)) {
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

	public async markLaunchSucceeded(attempt: ModuleLaunchAttempt): Promise<void> {
		if (!(await this.store.markLaunchAttemptSucceeded(attempt))) {
			return;
		}
		await this.store.collectGarbage({now: this.now()});
	}

	public async canLaunchWhileUnreachable(): Promise<boolean> {
		return (
			(await this.planner.evaluateUnreachableLaunch(this.isStartupUpdateEnforced(), this.isSecurityUpdateRequired()))
				.kind === ModuleUnreachableLaunchDecision.LAUNCH
		);
	}

	private async launchIfAtFloor(): Promise<ModuleUpdaterOutcome> {
		if (this.isSecurityUpdateRequired()) {
			return this.blockUpdateRequired(
				ModuleUpdaterBlockReason.BELOW_FLOOR,
				[],
				'the installed shell or module set is below the required Linux security minimum',
			);
		}
		const below = await this.planner.modulesBelowFloor(this.isStartupUpdateEnforced());
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

	private async launchRevertedSetBypassingFloor(
		committed: Readonly<Record<string, string>>,
	): Promise<ModuleUpdaterOutcome | null> {
		if (this.isSecurityUpdateRequired()) {
			return null;
		}
		const names = Object.keys(committed);
		if (names.length === 0) {
			return null;
		}
		for (const moduleName of names) {
			if (!(await this.planner.isInstalled(moduleName, committed[moduleName]))) {
				return null;
			}
		}
		const belowFloor = await this.planner.modulesBelowFloor(this.isStartupUpdateEnforced());
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
			requiredSecurityUpdate: this.isSecurityUpdateRequired(),
			updateServerUnreachable,
		};
	}

	private async applyUpdatePolicy(manifest: DesktopModuleUpdateManifest): Promise<void> {
		if (this.platform !== 'linux') {
			this.startupUpdatePolicy = ModuleStartupUpdatePolicy.REQUIRED;
			return;
		}
		await this.applyLinuxUpdatePolicy(manifest.linuxSecurityMinimum);
	}

	private persistedLinuxSecurityMinimum(): DesktopLinuxSecurityMinimum | null {
		const persisted = this.store.getState().linux_security_minimum;
		if (persisted == null) {
			return null;
		}
		return {
			version: parseModuleVersion(persisted.version, 'persisted Linux security minimum'),
			requiredModules: persisted.requiredModules,
		};
	}

	private effectiveLinuxSecurityMinimum(
		manifestMinimum: DesktopLinuxSecurityMinimum | null,
	): DesktopLinuxSecurityMinimum | null {
		const persisted = this.persistedLinuxSecurityMinimum();
		if (manifestMinimum == null) {
			return persisted;
		}
		if (persisted == null) {
			return manifestMinimum;
		}
		const comparison = compareModuleVersions(manifestMinimum.version, persisted.version);
		if (comparison < 0) {
			throw new Error(
				`manifest attempts to lower the Linux security minimum from ${persisted.version.source} to ${manifestMinimum.version.source}`,
			);
		}
		if (comparison === 0 && manifestMinimum.requiredModules.join('\0') !== persisted.requiredModules.join('\0')) {
			throw new Error(`manifest changes the required modules at Linux security minimum ${persisted.version.source}`);
		}
		return manifestMinimum;
	}

	private async applyLinuxUpdatePolicy(manifestMinimum: DesktopLinuxSecurityMinimum | null): Promise<void> {
		const minimum = this.effectiveLinuxSecurityMinimum(manifestMinimum);
		if (minimum != null) {
			this.startupUpdatePolicy = (await this.planner.securityUpdateRequired(minimum))
				? ModuleStartupUpdatePolicy.REQUIRED_SECURITY
				: ModuleStartupUpdatePolicy.OPTIONAL;
			return;
		}
		this.startupUpdatePolicy = ModuleStartupUpdatePolicy.OPTIONAL;
	}

	private isStartupUpdateEnforced(): boolean {
		return this.startupUpdatePolicy !== ModuleStartupUpdatePolicy.OPTIONAL;
	}

	private isSecurityUpdateRequired(): boolean {
		return this.startupUpdatePolicy === ModuleStartupUpdatePolicy.REQUIRED_SECURITY;
	}

	private async restoreCachedUpdatePolicy(): Promise<void> {
		if (this.platform !== 'linux') {
			return;
		}
		await this.applyLinuxUpdatePolicy(null);
		const document = await this.manifestRepository.readCached();
		if (document == null) {
			return;
		}
		try {
			await this.observeManifest(document, ModuleManifestObservationMode.READ_ONLY);
		} catch (error) {
			this.reportFailure(error);
		}
	}

	private observeManifest(
		document: ModuleManifestDocument,
		mode: ModuleManifestObservationMode = ModuleManifestObservationMode.RECORD_FETCH,
	): Promise<DesktopModuleUpdateManifest> {
		const observe = async (): Promise<DesktopModuleUpdateManifest> => {
			const manifest = this.manifestRepository.parse(document);
			const observation: ModuleManifestFeedObservation = {
				feed: this.manifestFeed,
				metadataVersion: manifest.metadataVersion,
				manifestSha256: createHash('sha256').update(document.bytes).digest('hex'),
			};
			this.store.requireManifestFresh(observation);
			await this.applyUpdatePolicy(manifest);
			if (
				mode === ModuleManifestObservationMode.RECORD_FETCH &&
				this.recordedManifestSha256 !== observation.manifestSha256
			) {
				await this.persistManifest(document, manifest, observation);
				this.recordedManifestSha256 = observation.manifestSha256;
			}
			return manifest;
		};
		const observed = this.manifestObservationTail.then(observe, observe);
		this.manifestObservationTail = observed.then(
			() => undefined,
			() => undefined,
		);
		return observed;
	}

	private async startupUpdateRequired(): Promise<boolean> {
		return (
			this.forceStartupUpdate || this.isStartupUpdateEnforced() || !(await this.planner.canLaunchCommittedModules())
		);
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
		let linuxSecurityMinimum: LinuxModuleSecurityMinimum | undefined;
		if (this.platform === 'linux') {
			if (manifest.linuxSecurityMinimum != null) {
				linuxSecurityMinimum = {
					version: manifest.linuxSecurityMinimum.version.source,
					requiredModules: manifest.linuxSecurityMinimum.requiredModules,
				};
			}
		} else {
			for (const moduleName of blockingModuleNames(manifest)) {
				floor[moduleName] = manifest.modules[moduleName].sha256;
			}
		}
		await this.manifestRepository.persist(document);
		await this.store.recordManifestFetch({
			etag: document.etag,
			fetchedAt: new Date(this.now()).toISOString(),
			manifest: observation,
			floor,
			linuxSecurityMinimum,
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
			requiredSecurityUpdate: this.isSecurityUpdateRequired(),
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
