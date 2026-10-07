// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	blockingModuleNames,
	type DesktopLinuxSecurityMinimum,
	type DesktopModuleManifestEntry,
	type DesktopModuleUpdateManifest,
} from '@electron/main/ModuleManifest';
import {MODULE_BOOT_ATTEMPT_ROLLBACK_THRESHOLD, type ModuleStore} from '@electron/main/ModuleStore';
import {compareModuleVersions, type ModuleVersion, parseModuleVersion} from '@electron/main/ModuleVersion';

export const ModuleUpdaterBlockReason = Object.freeze({
	REQUIRED_MODULE_UNAVAILABLE: 'required-module-unavailable',
	UNREACHABLE_BELOW_FLOOR: 'unreachable-below-floor',
	BELOW_FLOOR: 'below-floor',
	NOTHING_INSTALLED: 'nothing-installed',
	ROLLBACK_EXHAUSTED: 'rollback-exhausted',
} as const);

export type ModuleUpdaterBlockReason = (typeof ModuleUpdaterBlockReason)[keyof typeof ModuleUpdaterBlockReason];

export const ModulePlanItemRequirement = Object.freeze({
	REQUIRED: 'required',
	OPTIONAL: 'optional',
} as const);

export type ModulePlanItemRequirement = (typeof ModulePlanItemRequirement)[keyof typeof ModulePlanItemRequirement];

export interface ModulePlanItem {
	readonly module: string;
	readonly entry: DesktopModuleManifestEntry;
	readonly requirement: ModulePlanItemRequirement;
}

export interface ModulePlan {
	readonly items: ReadonlyArray<ModulePlanItem>;
	readonly base: Readonly<Record<string, string>>;
}

export const ModuleUnreachableLaunchDecision = Object.freeze({
	LAUNCH: 'launch',
	BLOCK: 'block',
} as const);

export type ModuleUnreachableLaunchDecision =
	| {readonly kind: typeof ModuleUnreachableLaunchDecision.LAUNCH}
	| {readonly kind: typeof ModuleUnreachableLaunchDecision.BLOCK; readonly reason: ModuleUpdaterBlockReason};

export class ModuleUpdatePlanner {
	private readonly store: ModuleStore;
	private readonly shellVersion: ModuleVersion;
	private readonly hasOfflineRenderer: boolean;

	public constructor(store: ModuleStore, shellVersion: ModuleVersion, hasOfflineRenderer: boolean) {
		this.store = store;
		this.shellVersion = shellVersion;
		this.hasOfflineRenderer = hasOfflineRenderer;
	}

	public hasSomethingToRender(committed: Readonly<Record<string, string>>): boolean {
		return this.hasOfflineRenderer || Object.keys(committed).length > 0;
	}

	public isShellCompatible(entry: DesktopModuleManifestEntry): boolean {
		if (entry.minimumShellVersion != null && compareModuleVersions(this.shellVersion, entry.minimumShellVersion) < 0) {
			return false;
		}
		if (entry.maximumShellVersion != null && compareModuleVersions(this.shellVersion, entry.maximumShellVersion) > 0) {
			return false;
		}
		return true;
	}

	public shellUpdateRequired(manifest: DesktopModuleUpdateManifest): boolean {
		if (compareModuleVersions(this.shellVersion, manifest.shell.minimumVersion) < 0) {
			return true;
		}
		for (const moduleName of blockingModuleNames(manifest)) {
			const entry = manifest.modules[moduleName];
			if (entry != null && !this.isShellCompatible(entry)) {
				return true;
			}
		}
		return false;
	}

	public async securityUpdateRequired(minimum: DesktopLinuxSecurityMinimum): Promise<boolean> {
		if (compareModuleVersions(this.shellVersion, minimum.version) < 0) {
			return true;
		}
		const committed = this.store.getCommitted();
		for (const moduleName of minimum.requiredModules) {
			const sha256 = committed[moduleName];
			if (sha256 == null) {
				return true;
			}
			const installed = await this.store.getInstalledManifest(moduleName, sha256);
			if (installed == null) {
				return true;
			}
			const installedVersion = parseModuleVersion(
				installed.build_version,
				`installed module ${moduleName} build version`,
			);
			if (compareModuleVersions(installedVersion, minimum.version) < 0) {
				return true;
			}
		}
		return false;
	}

	public async isInstalled(moduleName: string, sha256: string): Promise<boolean> {
		return await this.store.isInstalled(moduleName, sha256);
	}

	public async plan(manifest: DesktopModuleUpdateManifest): Promise<ModulePlan> {
		const committed = this.store.getCommitted();
		const blocking = blockingModuleNames(manifest);
		const wanted = new Set([...Object.keys(committed), ...blocking]);
		const items: Array<ModulePlanItem> = [];
		const base: Record<string, string> = {};
		for (const moduleName of Array.from(wanted).sort()) {
			const entry = manifest.modules[moduleName];
			if (entry == null) {
				continue;
			}
			if (!this.isShellCompatible(entry)) {
				await this.retainCommitted(base, moduleName);
				continue;
			}
			if (committed[moduleName] === entry.sha256 && (await this.isInstalled(moduleName, entry.sha256))) {
				base[moduleName] = entry.sha256;
				continue;
			}
			if (this.store.isRejected(moduleName, entry.sha256)) {
				await this.retainCommitted(base, moduleName);
				continue;
			}
			items.push({
				module: moduleName,
				entry,
				requirement: blocking.has(moduleName) ? ModulePlanItemRequirement.REQUIRED : ModulePlanItemRequirement.OPTIONAL,
			});
		}
		return {items, base};
	}

	public async retainCommitted(target: Record<string, string>, moduleName: string): Promise<void> {
		const current = this.store.getCommitted()[moduleName];
		if (current != null && (await this.isInstalled(moduleName, current))) {
			target[moduleName] = current;
		}
	}

	public async modulesBelowFloor(startupUpdateEnforced: boolean): Promise<ReadonlyArray<string>> {
		if (!startupUpdateEnforced) {
			return [];
		}
		const state = this.store.getState();
		const below: Array<string> = [];
		for (const [moduleName, sha256] of Object.entries(state.floor)) {
			if (state.rejected[moduleName] === sha256) {
				continue;
			}
			if (state.committed[moduleName] !== sha256 || !(await this.isInstalled(moduleName, sha256))) {
				below.push(moduleName);
			}
		}
		return below;
	}

	public async canLaunchCommittedModules(): Promise<boolean> {
		const committed = this.store.getCommitted();
		if (!this.hasSomethingToRender(committed)) {
			return false;
		}
		for (const [moduleName, sha256] of Object.entries(committed)) {
			if (!(await this.isInstalled(moduleName, sha256))) {
				return false;
			}
		}
		return true;
	}

	public async evaluateUnreachableLaunch(
		startupUpdateEnforced: boolean,
		securityUpdateRequired: boolean,
	): Promise<ModuleUnreachableLaunchDecision> {
		const state = this.store.getState();
		if (state.last_manifest_fetch == null || !this.hasSomethingToRender(state.committed)) {
			return {kind: ModuleUnreachableLaunchDecision.BLOCK, reason: ModuleUpdaterBlockReason.NOTHING_INSTALLED};
		}
		if (state.boot_attempt >= MODULE_BOOT_ATTEMPT_ROLLBACK_THRESHOLD) {
			return {kind: ModuleUnreachableLaunchDecision.BLOCK, reason: ModuleUpdaterBlockReason.ROLLBACK_EXHAUSTED};
		}
		if (securityUpdateRequired) {
			return {
				kind: ModuleUnreachableLaunchDecision.BLOCK,
				reason: ModuleUpdaterBlockReason.UNREACHABLE_BELOW_FLOOR,
			};
		}
		if ((await this.modulesBelowFloor(startupUpdateEnforced)).length > 0) {
			return {
				kind: ModuleUnreachableLaunchDecision.BLOCK,
				reason: ModuleUpdaterBlockReason.UNREACHABLE_BELOW_FLOOR,
			};
		}
		for (const [moduleName, sha256] of Object.entries(state.committed)) {
			if (!(await this.isInstalled(moduleName, sha256))) {
				return {
					kind: ModuleUnreachableLaunchDecision.BLOCK,
					reason: ModuleUpdaterBlockReason.UNREACHABLE_BELOW_FLOOR,
				};
			}
		}
		return {kind: ModuleUnreachableLaunchDecision.LAUNCH};
	}
}
