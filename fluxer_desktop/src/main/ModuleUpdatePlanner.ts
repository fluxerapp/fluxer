// SPDX-License-Identifier: AGPL-3.0-or-later

import type {DesktopModuleManifestEntry, DesktopModuleUpdateManifest} from '@electron/main/ModuleManifest';
import type {ModuleShellRange, ModuleStore} from '@electron/main/ModuleStore';
import {compareModuleVersions, type ModuleVersion, parseModuleVersion} from '@electron/main/ModuleVersion';
import {DESKTOP_RENDERER_MODULE_NAME} from '@fluxer/desktop_ipc/src/ModuleContract';

export const ModuleUpdaterBlockReason = Object.freeze({
	REQUIRED_MODULE_UNAVAILABLE: 'required-module-unavailable',
	UNREACHABLE_BELOW_FLOOR: 'unreachable-below-floor',
	BELOW_FLOOR: 'below-floor',
	NOTHING_INSTALLED: 'nothing-installed',
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

export const ServedRendererSource = Object.freeze({
	BUNDLED: 'bundled',
	MODULE: 'module',
	NONE: 'none',
} as const);

export type ServedRendererSource = (typeof ServedRendererSource)[keyof typeof ServedRendererSource];

export interface ServedModuleSelection {
	readonly modules: Readonly<Record<string, string>>;
	readonly renderer: {
		readonly source: ServedRendererSource;
		readonly version: string | null;
		readonly bundledVersion: string | null;
	};
}

export class ModuleUpdatePlanner {
	private readonly store: ModuleStore;
	private readonly shellVersion: ModuleVersion;
	private readonly hasOfflineRenderer: boolean;
	private readonly bundledRendererVersion: ModuleVersion | null;
	private readonly bundleOutranksEveryModule: boolean;

	public constructor(
		store: ModuleStore,
		shellVersion: ModuleVersion,
		hasOfflineRenderer: boolean,
		bundledRendererVersion: ModuleVersion | null = null,
		preferUnversionedBundle = false,
	) {
		this.store = store;
		this.shellVersion = shellVersion;
		this.hasOfflineRenderer = hasOfflineRenderer;
		this.bundledRendererVersion = hasOfflineRenderer ? bundledRendererVersion : null;
		this.bundleOutranksEveryModule = hasOfflineRenderer && bundledRendererVersion == null && preferUnversionedBundle;
	}

	public hasSomethingToRender(committed: Readonly<Record<string, string>>): boolean {
		return this.hasOfflineRenderer || Object.keys(committed).length > 0;
	}

	public hasBundledRenderer(): boolean {
		return this.bundledRendererVersion != null || this.bundleOutranksEveryModule;
	}

	public bundledModules(): ReadonlyArray<string> {
		return this.hasBundledRenderer() ? [DESKTOP_RENDERER_MODULE_NAME] : [];
	}

	public bundleCoversRendererAt(version: ModuleVersion): boolean {
		if (this.bundleOutranksEveryModule) {
			return true;
		}
		return this.bundledRendererVersion != null && compareModuleVersions(version, this.bundledRendererVersion) <= 0;
	}

	public async uninstalledCommittedModules(): Promise<ReadonlyArray<string>> {
		if (!this.hasBundledRenderer()) {
			return [];
		}
		const missing: Array<string> = [];
		for (const [moduleName, sha256] of Object.entries(this.store.getCommitted())) {
			if (!(await this.isInstalled(moduleName, sha256))) {
				missing.push(moduleName);
			}
		}
		return missing;
	}

	public bundleCoversModule(moduleName: string, manifest: DesktopModuleUpdateManifest): boolean {
		return moduleName === DESKTOP_RENDERER_MODULE_NAME && this.bundleCoversRendererAt(manifest.buildVersion);
	}

	private async installedRendererVersion(sha256: string): Promise<ModuleVersion | null> {
		const installed = await this.store.getInstalledManifest(DESKTOP_RENDERER_MODULE_NAME, sha256);
		if (installed == null) {
			return null;
		}
		try {
			return parseModuleVersion(installed.build_version, 'installed renderer build version');
		} catch {
			return null;
		}
	}

	public async staleRendererModule(): Promise<string | null> {
		if (!this.hasBundledRenderer()) {
			return null;
		}
		const sha256 = this.store.getCommitted()[DESKTOP_RENDERER_MODULE_NAME];
		if (sha256 == null) {
			return null;
		}
		const installedVersion = await this.installedRendererVersion(sha256);
		return installedVersion == null || this.bundleCoversRendererAt(installedVersion) ? sha256 : null;
	}

	public async selectServedModules(modules: Readonly<Record<string, string>>): Promise<ServedModuleSelection> {
		const bundledVersion = this.bundledRendererVersion?.source ?? null;
		const sha256 = modules[DESKTOP_RENDERER_MODULE_NAME];
		if (sha256 == null) {
			return {
				modules,
				renderer: {
					source: this.hasOfflineRenderer ? ServedRendererSource.BUNDLED : ServedRendererSource.NONE,
					version: bundledVersion,
					bundledVersion,
				},
			};
		}
		const installedVersion = await this.installedRendererVersion(sha256);
		if (this.hasBundledRenderer() && (installedVersion == null || this.bundleCoversRendererAt(installedVersion))) {
			const served = {...modules};
			delete served[DESKTOP_RENDERER_MODULE_NAME];
			return {
				modules: served,
				renderer: {source: ServedRendererSource.BUNDLED, version: bundledVersion, bundledVersion},
			};
		}
		return {
			modules,
			renderer: {
				source: ServedRendererSource.MODULE,
				version: installedVersion?.source ?? null,
				bundledVersion,
			},
		};
	}

	public isShellCompatible(entry: DesktopModuleManifestEntry): boolean {
		return this.shellInRange(entry.minimumShellVersion, entry.maximumShellVersion);
	}

	private shellInRange(minimum: ModuleVersion | null, maximum: ModuleVersion | null): boolean {
		if (minimum != null && compareModuleVersions(this.shellVersion, minimum) < 0) {
			return false;
		}
		if (maximum != null && compareModuleVersions(this.shellVersion, maximum) > 0) {
			return false;
		}
		return true;
	}

	private shellInRecordedRange(range: ModuleShellRange): boolean {
		return this.shellInRange(
			range.minimum_shell_version == null
				? null
				: parseModuleVersion(range.minimum_shell_version, 'recorded module minimum shell version'),
			range.maximum_shell_version == null
				? null
				: parseModuleVersion(range.maximum_shell_version, 'recorded module maximum shell version'),
		);
	}

	public shellUpdateRequired(manifest: DesktopModuleUpdateManifest): boolean {
		if (compareModuleVersions(this.shellVersion, manifest.shell.minimumVersion) < 0) {
			return true;
		}
		for (const moduleName of manifest.requiredModules) {
			const entry = manifest.modules[moduleName];
			if (entry != null && !this.isShellCompatible(entry)) {
				return true;
			}
		}
		return false;
	}

	public compatibleEntry(manifest: DesktopModuleUpdateManifest, moduleName: string): DesktopModuleManifestEntry | null {
		if (!Object.hasOwn(manifest.modules, moduleName)) {
			return null;
		}
		const entry = manifest.modules[moduleName];
		if (compareModuleVersions(this.shellVersion, manifest.shell.minimumVersion) < 0 || !this.isShellCompatible(entry)) {
			return null;
		}
		return entry;
	}

	public isRendererAssetModule(moduleName: string): boolean {
		return moduleName !== DESKTOP_RENDERER_MODULE_NAME && !moduleName.startsWith('fluxer_dict_');
	}

	public rendererFollowsFeed(manifest: DesktopModuleUpdateManifest): boolean {
		return (
			manifest.modules[DESKTOP_RENDERER_MODULE_NAME] == null ||
			this.bundleCoversModule(DESKTOP_RENDERER_MODULE_NAME, manifest) ||
			this.compatibleEntry(manifest, DESKTOP_RENDERER_MODULE_NAME) != null
		);
	}

	public skipsFeedEntry(manifest: DesktopModuleUpdateManifest, moduleName: string): boolean {
		return (
			this.compatibleEntry(manifest, moduleName) == null ||
			this.bundleCoversModule(moduleName, manifest) ||
			(this.isRendererAssetModule(moduleName) && !this.rendererFollowsFeed(manifest))
		);
	}

	public isRunnable(moduleName: string, sha256: string, cached: DesktopModuleUpdateManifest | null): boolean {
		const recorded = this.store.getShellRange(moduleName, sha256);
		if (recorded != null) {
			return this.shellInRecordedRange(recorded);
		}
		const entry = cached?.modules[moduleName];
		if (entry != null && entry.sha256 === sha256) {
			return this.isShellCompatible(entry);
		}
		return true;
	}

	public async isInstalled(moduleName: string, sha256: string): Promise<boolean> {
		return await this.store.isInstalled(moduleName, sha256);
	}

	public async plan(manifest: DesktopModuleUpdateManifest): Promise<ModulePlan> {
		const committed = this.store.getCommitted();
		const wanted = new Set([...Object.keys(committed), ...manifest.requiredModules]);
		const items: Array<ModulePlanItem> = [];
		const base: Record<string, string> = {};
		for (const moduleName of Array.from(wanted).sort()) {
			const entry = manifest.modules[moduleName];
			if (entry == null) {
				continue;
			}
			if (this.skipsFeedEntry(manifest, moduleName)) {
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
				requirement:
					manifest.requiredModules.includes(moduleName) && !this.bundledModules().includes(moduleName)
						? ModulePlanItemRequirement.REQUIRED
						: ModulePlanItemRequirement.OPTIONAL,
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
			if (this.bundledModules().includes(moduleName)) {
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

	public async evaluateUnreachableLaunch(startupUpdateEnforced: boolean): Promise<ModuleUnreachableLaunchDecision> {
		const state = this.store.getState();
		if (
			(state.last_manifest_fetch == null && !this.hasBundledRenderer()) ||
			!this.hasSomethingToRender(state.committed)
		) {
			return {kind: ModuleUnreachableLaunchDecision.BLOCK, reason: ModuleUpdaterBlockReason.NOTHING_INSTALLED};
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
