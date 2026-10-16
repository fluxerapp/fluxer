// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChildLogger} from '@electron/common/Logger';
import {getOnDemandModuleInstaller} from '@electron/main/ModuleBootHandoff';
import type {DesktopModuleEnsureResult} from '@fluxer/desktop_ipc/src/ModuleContract';
import {DesktopModuleEnsureStatus, isDesktopModuleName} from '@fluxer/desktop_ipc/src/ModuleContract';

const logger = createChildLogger('ModuleOnDemand');
const ON_DEMAND_MODULE_DISTINCT_INSTALLS_MAX = 8;

export class UnknownDesktopModuleRequestError extends Error {
	public constructor() {
		super('The renderer requested a module with an unusable name');
		this.name = 'UnknownDesktopModuleRequestError';
	}
}

class OnDemandModuleInstallCapacityError extends Error {
	public constructor() {
		super(`At most ${ON_DEMAND_MODULE_DISTINCT_INSTALLS_MAX} distinct on-demand modules may install concurrently`);
		this.name = 'OnDemandModuleInstallCapacityError';
	}
}

interface OnDemandModuleInstallerOptions {
	readonly ensure: (moduleName: string) => Promise<DesktopModuleEnsureResult>;
	readonly refresh: () => Promise<void>;
	readonly onFailure?: (moduleName: string, error: unknown) => void;
}

function createRefreshCoalescer(refresh: () => Promise<void>): () => Promise<void> {
	let running: Promise<void> | null = null;
	let pending: Promise<void> | null = null;
	const start = (): Promise<void> => {
		const started = refresh().finally(() => {
			running = null;
		});
		running = started;
		return started;
	};
	const startQueued = (): Promise<void> => {
		pending = null;
		return start();
	};
	return () => {
		if (running == null) {
			return start();
		}
		pending ??= running.then(startQueued, startQueued);
		return pending;
	};
}

export function createOnDemandModuleInstaller(
	options: OnDemandModuleInstallerOptions,
): (moduleName: string) => Promise<DesktopModuleEnsureResult> {
	const inFlight = new Map<string, Promise<DesktopModuleEnsureResult>>();
	const refresh = createRefreshCoalescer(options.refresh);
	const install = async (moduleName: string): Promise<DesktopModuleEnsureResult> => {
		let result: DesktopModuleEnsureResult;
		try {
			result = await options.ensure(moduleName);
		} catch (error) {
			options.onFailure?.(moduleName, error);
			return {module: moduleName, status: DesktopModuleEnsureStatus.UNAVAILABLE};
		}
		if (result.status !== DesktopModuleEnsureStatus.INSTALLED) {
			return result;
		}
		try {
			await refresh();
		} catch (error) {
			options.onFailure?.(moduleName, error);
			return {module: moduleName, status: DesktopModuleEnsureStatus.UNAVAILABLE};
		}
		return result;
	};
	return (moduleName: string): Promise<DesktopModuleEnsureResult> => {
		const pending = inFlight.get(moduleName);
		if (pending != null) {
			return pending;
		}
		if (inFlight.size >= ON_DEMAND_MODULE_DISTINCT_INSTALLS_MAX) {
			options.onFailure?.(moduleName, new OnDemandModuleInstallCapacityError());
			return Promise.resolve({module: moduleName, status: DesktopModuleEnsureStatus.UNAVAILABLE});
		}
		const started = install(moduleName).finally(() => {
			inFlight.delete(moduleName);
		});
		inFlight.set(moduleName, started);
		return started;
	};
}

export async function ensureDesktopModule(moduleName: unknown): Promise<DesktopModuleEnsureResult> {
	if (!isDesktopModuleName(moduleName)) {
		throw new UnknownDesktopModuleRequestError();
	}
	const installer = getOnDemandModuleInstaller();
	if (installer == null) {
		return {module: moduleName, status: DesktopModuleEnsureStatus.DISABLED};
	}
	const result = await installer(moduleName);
	logger.info('Resolved an on-demand module request', {module: moduleName, status: result.status});
	return result;
}
