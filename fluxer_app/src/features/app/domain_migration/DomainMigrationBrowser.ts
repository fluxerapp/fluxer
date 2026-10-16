// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	classifyDomainMigrationInstallKind,
	type DomainMigrationDisplayMode,
	type DomainMigrationEnvironment,
	type DomainMigrationGateInput,
	type DomainMigrationInstallKind,
	type DomainMigrationSide,
	domainMovedBrowserMigrationUrl,
	domainMovedInstallUrl,
	domainMovedManifestId,
	isDomainMigrationOneShotRoute,
	markDomainMigrationFailed,
	readDomainMigrationMarker,
	resolveDomainMigrationSide,
	writeDomainMigrationIntent,
} from '@app/features/app/domain_migration/DomainMigrationCore';
import InstanceSnapshotStore, {resolveDiscoveryApiEndpoint} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {
	AuthSessionStorageKey,
	parseStoredSessionValue,
} from '@app/features/platform/state/auth_session/AuthSessionStorage';
import {getProtectedLocalStorage, getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {hasUnavailableElectronNativeContext, isElectron} from '@app/features/ui/utils/ElectronRuntime';
import type {DomainMigrationDiscoveryResponse} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';

interface NavigatorWithStandalone extends Navigator {
	standalone?: boolean;
}

const DOMAIN_MIGRATION_DISCOVERY_TIMEOUT_MS = 5000;

const DISPLAY_MODES: ReadonlyArray<DomainMigrationDisplayMode> = [
	'window-controls-overlay',
	'standalone',
	'minimal-ui',
];

function domainMigrationDiscoveryApiEndpoint(): string | null {
	if (typeof window === 'undefined' || resolveDomainMigrationSide(window.location.origin) === null) {
		return null;
	}
	try {
		return resolveDiscoveryApiEndpoint(window.location.origin);
	} catch {
		return null;
	}
}

export async function loadDomainMigrationDiscovery(): Promise<void> {
	if (domainMigrationDiscoveryApiEndpoint() === null) {
		return;
	}
	await InstanceSnapshotStore.resolve({
		input: window.location.origin,
		signal: AbortSignal.timeout(DOMAIN_MIGRATION_DISCOVERY_TIMEOUT_MS),
	});
}

export function readDomainMigrationDiscovery(): DomainMigrationDiscoveryResponse | null {
	const apiEndpoint = domainMigrationDiscoveryApiEndpoint();
	if (apiEndpoint === null) {
		return null;
	}
	const cached = InstanceSnapshotStore.getForApiEndpoint(apiEndpoint);
	if (cached !== null) {
		return cached.domainMigration;
	}
	const active = RuntimeConfig.getSnapshotOrNull();
	return active?.apiEndpoint === apiEndpoint ? active.domainMigration : null;
}

function readDisplayMode(): DomainMigrationDisplayMode {
	for (const mode of DISPLAY_MODES) {
		if (window.matchMedia?.(`(display-mode: ${mode})`).matches) {
			return mode;
		}
	}
	return 'browser';
}

function isElectronEnvironment(): boolean {
	return isElectron() || hasUnavailableElectronNativeContext();
}

export function detectDomainMigrationInstallKind(): DomainMigrationInstallKind {
	if (typeof window === 'undefined') {
		return 'none';
	}
	const navigator = window.navigator as NavigatorWithStandalone;
	return classifyDomainMigrationInstallKind({
		displayMode: readDisplayMode(),
		navigatorStandalone: navigator.standalone === true,
		userAgent: navigator.userAgent,
		userAgentData: navigator.userAgentData ?? null,
		maxTouchPoints: navigator.maxTouchPoints ?? 0,
	});
}

export function readDomainMigrationEnvironment(): DomainMigrationEnvironment {
	return {
		installKind: detectDomainMigrationInstallKind(),
		electron: isElectronEnvironment(),
	};
}

export function readDomainMigrationGateInput(
	assignmentEnabled: boolean,
	voiceActive: boolean,
): DomainMigrationGateInput {
	return {
		environment: readDomainMigrationEnvironment(),
		assignmentEnabled,
		discovery: readDomainMigrationDiscovery(),
		marker: readDomainMigrationMarker(getProtectedLocalStorage()),
		now: Date.now(),
		voiceActive,
		oneShotRoute: isDomainMigrationOneShotRoute(window.location.pathname),
	};
}

export function startDomainMigrationFromSource(side: DomainMigrationSide): true {
	markDomainMigrationFailed(getProtectedLocalStorage(), Date.now());
	writeDomainMigrationIntent(getProtectedSessionStorage(), {at: Date.now()});
	const next = `${window.location.pathname}${window.location.search}${window.location.hash}`;
	window.location.replace(`${side.target}/migrate/begin?next=${encodeURIComponent(next)}`);
	return true;
}

export function readActiveSessionToken(): string | null {
	try {
		return parseStoredSessionValue(getProtectedLocalStorage()?.getItem(AuthSessionStorageKey.Token) ?? null);
	} catch {
		return null;
	}
}

export async function hasStoredAccount(): Promise<boolean> {
	if (readActiveSessionToken() !== null) {
		return true;
	}
	const {default: accountStorage} = await import('@app/features/auth/state/AccountStorage');
	const {records: accounts} = await accountStorage.getAllAccounts();
	return accounts.some((account) => Boolean(account.token));
}

function openInBrowser(url: string): void {
	window.open(url, '_blank', 'noopener');
}

export function installDomainMovedApp(target: string, onUnavailable: () => void): void {
	const installUrl = domainMovedInstallUrl(target);
	if (typeof navigator.install !== 'function') {
		openInBrowser(installUrl);
		return;
	}
	navigator.install(installUrl, domainMovedManifestId(target)).catch((err: unknown) => {
		if (err instanceof DOMException && err.name === 'AbortError') {
			return;
		}
		onUnavailable();
	});
}

export function openDomainMovedBrowserMigration(target: string): void {
	openInBrowser(domainMovedBrowserMigrationUrl(target));
}
