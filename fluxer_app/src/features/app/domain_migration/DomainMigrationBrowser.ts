// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	classifyDomainMigrationInstallKind,
	type DomainMigrationDisplayMode,
	type DomainMigrationEnvironment,
	type DomainMigrationInstallKind,
	domainMovedBrowserMigrationUrl,
	domainMovedInstallUrl,
	domainMovedManifestId,
} from '@app/features/app/domain_migration/DomainMigrationCore';
import {
	AuthSessionStorageKey,
	parseStoredSessionValue,
} from '@app/features/platform/state/auth_session/AuthSessionStorage';
import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {hasUnavailableElectronNativeContext, isElectron} from '@app/features/ui/utils/NativeUtils';
import type {DomainMigrationDiscoveryResponse} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';

interface NavigatorWithStandalone extends Navigator {
	standalone?: boolean;
}

const DISPLAY_MODES: ReadonlyArray<DomainMigrationDisplayMode> = [
	'window-controls-overlay',
	'standalone',
	'minimal-ui',
];

export function readDomainMigrationDiscovery(): DomainMigrationDiscoveryResponse | null {
	return window.__FLUXER_BOOTSTRAP__?.instance.domain_migration ?? null;
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
		electron: isElectronEnvironment(),
	});
}

export function readDomainMigrationEnvironment(): DomainMigrationEnvironment {
	return {
		installKind: detectDomainMigrationInstallKind(),
		electron: isElectronEnvironment(),
		electronMigrationVersion: window.electron?.domainMigration?.version ?? null,
		electronPasskeyRpIds: window.electron?.passkeyRpIds ?? [],
	};
}

export async function desktopPasskeysSupported(): Promise<boolean> {
	if (!isElectronEnvironment()) {
		return true;
	}
	try {
		return (await window.electron?.passkeyIsSupported?.()) === true;
	} catch {
		return false;
	}
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
	const accounts = await accountStorage.getAllAccounts();
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
