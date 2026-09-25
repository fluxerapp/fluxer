// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	detectDomainMigrationInstallKind,
	readDomainMigrationDiscovery,
} from '@app/features/app/domain_migration/DomainMigrationBrowser';
import {
	DOMAIN_MIGRATION_MARKER_KEY,
	DOMAIN_MIGRATION_MOVED_DISMISSED_KEY,
	DOMAIN_MIGRATION_SOURCE_TO_TARGET,
	type DomainMigrationInstallKind,
	type DomainMigrationMarker,
	type DomainMigrationSide,
	readDomainMigrationMarker,
	resolveDomainMigrationSide,
	shouldShowDomainMovedNotice,
} from '@app/features/app/domain_migration/DomainMigrationCore';
import DomainMigrationRollout from '@app/features/app/domain_migration/DomainMigrationRollout';
import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {makeAutoObservable} from 'mobx';

const FALLBACK_TARGET = DOMAIN_MIGRATION_SOURCE_TO_TARGET['https://web.fluxer.app'];

function readDismissedAt(): number | null {
	try {
		const value = Number(getProtectedLocalStorage()?.getItem(DOMAIN_MIGRATION_MOVED_DISMISSED_KEY) ?? Number.NaN);
		return Number.isFinite(value) ? value : null;
	} catch {
		return null;
	}
}

class DomainMovedNotice {
	readonly side: DomainMigrationSide | null;
	readonly installKind: DomainMigrationInstallKind;
	marker: DomainMigrationMarker | null;
	dismissedAt: number | null;

	constructor() {
		const origin = typeof window === 'undefined' ? '' : window.location.origin;
		this.side = resolveDomainMigrationSide(origin);
		this.installKind = detectDomainMigrationInstallKind();
		this.marker = readDomainMigrationMarker(getProtectedLocalStorage());
		this.dismissedAt = readDismissedAt();
		makeAutoObservable(this, {side: false, installKind: false}, {autoBind: true});
		if (this.side?.role === 'source') {
			window.addEventListener('storage', this.handleStorage);
		}
	}

	get target(): string {
		return this.side?.target ?? FALLBACK_TARGET;
	}

	get targetHost(): string {
		return new URL(this.target).host;
	}

	shouldShow(now: number): boolean {
		return shouldShowDomainMovedNotice({
			side: this.side,
			installKind: this.installKind,
			discovery: readDomainMigrationDiscovery(),
			assignmentEnabled: DomainMigrationRollout.enabled,
			marker: this.marker,
			dismissedAt: this.dismissedAt,
			now,
		});
	}

	dismiss(now: number): void {
		this.dismissedAt = now;
		try {
			getProtectedLocalStorage()?.setItem(DOMAIN_MIGRATION_MOVED_DISMISSED_KEY, String(now));
		} catch {}
	}

	resetDismissal(): void {
		this.dismissedAt = null;
		try {
			getProtectedLocalStorage()?.removeItem(DOMAIN_MIGRATION_MOVED_DISMISSED_KEY);
		} catch {}
	}

	private handleStorage(event: StorageEvent): void {
		if (event.key === DOMAIN_MIGRATION_MARKER_KEY || event.key === null) {
			this.marker = readDomainMigrationMarker(getProtectedLocalStorage());
		}
		if (event.key === DOMAIN_MIGRATION_MOVED_DISMISSED_KEY || event.key === null) {
			this.dismissedAt = readDismissedAt();
		}
	}
}

export default new DomainMovedNotice();
