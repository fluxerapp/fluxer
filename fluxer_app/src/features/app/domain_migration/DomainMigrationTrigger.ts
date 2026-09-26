// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import {
	desktopPasskeysSupported,
	readDomainMigrationDiscovery,
	readDomainMigrationEnvironment,
} from '@app/features/app/domain_migration/DomainMigrationBrowser';
import {
	DOMAIN_MIGRATION_NOTIFICATIONS_KEY,
	type DomainMigrationGateInput,
	type DomainMigrationSide,
	markDomainMigrationFailed,
	readDomainMigrationMarker,
	resolveDomainMigrationSide,
	shouldStartDomainMigration,
	writeDomainMigrationIntent,
} from '@app/features/app/domain_migration/DomainMigrationCore';
import DomainMigrationRollout from '@app/features/app/domain_migration/DomainMigrationRollout';
import ExperimentAssignments from '@app/features/experiment/state/ExperimentAssignments';
import {getProtectedLocalStorage, getProtectedSessionStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {when} from 'mobx';

const logger = new Logger('DomainMigrationTrigger');

const ONE_SHOT_ROUTE_PREFIXES: ReadonlyArray<string> = [
	Routes.RESET_PASSWORD,
	Routes.VERIFY_EMAIL,
	Routes.AUTHORIZE_IP,
	Routes.EMAIL_REVERT,
	Routes.OAUTH_AUTHORIZE,
	Routes.SSO_CALLBACK,
	Routes.PREMIUM_CALLBACK,
	Routes.AGE_VERIFICATION_CALLBACK,
	Routes.CONNECTION_CALLBACK,
];

let started = false;
let navigating = false;

function isVoiceActive(): boolean {
	return MediaEngine.connected || MediaEngine.connecting;
}

function isOneShotRoute(pathname: string): boolean {
	return ONE_SHOT_ROUTE_PREFIXES.some((prefix) => pathname === prefix || pathname.startsWith(`${prefix}/`));
}

function readGateInput(assignmentEnabled: boolean): DomainMigrationGateInput {
	return {
		environment: readDomainMigrationEnvironment(),
		assignmentEnabled,
		discovery: readDomainMigrationDiscovery(),
		marker: readDomainMigrationMarker(getProtectedLocalStorage()),
		now: Date.now(),
		voiceActive: isVoiceActive(),
		oneShotRoute: isOneShotRoute(window.location.pathname),
	};
}

async function evaluateSource(side: DomainMigrationSide, assignmentEnabled: boolean): Promise<void> {
	if (navigating || !shouldStartDomainMigration(readGateInput(assignmentEnabled))) {
		return;
	}
	const passkeysSupported = await desktopPasskeysSupported();
	if (navigating || !passkeysSupported || !shouldStartDomainMigration(readGateInput(DomainMigrationRollout.enabled))) {
		return;
	}
	navigating = true;
	markDomainMigrationFailed(getProtectedLocalStorage(), Date.now());
	writeDomainMigrationIntent(getProtectedSessionStorage(), {at: Date.now()});
	const next = `${window.location.pathname}${window.location.search}${window.location.hash}`;
	window.location.replace(`${side.target}/migrate/begin?next=${encodeURIComponent(next)}`);
}

function offerNotificationReenable(): void {
	const storage = getProtectedLocalStorage();
	try {
		if (storage?.getItem(DOMAIN_MIGRATION_NOTIFICATIONS_KEY) !== 'granted') {
			return;
		}
		storage.removeItem(DOMAIN_MIGRATION_NOTIFICATIONS_KEY);
	} catch {
		return;
	}
	if (typeof Notification !== 'undefined' && Notification.permission === 'default') {
		NagbarCommands.resetNagbar('desktopNotificationDismissed');
	}
}

export function startDomainMigrationTrigger(): void {
	if (started || typeof window === 'undefined') {
		return;
	}
	const side = resolveDomainMigrationSide(window.location.origin);
	if (side === null) {
		return;
	}
	started = true;
	if (side.role === 'target') {
		setTimeout(offerNotificationReenable, 0);
		return;
	}
	when(
		() => ExperimentAssignments.response !== INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE,
		() => {
			evaluateSource(side, DomainMigrationRollout.enabled).catch((err) => {
				logger.warn('Domain migration trigger failed:', err);
			});
		},
	);
}
