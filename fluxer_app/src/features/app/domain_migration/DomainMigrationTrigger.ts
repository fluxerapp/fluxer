// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	readDomainMigrationGateInput,
	startDomainMigrationFromSource,
} from '@app/features/app/domain_migration/DomainMigrationBrowser';
import {
	DOMAIN_MIGRATION_IMPORT_KEY,
	DOMAIN_MIGRATION_NOTIFICATIONS_KEY,
	type DomainMigrationSide,
	domainMigrationProbeIsDue,
	environmentAllowsDomainMigration,
	markDomainMigrationProbed,
	markerAllowsDomainMigration,
	readDomainMigrationImport,
	resolveDomainMigrationSide,
	shouldStartDomainMigration,
} from '@app/features/app/domain_migration/DomainMigrationCore';
import DomainMigrationRollout from '@app/features/app/domain_migration/DomainMigrationRollout';
import {runtimeConfigSnapshotsAreSameInstance} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import Accounts from '@app/features/auth/state/Accounts';
import {EXPERIMENT_ASSIGNMENTS_PATH} from '@app/features/experiment/state/ExperimentAssignments';
import SessionManager from '@app/features/platform/state/AuthSession';
import {getProtectedLocalStorage} from '@app/features/platform/state/ProtectedWebStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as NagbarCommands from '@app/features/ui/commands/NagbarCommands';
import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {
	ExperimentAssignmentsResponse,
	readDomainMigrationAssignment,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {compareShallow, reaction} from 'mobx';

const logger = new Logger('DomainMigrationTrigger');

let started = false;
let navigating = false;
let probing = false;

interface SourceGateState {
	assignmentReady: boolean;
	assignmentEnabled: boolean;
	deviceEnrolled: boolean;
	userId: string | null;
	sessionSettled: boolean;
	voiceActive: boolean;
}

function isVoiceActive(): boolean {
	return MediaEngine.connected || MediaEngine.connecting;
}

function isSessionSettled(): boolean {
	return SessionManager.isAuthenticated && SessionManager.userId !== null && !Accounts.transitioning;
}

function sourceMayStart(): boolean {
	return (
		!navigating &&
		isSessionSettled() &&
		shouldStartDomainMigration(readDomainMigrationGateInput(DomainMigrationRollout.enabled, isVoiceActive()))
	);
}

function evaluateSource(side: DomainMigrationSide): void {
	if (!sourceMayStart()) {
		return;
	}
	navigating = true;
	startDomainMigrationFromSource(side);
}

async function accountIsEnrolled(token: string): Promise<boolean> {
	try {
		const response = await fetch(
			`${RuntimeConfig.apiEndpoint}/v${RuntimeConfig.apiCodeVersion}${EXPERIMENT_ASSIGNMENTS_PATH}`,
			{credentials: 'omit', headers: {Authorization: token}},
		);
		if (!response.ok) {
			return false;
		}
		const parsed = ExperimentAssignmentsResponse.safeParse(await response.json());
		return parsed.success && readDomainMigrationAssignment(parsed.data).enabled;
	} catch {
		return false;
	}
}

async function probeStoredAccounts(): Promise<void> {
	const storage = getProtectedLocalStorage();
	const gate = readDomainMigrationGateInput(false, false);
	if (
		probing ||
		DomainMigrationRollout.deviceEnrolled ||
		gate.discovery?.enabled !== true ||
		!environmentAllowsDomainMigration(gate.environment) ||
		!markerAllowsDomainMigration(gate.marker, gate.now) ||
		!domainMigrationProbeIsDue(storage, gate.now)
	) {
		return;
	}
	const activeAccountKey = SessionManager.currentAccountKey;
	const runtimeSnapshot = RuntimeConfig.getSnapshotOrNull();
	const tokens = SessionManager.accounts
		.filter(
			(account) =>
				account.storageKey !== activeAccountKey &&
				account.isValid !== false &&
				Boolean(account.token) &&
				runtimeSnapshot !== null &&
				runtimeConfigSnapshotsAreSameInstance(account.instance, runtimeSnapshot),
		)
		.map((account) => account.token);
	if (tokens.length === 0) {
		return;
	}
	probing = true;
	markDomainMigrationProbed(storage, gate.now);
	try {
		for (const token of tokens) {
			if (await accountIsEnrolled(token)) {
				DomainMigrationRollout.markDeviceEnrolled();
				return;
			}
		}
	} finally {
		probing = false;
	}
}

function readSourceGateState(): SourceGateState {
	return {
		assignmentReady: DomainMigrationRollout.assignmentReady,
		assignmentEnabled: DomainMigrationRollout.assignmentEnabled,
		deviceEnrolled: DomainMigrationRollout.deviceEnrolled,
		userId: SessionManager.userId,
		sessionSettled: isSessionSettled(),
		voiceActive: isVoiceActive(),
	};
}

function handleSourceGateState(side: DomainMigrationSide, state: SourceGateState): void {
	if (state.assignmentEnabled && !state.deviceEnrolled) {
		DomainMigrationRollout.markDeviceEnrolled();
		return;
	}
	if (navigating || !state.sessionSettled || (!state.assignmentReady && !state.deviceEnrolled)) {
		return;
	}
	if (!state.deviceEnrolled) {
		probeStoredAccounts().catch((err) => {
			logger.warn('Domain migration enrollment probe failed:', err);
		});
		return;
	}
	evaluateSource(side);
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

function reloadAfterSiblingImport(event: StorageEvent): void {
	if (
		event.key === DOMAIN_MIGRATION_IMPORT_KEY &&
		readDomainMigrationImport(getProtectedLocalStorage())?.state === 'done'
	) {
		window.location.reload();
	}
}

export function probeSignedOutDeviceEnrollment(): void {
	if (typeof window === 'undefined' || SessionManager.isAuthenticated) {
		return;
	}
	if (resolveDomainMigrationSide(window.location.origin)?.role !== 'source') {
		return;
	}
	probeStoredAccounts().catch((err) => {
		logger.warn('Domain migration enrollment probe failed:', err);
	});
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
		window.addEventListener('storage', reloadAfterSiblingImport);
		return;
	}
	reaction(readSourceGateState, (state) => handleSourceGateState(side, state), {
		fireImmediately: true,
		equals: compareShallow,
	});
}
