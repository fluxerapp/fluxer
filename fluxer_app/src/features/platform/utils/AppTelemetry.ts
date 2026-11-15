// SPDX-License-Identifier: AGPL-3.0-or-later

export const TelemetryEvent = Object.freeze({
	STORAGE_LEGACY_MIGRATION_OK: 'storage.legacy_migration.ok',
	STORAGE_LEGACY_MIGRATION_PARTIAL: 'storage.legacy_migration.partial',
	STORAGE_LEGACY_MIGRATION_FAILED: 'storage.legacy_migration.failed',
	STORAGE_UNCLASSIFIED_KEYS: 'storage.unclassified_keys',
	STORAGE_SCOPE_ACTIVATION_FAILED: 'storage.scope_activation.failed',
	ACCOUNT_STORAGE_KEY_COLLISION: 'account.storage.storage_key_collision',
	ACCOUNT_CROSS_INSTANCE_USERID_COLLISION: 'account.storage.cross_instance_userid_collision',
} as const);

export type TelemetryEvent = (typeof TelemetryEvent)[keyof typeof TelemetryEvent];

const counts = new Map<string, number>();

export function countTelemetryEvent(event: TelemetryEvent, amount = 1, ...args: Array<unknown>): void {
	if (!Number.isFinite(amount) || amount <= 0) {
		return;
	}
	const total = (counts.get(event) ?? 0) + amount;
	counts.set(event, total);
	console.info(`[Telemetry] ${event}`, total, ...args);
}
