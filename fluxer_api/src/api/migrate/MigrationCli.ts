// SPDX-License-Identifier: AGPL-3.0-or-later

export const MIGRATE_EXIT = {
	ok: 0,
	failed: 1,
	usage: 2,
	mismatch: 3,
} as const;

export type MigrateExitCode = (typeof MIGRATE_EXIT)[keyof typeof MIGRATE_EXIT];

export class MigrationUsageError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'MigrationUsageError';
	}
}

export interface MigrationOutput {
	info(message: string): void;
	report(report: object): void;
}

export function positiveInteger(flag: string, value: string | undefined, fallback: number): number {
	if (value === undefined) {
		return fallback;
	}
	if (!/^\d+$/.test(value) || Number(value) < 1) {
		throw new MigrationUsageError(`--${flag} must be a whole number of at least 1, not ${JSON.stringify(value)}`);
	}
	return Number(value);
}
