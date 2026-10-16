// SPDX-License-Identifier: AGPL-3.0-or-later

export type WorkerJobPayload = Record<string, unknown>;

export interface WorkerJobOptions {
	runAt?: Date | undefined;
	maxAttempts?: number | undefined;
	jobKey?: string | undefined;
	requestedByUserId?: bigint | undefined;
	auditLogReason?: string | undefined;
	skipLedger?: boolean | undefined;
	requireLedger?: boolean | undefined;
}
