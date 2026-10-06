// SPDX-License-Identifier: AGPL-3.0-or-later

import type {AdminAuditReadAction} from '@app/api/admin/AdminAuditActions';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import type {Context} from 'hono';

type AdminAuditMetadataValue = string | number | bigint | boolean | null | undefined;

type AdminAuditMetadataInput = Readonly<Record<string, AdminAuditMetadataValue>>;

interface AdminAuditEntryInput<TAction extends string> {
	targetType: string;
	targetId: bigint;
	action: TAction;
	metadata?: AdminAuditMetadataInput;
}

const SNOWFLAKE_PATTERN = /^(0|[1-9][0-9]{0,19})$/;

export function snowflakeOrUndefined(value: string | undefined): string | undefined {
	return value !== undefined && SNOWFLAKE_PATTERN.test(value) ? value : undefined;
}

function toAdminAuditMetadata(input: AdminAuditMetadataInput = {}): Map<string, string> {
	const metadata = new Map<string, string>();
	for (const [key, value] of Object.entries(input)) {
		if (value === undefined || value === null) continue;
		metadata.set(key, String(value));
	}
	return metadata;
}

async function recordAdminAuditEntry(ctx: Context<HonoEnv>, entry: AdminAuditEntryInput<string>): Promise<void> {
	await ctx.get('adminService').auditService.createAuditLog({
		adminUserId: ctx.get('adminUserId'),
		targetType: entry.targetType,
		targetId: entry.targetId,
		action: entry.action,
		auditLogReason: ctx.get('auditLogReason'),
		metadata: toAdminAuditMetadata(entry.metadata),
	});
}

export async function recordAdminRead(
	ctx: Context<HonoEnv>,
	entry: AdminAuditEntryInput<AdminAuditReadAction>,
): Promise<void> {
	await recordAdminAuditEntry(ctx, entry);
}

export async function recordAdminWrite(ctx: Context<HonoEnv>, entry: AdminAuditEntryInput<string>): Promise<void> {
	await recordAdminAuditEntry(ctx, entry);
}
