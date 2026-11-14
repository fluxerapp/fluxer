// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GuildID, UserID} from '@app/api/BrandedTypes';
import {emitActivity} from '@app/api/infrastructure/activity/ActivityEvents';
import {anonymousActivityMeta} from '@app/api/infrastructure/activity/ActivityMeta';
import type {BanBy, ReportOutcome, ReportTarget, ResolvedBy} from '@app/api/infrastructure/activity/Contract.generated';
import {type IARSubmission, ReportType} from '@app/api/report/ReportModels';

interface GuildBanFacts {
	guildId: GuildID;
	userId: UserID;
	moderatorId: UserID;
	by: BanBy;
	memberCount: number;
	targetModerator: boolean;
	bannedAt: Date;
	expiresAt: Date | null;
}

export async function emitGuildMemberBanned(facts: GuildBanFacts): Promise<void> {
	const target = facts.userId.toString();
	await emitActivity(
		'guild_member_banned',
		target,
		{
			guild_id: facts.guildId.toString(),
			user_id: target,
			moderator_id: facts.moderatorId.toString(),
			by: facts.by,
			guild_member_count: Math.max(0, Math.trunc(facts.memberCount)),
			target_moderator: facts.targetModerator,
			expires_at_ms: facts.expiresAt?.getTime() ?? null,
		},
		anonymousActivityMeta(),
		`${facts.guildId}:${target}:${facts.bannedAt.getTime()}`,
	);
}

export async function emitGuildMemberUnbanned(facts: {
	guildId: GuildID;
	userId: UserID;
	moderatorId: UserID;
	by: BanBy;
}): Promise<void> {
	const target = facts.userId.toString();
	await emitActivity(
		'guild_member_unbanned',
		target,
		{
			guild_id: facts.guildId.toString(),
			user_id: target,
			moderator_id: facts.moderatorId.toString(),
			by: facts.by,
		},
		anonymousActivityMeta(),
	);
}

const REPORT_TARGETS: Record<number, ReportTarget> = {
	[ReportType.MESSAGE]: 'message',
	[ReportType.USER]: 'user',
	[ReportType.GUILD]: 'guild',
};

export async function emitReportResolved(
	report: Pick<IARSubmission, 'reportId' | 'reporterId' | 'reportedUserId' | 'category' | 'reportType'>,
	outcome: ReportOutcome,
	resolvedBy: ResolvedBy,
): Promise<void> {
	const key = report.reportedUserId ?? report.reporterId;
	const targetType = REPORT_TARGETS[report.reportType];
	if (key === null || targetType === undefined) return;
	await emitActivity(
		'report_resolved',
		key.toString(),
		{
			report_id: report.reportId.toString(),
			reporter_id: (report.reporterId ?? 0n).toString(),
			category: report.category,
			target_type: targetType,
			reported_user_id: report.reportedUserId?.toString() ?? null,
			outcome,
			resolved_by: resolvedBy,
		},
		anonymousActivityMeta(),
		report.reportId.toString(),
	);
}
