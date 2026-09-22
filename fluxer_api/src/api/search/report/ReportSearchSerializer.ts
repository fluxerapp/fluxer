// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IARSubmission} from '@app/api/report/IReportRepository';
import type {SearchableReport} from '@fluxer/schema/src/contracts/search/SearchDocumentTypes';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';

export function convertToSearchableReport(report: IARSubmission): SearchableReport {
	const createdAt = Math.floor(snowflakeToDate(BigInt(report.reportId)).getTime() / 1000);
	const reportedAt = Math.floor(report.reportedAt.getTime() / 1000);
	const resolvedAt = report.resolvedAt ? Math.floor(report.resolvedAt.getTime() / 1000) : null;
	return {
		id: report.reportId.toString(),
		reporterId: report.reporterId ? report.reporterId.toString() : 'anonymous',
		reportedAt,
		status: report.status,
		reportType: report.reportType,
		category: report.category,
		additionalInfo: report.additionalInfo,
		reportedUserId: report.reportedUserId?.toString() || null,
		reportedGuildId: report.reportedGuildId?.toString() || null,
		reportedGuildName: report.reportedGuildName,
		reportedMessageId: report.reportedMessageId?.toString() || null,
		reportedChannelId: report.reportedChannelId?.toString() || null,
		reportedChannelName: report.reportedChannelName,
		guildContextId: report.guildContextId?.toString() || null,
		resolvedAt,
		resolvedByAdminId: report.resolvedByAdminId?.toString() || null,
		publicComment: report.publicComment,
		createdAt,
	};
}
