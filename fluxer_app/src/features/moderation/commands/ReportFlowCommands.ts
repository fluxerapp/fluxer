// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {
	ReportFlowResponse,
	ReportFlowStep,
	ReportFlowSurface,
	ReportFlowTargetType,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';

const logger = new Logger('ReportFlow');

export type ReportFlowSubmissionTarget =
	| {type: 'message'; channelId: string; messageId: string}
	| {type: 'user'; userId: string; guildId?: string};

export async function fetchReportFlow(
	targetType: ReportFlowTargetType,
	surface: ReportFlowSurface,
	locale: string,
): Promise<ReportFlowResponse> {
	const response = await http.get<ReportFlowResponse>(Endpoints.REPORT_FLOW(targetType), {
		query: {surface, locale},
	});
	return response.body;
}

function buildTargetFields(target: ReportFlowSubmissionTarget): Record<string, string> {
	switch (target.type) {
		case 'message':
			return {channel_id: target.channelId, message_id: target.messageId};
		case 'user':
			return target.guildId ? {user_id: target.userId, guild_id: target.guildId} : {user_id: target.userId};
	}
}

export async function submitReportFlow(
	target: ReportFlowSubmissionTarget,
	flow: ReportFlowResponse,
	steps: ReadonlyArray<ReportFlowStep>,
): Promise<void> {
	if (DeveloperOptions.noOpInAppReports) {
		logger.info('No-op in-app reports is enabled; skipping network call.');
		return;
	}
	try {
		await http.post(Endpoints.REPORT_FLOW_SUBMISSIONS(target.type), {
			body: {
				...buildTargetFields(target),
				revision_hash: flow.revision_hash,
				locale: flow.locale,
				steps,
			},
		});
	} catch (error) {
		logger.error(`Failed to submit ${target.type} report:`, error);
		throw error;
	}
}
