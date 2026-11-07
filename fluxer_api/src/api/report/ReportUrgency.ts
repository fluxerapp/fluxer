// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveReportFlowAnswers} from '@app/api/report/flows/ReportFlowRegistry';
import {isUrgentLegacyCategory} from '@app/api/report/flows/ReportReasonCatalog';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import {readRequestJsonBody} from '@app/api/utils/RequestJsonBody';
import {
	ReportFlowGuildSubmissionRequest,
	ReportFlowMessageSubmissionRequest,
	type ReportFlowTargetType,
	ReportFlowUserSubmissionRequest,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {
	ReportGuildRequest,
	ReportMessageRequest,
	ReportUserRequest,
} from '@fluxer/schema/src/domains/report/ReportSchemas';
import type {Context} from 'hono';

const FLOW_SUBMISSION_SCHEMAS = {
	message: ReportFlowMessageSubmissionRequest,
	user: ReportFlowUserSubmissionRequest,
	guild: ReportFlowGuildSubmissionRequest,
} as const;

const LEGACY_REPORT_SCHEMAS = {
	message: ReportMessageRequest,
	user: ReportUserRequest,
	guild: ReportGuildRequest,
} as const;

function isUrgentFlowSubmission(target: ReportFlowTargetType, body: unknown): boolean {
	const parsed = FLOW_SUBMISSION_SCHEMAS[target].safeParse(body);
	if (!parsed.success) return false;
	try {
		return resolveReportFlowAnswers({
			target,
			surface: 'in_app',
			revisionHash: parsed.data.revision_hash,
			steps: parsed.data.steps,
		}).urgent;
	} catch {
		return false;
	}
}

function isUrgentLegacyReport(target: ReportFlowTargetType, body: unknown): boolean {
	const parsed = LEGACY_REPORT_SCHEMAS[target].safeParse(body);
	return parsed.success && isUrgentLegacyCategory(target, parsed.data.category);
}

export function urgentFlowSubmissionRequest(target: ReportFlowTargetType) {
	return async (ctx: Context<HonoEnv>): Promise<boolean> => {
		const body = await readRequestJsonBody(ctx.req);
		return body.parsed && isUrgentFlowSubmission(target, body.value);
	};
}

export function urgentLegacyReportRequest(target: ReportFlowTargetType) {
	return async (ctx: Context<HonoEnv>): Promise<boolean> => {
		const body = await readRequestJsonBody(ctx.req);
		return body.parsed && isUrgentLegacyReport(target, body.value);
	};
}
