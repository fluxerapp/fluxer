// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead} from '@app/api/admin/AdminAuditRecorder';
import {enforcementActor} from '@app/api/enforcement/EnforcementActor';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	EnforcementDecisionEndRequest,
	EnforcementDecisionIdParam,
	EnforcementDecisionListQuery,
	EnforcementDecisionListResponse,
	EnforcementDecisionNoteRequest,
	EnforcementDecisionNoteResponse,
	EnforcementDecisionResponse,
	NetworkBlockCandidatesResponse,
} from '@fluxer/schema/src/domains/admin/EnforcementSchemas';
import {UserIdParam} from '@fluxer/schema/src/domains/common/CommonParamSchemas';

export function EnforcementAdminController(app: HonoApp) {
	app.get(
		'/admin/users/:user_id/network-block-candidates',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.USER_VIEW_IP),
		Validator('param', UserIdParam),
		OpenAPI({
			operationId: 'list_admin_user_network_block_candidates',
			summary: 'List networks a suspension or closure can block',
			responseSchema: NetworkBlockCandidatesResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Lists the addresses the account used in the last 30 days, most recent first, with how many other accounts that are not suspended or closed were last active on each. These are the addresses block_networks accepts on the suspend and close routes. Requires USER_VIEW_IP permission.',
		}),
		async (ctx) => {
			const {user_id: userId} = ctx.req.valid('param');
			const result = await ctx.get('adminService').enforcementService.listNetworkBlockCandidates(userId);
			await recordAdminRead(ctx, {
				targetType: 'user',
				targetId: userId,
				action: AdminAuditReadActions.LIST_NETWORK_BLOCK_CANDIDATES,
				metadata: {result_count: result.networks.length},
			});
			return ctx.json(result);
		},
	);
	app.get(
		'/admin/enforcement-decisions',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.ENFORCEMENT_VIEW),
		Validator('query', EnforcementDecisionListQuery),
		OpenAPI({
			operationId: 'list_admin_enforcement_decisions',
			summary: 'List enforcement decisions',
			responseSchema: EnforcementDecisionListResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Lists the enforcement decisions about one user or community, newest first, without notes or evidence. With review_queue=true it lists the automated decisions waiting for a person instead, such as purchase limits after a chargeback. A person takes one out of the queue by ending it or by recording a warning, suspension or closure on the same account. Report IDs are empty without REPORT_VIEW. Requires ENFORCEMENT_VIEW permission.',
		}),
		async (ctx) => {
			const query = ctx.req.valid('query');
			const result = await ctx.get('adminService').enforcementService.list(query, ctx.get('adminUserAcls'));
			await recordAdminRead(
				ctx,
				query.review_queue || query.subject_type === undefined || query.subject_id === undefined
					? {
							targetType: 'enforcement_decision',
							targetId: 0n,
							action: AdminAuditReadActions.LIST_ENFORCEMENT_DECISIONS,
							metadata: {list: 'review_queue', result_count: result.decisions.length},
						}
					: {
							targetType: query.subject_type,
							targetId: query.subject_id,
							action: AdminAuditReadActions.LIST_ENFORCEMENT_DECISIONS,
							metadata: {list: 'subject', result_count: result.decisions.length},
						},
			);
			return ctx.json(result);
		},
	);
	app.get(
		'/admin/enforcement-decisions/:decision_id',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.ENFORCEMENT_VIEW),
		Validator('param', EnforcementDecisionIdParam),
		OpenAPI({
			operationId: 'get_admin_enforcement_decision',
			summary: 'Get enforcement decision',
			responseSchema: EnforcementDecisionResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Returns one enforcement decision with its notes, evidence and the notice the holder sees now. Evidence content and profile values are null without MESSAGE_LOOKUP, and report IDs are empty without REPORT_VIEW. Requires ENFORCEMENT_VIEW permission.',
		}),
		async (ctx) => {
			const {decision_id: decisionId} = ctx.req.valid('param');
			const result = await ctx.get('adminService').enforcementService.get(decisionId, ctx.get('adminUserAcls'));
			await recordAdminRead(ctx, {
				targetType: 'enforcement_decision',
				targetId: decisionId,
				action: AdminAuditReadActions.VIEW_ENFORCEMENT_DECISION,
				metadata: {decision_id: decisionId, kind: result.kind},
			});
			return ctx.json(result);
		},
	);
	app.post(
		'/admin/enforcement-decisions/:decision_id/notes',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.ENFORCEMENT_ANNOTATE),
		Validator('param', EnforcementDecisionIdParam),
		Validator('json', EnforcementDecisionNoteRequest),
		OpenAPI({
			operationId: 'annotate_admin_enforcement_decision',
			summary: 'Add a note to an enforcement decision',
			responseSchema: EnforcementDecisionNoteResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Appends an internal note to an enforcement decision. Notes are staff only, never change and expire with the evidence after a year. Creates audit log entry. Requires ENFORCEMENT_ANNOTATE permission.',
		}),
		async (ctx) => {
			const {decision_id: decisionId} = ctx.req.valid('param');
			return ctx.json(
				await ctx
					.get('adminService')
					.enforcementService.addNote(decisionId, ctx.req.valid('json').body, enforcementActor(ctx)),
			);
		},
	);
	app.post(
		'/admin/enforcement-decisions/:decision_id/end',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.ENFORCEMENT_VIEW),
		Validator('param', EnforcementDecisionIdParam),
		Validator('json', EnforcementDecisionEndRequest),
		OpenAPI({
			operationId: 'end_admin_enforcement_decision',
			summary: 'End an enforcement decision',
			responseSchema: EnforcementDecisionResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
			description:
				'Ends an active decision as lifted or reversed, undoes what can be undone and emails the holder. Ending a suspension lifts it, and returns 409 when a later decision replaced it. Warnings, removals and suspensions that ended as expired can only be reversed. Creates audit log entry. Requires ENFORCEMENT_VIEW permission plus the permission of the route that made the decision, such as USER_TEMP_BAN for a suspension or USER_UPDATE_FLAGS for a warning.',
		}),
		async (ctx) => {
			const {decision_id: decisionId} = ctx.req.valid('param');
			return ctx.json(
				await ctx.get('adminService').enforcementService.end(decisionId, ctx.req.valid('json'), enforcementActor(ctx)),
			);
		},
	);
}
