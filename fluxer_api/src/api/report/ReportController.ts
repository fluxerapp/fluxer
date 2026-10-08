// SPDX-License-Identifier: AGPL-3.0-or-later

import {viewerFromCtx} from '@app/api/experiment/ChannelThreadsGate';
import {RequireEmailAccountIdentity} from '@app/api/middleware/AccountIdentityMiddleware';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {
	ReportFlowMessageSubmissionRequest,
	ReportFlowPathParams,
	ReportFlowQuery,
	ReportFlowResponse,
	ReportFlowUserSubmissionRequest,
} from '@fluxer/schema/src/domains/report/ReportFlowSchemas';
import {
	DsaReportEmailSendRequest,
	DsaReportEmailVerifyRequest,
	DsaReportFlowRequest,
	DsaReportRequest,
	OkResponse,
	ReportGuildRequest,
	ReportMessageRequest,
	ReportResponse,
	ReportUserRequest,
	TicketResponse,
} from '@fluxer/schema/src/domains/report/ReportSchemas';

export function ReportController(app: HonoApp) {
	app.get(
		'/reports/flows/:target_type',
		RateLimitMiddleware(RateLimitConfigs.REPORT_LIST),
		OpenAPI({
			operationId: 'get_report_flow',
			summary: 'Get report flow',
			description:
				'Returns every screen of the report flow for a target type, rendered in the requested locale. Message and user flows are available in app and on the DSA form, the guild flow only on the DSA form.',
			responseSchema: ReportFlowResponse,
			statusCode: 200,
			security: [],
			tags: 'Reports',
		}),
		Validator('param', ReportFlowPathParams),
		Validator('query', ReportFlowQuery),
		async (ctx) => {
			const {target_type} = ctx.req.valid('param');
			const {surface, locale} = ctx.req.valid('query');
			const flow = ctx.get('reportRequestService').getReportFlow({
				target: target_type,
				surface,
				locale: locale ?? ctx.get('requestLocale') ?? null,
			});
			ctx.header('Cache-Control', 'private, no-cache');
			ctx.header('Vary', 'Accept-Language');
			return ctx.json(flow);
		},
	);
	app.post(
		'/reports/flows/message/submissions',
		RateLimitMiddleware(RateLimitConfigs.REPORT_CREATE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'submit_message_report_flow',
			summary: 'Submit message report flow',
			description:
				'Files a report about a message from the answers given in the in-app message report flow. The reporter must be able to access the channel and target message.',
			responseSchema: ReportResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Reports',
		}),
		Validator('json', ReportFlowMessageSubmissionRequest),
		async (ctx) => {
			return ctx.json(
				await ctx.get('reportRequestService').submitMessageReportFlow({
					user: ctx.get('user'),
					viewer: viewerFromCtx(ctx),
					data: ctx.req.valid('json'),
					locale: ctx.get('requestLocale') ?? null,
				}),
			);
		},
	);
	app.post(
		'/reports/flows/user/submissions',
		RateLimitMiddleware(RateLimitConfigs.REPORT_CREATE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'submit_user_report_flow',
			summary: 'Submit user report flow',
			description: 'Files a report about a user profile from the answers given in the in-app user profile report flow.',
			responseSchema: ReportResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Reports',
		}),
		Validator('json', ReportFlowUserSubmissionRequest),
		async (ctx) => {
			return ctx.json(
				await ctx.get('reportRequestService').submitUserReportFlow({
					user: ctx.get('user'),
					data: ctx.req.valid('json'),
					locale: ctx.get('requestLocale') ?? null,
				}),
			);
		},
	);
	app.post(
		'/reports/message',
		RateLimitMiddleware(RateLimitConfigs.REPORT_CREATE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'report_message',
			summary: 'Report message',
			description:
				'Submits a report about a message to moderators for content violation review. The reporter must be able to access the channel and target message.',
			responseSchema: ReportResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Reports',
		}),
		Validator('json', ReportMessageRequest),
		async (ctx) => {
			return ctx.json(
				await ctx.get('reportRequestService').reportMessage({
					user: ctx.get('user'),
					viewer: viewerFromCtx(ctx),
					data: ctx.req.valid('json'),
				}),
			);
		},
	);
	app.post(
		'/reports/user',
		RateLimitMiddleware(RateLimitConfigs.REPORT_CREATE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'report_user',
			summary: 'Report user',
			description: 'Submits a report about a user to moderators for content violation or behaviour review.',
			responseSchema: ReportResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Reports',
		}),
		Validator('json', ReportUserRequest),
		async (ctx) => {
			return ctx.json(
				await ctx.get('reportRequestService').reportUser({
					user: ctx.get('user'),
					data: ctx.req.valid('json'),
				}),
			);
		},
	);
	app.post(
		'/reports/guild',
		RateLimitMiddleware(RateLimitConfigs.REPORT_CREATE),
		LoginRequired,
		DefaultUserOnly,
		OpenAPI({
			operationId: 'report_guild',
			summary: 'Report guild',
			description: 'Submits a report about a guild to moderators for policy violation review.',
			responseSchema: ReportResponse,
			statusCode: 200,
			security: ['bearerToken', 'sessionToken'],
			tags: 'Reports',
		}),
		Validator('json', ReportGuildRequest),
		async (ctx) => {
			return ctx.json(
				await ctx.get('reportRequestService').reportGuild({
					user: ctx.get('user'),
					data: ctx.req.valid('json'),
				}),
			);
		},
	);
	app.post(
		'/reports/dsa/email/send',
		RateLimitMiddleware(RateLimitConfigs.DSA_REPORT_EMAIL_SEND),
		RequireEmailAccountIdentity,
		OpenAPI({
			operationId: 'send_dsa_report_email',
			summary: 'Send DSA report email',
			description:
				'Initiates DSA (Digital Services Act) report submission by sending verification email to reporter. Responds with 503 when the email could not be sent.',
			responseSchema: OkResponse,
			statusCode: 200,
			errorStatusCodes: [503],
			security: [],
			tags: 'Reports',
		}),
		Validator('json', DsaReportEmailSendRequest),
		async (ctx) => {
			await ctx.get('reportRequestService').sendDsaReportVerificationEmail({
				data: ctx.req.valid('json'),
				locale: ctx.get('requestLocale') ?? null,
			});
			return ctx.json({ok: true});
		},
	);
	app.post(
		'/reports/dsa/email/verify',
		RateLimitMiddleware(RateLimitConfigs.DSA_REPORT_EMAIL_VERIFY),
		RequireEmailAccountIdentity,
		OpenAPI({
			operationId: 'verify_dsa_report_email',
			summary: 'Verify DSA report email',
			description: 'Verifies the DSA report email and creates a report ticket for legal compliance.',
			responseSchema: TicketResponse,
			statusCode: 200,
			security: [],
			tags: 'Reports',
		}),
		Validator('json', DsaReportEmailVerifyRequest),
		async (ctx) => {
			return ctx.json(await ctx.get('reportRequestService').verifyDsaReportEmail({data: ctx.req.valid('json')}));
		},
	);
	app.post(
		'/reports/dsa',
		RateLimitMiddleware(RateLimitConfigs.DSA_REPORT_CREATE),
		OpenAPI({
			operationId: 'create_dsa_report',
			summary: 'Create DSA report',
			description:
				'Creates a DSA complaint report with verified email for Digital Services Act compliance. The reason comes from the answers given in the DSA report flow.',
			responseSchema: ReportResponse,
			requestSchema: DsaReportFlowRequest,
			statusCode: 200,
			security: [],
			tags: 'Reports',
		}),
		Validator('json', DsaReportRequest),
		async (ctx) => {
			return ctx.json(
				await ctx.get('reportRequestService').createDsaReport({
					data: ctx.req.valid('json'),
					locale: ctx.get('requestLocale') ?? null,
				}),
			);
		},
	);
}
