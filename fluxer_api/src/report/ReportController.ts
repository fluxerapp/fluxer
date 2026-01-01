/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {HonoApp} from '~/App';
import {createChannelID, createGuildID, createMessageID, createUserID} from '~/BrandedTypes';
import {DefaultUserOnly, LoginRequired} from '~/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {Validator} from '~/Validator';
import {reportStatusToString} from './IReportRepository';
import {
	DsaReportEmailSendRequest,
	DsaReportEmailVerifyRequest,
	DsaReportRequest,
	ReportGuildRequest,
	ReportMessageRequest,
	ReportUserRequest,
} from './ReportModel';

export const ReportController = (app: HonoApp) => {
	app.post(
		'/reports/message',
		RateLimitMiddleware(RateLimitConfigs.REPORT_CREATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', ReportMessageRequest),
		async (ctx) => {
			const {channel_id, message_id, category, additional_info} = ctx.req.valid('json');
			const user = ctx.get('user');
			const reporter = {
				id: user.id,
				email: user.email,
				fullLegalName: null,
				countryOfResidence: null,
			};
			const report = await ctx
				.get('reportService')
				.reportMessage(reporter, createChannelID(channel_id), createMessageID(message_id), category, additional_info);

			return ctx.json({
				report_id: report.reportId.toString(),
				status: reportStatusToString(report.status),
				reported_at: report.reportedAt.toISOString(),
			});
		},
	);

	app.post(
		'/reports/user',
		RateLimitMiddleware(RateLimitConfigs.REPORT_CREATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', ReportUserRequest),
		async (ctx) => {
			const {user_id, category, additional_info, guild_id} = ctx.req.valid('json');
			const user = ctx.get('user');
			const reporter = {
				id: user.id,
				email: user.email,
				fullLegalName: null,
				countryOfResidence: null,
			};
			const report = await ctx
				.get('reportService')
				.reportUser(
					reporter,
					createUserID(user_id),
					category,
					additional_info,
					guild_id ? createGuildID(guild_id) : undefined,
				);

			return ctx.json({
				report_id: report.reportId.toString(),
				status: reportStatusToString(report.status),
				reported_at: report.reportedAt.toISOString(),
			});
		},
	);

	app.post(
		'/reports/guild',
		RateLimitMiddleware(RateLimitConfigs.REPORT_CREATE),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', ReportGuildRequest),
		async (ctx) => {
			const {guild_id, category, additional_info} = ctx.req.valid('json');
			const user = ctx.get('user');
			const reporter = {
				id: user.id,
				email: user.email,
				fullLegalName: null,
				countryOfResidence: null,
			};
			const report = await ctx
				.get('reportService')
				.reportGuild(reporter, createGuildID(guild_id), category, additional_info);

			return ctx.json({
				report_id: report.reportId.toString(),
				status: reportStatusToString(report.status),
				reported_at: report.reportedAt.toISOString(),
			});
		},
	);

	app.post(
		'/reports/dsa/email/send',
		RateLimitMiddleware(RateLimitConfigs.DSA_REPORT_EMAIL_SEND),
		Validator('json', DsaReportEmailSendRequest),
		async (ctx) => {
			const {email} = ctx.req.valid('json');
			await ctx.get('reportService').sendDsaReportVerificationCode(email);
			return ctx.json({ok: true});
		},
	);

	app.post(
		'/reports/dsa/email/verify',
		RateLimitMiddleware(RateLimitConfigs.DSA_REPORT_EMAIL_VERIFY),
		Validator('json', DsaReportEmailVerifyRequest),
		async (ctx) => {
			const {email, code} = ctx.req.valid('json');
			const ticket = await ctx.get('reportService').verifyDsaReportEmail(email, code);
			return ctx.json({ticket});
		},
	);

	app.post(
		'/reports/dsa',
		RateLimitMiddleware(RateLimitConfigs.DSA_REPORT_CREATE),
		Validator('json', DsaReportRequest),
		async (ctx) => {
			const report = await ctx.get('reportService').createDsaReport(ctx.req.valid('json'));
			return ctx.json({
				report_id: report.reportId.toString(),
				status: reportStatusToString(report.status),
				reported_at: report.reportedAt.toISOString(),
			});
		},
	);
};
