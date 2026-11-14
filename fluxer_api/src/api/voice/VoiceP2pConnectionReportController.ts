// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createGuildID} from '@app/api/BrandedTypes';
import {getInstanceProductName} from '@app/api/instance/ProductName';
import {DefaultUserOnly, LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {getVoiceP2pConnectionReportRepository} from '@app/api/middleware/ServiceSingletons';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {lookupGeoip} from '@app/api/utils/IpUtils';
import {resolveSessionClientInfo} from '@app/api/utils/SessionClientIdentity';
import {Validator} from '@app/api/Validator';
import {VoiceP2pConnectionReportsRequest} from '@fluxer/schema/src/domains/voice/VoiceP2pConnectionReportSchemas';

export function VoiceP2pConnectionReportController(app: HonoApp) {
	app.post(
		'/voice/p2p/connection-reports',
		RateLimitMiddleware(RateLimitConfigs.VOICE_P2P_CONNECTION_REPORTS),
		LoginRequired,
		DefaultUserOnly,
		Validator('json', VoiceP2pConnectionReportsRequest),
		OpenAPI({
			operationId: 'create_voice_p2p_connection_reports',
			summary: 'Report peer-to-peer connection outcomes',
			description:
				'Records how peer connections of a peer-to-peer voice call settled, one report for each remote peer. Fluxer adds the request address, its country and the client platform of the session.',
			requestSchema: VoiceP2pConnectionReportsRequest,
			responseSchema: null,
			statusCode: 204,
			security: ['sessionToken'],
			tags: ['Voice'],
		}),
		async (ctx) => {
			const {reports} = ctx.req.valid('json');
			if (!(await ctx.get('instanceConfigRepository').getVoiceP2pConfig()).enabled) {
				return ctx.body(null, 204);
			}
			const authSession = ctx.get('authSession');
			const {countryCode, normalizedIp} = await lookupGeoip(ctx.req.raw);
			const clientInfo = resolveSessionClientInfo({
				userAgent: authSession?.clientUserAgent ?? null,
				reportedOs: authSession?.clientOs ?? null,
				productName: getInstanceProductName(),
			});
			const userId = ctx.get('user').id;
			const snowflakeService = ctx.get('snowflakeService');
			const reportedAt = new Date();
			const rows = await Promise.all(
				reports.map(async (report) => ({
					user_id: userId,
					report_id: await snowflakeService.generate(),
					reported_at: reportedAt,
					channel_id: createChannelID(report.channel_id),
					guild_id: report.guild_id === null ? null : createGuildID(report.guild_id),
					participant_count: report.participant_count,
					outcome: report.outcome,
					local_candidate_type: report.local_candidate_type,
					remote_candidate_type: report.remote_candidate_type,
					ip_family: report.ip_family,
					protocol: report.protocol,
					setup_ms: report.setup_ms,
					ice_restarted: report.ice_restarted,
					country: countryCode,
					ip: normalizedIp,
					client_platform: clientInfo.platform,
					client_os: clientInfo.os,
				})),
			);
			await getVoiceP2pConnectionReportRepository().insertReports(rows);
			return ctx.body(null, 204);
		},
	);
}
