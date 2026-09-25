// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {LoginRequired} from '@app/api/middleware/AuthMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {entityTagMatches} from '@app/api/utils/EntityTag';
import {Headers as HttpHeaders} from '@fluxer/constants/src/Headers';
import {resolveDomainMigrationAssignment} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import {resolveVoiceNoiseSuppressionAssignment} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {ExperimentAssignmentsResponse} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';

export function ExperimentController(app: HonoApp) {
	app.get(
		'/experiments',
		RateLimitMiddleware(RateLimitConfigs.DEFAULT),
		LoginRequired,
		OpenAPI({
			operationId: 'get_experiments',
			summary: 'Get the experiment assignments',
			description:
				'Returns the polling cadence and every experiment assignment resolved for the authenticated user from the instance configuration. Clients revalidate with If-None-Match and receive 304 when nothing changed.',
			responseSchema: ExperimentAssignmentsResponse,
			statusCode: [200, 304],
			security: ['bearerToken', 'sessionToken', 'botToken'],
			tags: ['Experiments'],
		}),
		async (ctx) => {
			const instanceConfigRepository = ctx.get('instanceConfigRepository');
			const [delivery, voiceConfig, domainMigrationConfig] = await Promise.all([
				instanceConfigRepository.getExperimentDeliveryConfig(),
				instanceConfigRepository.getVoiceNoiseSuppressionConfig(),
				instanceConfigRepository.getDomainMigrationConfig(),
			]);
			const userId = ctx.get('user').id.toString();
			const body: ExperimentAssignmentsResponse = {
				poll_interval_seconds: delivery.poll_interval_seconds,
				poll_jitter_percent: delivery.poll_jitter_percent,
				assignments: {
					voice_noise_suppression: resolveVoiceNoiseSuppressionAssignment(voiceConfig, userId),
					domain_migration: resolveDomainMigrationAssignment(domainMigrationConfig, userId),
				},
			};
			const etag = `"${createHash('sha256').update(JSON.stringify(body)).digest('hex')}"`;
			ctx.header(HttpHeaders.ETAG, etag);
			ctx.header(HttpHeaders.CACHE_CONTROL, 'private, no-cache');
			ctx.header('Vary', 'Authorization');
			const ifNoneMatch = ctx.req.header(HttpHeaders.IF_NONE_MATCH);
			if (ifNoneMatch !== undefined && entityTagMatches(ifNoneMatch, etag)) {
				return ctx.body(null, 304);
			}
			return ctx.json(body);
		},
	);
}
