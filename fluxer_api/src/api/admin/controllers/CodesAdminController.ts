// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {FeatureNotAvailableSelfHostedError} from '@fluxer/errors/src/domains/core/FeatureNotAvailableSelfHostedError';
import {CodesResponse, GenerateGiftCodesRequest} from '@fluxer/schema/src/domains/admin/AdminSchemas';

function trimTrailingSlash(value: string): string {
	return value.endsWith('/') ? value.slice(0, -1) : value;
}

export function CodesAdminController(app: HonoApp) {
	app.post(
		'/admin/gift-codes',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_CODE_GENERATION),
		requireAdminACL(AdminACLs.GIFT_CODES_GENERATE),
		Validator('json', GenerateGiftCodesRequest),
		OpenAPI({
			operationId: 'create_admin_gift_codes',
			summary: 'Issue gift codes',
			description:
				'Create one-use Plutonium gift codes with an explicit positive duration and return their complete redemption links. Lifetime gifts are not supported. Not available on self-hosted instances. Requires GIFT_CODES_GENERATE permission.',
			responseSchema: CodesResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			if (Config.instance.selfHosted) {
				throw new FeatureNotAvailableSelfHostedError();
			}
			const {count, duration_type, duration_quantity} = ctx.req.valid('json');
			const codes = await ctx.get('adminService').codeGenerationService.generateGiftCodes({
				count,
				durationType: duration_type,
				durationQuantity: duration_quantity,
			});
			const baseUrl = trimTrailingSlash(Config.endpoints.gift);
			return ctx.json({
				codes: codes.map((code) => `${baseUrl}/${code}`),
			});
		},
	);
}
