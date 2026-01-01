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
import {Config} from '~/Config';
import {AdminACLs} from '~/Constants';
import {requireAdminACL} from '~/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {ProductType} from '~/stripe/ProductRegistry';
import {Validator} from '~/Validator';
import {GenerateBetaCodesRequest, GenerateGiftCodesRequest, type GiftProductType} from '../models/CodeRequestTypes';

const trimTrailingSlash = (value: string): string => (value.endsWith('/') ? value.slice(0, -1) : value);

const giftDurations: Record<GiftProductType, number> = {
	[ProductType.GIFT_1_MONTH]: 1,
	[ProductType.GIFT_1_YEAR]: 12,
	[ProductType.GIFT_VISIONARY]: 0,
};

export const CodesAdminController = (app: HonoApp) => {
	app.post(
		'/admin/codes/beta',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_CODE_GENERATION),
		requireAdminACL(AdminACLs.BETA_CODES_GENERATE),
		Validator('json', GenerateBetaCodesRequest),
		async (ctx) => {
			const {count} = ctx.req.valid('json');
			const codes = await ctx.get('adminService').generateBetaCodes(count);
			return ctx.json({codes});
		},
	);

	app.post(
		'/admin/codes/gift',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_CODE_GENERATION),
		requireAdminACL(AdminACLs.GIFT_CODES_GENERATE),
		Validator('json', GenerateGiftCodesRequest),
		async (ctx) => {
			const {count, product_type} = ctx.req.valid('json');
			const durationMonths = giftDurations[product_type];
			const codes = await ctx.get('adminService').generateGiftCodes(count, durationMonths);
			const baseUrl = trimTrailingSlash(Config.endpoints.gift);
			return ctx.json({
				codes: codes.map((code) => `${baseUrl}/${code}`),
			});
		},
	);
};
