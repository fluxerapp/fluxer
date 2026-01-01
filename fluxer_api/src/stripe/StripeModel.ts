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

import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {GiftCode} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {createStringType, z} from '~/Schema';
import {getCachedUserPartialResponse} from '~/user/UserCacheHelpers';
import {UserPartialResponse} from '~/user/UserModel';

export const CreateCheckoutSessionRequest = z.object({
	price_id: createStringType(),
});

export type CreateCheckoutSessionRequest = z.infer<typeof CreateCheckoutSessionRequest>;

export const GiftCodeResponse = z.object({
	code: z.string(),
	duration_months: z.number().int(),
	redeemed: z.boolean(),
	created_by: z.lazy(() => UserPartialResponse).nullish(),
});

export type GiftCodeResponse = z.infer<typeof GiftCodeResponse>;

export const GiftCodeMetadataResponse = z.object({
	code: z.string(),
	duration_months: z.number().int(),
	created_at: z.iso.datetime(),
	created_by: z.lazy(() => UserPartialResponse),
	redeemed_at: z.iso.datetime().nullish(),
	redeemed_by: z.lazy(() => UserPartialResponse).nullish(),
});

export type GiftCodeMetadataResponse = z.infer<typeof GiftCodeMetadataResponse>;

interface MapGiftCodeToResponseParams {
	giftCode: GiftCode;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
	includeCreator?: boolean;
}

interface MapGiftCodeToMetadataResponseParams {
	giftCode: GiftCode;
	userCacheService: UserCacheService;
	requestCache: RequestCache;
}

export const mapGiftCodeToResponse = async ({
	giftCode,
	userCacheService,
	requestCache,
	includeCreator = false,
}: MapGiftCodeToResponseParams): Promise<GiftCodeResponse> => {
	let createdBy = null;
	if (includeCreator) {
		createdBy = await getCachedUserPartialResponse({
			userId: giftCode.createdByUserId,
			userCacheService,
			requestCache,
		});
	}

	return {
		code: giftCode.code,
		duration_months: giftCode.durationMonths,
		redeemed: !!giftCode.redeemedAt,
		created_by: createdBy,
	};
};

export const mapGiftCodeToMetadataResponse = async ({
	giftCode,
	userCacheService,
	requestCache,
}: MapGiftCodeToMetadataResponseParams): Promise<GiftCodeMetadataResponse> => {
	const [createdBy, redeemedBy] = await Promise.all([
		getCachedUserPartialResponse({
			userId: giftCode.createdByUserId,
			userCacheService,
			requestCache,
		}),
		giftCode.redeemedByUserId
			? getCachedUserPartialResponse({
					userId: giftCode.redeemedByUserId,
					userCacheService,
					requestCache,
				})
			: null,
	]);

	return {
		code: giftCode.code,
		duration_months: giftCode.durationMonths,
		created_at: giftCode.createdAt.toISOString(),
		created_by: createdBy,
		redeemed_at: giftCode.redeemedAt?.toISOString() ?? null,
		redeemed_by: redeemedBy,
	};
};
