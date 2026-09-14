// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {GiftCode} from '@app/api/models/GiftCode';
import {getCachedUserPartialResponse} from '@app/api/user/UserCacheHelpers';
import type {GiftCodeMetadataResponse, GiftCodeResponse} from '@fluxer/schema/src/domains/premium/GiftCodeSchemas';

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

export async function mapGiftCodeToResponse({
	giftCode,
	userCacheService,
	requestCache,
	includeCreator = false,
}: MapGiftCodeToResponseParams): Promise<GiftCodeResponse> {
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
		duration_type: giftCode.durationType,
		duration_quantity: giftCode.durationQuantity,
		redeemed: !!giftCode.redeemedAt,
		created_by: createdBy,
	};
}

export async function mapGiftCodeToMetadataResponse({
	giftCode,
	userCacheService,
	requestCache,
}: MapGiftCodeToMetadataResponseParams): Promise<GiftCodeMetadataResponse> {
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
		duration_type: giftCode.durationType,
		duration_quantity: giftCode.durationQuantity,
		created_at: giftCode.createdAt.toISOString(),
		created_by: createdBy,
		redeemed_at: giftCode.redeemedAt?.toISOString() ?? null,
		redeemed_by: redeemedBy,
	};
}
