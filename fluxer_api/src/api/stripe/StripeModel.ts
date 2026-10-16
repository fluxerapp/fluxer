// SPDX-License-Identifier: AGPL-3.0-or-later

import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {GiftCode} from '@app/api/models/GiftCode';

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
		createdBy = await userCacheService.getUserPartialResponse(giftCode.createdByUserId, requestCache);
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
		userCacheService.getUserPartialResponse(giftCode.createdByUserId, requestCache),
		giftCode.redeemedByUserId ? userCacheService.getUserPartialResponse(giftCode.redeemedByUserId, requestCache) : null,
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
