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

import {z} from '~/Schema';
import {ProductType} from '~/stripe/ProductRegistry';

const GiftProductTypes = [ProductType.GIFT_1_MONTH, ProductType.GIFT_1_YEAR, ProductType.GIFT_VISIONARY] as const;

const MAX_CODES_PER_REQUEST = 100;

export const GiftProductTypeEnum = z.enum(GiftProductTypes);

export type GiftProductType = z.infer<typeof GiftProductTypeEnum>;

export const GenerateBetaCodesRequest = z.object({
	count: z.number().int().min(1).max(MAX_CODES_PER_REQUEST),
});

export type GenerateBetaCodesRequest = z.infer<typeof GenerateBetaCodesRequest>;

export const GenerateGiftCodesRequest = z.object({
	count: z.number().int().min(1).max(MAX_CODES_PER_REQUEST),
	product_type: GiftProductTypeEnum,
});

export type GenerateGiftCodesRequest = z.infer<typeof GenerateGiftCodesRequest>;
