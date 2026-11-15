// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';

export const DISTRIBUTION_ID_PATTERN = /^[a-z0-9_]{1,32}$/;
export const DISTRIBUTION_RATE_BPS_MAX = 3000;

export const DistributionQueryParam = z
	.string()
	.max(64)
	.optional()
	.describe(
		'Distribution the client build was sold through. Unknown or inactive values fall back to the standard price',
	);

export const DistributionTokenParam = z
	.string()
	.max(4096)
	.optional()
	.describe('Opaque purchase token from the distribution, kept for reporting');

export const PriceDistributionResponse = z.object({
	id: z.string().regex(DISTRIBUTION_ID_PATTERN),
	rate_bps: z.number().int().min(1).max(DISTRIBUTION_RATE_BPS_MAX),
	rule_version: z.number().int().min(1),
});

export type PriceDistributionResponse = z.infer<typeof PriceDistributionResponse>;
