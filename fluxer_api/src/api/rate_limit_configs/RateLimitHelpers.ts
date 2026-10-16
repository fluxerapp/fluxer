// SPDX-License-Identifier: AGPL-3.0-or-later

import type {RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';

export type RateLimitSection = {
	readonly [key: string]: RouteRateLimitConfig;
};
type UnionToIntersection<U> = (U extends unknown ? (k: U) => void : never) extends (k: infer I) => void ? I : never;

export function mergeRateLimitSections<T extends ReadonlyArray<RateLimitSection>>(
	...sections: T
): UnionToIntersection<T[number]> {
	return Object.assign({}, ...sections) as UnionToIntersection<T[number]>;
}
