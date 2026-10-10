// SPDX-License-Identifier: AGPL-3.0-or-later

import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {DEFAULT_FREE_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import {applyRuleToResolvedLimits, ruleMatches, sortRulesBySpecificity} from '@fluxer/limits/src/LimitRuleRuntime';
import type {
	LimitConfigSnapshot,
	LimitEvaluationOptions,
	LimitEvaluationResult,
	LimitMatchContext,
} from '@fluxer/limits/src/LimitTypes';

export function resolveLimits(
	snapshot: LimitConfigSnapshot,
	ctx: LimitMatchContext,
	options?: LimitEvaluationOptions,
): LimitEvaluationResult {
	const evaluationContext = options?.evaluationContext ?? 'user';
	const resolvedLimits = {...(options?.baseLimits ?? DEFAULT_FREE_LIMITS)};
	for (const rule of sortRulesBySpecificity(snapshot.rules)) {
		if (ruleMatches(rule.filters, ctx)) {
			applyRuleToResolvedLimits(resolvedLimits, rule, evaluationContext);
		}
	}
	return {limits: resolvedLimits};
}

export function resolveLimit(
	snapshot: LimitConfigSnapshot,
	ctx: LimitMatchContext,
	key: LimitKey,
	options?: LimitEvaluationOptions,
): number {
	return resolveLimits(snapshot, ctx, options).limits[key];
}
