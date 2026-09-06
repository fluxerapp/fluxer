// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {
	LIMIT_CATEGORY_LABELS,
	LIMIT_KEY_METADATA,
	LIMIT_KEYS,
	type LimitKey,
} from '@fluxer/constants/src/LimitConfigMetadata';
import type {LimitConfigSnapshot, LimitRule} from '@fluxer/limits/src/LimitTypes';
import {LimitConfigGetResponse, LimitConfigUpdateRequest} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {Config} from '../../Config';
import {createDefaultLimitConfig} from '../../constants/LimitConfig';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import {requireAdminACL} from '../../middleware/AdminMiddleware';
import {RateLimitMiddleware} from '../../middleware/RateLimitMiddleware';
import {OpenAPI} from '../../middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '../../RateLimitConfig';
import type {HonoApp} from '../../types/HonoEnv';
import {Validator} from '../../Validator';

function formatConfig(config: LimitConfigSnapshot) {
	const defaults = createDefaultLimitConfig({selfHosted: Config.instance.selfHosted});
	const defaultLimitsMap: Record<string, Record<LimitKey, number>> = {};
	for (const rule of defaults.rules) {
		defaultLimitsMap[rule.id] = rule.limits as Record<LimitKey, number>;
	}
	return {
		limit_config: config,
		limit_config_json: JSON.stringify(config, null, 2),
		self_hosted: Config.instance.selfHosted,
		defaults: defaultLimitsMap,
		metadata: LIMIT_KEY_METADATA,
		categories: LIMIT_CATEGORY_LABELS,
		limit_keys: LIMIT_KEYS,
	};
}

function trackModifiedFields(config: LimitConfigSnapshot): LimitConfigSnapshot {
	const defaults = createDefaultLimitConfig({selfHosted: Config.instance.selfHosted});
	const defaultRulesMap = buildRulesMap(defaults.rules);
	const rulesWithTracking = config.rules.map((rule) => trackRuleModifiedFields(rule, defaultRulesMap));
	return {
		...config,
		rules: rulesWithTracking,
	};
}

function buildRulesMap(rules: Array<LimitRule>): Map<string, LimitRule> {
	const map = new Map<string, LimitRule>();
	for (const rule of rules) {
		map.set(rule.id, rule);
	}
	return map;
}

function trackRuleModifiedFields(
	rule: LimitRule,
	defaultRulesMap: Map<string, LimitRule>,
): LimitRule & {
	modifiedFields?: Array<LimitKey>;
} {
	const defaultRule = defaultRulesMap.get(rule.id);
	const fallbackDefault = defaultRule ?? defaultRulesMap.get('default');
	if (!fallbackDefault) {
		return {
			...rule,
			modifiedFields: Object.keys(rule.limits) as Array<LimitKey>,
		};
	}
	const modifiedFields = findModifiedLimits(rule.limits, fallbackDefault.limits);
	return {
		...rule,
		modifiedFields: modifiedFields.length > 0 ? modifiedFields : undefined,
	};
}

function findModifiedLimits(
	currentLimits: Partial<Record<LimitKey, number>>,
	defaultLimits: Partial<Record<LimitKey, number>>,
): Array<LimitKey> {
	const modified: Array<LimitKey> = [];
	for (const key of LIMIT_KEYS) {
		const currentValue = currentLimits[key];
		const defaultValue = defaultLimits[key];
		if (currentValue !== undefined && currentValue !== defaultValue) {
			modified.push(key);
		}
	}
	return modified;
}

export function LimitConfigAdminController(app: HonoApp) {
	app.get(
		'/admin/limit-config',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.INSTANCE_LIMIT_CONFIG_VIEW),
		OpenAPI({
			operationId: 'get_admin_limit_config',
			summary: 'Get limit configuration',
			description:
				'Retrieves rate limit configuration including message limits, upload limits, and request throttles. Shows defaults, metadata, and any modifications from defaults. Requires INSTANCE_LIMIT_CONFIG_VIEW permission.',
			responseSchema: LimitConfigGetResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const limitConfigService = ctx.get('limitConfigService') as LimitConfigService;
			const snapshot = limitConfigService.getConfigSnapshot();
			return ctx.json(formatConfig(snapshot));
		},
	);
	app.put(
		'/admin/limit-config',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_LIMIT_CONFIG_UPDATE),
		Validator('json', LimitConfigUpdateRequest),
		OpenAPI({
			operationId: 'replace_admin_limit_config',
			summary: 'Replace limit configuration',
			description:
				'Replaces the stored limit configuration, which covers message throughput, upload sizes, and request throttles, with the supplied document. Changes apply immediately to all new operations. Requires INSTANCE_LIMIT_CONFIG_UPDATE permission.',
			responseSchema: LimitConfigGetResponse,
			statusCode: 200,
			security: 'adminApiKey',
			tags: 'Admin',
		}),
		async (ctx) => {
			const limitConfigService = ctx.get('limitConfigService') as LimitConfigService;
			const data = ctx.req.valid('json');
			const normalized: LimitConfigSnapshot = {
				...data.limit_config,
				traitDefinitions: data.limit_config.traitDefinitions ?? [],
			};
			const withTracking = trackModifiedFields(normalized);
			await limitConfigService.updateConfig(withTracking);
			return ctx.json(formatConfig(limitConfigService.getConfigSnapshot()));
		},
	);
}
