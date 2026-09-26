// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminAuditReadActions} from '@app/api/admin/AdminAuditActions';
import {recordAdminRead, recordAdminWrite} from '@app/api/admin/AdminAuditRecorder';
import {Config} from '@app/api/Config';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {requireAdminACL} from '@app/api/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '@app/api/middleware/RateLimitMiddleware';
import {OpenAPI} from '@app/api/middleware/ResponseTypeMiddleware';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import type {HonoApp} from '@app/api/types/HonoEnv';
import {Validator} from '@app/api/Validator';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {LIMIT_CATEGORY_LABELS, LIMIT_KEY_METADATA, LIMIT_KEYS} from '@fluxer/constants/src/LimitConfigMetadata';
import type {LimitConfigSnapshot, LimitRule} from '@fluxer/limits/src/LimitTypes';
import {LimitConfigGetResponse, LimitConfigUpdateRequest} from '@fluxer/schema/src/domains/admin/AdminSchemas';

function formatConfig(service: LimitConfigService) {
	const config = service.getConfigSnapshot();
	const defaults = service.getDefaultConfigSnapshot();
	return {
		limit_config: config,
		limit_config_json: JSON.stringify(config, null, 2),
		self_hosted: Config.instance.selfHosted,
		defaults: Object.fromEntries(defaults.rules.map((rule) => [rule.id, rule.limits])),
		metadata: LIMIT_KEY_METADATA,
		categories: LIMIT_CATEGORY_LABELS,
		limit_keys: LIMIT_KEYS,
	};
}

function describeLimitRule(rule: LimitRule): string {
	return JSON.stringify([
		rule.filters?.traits ?? [],
		rule.filters?.guildFeatures ?? [],
		LIMIT_KEYS.map((key) => rule.limits[key] ?? null),
	]);
}

function countChangedLimitRules(before: LimitConfigSnapshot, after: LimitConfigSnapshot): number {
	const previous = new Map(before.rules.map((rule) => [rule.id, describeLimitRule(rule)]));
	const next = new Map(after.rules.map((rule) => [rule.id, describeLimitRule(rule)]));
	const ruleIds = new Set([...previous.keys(), ...next.keys()]);
	return Array.from(ruleIds).filter((ruleId) => previous.get(ruleId) !== next.get(ruleId)).length;
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
			const response = formatConfig(limitConfigService);
			await recordAdminRead(ctx, {
				targetType: 'limit_config',
				targetId: 0n,
				action: AdminAuditReadActions.GET_LIMIT_CONFIG,
				metadata: {rule_count: response.limit_config.rules.length},
			});
			return ctx.json(response);
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
			const previous = limitConfigService.getConfigSnapshot();
			await limitConfigService.updateConfig(normalized);
			const response = formatConfig(limitConfigService);
			await recordAdminWrite(ctx, {
				targetType: 'limit_config',
				targetId: 0n,
				action: 'update_limit_config',
				metadata: {
					rule_count: response.limit_config.rules.length,
					changed_rule_count: countChangedLimitRules(previous, response.limit_config),
					trait_definition_count: response.limit_config.traitDefinitions.length,
				},
			});
			return ctx.json(response);
		},
	);
}
