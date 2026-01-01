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
import {AdminACLs} from '~/Constants';
import {ALL_FEATURE_FLAGS, FeatureFlags} from '~/constants/FeatureFlags';
import {requireAdminACL} from '~/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {z} from '~/Schema';
import {Validator} from '~/Validator';

export const FeatureFlagAdminController = (app: HonoApp) => {
	app.post(
		'/admin/feature-flags/get',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.FEATURE_FLAG_VIEW),
		async (ctx) => {
			const featureFlagService = ctx.get('featureFlagService');
			const config = featureFlagService.getConfigForSession();

			return ctx.json({
				feature_flags: config,
			});
		},
	);

	app.post(
		'/admin/feature-flags/update',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.FEATURE_FLAG_MANAGE),
		Validator(
			'json',
			z.object({
				flag: z.enum([FeatureFlags.MESSAGE_SCHEDULING, FeatureFlags.EXPRESSION_PACKS]),
				guild_ids: z.string(),
			}),
		),
		async (ctx) => {
			const {flag, guild_ids} = ctx.req.valid('json');
			const featureFlagService = ctx.get('featureFlagService');

			const guildIdArray = guild_ids
				.split(',')
				.map((id) => id.trim())
				.filter((id) => id.length > 0);
			const guildIdSet = new Set(guildIdArray);

			await featureFlagService.setFeatureGuildIds(flag, guildIdSet);

			const updatedConfig = featureFlagService.getConfigForSession();

			return ctx.json({
				feature_flags: updatedConfig,
			});
		},
	);

	app.post(
		'/admin/feature-flags/list',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.FEATURE_FLAG_VIEW),
		async (ctx) => {
			return ctx.json({
				flags: ALL_FEATURE_FLAGS.map((flag) => ({
					key: flag,
					label: getFeatureFlagLabel(flag),
					description: getFeatureFlagDescription(flag),
				})),
			});
		},
	);
};

function getFeatureFlagLabel(flag: string): string {
	switch (flag) {
		case FeatureFlags.MESSAGE_SCHEDULING:
			return 'Message Scheduling';
		case FeatureFlags.EXPRESSION_PACKS:
			return 'Expression Packs';
		default:
			return flag;
	}
}

function getFeatureFlagDescription(flag: string): string {
	switch (flag) {
		case FeatureFlags.MESSAGE_SCHEDULING:
			return 'Allows users to schedule messages to be sent later';
		case FeatureFlags.EXPRESSION_PACKS:
			return 'Allows users to create and share emoji/sticker packs';
		default:
			return '';
	}
}
