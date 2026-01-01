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
import {InstanceConfigRepository} from '~/instance/InstanceConfigRepository';
import {requireAdminACL} from '~/middleware/AdminMiddleware';
import {RateLimitMiddleware} from '~/middleware/RateLimitMiddleware';
import {RateLimitConfigs} from '~/RateLimitConfig';
import {z} from '~/Schema';
import {Validator} from '~/Validator';

const instanceConfigRepository = new InstanceConfigRepository();

export const InstanceConfigAdminController = (app: HonoApp) => {
	app.post(
		'/admin/instance-config/get',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_LOOKUP),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_VIEW),
		async (ctx) => {
			const config = await instanceConfigRepository.getInstanceConfig();
			const isActiveNow = instanceConfigRepository.isManualReviewActiveNow(config);

			return ctx.json({
				manual_review_enabled: config.manualReviewEnabled,
				manual_review_schedule_enabled: config.manualReviewScheduleEnabled,
				manual_review_schedule_start_hour_utc: config.manualReviewScheduleStartHourUtc,
				manual_review_schedule_end_hour_utc: config.manualReviewScheduleEndHourUtc,
				manual_review_active_now: isActiveNow,
				registration_alerts_webhook_url: config.registrationAlertsWebhookUrl,
				system_alerts_webhook_url: config.systemAlertsWebhookUrl,
			});
		},
	);

	app.post(
		'/admin/instance-config/update',
		RateLimitMiddleware(RateLimitConfigs.ADMIN_USER_MODIFY),
		requireAdminACL(AdminACLs.INSTANCE_CONFIG_UPDATE),
		Validator(
			'json',
			z.object({
				manual_review_enabled: z.boolean().optional(),
				manual_review_schedule_enabled: z.boolean().optional(),
				manual_review_schedule_start_hour_utc: z.number().min(0).max(23).optional(),
				manual_review_schedule_end_hour_utc: z.number().min(0).max(23).optional(),
				registration_alerts_webhook_url: z.string().url().nullable().optional(),
				system_alerts_webhook_url: z.string().url().nullable().optional(),
			}),
		),
		async (ctx) => {
			const data = ctx.req.valid('json');

			if (data.manual_review_enabled !== undefined) {
				await instanceConfigRepository.setManualReviewEnabled(data.manual_review_enabled);
			}

			if (
				data.manual_review_schedule_enabled !== undefined ||
				data.manual_review_schedule_start_hour_utc !== undefined ||
				data.manual_review_schedule_end_hour_utc !== undefined
			) {
				const currentConfig = await instanceConfigRepository.getInstanceConfig();

				const scheduleEnabled = data.manual_review_schedule_enabled ?? currentConfig.manualReviewScheduleEnabled;
				const startHour = data.manual_review_schedule_start_hour_utc ?? currentConfig.manualReviewScheduleStartHourUtc;
				const endHour = data.manual_review_schedule_end_hour_utc ?? currentConfig.manualReviewScheduleEndHourUtc;

				await instanceConfigRepository.setManualReviewSchedule(scheduleEnabled, startHour, endHour);
			}

			if (data.registration_alerts_webhook_url !== undefined) {
				await instanceConfigRepository.setRegistrationAlertsWebhookUrl(data.registration_alerts_webhook_url);
			}

			if (data.system_alerts_webhook_url !== undefined) {
				await instanceConfigRepository.setSystemAlertsWebhookUrl(data.system_alerts_webhook_url);
			}

			const updatedConfig = await instanceConfigRepository.getInstanceConfig();
			const isActiveNow = instanceConfigRepository.isManualReviewActiveNow(updatedConfig);

			return ctx.json({
				manual_review_enabled: updatedConfig.manualReviewEnabled,
				manual_review_schedule_enabled: updatedConfig.manualReviewScheduleEnabled,
				manual_review_schedule_start_hour_utc: updatedConfig.manualReviewScheduleStartHourUtc,
				manual_review_schedule_end_hour_utc: updatedConfig.manualReviewScheduleEndHourUtc,
				manual_review_active_now: isActiveNow,
				registration_alerts_webhook_url: updatedConfig.registrationAlertsWebhookUrl,
				system_alerts_webhook_url: updatedConfig.systemAlertsWebhookUrl,
			});
		},
	);
};
