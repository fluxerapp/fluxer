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

import {fetchMany, fetchOne, upsertOne} from '~/database/Cassandra';
import type {InstanceConfigurationRow} from '~/database/CassandraTypes';
import {InstanceConfiguration} from '~/Tables';

const FETCH_CONFIG_QUERY = InstanceConfiguration.selectCql({
	where: InstanceConfiguration.where.eq('key'),
	limit: 1,
});

const FETCH_ALL_CONFIG_QUERY = InstanceConfiguration.selectCql();

export interface InstanceConfig {
	manualReviewEnabled: boolean;
	manualReviewScheduleEnabled: boolean;
	manualReviewScheduleStartHourUtc: number;
	manualReviewScheduleEndHourUtc: number;
	registrationAlertsWebhookUrl: string | null;
	systemAlertsWebhookUrl: string | null;
}

export class InstanceConfigRepository {
	async getConfig(key: string): Promise<string | null> {
		const row = await fetchOne<InstanceConfigurationRow>(FETCH_CONFIG_QUERY, {key});
		return row?.value ?? null;
	}

	async getAllConfigs(): Promise<Map<string, string>> {
		const rows = await fetchMany<InstanceConfigurationRow>(FETCH_ALL_CONFIG_QUERY, {});
		const configs = new Map<string, string>();
		for (const row of rows) {
			if (row.value != null) {
				configs.set(row.key, row.value);
			}
		}
		return configs;
	}

	async setConfig(key: string, value: string): Promise<void> {
		await upsertOne(
			InstanceConfiguration.upsertAll({
				key,
				value,
				updated_at: new Date(),
			}),
		);
	}

	async getInstanceConfig(): Promise<InstanceConfig> {
		const configs = await this.getAllConfigs();

		return {
			manualReviewEnabled: configs.get('manual_review_enabled') === 'true',
			manualReviewScheduleEnabled: configs.get('manual_review_schedule_enabled') === 'true',
			manualReviewScheduleStartHourUtc: Number.parseInt(
				configs.get('manual_review_schedule_start_hour_utc') ?? '0',
				10,
			),
			manualReviewScheduleEndHourUtc: Number.parseInt(configs.get('manual_review_schedule_end_hour_utc') ?? '23', 10),
			registrationAlertsWebhookUrl: configs.get('registration_alerts_webhook_url') ?? null,
			systemAlertsWebhookUrl: configs.get('system_alerts_webhook_url') ?? null,
		};
	}

	async setManualReviewEnabled(enabled: boolean): Promise<void> {
		await this.setConfig('manual_review_enabled', enabled ? 'true' : 'false');
	}

	async setManualReviewSchedule(scheduleEnabled: boolean, startHourUtc: number, endHourUtc: number): Promise<void> {
		await this.setConfig('manual_review_schedule_enabled', scheduleEnabled ? 'true' : 'false');
		await this.setConfig('manual_review_schedule_start_hour_utc', String(startHourUtc));
		await this.setConfig('manual_review_schedule_end_hour_utc', String(endHourUtc));
	}

	async setRegistrationAlertsWebhookUrl(url: string | null): Promise<void> {
		if (url) {
			await this.setConfig('registration_alerts_webhook_url', url);
		} else {
			await this.setConfig('registration_alerts_webhook_url', '');
		}
	}

	async setSystemAlertsWebhookUrl(url: string | null): Promise<void> {
		if (url) {
			await this.setConfig('system_alerts_webhook_url', url);
		} else {
			await this.setConfig('system_alerts_webhook_url', '');
		}
	}

	isManualReviewActiveNow(config: InstanceConfig): boolean {
		if (!config.manualReviewEnabled) {
			return false;
		}

		if (!config.manualReviewScheduleEnabled) {
			return true;
		}

		const nowUtc = new Date();
		const currentHour = nowUtc.getUTCHours();

		const start = config.manualReviewScheduleStartHourUtc;
		const end = config.manualReviewScheduleEndHourUtc;

		if (start <= end) {
			return currentHour >= start && currentHour <= end;
		}
		return currentHour >= start || currentHour <= end;
	}
}
