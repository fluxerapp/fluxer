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

import {ALL_FEATURE_FLAGS, FEATURE_FLAG_KEY_PREFIX, type FeatureFlag} from '~/constants/FeatureFlags';
import {fetchMany, upsertOne} from '~/database/Cassandra';
import type {InstanceConfigurationRow} from '~/database/CassandraTypes';
import {InstanceConfiguration} from '~/Tables';

const FETCH_ALL_CONFIG_QUERY = InstanceConfiguration.selectCql();

export class FeatureFlagRepository {
	async getFeatureFlag(flag: FeatureFlag): Promise<Set<string>> {
		const allConfigs = await this.getAllFeatureFlags();
		return allConfigs.get(flag) ?? new Set();
	}

	async setFeatureFlag(flag: FeatureFlag, guildIds: Set<string>): Promise<void> {
		const key = `${FEATURE_FLAG_KEY_PREFIX}${flag}`;
		const value = Array.from(guildIds).join(',');
		await upsertOne(
			InstanceConfiguration.upsertAll({
				key,
				value,
				updated_at: new Date(),
			}),
		);
	}

	async getAllFeatureFlags(): Promise<Map<FeatureFlag, Set<string>>> {
		const rows = await fetchMany<InstanceConfigurationRow>(FETCH_ALL_CONFIG_QUERY, {});
		const result = new Map<FeatureFlag, Set<string>>();

		for (const flag of ALL_FEATURE_FLAGS) {
			result.set(flag, new Set());
		}

		for (const row of rows) {
			if (row.key.startsWith(FEATURE_FLAG_KEY_PREFIX) && row.value != null) {
				const flagName = row.key.slice(FEATURE_FLAG_KEY_PREFIX.length) as FeatureFlag;
				if (ALL_FEATURE_FLAGS.includes(flagName)) {
					const guildIds = row.value
						.split(',')
						.map((id) => id.trim())
						.filter((id) => id.length > 0);
					result.set(flagName, new Set(guildIds));
				}
			}
		}

		return result;
	}
}
