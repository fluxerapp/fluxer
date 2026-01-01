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

import {makeAutoObservable} from 'mobx';
import {ALL_FEATURE_FLAGS, type FeatureFlag, FeatureFlags} from '~/Constants';
import FeatureFlagOverridesStore from '~/stores/FeatureFlagOverridesStore';

type FeatureFlagGuildMap = Record<FeatureFlag, Set<string>>;

class FeatureFlagStore {
	private featureFlagGuilds: FeatureFlagGuildMap;

	constructor() {
		this.featureFlagGuilds = FeatureFlagStore.createEmptyMap();
		makeAutoObservable(this, {}, {autoBind: true});
	}

	private static createEmptyMap(): FeatureFlagGuildMap {
		const map: FeatureFlagGuildMap = {} as FeatureFlagGuildMap;
		for (const flag of ALL_FEATURE_FLAGS) {
			map[flag] = new Set();
		}
		return map;
	}

	handleConnectionOpen(featureFlags?: Record<FeatureFlag, Array<string>>): void {
		this.featureFlagGuilds = FeatureFlagStore.createEmptyMap();

		if (!featureFlags) {
			return;
		}

		for (const flag of ALL_FEATURE_FLAGS) {
			const guildIds = featureFlags[flag] ?? [];
			this.featureFlagGuilds[flag] = new Set(guildIds);
		}
	}

	private getGuildSet(flag: FeatureFlag): Set<string> {
		return this.featureFlagGuilds[flag] ?? new Set();
	}

	isFeatureEnabled(flag: FeatureFlag, guildId?: string): boolean {
		const overrideEnabled = FeatureFlagOverridesStore.getOverride(flag);
		if (overrideEnabled !== null) {
			return overrideEnabled;
		}

		if (!guildId) {
			return false;
		}

		return this.getGuildSet(flag).has(guildId);
	}

	isMessageSchedulingEnabled(guildId?: string): boolean {
		return this.isFeatureEnabled(FeatureFlags.MESSAGE_SCHEDULING, guildId);
	}

	isExpressionPacksEnabled(guildId?: string): boolean {
		return this.isFeatureEnabled(FeatureFlags.EXPRESSION_PACKS, guildId);
	}

	getGuildIdsForFlag(flag: FeatureFlag): Array<string> {
		return Array.from(this.getGuildSet(flag));
	}

	hasAccessToAnyEnabledGuild(flag: FeatureFlag, guildIds: Array<string>): boolean {
		const guildSet = this.getGuildSet(flag);
		if (guildSet.size === 0 || guildIds.length === 0) {
			return false;
		}
		return guildIds.some((guildId) => guildSet.has(guildId));
	}
}

export default new FeatureFlagStore();
