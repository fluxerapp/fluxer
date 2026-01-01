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
import type {FeatureFlag} from '~/Constants';
import {makePersistent} from '~/lib/MobXPersistence';

type FeatureFlagOverrides = Partial<Record<FeatureFlag, boolean>>;

class FeatureFlagOverridesStore {
	overrides: FeatureFlagOverrides = {};

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		void this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'FeatureFlagOverridesStore', ['overrides']);
	}

	getOverride(flag: FeatureFlag): boolean | null {
		if (Object.hasOwn(this.overrides, flag)) {
			return this.overrides[flag] ?? null;
		}
		return null;
	}

	setOverride(flag: FeatureFlag, value: boolean | null): void {
		const next = {...this.overrides};

		if (value === null) {
			delete next[flag];
		} else {
			next[flag] = value;
		}

		this.overrides = next;
	}
}

export default new FeatureFlagOverridesStore();
