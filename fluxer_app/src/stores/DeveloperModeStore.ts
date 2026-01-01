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
import {IS_DEV} from '~/lib/env';
import {makePersistent} from '~/lib/MobXPersistence';
import UserStore from '~/stores/UserStore';

const UNLOCK_TAP_THRESHOLD = 7;
const MAX_TAP_INTERVAL_MS = 1200;

class DeveloperModeStore {
	manuallyEnabled = false;

	private tapCount = 0;
	private lastTapAt: number | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'DeveloperModeStore', ['manuallyEnabled']);
	}

	get isDeveloper(): boolean {
		if (IS_DEV) return true;

		if (UserStore.currentUser?.isStaff?.()) return true;

		return this.manuallyEnabled;
	}

	private resetTaps(): void {
		this.tapCount = 0;
		this.lastTapAt = null;
	}

	registerBuildTap(): boolean {
		if (this.isDeveloper) {
			this.resetTaps();
			return false;
		}

		const now = Date.now();
		if (this.lastTapAt && now - this.lastTapAt <= MAX_TAP_INTERVAL_MS) {
			this.tapCount += 1;
		} else {
			this.tapCount = 1;
		}
		this.lastTapAt = now;

		if (this.tapCount >= UNLOCK_TAP_THRESHOLD) {
			this.manuallyEnabled = true;
			this.resetTaps();
			return true;
		}

		return false;
	}
}

export default new DeveloperModeStore();
