// SPDX-License-Identifier: AGPL-3.0-or-later

import {IS_DEV} from '@app/features/platform/types/Env';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import Users from '@app/features/user/state/Users';
import {makeAutoObservable} from 'mobx';

const UNLOCK_TAP_THRESHOLD = 7;
const MAX_TAP_INTERVAL_MS = 1200;

class DeveloperMode {
	manuallyEnabled = false;
	private tapCount = 0;
	private lastTapAt: number | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'DeveloperMode', ['manuallyEnabled']);
	}

	get isDeveloper(): boolean {
		if (IS_DEV) return true;
		if (Users.currentUser?.isStaff?.()) return true;
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

export default new DeveloperMode();
