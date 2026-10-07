// SPDX-License-Identifier: AGPL-3.0-or-later

import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {makeAutoObservable} from 'mobx';

const DEFAULT_VOLUME = 1;

class VideoVolume {
	volume = DEFAULT_VOLUME;
	isMuted = false;
	private previousVolume = DEFAULT_VOLUME;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'VideoVolume', ['volume', 'isMuted']);
		if (this.volume > 0) {
			this.previousVolume = this.volume;
		}
	}

	setVolume(newVolume: number): void {
		const clamped = Math.max(0, Math.min(1, newVolume));
		this.volume = clamped;
		if (clamped > 0) {
			this.previousVolume = clamped;
		}
		if (this.isMuted && clamped > 0) {
			this.isMuted = false;
		}
	}

	toggleMute(): void {
		if (this.isMuted) {
			this.isMuted = false;
			if (this.volume === 0) {
				this.volume = this.previousVolume;
			}
		} else {
			this.isMuted = true;
		}
	}

	setMuted(muted: boolean): void {
		this.isMuted = muted;
		if (!muted && this.volume === 0) {
			this.volume = this.previousVolume;
		}
	}

	get effectiveVolume(): number {
		return this.isMuted ? 0 : this.volume;
	}
}

export default new VideoVolume();
