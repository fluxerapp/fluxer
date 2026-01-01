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
import {makePersistent} from '~/lib/MobXPersistence';

const DEFAULT_VOLUME = 1;

class AudioVolumeStore {
	volume = DEFAULT_VOLUME;
	isMuted = false;
	private previousVolume = DEFAULT_VOLUME;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'AudioVolumeStore', ['volume', 'isMuted']);
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

export default new AudioVolumeStore();
