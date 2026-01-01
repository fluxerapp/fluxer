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

import {makeAutoObservable, runInAction} from 'mobx';
import {makePersistent} from '~/lib/MobXPersistence';
import * as SoundUtils from '~/utils/SoundUtils';
import {SoundType} from '~/utils/SoundUtils';

interface SoundSettings {
	allSoundsDisabled: boolean;
	disabledSounds: Partial<Record<SoundType, boolean>>;
}

class SoundStore {
	currentlyPlaying = new Set<SoundType>();
	volume = 0.7;
	incomingCallActive = false;
	settings: SoundSettings = {
		allSoundsDisabled: false,
		disabledSounds: {},
	};

	constructor() {
		makeAutoObservable(
			this,
			{
				currentlyPlaying: false,
			},
			{autoBind: true},
		);
		this.initPersistence();
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'SoundStore', ['settings'], {version: 3});
	}

	playSound(sound: SoundType, loop = false): void {
		if (!this.isSoundEnabled(sound)) {
			return;
		}

		SoundUtils.playSound(sound, loop, this.volume)
			.then((result) => {
				if (result) {
					runInAction(() => {
						const newPlaying = new Set(this.currentlyPlaying);
						newPlaying.add(sound);
						this.currentlyPlaying = newPlaying;
					});
				}
			})
			.catch((error) => console.warn('Failed to play sound:', error));
	}

	stopSound(sound: SoundType): void {
		SoundUtils.stopSound(sound);
		const newPlaying = new Set(this.currentlyPlaying);
		newPlaying.delete(sound);
		this.currentlyPlaying = newPlaying;
	}

	stopAllSounds(): void {
		SoundUtils.stopAllSounds();
		this.currentlyPlaying = new Set();
		this.incomingCallActive = false;
	}

	startIncomingRing(): void {
		this.playSound(SoundType.IncomingRing, true);
		this.incomingCallActive = true;
	}

	stopIncomingRing(): void {
		this.stopSound(SoundType.IncomingRing);
		this.incomingCallActive = false;
	}

	private isSoundEnabled(soundType: SoundType): boolean {
		return !this.settings.allSoundsDisabled && !this.settings.disabledSounds[soundType];
	}

	toggleEnabled(): void {
		this.settings = {
			...this.settings,
			allSoundsDisabled: !this.settings.allSoundsDisabled,
		};
		if (this.settings.allSoundsDisabled) {
			SoundUtils.stopAllSounds();
		}
	}

	updateSettings(settings: {allSoundsDisabled?: boolean; soundType?: SoundType; enabled?: boolean}): void {
		const {soundType, enabled, allSoundsDisabled} = settings;

		if (allSoundsDisabled !== undefined) {
			this.settings = {
				...this.settings,
				allSoundsDisabled,
			};
			if (allSoundsDisabled) {
				this.stopAllSounds();
			}
		} else if (soundType && enabled !== undefined) {
			const newDisabledSounds = {...this.settings.disabledSounds};
			if (enabled) {
				delete newDisabledSounds[soundType];
			} else {
				newDisabledSounds[soundType] = true;
			}

			this.settings = {
				...this.settings,
				disabledSounds: newDisabledSounds,
			};
		}
	}

	setVolume(volume: number): void {
		this.volume = Math.max(0, Math.min(1, volume));
	}

	getSoundEnabled(): boolean {
		return !this.settings.allSoundsDisabled;
	}

	getSoundSettings(): SoundSettings {
		return this.settings;
	}

	isSoundTypeEnabled(soundType: SoundType): boolean {
		return this.isSoundEnabled(soundType);
	}

	getVolume(): number {
		return this.volume;
	}

	isIncomingCallActive(): boolean {
		return this.incomingCallActive;
	}

	isPlayingSound(sound: SoundType): boolean {
		return this.currentlyPlaying.has(sound);
	}
}

export default new SoundStore();
