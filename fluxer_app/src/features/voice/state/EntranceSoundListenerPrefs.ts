// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import {makePersistent} from '@app/features/platform/utils/MobXPersistence';
import {initializeStore} from '@app/features/platform/utils/StoreInitialization';
import {clampVoiceVolumePercent} from '@app/features/voice/utils/VoiceVolumeUtils';
import {makeAutoObservable} from 'mobx';

const logger = new Logger('EntranceSoundListenerPrefs');

class EntranceSoundListenerPrefs {
	volumes: Record<string, number> = {};
	localMutes: Record<string, boolean> = {};

	constructor() {
		makeAutoObservable(
			this,
			{
				getVolume: false,
				isMuted: false,
			},
			{autoBind: true},
		);
		initializeStore(this, () => this.initPersistence());
	}

	private async initPersistence(): Promise<void> {
		await makePersistent(this, 'EntranceSoundListenerPrefs', ['volumes', 'localMutes']);
	}

	getVolume(userId: string): number {
		return clampVoiceVolumePercent(this.volumes[userId] ?? 100);
	}

	setVolume(userId: string, volume: number): void {
		const clamped = clampVoiceVolumePercent(volume);
		this.volumes = {...this.volumes, [userId]: clamped};
		logger.debug(`Set entrance volume for ${userId}: ${clamped}`);
	}

	isMuted(userId: string): boolean {
		return this.localMutes[userId] ?? false;
	}

	setMuted(userId: string, muted: boolean): void {
		this.localMutes = {...this.localMutes, [userId]: muted};
		logger.debug(`Set entrance mute for ${userId}: ${muted}`);
	}
}

export default new EntranceSoundListenerPrefs();
