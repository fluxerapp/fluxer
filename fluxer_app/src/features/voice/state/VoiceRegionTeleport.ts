// SPDX-License-Identifier: AGPL-3.0-or-later

import {makeAutoObservable} from 'mobx';

const VOICE_REGION_TELEPORT_SOUND_GRACE_MS = 2500;

class VoiceRegionTeleport {
	isTeleporting = false;
	private soundSuppressedUntilMs = 0;

	constructor() {
		makeAutoObservable<this, 'soundSuppressedUntilMs'>(this, {soundSuppressedUntilMs: false}, {autoBind: true});
	}

	beginTeleport(): void {
		this.isTeleporting = true;
	}

	endTeleport(): void {
		if (!this.isTeleporting) {
			return;
		}
		this.isTeleporting = false;
		const graceUntilMs = Date.now() + VOICE_REGION_TELEPORT_SOUND_GRACE_MS;
		this.soundSuppressedUntilMs = Math.max(this.soundSuppressedUntilMs, graceUntilMs);
	}

	shouldSuppressRejoinSounds(): boolean {
		if (this.isTeleporting) {
			return true;
		}
		return Date.now() < this.soundSuppressedUntilMs;
	}

	reset(): void {
		this.isTeleporting = false;
		this.soundSuppressedUntilMs = 0;
	}
}

export default new VoiceRegionTeleport();
