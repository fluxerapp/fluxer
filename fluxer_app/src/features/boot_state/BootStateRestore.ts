// SPDX-License-Identifier: AGPL-3.0-or-later

import Initialization from '@app/features/app/state/Initialization';
import {makeAutoObservable, reaction} from 'mobx';

const STALE_RECONNECT_NOTICE_GRACE_MS = 10_000;

type BootViewPhase = 'idle' | 'restoring' | 'stale' | 'live' | 'cold';

class BootStateRestore {
	phase: BootViewPhase = 'idle';
	staleGraceActive = false;
	private generation = 0;
	private graceTimer: ReturnType<typeof setTimeout> | null = null;

	constructor() {
		makeAutoObservable<this, 'generation' | 'graceTimer'>(
			this,
			{generation: false, graceTimer: false},
			{autoBind: true},
		);
		reaction(
			() => Initialization.isStale,
			(stale) => {
				if (!stale) {
					this.leaveStale();
				}
			},
		);
	}

	begin(): number | null {
		if (this.phase !== 'idle') {
			return null;
		}
		this.generation += 1;
		this.phase = 'restoring';
		return this.generation;
	}

	isCurrent(generation: number): boolean {
		return this.phase === 'restoring' && this.generation === generation;
	}

	enterStale(generation: number): boolean {
		if (!this.isCurrent(generation) || !Initialization.setStale()) {
			return false;
		}
		this.phase = 'stale';
		this.staleGraceActive = true;
		this.graceTimer = setTimeout(this.endGrace, STALE_RECONNECT_NOTICE_GRACE_MS);
		return true;
	}

	goCold(generation: number): void {
		if (!this.isCurrent(generation)) {
			return;
		}
		this.phase = 'cold';
	}

	private endGrace(): void {
		this.graceTimer = null;
		this.staleGraceActive = false;
	}

	private leaveStale(): void {
		if (this.phase !== 'stale') {
			return;
		}
		if (this.graceTimer !== null) {
			clearTimeout(this.graceTimer);
			this.graceTimer = null;
		}
		this.staleGraceActive = false;
		this.generation += 1;
		if (Initialization.isReady) {
			this.phase = 'live';
			return;
		}
		this.phase = 'idle';
	}
}

export default new BootStateRestore();
