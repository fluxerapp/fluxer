// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HoldAction} from '@app/features/app/keybindings/utils/RuntimeKeybinds';

export class HoldSources {
	private readonly pttSources = new Set<string>();
	private readonly prioritySources = new Set<string>();
	private readonly ptmSources = new Set<string>();
	private readonly priorityVadSources = new Set<string>();
	private releaseTimer: ReturnType<typeof setTimeout> | null = null;

	constructor(private readonly getReleaseDelay: () => number) {}

	get prioritySpeakerHeld(): boolean {
		return this.prioritySources.size > 0 || this.priorityVadSources.size > 0;
	}

	press(action: HoldAction, sourceId: string): boolean {
		switch (action) {
			case 'voice_push_to_talk':
			case 'voice_push_to_talk_priority': {
				if (this.pttSources.has(sourceId)) return false;
				const wasEmpty = this.pttSources.size === 0;
				this.pttSources.add(sourceId);
				if (action === 'voice_push_to_talk_priority') this.prioritySources.add(sourceId);
				this.clearReleaseTimer();
				return wasEmpty;
			}
			case 'voice_push_to_mute':
				return this.addSource(this.ptmSources, sourceId);
			case 'voice_priority_vad':
				return this.addSource(this.priorityVadSources, sourceId);
		}
	}

	release(action: HoldAction, sourceId: string, onReleased?: () => void): void {
		switch (action) {
			case 'voice_push_to_talk':
			case 'voice_push_to_talk_priority': {
				if (!this.pttSources.delete(sourceId)) return;
				this.prioritySources.delete(sourceId);
				if (this.pttSources.size > 0) return;
				this.clearReleaseTimer();
				this.releaseTimer = setTimeout(() => {
					this.releaseTimer = null;
					onReleased?.();
				}, this.getReleaseDelay());
				return;
			}
			case 'voice_push_to_mute':
				if (this.removeSource(this.ptmSources, sourceId)) onReleased?.();
				return;
			case 'voice_priority_vad':
				if (this.removeSource(this.priorityVadSources, sourceId)) onReleased?.();
				return;
		}
	}

	resetPushToTalk(): void {
		this.pttSources.clear();
		this.prioritySources.clear();
		this.clearReleaseTimer();
	}

	resetPushToMute(): void {
		this.ptmSources.clear();
		this.priorityVadSources.clear();
	}

	private addSource(sources: Set<string>, sourceId: string): boolean {
		if (sources.has(sourceId)) return false;
		sources.add(sourceId);
		return sources.size === 1;
	}

	private removeSource(sources: Set<string>, sourceId: string): boolean {
		if (!sources.delete(sourceId)) return false;
		return sources.size === 0;
	}

	private clearReleaseTimer(): void {
		if (this.releaseTimer === null) return;
		clearTimeout(this.releaseTimer);
		this.releaseTimer = null;
	}
}
