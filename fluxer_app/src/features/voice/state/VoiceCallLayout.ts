// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PinnableVoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {makeAutoObservable} from 'mobx';

export type LayoutMode = 'grid' | 'focus';
export type PinnedParticipantSource = PinnableVoiceTrackSource | null;

class VoiceCallLayout {
	layoutMode: LayoutMode = 'grid';
	pinnedParticipantIdentity: string | null = null;
	pinnedParticipantSource: PinnedParticipantSource = null;
	userOverride = false;
	focusMembersRowVisible = true;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	setLayoutMode(mode: LayoutMode): void {
		this.layoutMode = mode;
	}

	setPinnedParticipant(identity: string | null, source: PinnedParticipantSource = null): void {
		this.pinnedParticipantIdentity = identity;
		this.pinnedParticipantSource = identity ? source : null;
		this.layoutMode = identity ? 'focus' : 'grid';
	}

	toggleFocusMembersRowVisible(): void {
		this.focusMembersRowVisible = !this.focusMembersRowVisible;
	}

	markUserOverride(): void {
		this.userOverride = true;
	}

	reset(): void {
		this.layoutMode = 'grid';
		this.pinnedParticipantIdentity = null;
		this.pinnedParticipantSource = null;
		this.userOverride = false;
		this.focusMembersRowVisible = true;
	}
}

export default new VoiceCallLayout();
