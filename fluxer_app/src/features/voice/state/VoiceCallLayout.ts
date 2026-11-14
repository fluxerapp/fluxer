// SPDX-License-Identifier: AGPL-3.0-or-later

import AppStorage, {subscribeAppStorageScope} from '@app/features/platform/state/PersistentStorage';
import type {PinnableVoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {isRecord} from '@fluxer/record_utils/src/RecordGuards';
import {makeAutoObservable} from 'mobx';

export type LayoutMode = 'grid' | 'focus';
export type PinnedParticipantSource = PinnableVoiceTrackSource | null;

const FOCUS_MEMBERS_ROW_STORAGE_KEY = 'voice_focus_members_row';

class VoiceCallLayout {
	layoutMode: LayoutMode = 'grid';
	pinnedParticipantIdentity: string | null = null;
	pinnedParticipantSource: PinnedParticipantSource = null;
	focusMembersRowVisible = true;
	focusMembersRowHeight: number | null = null;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
		this.reload();
	}

	reload(): void {
		const stored = AppStorage.getJSON<unknown>(FOCUS_MEMBERS_ROW_STORAGE_KEY);
		const visible = isRecord(stored) ? stored.visible : undefined;
		const height = isRecord(stored) ? stored.height : undefined;
		this.focusMembersRowVisible = typeof visible === 'boolean' ? visible : true;
		this.focusMembersRowHeight = typeof height === 'number' && Number.isFinite(height) && height > 0 ? height : null;
	}

	setLayoutMode(mode: LayoutMode): void {
		if (mode === 'grid') {
			this.setPinnedParticipant(null);
			return;
		}
		this.layoutMode = mode;
	}

	setPinnedParticipant(identity: string | null, source: PinnedParticipantSource = null): void {
		this.pinnedParticipantIdentity = identity;
		this.pinnedParticipantSource = identity ? source : null;
		this.layoutMode = identity ? 'focus' : 'grid';
	}

	toggleFocusMembersRowVisible(): void {
		this.focusMembersRowVisible = !this.focusMembersRowVisible;
		this.persistFocusMembersRow();
	}

	setFocusMembersRowHeight(height: number | null): void {
		this.focusMembersRowHeight = height;
		this.persistFocusMembersRow();
	}

	reset(): void {
		this.layoutMode = 'grid';
		this.pinnedParticipantIdentity = null;
		this.pinnedParticipantSource = null;
	}

	private persistFocusMembersRow(): void {
		AppStorage.setJSON(FOCUS_MEMBERS_ROW_STORAGE_KEY, {
			visible: this.focusMembersRowVisible,
			height: this.focusMembersRowHeight,
		});
	}
}

const voiceCallLayout = new VoiceCallLayout();

subscribeAppStorageScope(voiceCallLayout.reload);

export default voiceCallLayout;
