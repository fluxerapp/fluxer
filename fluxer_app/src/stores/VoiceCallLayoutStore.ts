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

export type LayoutMode = 'grid' | 'focus';

class VoiceCallLayoutStore {
	layoutMode: LayoutMode = 'grid';
	pinnedParticipantIdentity: string | null = null;
	userOverride = false;

	constructor() {
		makeAutoObservable(this, {}, {autoBind: true});
	}

	getLayoutMode(): LayoutMode {
		return this.layoutMode;
	}

	getPinnedParticipantIdentity(): string | null {
		return this.pinnedParticipantIdentity;
	}

	setLayoutMode(mode: LayoutMode): void {
		this.layoutMode = mode;
	}

	setPinnedParticipant(identity: string | null): void {
		this.pinnedParticipantIdentity = identity;
		this.layoutMode = identity ? 'focus' : 'grid';
	}

	setUserOverride(value: boolean): void {
		this.userOverride = value;
	}

	markUserOverride(): void {
		this.userOverride = true;
	}

	toggleLayoutMode(): void {
		const newLayoutMode = this.layoutMode === 'grid' ? 'focus' : 'grid';
		this.layoutMode = newLayoutMode;
		if (this.layoutMode === 'grid') {
			this.pinnedParticipantIdentity = null;
		}
	}

	clearPinnedParticipant(): void {
		this.pinnedParticipantIdentity = null;
		this.layoutMode = 'grid';
	}

	reset(): void {
		this.layoutMode = 'grid';
		this.pinnedParticipantIdentity = null;
		this.userOverride = false;
	}
}

export default new VoiceCallLayoutStore();
