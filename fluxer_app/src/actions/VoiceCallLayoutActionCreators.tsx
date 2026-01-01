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

import type {LayoutMode} from '~/stores/VoiceCallLayoutStore';
import VoiceCallLayoutStore from '~/stores/VoiceCallLayoutStore';

export const setLayoutMode = (mode: LayoutMode): void => {
	VoiceCallLayoutStore.setLayoutMode(mode);
};

export const setPinnedParticipant = (identity: string | null): void => {
	VoiceCallLayoutStore.setPinnedParticipant(identity);
};

export const markUserOverride = (): void => {
	VoiceCallLayoutStore.markUserOverride();
};
