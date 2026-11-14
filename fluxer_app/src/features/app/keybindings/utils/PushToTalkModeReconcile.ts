// SPDX-License-Identifier: AGPL-3.0-or-later

import Keybind from '@app/features/input/state/InputKeybind';
import {compareStructural, reaction} from 'mobx';

export function reactToPushToTalkModeChanges(onChange: () => void): () => void {
	return reaction(
		() => ({
			pushToTalk: Keybind.isPushToTalkEffective(),
			pushToMute: Keybind.isPushToMuteEffective(),
		}),
		() => onChange(),
		{equals: compareStructural},
	);
}
