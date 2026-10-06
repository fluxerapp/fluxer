// SPDX-License-Identifier: AGPL-3.0-or-later

import Keybind from '@app/features/input/state/InputKeybind';
import {compareStructural, reaction} from 'mobx';

export function reactToPushToTalkModeChanges(onChange: (options: {preserveSelfMute: boolean}) => void): () => void {
	return reaction(
		() => ({
			transmitMode: Keybind.transmitMode,
			pushToTalk: Keybind.isPushToTalkEffective(),
			pushToMute: Keybind.isPushToMuteEffective(),
		}),
		(current, previous) => onChange({preserveSelfMute: current.transmitMode === previous.transmitMode}),
		{equals: compareStructural},
	);
}
