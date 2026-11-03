// SPDX-License-Identifier: AGPL-3.0-or-later

import * as PlutoniumPageCommands from '@app/features/premium/commands/PlutoniumPageCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';

interface OpenOptions {
	defaultGiftMode?: boolean;
}

export function open(optionsOrDefaultGiftMode: OpenOptions | boolean = {}): void {
	if (!shouldShowPremiumFeatures()) {
		return;
	}
	const options =
		typeof optionsOrDefaultGiftMode === 'boolean'
			? {defaultGiftMode: optionsOrDefaultGiftMode}
			: optionsOrDefaultGiftMode;
	const {defaultGiftMode = false} = options;
	if (defaultGiftMode) {
		PlutoniumPageCommands.openGiftPlutoniumModal();
	} else {
		PlutoniumPageCommands.openPlutoniumPage();
	}
}
