// SPDX-License-Identifier: AGPL-3.0-or-later

import * as PlutoniumPageCommands from '@app/features/premium/commands/PlutoniumPageCommands';
import {PremiumModal} from '@app/features/premium/components/modals/PremiumModal';
import PlutoniumPageRollout from '@app/features/premium/state/PlutoniumPageRollout';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';

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
	if (PlutoniumPageRollout.enabled) {
		if (defaultGiftMode) {
			PlutoniumPageCommands.openGiftPlutoniumModal();
		} else {
			PlutoniumPageCommands.openPlutoniumPage();
		}
		return;
	}
	ModalCommands.push(
		modal(() => (
			<PremiumModal defaultGiftMode={defaultGiftMode} data-flx="premium.premium-modal-commands.open.premium-modal" />
		)),
	);
}
