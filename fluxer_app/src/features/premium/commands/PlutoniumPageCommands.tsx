// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import {GiftPlutoniumModal} from '@app/features/premium/components/modals/GiftPlutoniumModal';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';

export function openPlutoniumPage(): void {
	ModalCommands.popAll();
	RouterUtils.transitionTo(Routes.PLUTONIUM);
}

export function openGiftPlutoniumModal(): void {
	ModalCommands.push(
		modal(() => <GiftPlutoniumModal data-flx="premium.plutonium-page-commands.open-gift-plutonium-modal.modal" />),
	);
}

export function openGiftInventorySettings(): void {
	ModalCommands.push(
		modal(() => (
			<UserSettingsModal
				initialTab="gift_inventory"
				data-flx="premium.plutonium-page-commands.open-gift-inventory-settings.user-settings-modal"
			/>
		)),
	);
}
