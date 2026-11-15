// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	OPERATOR_PASS_CHECKOUT_MODAL_KEY,
	OperatorPassCheckoutModal,
} from '@app/features/premium/components/modals/OperatorPassCheckoutModal';
import OperatorPassCheckoutSession from '@app/features/premium/state/OperatorPassCheckoutSession';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';

let checkoutModalMount = 0;

export function openCheckoutModal({canceled = false, sessionId}: {canceled?: boolean; sessionId?: string} = {}): void {
	if (sessionId) OperatorPassCheckoutSession.adoptReturnedSession(sessionId, OperatorPassHoldings.count ?? 0);
	else if (canceled) OperatorPassCheckoutSession.clear();
	const mount = ++checkoutModalMount;
	ModalCommands.pushWithKey(
		modal(() => (
			<OperatorPassCheckoutModal
				key={mount}
				canceled={canceled}
				data-flx="premium.operator-pass-modal-commands.open-checkout-modal.operator-pass-checkout-modal"
			/>
		)),
		OPERATOR_PASS_CHECKOUT_MODAL_KEY,
	);
}

export function reopenCheckoutModalAfterReturn(): void {
	if (ModalCommands.getTopModalKey() !== null) return;
	openCheckoutModal();
}
