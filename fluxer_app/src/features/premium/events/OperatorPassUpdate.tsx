// SPDX-License-Identifier: AGPL-3.0-or-later

import {OPERATOR_PASS_NAME} from '@app/features/app/config/I18nDisplayConstants';
import type {GatewayHandlerContext} from '@app/features/gateway/events/EventRouter';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {loadLazyModule} from '@app/features/platform/utils/LazyModuleLoader';
import OperatorPassHoldings from '@app/features/premium/state/OperatorPassHoldings';
import {getOperatorPassOfferUserName} from '@app/features/premium/utils/OperatorPassUtils';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import * as OwnProfileBadgesCommands from '@app/features/user/commands/OwnProfileBadgesCommands';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import type {
	OperatorPassOfferResponse,
	OperatorPassPassesResponse,
} from '@fluxer/schema/src/domains/operator_pass/OperatorPassSchemas';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';

const OFFER_RECEIVED_DESCRIPTOR = msg({
	message: '{senderName} is passing you an {operatorPassName}. Select to accept or decline.',
	comment:
		'In-app notification when someone offers this account one of their Operator Passes. Selecting it opens the offer. {senderName} is a display name. Operator Pass is a product name.',
});

const OFFER_TOAST_TIMEOUT_MS = 15000;

const logger = new Logger('OperatorPassUpdate');

function OfferReceivedToast({offer}: {offer: OperatorPassOfferResponse}) {
	const {i18n} = useLingui();
	return (
		<span data-flx="premium.operator-pass-update.offer-toast">
			{i18n._(OFFER_RECEIVED_DESCRIPTOR, {
				senderName: getOperatorPassOfferUserName(offer.sender),
				operatorPassName: OPERATOR_PASS_NAME,
			})}
		</span>
	);
}

export function handleOperatorPassUpdate(data: OperatorPassPassesResponse, _context: GatewayHandlerContext): void {
	const previous = OperatorPassHoldings.passes;
	OperatorPassHoldings.setPasses(data);
	if (previous?.count !== data.count) {
		UserProfileCommands.clearCurrentUserProfiles();
		void OwnProfileBadgesCommands.refresh();
	}
	for (const offer of OperatorPassHoldings.takeNewIncomingOffers()) {
		ToastCommands.createToast({
			type: 'info',
			timeout: OFFER_TOAST_TIMEOUT_MS,
			children: <OfferReceivedToast offer={offer} data-flx="premium.operator-pass-update.offer-received-toast" />,
			onClick: () => {
				void loadLazyModule(() => import('@app/features/premium/components/operator_pass/OperatorPassOfferModal'))
					.then(({openOperatorPassOfferModal}) => openOperatorPassOfferModal(offer.id))
					.catch((error: unknown) => logger.error('Failed to open the Pass it on offer', error));
			},
		});
	}
}
