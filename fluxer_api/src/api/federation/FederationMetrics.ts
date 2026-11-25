// SPDX-License-Identifier: AGPL-3.0-or-later

import {registerCounter} from '@fluxer/hono/src/middleware/Metrics';

export const federationIncomingTotal = registerCounter(
	'fluxer_api_federation_incoming_total',
	'Incoming federation events, by result',
);
export const federationAppliedTotal = registerCounter(
	'fluxer_api_federation_applied_total',
	'Federation inbox events applied, by result',
);
export const federationOutboxTotal = registerCounter(
	'fluxer_api_federation_outbox_total',
	'Federation outbox rows, by result',
);