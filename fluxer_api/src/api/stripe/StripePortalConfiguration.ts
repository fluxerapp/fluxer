// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type Stripe from 'stripe';

const PORTAL_METADATA_KEY = 'fluxer_portal';
const PORTAL_METADATA_VALUE = 'without_invoice_history';

const configurationIds = new WeakMap<Stripe, Promise<string>>();

async function findOrCreatePortalConfiguration(stripe: Stripe): Promise<string> {
	for await (const configuration of stripe.billingPortal.configurations.list({active: true, limit: 100})) {
		if (
			configuration.metadata?.[PORTAL_METADATA_KEY] === PORTAL_METADATA_VALUE &&
			!configuration.features.invoice_history.enabled
		) {
			return configuration.id;
		}
	}
	const created = await stripe.billingPortal.configurations.create({
		features: {
			customer_update: {enabled: true, allowed_updates: ['address']},
			invoice_history: {enabled: false},
			payment_method_update: {enabled: true},
			subscription_cancel: {enabled: true, mode: 'at_period_end', proration_behavior: 'none'},
		},
		metadata: {[PORTAL_METADATA_KEY]: PORTAL_METADATA_VALUE},
	});
	return created.id;
}

export function getPortalConfigurationId(stripe: Stripe): Promise<string | undefined> {
	if (Config.instance.selfHosted) return Promise.resolve(undefined);
	const cached = configurationIds.get(stripe);
	if (cached) return cached;
	const pending = findOrCreatePortalConfiguration(stripe);
	configurationIds.set(stripe, pending);
	pending.catch(() => configurationIds.delete(stripe));
	return pending;
}
