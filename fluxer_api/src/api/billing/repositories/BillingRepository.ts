// SPDX-License-Identifier: AGPL-3.0-or-later

import {BillingChargeRepository} from '@app/api/billing/repositories/BillingChargeRepository';
import {BillingCustomerRepository} from '@app/api/billing/repositories/BillingCustomerRepository';
import {BillingInvoiceRepository} from '@app/api/billing/repositories/BillingInvoiceRepository';
import {BillingPaymentIntentRepository} from '@app/api/billing/repositories/BillingPaymentIntentRepository';
import {BillingPaymentMethodRepository} from '@app/api/billing/repositories/BillingPaymentMethodRepository';
import {BillingPaymentRepository} from '@app/api/billing/repositories/BillingPaymentRepository';
import {BillingPriceRepository} from '@app/api/billing/repositories/BillingPriceRepository';
import {BillingRefundRepository} from '@app/api/billing/repositories/BillingRefundRepository';
import {BillingSubscriptionRepository} from '@app/api/billing/repositories/BillingSubscriptionRepository';
import {KvOnceClaims} from '@app/api/infrastructure/KvOnceClaims';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export class BillingRepository {
	readonly customers: BillingCustomerRepository;
	readonly prices: BillingPriceRepository;
	readonly paymentMethods: BillingPaymentMethodRepository;
	readonly subscriptions: BillingSubscriptionRepository;
	readonly invoices: BillingInvoiceRepository;
	readonly paymentIntents: BillingPaymentIntentRepository;
	readonly charges: BillingChargeRepository;
	readonly payments: BillingPaymentRepository;
	readonly refunds: BillingRefundRepository;
	readonly webhookEvents: KvOnceClaims;

	constructor(kv: IKVProvider) {
		this.payments = new BillingPaymentRepository();
		this.invoices = new BillingInvoiceRepository(this.payments);
		this.customers = new BillingCustomerRepository();
		this.prices = new BillingPriceRepository();
		this.paymentMethods = new BillingPaymentMethodRepository();
		this.subscriptions = new BillingSubscriptionRepository();
		this.paymentIntents = new BillingPaymentIntentRepository();
		this.charges = new BillingChargeRepository();
		this.refunds = new BillingRefundRepository();
		this.webhookEvents = new KvOnceClaims(kv, {keyPrefix: 'stripe-webhook:', inFlightTtlSeconds: 600});
	}
}
