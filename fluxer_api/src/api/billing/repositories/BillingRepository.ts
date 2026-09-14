// SPDX-License-Identifier: AGPL-3.0-or-later

import {BillingActionIntentRepository} from '@app/api/billing/repositories/BillingActionIntentRepository';
import {BillingChargeRepository} from '@app/api/billing/repositories/BillingChargeRepository';
import {BillingCheckoutSessionRepository} from '@app/api/billing/repositories/BillingCheckoutSessionRepository';
import {BillingCustomerRepository} from '@app/api/billing/repositories/BillingCustomerRepository';
import {BillingDisputeRepository} from '@app/api/billing/repositories/BillingDisputeRepository';
import {BillingInvoiceRepository} from '@app/api/billing/repositories/BillingInvoiceRepository';
import {BillingPaymentIntentRepository} from '@app/api/billing/repositories/BillingPaymentIntentRepository';
import {BillingPaymentMethodRepository} from '@app/api/billing/repositories/BillingPaymentMethodRepository';
import {BillingPaymentRepository} from '@app/api/billing/repositories/BillingPaymentRepository';
import {BillingPriceRepository} from '@app/api/billing/repositories/BillingPriceRepository';
import {BillingProductRepository} from '@app/api/billing/repositories/BillingProductRepository';
import {BillingRefundRepository} from '@app/api/billing/repositories/BillingRefundRepository';
import {BillingSubscriptionRepository} from '@app/api/billing/repositories/BillingSubscriptionRepository';
import {BillingWebhookEventRepository} from '@app/api/billing/repositories/BillingWebhookEventRepository';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';

export class BillingRepository {
	readonly customers: BillingCustomerRepository;
	readonly products: BillingProductRepository;
	readonly prices: BillingPriceRepository;
	readonly paymentMethods: BillingPaymentMethodRepository;
	readonly subscriptions: BillingSubscriptionRepository;
	readonly invoices: BillingInvoiceRepository;
	readonly paymentIntents: BillingPaymentIntentRepository;
	readonly charges: BillingChargeRepository;
	readonly payments: BillingPaymentRepository;
	readonly refunds: BillingRefundRepository;
	readonly checkoutSessions: BillingCheckoutSessionRepository;
	readonly disputes: BillingDisputeRepository;
	readonly webhookEvents: BillingWebhookEventRepository;
	readonly actionIntents: BillingActionIntentRepository;

	constructor(snowflakeService: ISnowflakeService, kv: IKVProvider) {
		this.payments = new BillingPaymentRepository();
		this.invoices = new BillingInvoiceRepository(this.payments);
		this.customers = new BillingCustomerRepository();
		this.products = new BillingProductRepository();
		this.prices = new BillingPriceRepository();
		this.paymentMethods = new BillingPaymentMethodRepository();
		this.subscriptions = new BillingSubscriptionRepository();
		this.paymentIntents = new BillingPaymentIntentRepository();
		this.charges = new BillingChargeRepository();
		this.refunds = new BillingRefundRepository();
		this.checkoutSessions = new BillingCheckoutSessionRepository();
		this.disputes = new BillingDisputeRepository();
		this.webhookEvents = new BillingWebhookEventRepository(kv);
		this.actionIntents = new BillingActionIntentRepository(snowflakeService);
	}
}
