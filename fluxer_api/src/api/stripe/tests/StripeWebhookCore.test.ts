// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {Config} from '@app/api/Config';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createMockWebhookPayload, type StripeWebhookEventData} from '@app/api/test/msw/handlers/StripeApiHandlers';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Stripe Webhook - Core Handling', () => {
	let harness: ApiTestHarness;
	let originalWebhookSecret: string | undefined;
	beforeAll(async () => {
		harness = await createApiTestHarness();
		originalWebhookSecret = Config.stripe.webhookSecret;
		Config.stripe.webhookSecret = 'whsec_test_secret';
	});
	afterAll(async () => {
		await harness.shutdown();
		Config.stripe.webhookSecret = originalWebhookSecret;
	});
	beforeEach(async () => {
		await harness.resetData();
	});
	function createWebhookSignature(payload: string, timestamp: number, secret: string): string {
		const signedPayload = `${timestamp}.${payload}`;
		const signature = crypto.createHmac('sha256', secret).update(signedPayload).digest('hex');
		return `t=${timestamp},v1=${signature}`;
	}
	async function sendWebhook(
		eventData: StripeWebhookEventData,
		options?: {
			signature?: string;
			expectStatus?: number;
		},
	): Promise<{
		response: Response;
		text: string;
	}> {
		const {payload, timestamp} = createMockWebhookPayload(eventData);
		const signature = options?.signature ?? createWebhookSignature(payload, timestamp, Config.stripe.webhookSecret!);
		const result = await createBuilder(harness, '')
			.post('/stripe/webhook')
			.header('stripe-signature', signature)
			.header('content-type', 'application/json')
			.body(payload)
			.executeRaw();
		return result;
	}
	describe('handleWebhook', () => {
		test('rejects webhook with invalid signature', async () => {
			const eventData: StripeWebhookEventData = {
				type: 'checkout.session.completed',
				data: {object: {id: 'cs_test_123'}},
			};
			const {response} = await sendWebhook(eventData, {signature: 'invalid_signature'});
			expect(response.status).toBe(401);
		});
		test('accepts webhook with valid signature', async () => {
			const eventData: StripeWebhookEventData = {
				type: 'customer.created',
				data: {object: {id: 'cus_test_123'}},
			};
			const {response} = await sendWebhook(eventData);
			expect(response.status).toBe(200);
		});
		test('handles unhandled webhook event types gracefully', async () => {
			const eventData: StripeWebhookEventData = {
				type: 'customer.created',
				data: {object: {id: 'cus_test_unhandled'}},
			};
			const {response} = await sendWebhook(eventData);
			expect(response.status).toBe(200);
		});
		test('handles unknown webhook event types gracefully', async () => {
			const eventData: StripeWebhookEventData = {
				type: 'some.unknown.event',
				data: {object: {id: 'unknown_123'}},
			};
			const {response} = await sendWebhook(eventData);
			expect(response.status).toBe(200);
		});
		test('rejects webhook with tampered payload', async () => {
			const eventData: StripeWebhookEventData = {
				type: 'checkout.session.completed',
				data: {object: {id: 'cs_test_123'}},
			};
			const {payload, timestamp} = createMockWebhookPayload(eventData);
			const signature = createWebhookSignature(payload, timestamp, Config.stripe.webhookSecret!);
			const tamperedPayload = payload.replace('cs_test_123', 'cs_test_tampered');
			const result = await createBuilder(harness, '')
				.post('/stripe/webhook')
				.header('stripe-signature', signature)
				.header('content-type', 'application/json')
				.body(tamperedPayload)
				.executeRaw();
			expect(result.response.status).toBe(401);
		});
		test('rejects webhook with expired timestamp', async () => {
			const eventData: StripeWebhookEventData = {
				type: 'checkout.session.completed',
				data: {object: {id: 'cs_test_123'}},
			};
			const {payload} = createMockWebhookPayload(eventData);
			const oldTimestamp = Math.floor(Date.now() / 1000) - 600;
			const signature = createWebhookSignature(payload, oldTimestamp, Config.stripe.webhookSecret!);
			const result = await createBuilder(harness, '')
				.post('/stripe/webhook')
				.header('stripe-signature', signature)
				.header('content-type', 'application/json')
				.body(payload)
				.executeRaw();
			expect(result.response.status).toBe(401);
		});
	});
});
