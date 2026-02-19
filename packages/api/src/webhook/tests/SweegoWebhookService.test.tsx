/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import crypto from 'node:crypto';
import {initializeLogger} from '@fluxer/api/src/Logger';
import {NoopLogger} from '@fluxer/api/src/test/mocks/NoopLogger';
import {
	type ISweegoGatewayService,
	type ISweegoUserRepository,
	type SweegoEvent,
	SweegoWebhookService,
} from '@fluxer/api/src/webhook/SweegoWebhookService';
import {SuspiciousActivityFlags} from '@fluxer/constants/src/UserConstants';
import {beforeAll, describe, expect, it, vi} from 'vitest';

beforeAll(() => {
	initializeLogger(new NoopLogger());
});

function createMockUserRepository(): ISweegoUserRepository {
	return {
		findByEmail: vi.fn().mockResolvedValue(null),
		patchUpsert: vi.fn().mockResolvedValue(null),
	};
}

function createMockGatewayService(): ISweegoGatewayService {
	return {
		dispatchPresence: vi.fn().mockResolvedValue(undefined),
	};
}

function signPayload(secret: string, webhookId: string, timestamp: string, body: string): string {
	const secretBytes = Buffer.from(secret, 'base64');
	const contentToSign = `${webhookId}.${timestamp}.${body}`;
	return crypto.createHmac('sha256', secretBytes).update(contentToSign).digest('base64');
}

describe('SweegoWebhookService', () => {
	const testSecret = crypto.randomBytes(32).toString('base64');

	describe('verifySignature', () => {
		it('returns true for valid HMAC-SHA256 signature', () => {
			const userRepo = createMockUserRepository();
			const gateway = createMockGatewayService();
			const service = new SweegoWebhookService(userRepo, gateway);

			const body = JSON.stringify({event_type: 'hard_bounce', recipient: 'test@example.com'});
			const webhookId = '237e3736c687425d9ea8665216bcfe8a';
			const timestamp = '1769696506';
			const signature = signPayload(testSecret, webhookId, timestamp, body);

			const result = service.verifySignature(body, webhookId, timestamp, signature, testSecret);
			expect(result).toBe(true);
		});

		it('returns false for invalid signature', () => {
			const userRepo = createMockUserRepository();
			const gateway = createMockGatewayService();
			const service = new SweegoWebhookService(userRepo, gateway);

			const body = JSON.stringify({event_type: 'hard_bounce', recipient: 'test@example.com'});
			const webhookId = '237e3736c687425d9ea8665216bcfe8a';
			const timestamp = '1769696506';

			const result = service.verifySignature(body, webhookId, timestamp, 'invalid-signature', testSecret);
			expect(result).toBe(false);
		});

		it('returns false for tampered body', () => {
			const userRepo = createMockUserRepository();
			const gateway = createMockGatewayService();
			const service = new SweegoWebhookService(userRepo, gateway);

			const originalBody = JSON.stringify({event_type: 'hard_bounce', recipient: 'test@example.com'});
			const webhookId = '237e3736c687425d9ea8665216bcfe8a';
			const timestamp = '1769696506';
			const signature = signPayload(testSecret, webhookId, timestamp, originalBody);

			const tamperedBody = JSON.stringify({event_type: 'hard_bounce', recipient: 'other@example.com'});
			const result = service.verifySignature(tamperedBody, webhookId, timestamp, signature, testSecret);
			expect(result).toBe(false);
		});
	});

	describe('processEvent', () => {
		it('ignores non-bounce events', async () => {
			const userRepo = createMockUserRepository();
			const gateway = createMockGatewayService();
			const service = new SweegoWebhookService(userRepo, gateway);

			const event: SweegoEvent = {
				event_type: 'delivered',
				timestamp: '2026-01-29T14:21:46.729251+00:00',
				recipient: 'soft@example.com',
			};
			await service.processEvent(event);

			expect(userRepo.findByEmail).not.toHaveBeenCalled();
			expect(userRepo.patchUpsert).not.toHaveBeenCalled();
		});

		it('logs soft bounces without marking as bounced', async () => {
			const userRepo = createMockUserRepository();
			const gateway = createMockGatewayService();
			const service = new SweegoWebhookService(userRepo, gateway);

			const event: SweegoEvent = {
				event_type: 'soft-bounce',
				timestamp: '2026-01-29T14:21:46.729251+00:00',
				recipient: 'soft@example.com',
				details: 'Temporary failure',
			};
			await service.processEvent(event);

			expect(userRepo.findByEmail).not.toHaveBeenCalled();
			expect(userRepo.patchUpsert).not.toHaveBeenCalled();
		});

		it('marks hard bounces as unverified', async () => {
			const userRepo = createMockUserRepository();
			const gateway = createMockGatewayService();
			const service = new SweegoWebhookService(userRepo, gateway);

			const mockUser = {
				id: BigInt(123),
				email: 'bounced@example.com',
				emailBounced: false,
				emailVerified: true,
				suspiciousActivityFlags: 0,
				toRow: () => ({}),
			};
			(userRepo.findByEmail as ReturnType<typeof vi.fn>).mockResolvedValue(mockUser);
			(userRepo.patchUpsert as ReturnType<typeof vi.fn>).mockResolvedValue(null);

			const event: SweegoEvent = {
				event_type: 'hard_bounce',
				timestamp: '2026-01-29T14:21:46.729251+00:00',
				recipient: 'bounced@example.com',
				event_id: 'event-1',
			};

			await service.processEvent(event);

			expect(userRepo.findByEmail).toHaveBeenCalledWith('bounced@example.com');
			expect(userRepo.patchUpsert).toHaveBeenCalledWith(
				mockUser.id,
				{
					email_bounced: true,
					email_verified: false,
					suspicious_activity_flags: SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL,
				},
				mockUser.toRow(),
			);
		});

		it('marks complaints as unverified', async () => {
			const userRepo = createMockUserRepository();
			const gateway = createMockGatewayService();
			const service = new SweegoWebhookService(userRepo, gateway);

			const mockUser = {
				id: BigInt(124),
				email: 'complaint@example.com',
				emailBounced: false,
				emailVerified: true,
				suspiciousActivityFlags: 0,
				toRow: () => ({}),
			};
			(userRepo.findByEmail as ReturnType<typeof vi.fn>).mockResolvedValue(mockUser);
			(userRepo.patchUpsert as ReturnType<typeof vi.fn>).mockResolvedValue(null);

			const event: SweegoEvent = {
				event_type: 'complaint',
				timestamp: '2026-01-29T14:21:46.729251+00:00',
				recipient: 'complaint@example.com',
				event_id: 'event-2',
			};

			await service.processEvent(event);

			expect(userRepo.findByEmail).toHaveBeenCalledWith('complaint@example.com');
			expect(userRepo.patchUpsert).toHaveBeenCalledWith(
				mockUser.id,
				{
					email_bounced: true,
					email_verified: false,
					suspicious_activity_flags: SuspiciousActivityFlags.REQUIRE_REVERIFIED_EMAIL,
				},
				mockUser.toRow(),
			);
		});

		it('skips already bounced users', async () => {
			const userRepo = createMockUserRepository();
			const gateway = createMockGatewayService();
			const service = new SweegoWebhookService(userRepo, gateway);

			const mockUser = {
				id: BigInt(456),
				email: 'already@example.com',
				emailBounced: true,
				suspiciousActivityFlags: 0,
				toRow: () => ({}),
			};
			(userRepo.findByEmail as ReturnType<typeof vi.fn>).mockResolvedValue(mockUser);

			const event: SweegoEvent = {
				event_type: 'hard_bounce',
				timestamp: '2026-01-29T14:21:46.729251+00:00',
				recipient: 'already@example.com',
			};

			await service.processEvent(event);

			expect(userRepo.patchUpsert).not.toHaveBeenCalled();
		});
	});
});
