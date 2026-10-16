// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {MessageValidationService} from '@app/api/channel/services/message/MessageValidationService';
import {createDefaultLimitConfig} from '@app/api/constants/LimitConfig';
import {EMPTY_USER_ROW} from '@app/api/database/types/UserTypes';
import {User} from '@app/api/models/User';
import {MAX_MESSAGE_LENGTH_NON_PREMIUM, MAX_MESSAGE_LENGTH_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {CannotSendEmptyMessageError} from '@fluxer/errors/src/domains/channel/CannotSendEmptyMessageError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {describe, expect, it} from 'vitest';

function createValidationService(): MessageValidationService {
	return new MessageValidationService(
		{} as never,
		{
			getConfigSnapshot: () => createDefaultLimitConfig({selfHosted: false}),
		} as never,
	);
}

function createUser(params?: {isBot?: boolean}): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: createUserID(params?.isBot ? 2n : 1n),
		username: params?.isBot ? 'bot-user' : 'regular-user',
		discriminator: 1234,
		bot: params?.isBot ?? false,
		version: 1,
	});
}

function expectContentLengthError(run: () => void, maxLength: number): void {
	try {
		run();
		throw new Error('Expected content length validation to fail');
	} catch (error) {
		expect(error).toBeInstanceOf(InputValidationError);
		expect((error as InputValidationError).getLocalizedErrors()).toEqual([
			{
				path: 'content',
				code: ValidationErrorCodes.CONTENT_EXCEEDS_MAX_LENGTH,
				variables: {maxLength},
			},
		]);
	}
}

describe('MessageValidationService.validateMessageContent', () => {
	it('rejects content with no visible characters', () => {
		const service = createValidationService();
		for (const content of [' ', '\u200e \u200b', '\u2800\u3164\u{e0100}']) {
			expect(() => service.validateMessageContent({content} as never, null)).toThrow(CannotSendEmptyMessageError);
		}
	});

	it('accepts visible message content', () => {
		const service = createValidationService();
		expect(() => service.validateMessageContent({content: '\u200e hello'} as never, null)).not.toThrow();
	});

	it('limits regular users to the non-premium message length', () => {
		const service = createValidationService();
		const user = createUser();
		expect(() =>
			service.validateMessageContent({content: 'x'.repeat(MAX_MESSAGE_LENGTH_NON_PREMIUM)} as never, user),
		).not.toThrow();
		expectContentLengthError(
			() => service.validateMessageContent({content: 'x'.repeat(MAX_MESSAGE_LENGTH_NON_PREMIUM + 1)} as never, user),
			MAX_MESSAGE_LENGTH_NON_PREMIUM,
		);
	});

	it('allows bot users to send premium-length message content', () => {
		const service = createValidationService();
		const bot = createUser({isBot: true});
		expect(() =>
			service.validateMessageContent({content: 'x'.repeat(MAX_MESSAGE_LENGTH_PREMIUM)} as never, bot),
		).not.toThrow();
		expectContentLengthError(
			() => service.validateMessageContent({content: 'x'.repeat(MAX_MESSAGE_LENGTH_PREMIUM + 1)} as never, bot),
			MAX_MESSAGE_LENGTH_PREMIUM,
		);
	});

	it('allows webhooks to send premium-length message content', () => {
		const service = createValidationService();
		expect(() =>
			service.validateMessageContent({content: 'x'.repeat(MAX_MESSAGE_LENGTH_PREMIUM)} as never, null, {
				messageAuthorType: 'webhook',
			}),
		).not.toThrow();
		expectContentLengthError(
			() =>
				service.validateMessageContent({content: 'x'.repeat(MAX_MESSAGE_LENGTH_PREMIUM + 1)} as never, null, {
					messageAuthorType: 'webhook',
				}),
			MAX_MESSAGE_LENGTH_PREMIUM,
		);
	});
});
