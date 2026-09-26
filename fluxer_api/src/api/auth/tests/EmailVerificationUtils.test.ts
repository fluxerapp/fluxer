// SPDX-License-Identifier: AGPL-3.0-or-later

import {type EmailVerificationRequiredReason, requireEmailVerified} from '@app/api/auth/EmailVerificationUtils';
import {type APIErrorCode, APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {describe, expect, it} from 'vitest';

const ReasonCodeCases: Array<[EmailVerificationRequiredReason, APIErrorCode]> = [
	['direct_message', APIErrorCodes.DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED],
	['friend_request', APIErrorCodes.FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED],
	['guild', APIErrorCodes.GUILD_EMAIL_VERIFICATION_REQUIRED],
	['guild_creation', APIErrorCodes.GUILD_CREATION_EMAIL_VERIFICATION_REQUIRED],
	['mfa', APIErrorCodes.MFA_EMAIL_VERIFICATION_REQUIRED],
	['profile', APIErrorCodes.PROFILE_EMAIL_VERIFICATION_REQUIRED],
	['reaction', APIErrorCodes.REACTION_EMAIL_VERIFICATION_REQUIRED],
	['report', APIErrorCodes.REPORT_EMAIL_VERIFICATION_REQUIRED],
];

describe('requireEmailVerified', () => {
	it.each(ReasonCodeCases)('throws the %s email verification code', (reason, expectedCode) => {
		try {
			requireEmailVerified({emailVerified: false}, reason);
			throw new Error('Expected requireEmailVerified to throw');
		} catch (error) {
			expect(error).toHaveProperty('code', expectedCode);
			expect(error).toHaveProperty('status', 403);
		}
	});
	it('keeps the generic code when no reason is provided', () => {
		try {
			requireEmailVerified({emailVerified: false});
			throw new Error('Expected requireEmailVerified to throw');
		} catch (error) {
			expect(error).toHaveProperty('code', APIErrorCodes.EMAIL_VERIFICATION_REQUIRED);
		}
	});
	it('allows verified users and bots', () => {
		expect(() => requireEmailVerified({emailVerified: true}, 'mfa')).not.toThrow();
		expect(() => requireEmailVerified({emailVerified: false, isBot: true}, 'mfa')).not.toThrow();
	});
});
