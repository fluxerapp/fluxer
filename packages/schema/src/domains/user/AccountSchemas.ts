// SPDX-License-Identifier: AGPL-3.0-or-later

import {ClaimAccountNextSteps} from '@fluxer/constants/src/AccountConfigConstants';
import {UserPrivateResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {createStringType, SnowflakeStringType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import {EmailType, PasswordType} from '@fluxer/schema/src/primitives/UserValidators';
import {z} from 'zod';

export const GuildEntryResult = z
	.object({
		guild_id: SnowflakeStringType.describe('ID of the community the account entered'),
		mode: z.enum(['visit', 'member']).describe('Whether the account visits the community or became a member'),
	})
	.describe('Community the account entered after sign-up');

export type GuildEntryResult = z.infer<typeof GuildEntryResult>;

export const DateOfBirthUpdateRequest = z.object({
	date_of_birth: createStringType(10, 10)
		.refine((value) => /^\d{4}-\d{2}-\d{2}$/.test(value), 'Invalid date format')
		.describe('Date of birth in YYYY-MM-DD format'),
});

export type DateOfBirthUpdateRequest = z.infer<typeof DateOfBirthUpdateRequest>;

export const DateOfBirthUpdateResponse = z.object({
	user: UserPrivateResponse,
	entry: GuildEntryResult.nullable().describe('Community to open next, from an invite used during sign-up'),
});

export type DateOfBirthUpdateResponse = z.infer<typeof DateOfBirthUpdateResponse>;

const CodeDeliveryFields = {
	code_expires_at: z.iso.datetime().describe('When the emailed code stops working'),
	resend_available_at: z.iso.datetime().describe('When another code can be requested'),
};

export const ClaimAccountRequest = z.object({
	email: EmailType.optional().describe('Email address to add to the account. Required except on username instances'),
	password: PasswordType.optional().describe(
		'Password to set at the same time. Required on username instances and instances that cannot send email',
	),
});

export type ClaimAccountRequest = z.infer<typeof ClaimAccountRequest>;

export const ClaimAccountResponse = z.discriminatedUnion('next_step', [
	z.object({
		next_step: z.literal(ClaimAccountNextSteps.NONE).describe('The account is set up, nothing else is needed'),
		user: UserPrivateResponse,
	}),
	z.object({
		next_step: z
			.literal(ClaimAccountNextSteps.CONFIRM_EMAIL)
			.describe('Enter the code sent to the new email address to finish'),
		user: UserPrivateResponse,
		...CodeDeliveryFields,
	}),
]);

export type ClaimAccountResponse = z.infer<typeof ClaimAccountResponse>;

export const EmailVerificationCodeResponse = z.object(CodeDeliveryFields);

export type EmailVerificationCodeResponse = z.infer<typeof EmailVerificationCodeResponse>;

export const EmailVerificationConfirmRequest = z.object({
	code: createStringType(6, 6)
		.refine((value) => /^\d{6}$/.test(value), 'Invalid code')
		.describe('The 6-digit code from the email'),
});

export type EmailVerificationConfirmRequest = z.infer<typeof EmailVerificationConfirmRequest>;

export const SudoEmailCodeResponse = z.object({
	ticket: z.string().describe('Ticket to send back with the code in mfa_method email_code'),
	...CodeDeliveryFields,
});

export type SudoEmailCodeResponse = z.infer<typeof SudoEmailCodeResponse>;
