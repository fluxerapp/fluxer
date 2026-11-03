// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	CleanupModes,
	DateOfBirthPolicies,
	EmailDeliveryModes,
	EmailDeliveryStates,
	GuildJoinGates,
	IdentifierFirstModes,
	InstantAccountAvailability,
	InteractionTierModes,
	LastSessionCleanupModes,
	VerificationBases,
} from '@fluxer/constants/src/AccountConfigConstants';
import {AccountTiers, AccountVerificationNeeds} from '@fluxer/constants/src/AccountTierConstants';
import {CaptchaProviders} from '@fluxer/constants/src/CaptchaConstants';
import {createNamedStringLiteralUnion, withOpenApiType} from '@fluxer/schema/src/primitives/SchemaPrimitives';

export const AccountTierSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[AccountTiers.UNCLAIMED, 'UNCLAIMED', 'The account has no way to sign in from another device yet'],
			[AccountTiers.UNVERIFIED, 'UNVERIFIED', 'The account has an email address that is not confirmed yet'],
			[AccountTiers.VERIFIED, 'VERIFIED', 'The account has finished setting up and can use everything'],
		],
		'How far the account has got with setting up a way to sign in',
	),
	'AccountTier',
);

export const AccountVerificationNeedSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[AccountVerificationNeeds.ADD_EMAIL, 'ADD_EMAIL', 'Add an email address to the account'],
			[AccountVerificationNeeds.CONFIRM_EMAIL, 'CONFIRM_EMAIL', 'Confirm the email address on the account'],
			[AccountVerificationNeeds.SET_PASSWORD, 'SET_PASSWORD', 'Set a password, or add a passkey, on the account'],
		],
		'What the account still has to do before it can use everything',
	),
	'AccountVerificationNeed',
);

export const CaptchaProviderSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[CaptchaProviders.NONE, 'NONE', 'No captcha is asked for'],
			[CaptchaProviders.ALTCHA, 'ALTCHA', 'An ALTCHA proof of work is asked for'],
		],
		'Captcha the instance asks for',
	),
	'CaptchaProvider',
);

export const InstantAccountAvailabilitySchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[InstantAccountAvailability.AVAILABLE, 'AVAILABLE', 'Accounts can be created from a display name'],
			[InstantAccountAvailability.DISABLED, 'DISABLED', 'Instant accounts are turned off'],
			[InstantAccountAvailability.CAPTCHA_OFF, 'CAPTCHA_OFF', 'Instant accounts need the captcha, which is off'],
			[InstantAccountAvailability.REGISTRATION_CLOSED, 'REGISTRATION_CLOSED', 'Public registration is closed'],
			[
				InstantAccountAvailability.APPROVAL_REQUIRED,
				'APPROVAL_REQUIRED',
				'New accounts need approval, so they need credentials to sign in once approved',
			],
		],
		'Whether accounts can be created from just a display name, and why not',
	),
	'InstantAccountAvailability',
);

export const EmailDeliveryStateSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[EmailDeliveryStates.ON, 'ON', 'The instance sends email'],
			[
				EmailDeliveryStates.OFF_USERNAME_MODE,
				'OFF_USERNAME_MODE',
				'People sign in with a username, so no email is sent',
			],
			[EmailDeliveryStates.OFF_DISABLED, 'OFF_DISABLED', 'Email delivery is turned off'],
			[EmailDeliveryStates.OFF_INCOMPLETE, 'OFF_INCOMPLETE', 'Email delivery is on but its settings are incomplete'],
		],
		'Whether the instance sends email, and why not',
	),
	'EmailDeliveryState',
);

export const InteractionTierModeSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[InteractionTierModes.LEGACY, 'LEGACY', 'Accounts that have not finished setting up keep the older limits'],
			[
				InteractionTierModes.ENFORCED,
				'ENFORCED',
				'Accounts that have not finished setting up may only do things that do not reach other people',
			],
		],
		'Which limits apply to accounts that have not finished setting up',
	),
	'InteractionTierMode',
);

export const GuildJoinGateSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[GuildJoinGates.OFF, 'OFF', 'Joining a community never needs a finished account'],
			[
				GuildJoinGates.VISITS_CAPABLE,
				'VISITS_CAPABLE',
				'Joining needs a finished account only in clients that can preview communities',
			],
			[GuildJoinGates.ALWAYS, 'ALWAYS', 'Joining needs a finished account in every client'],
		],
		'When joining a community needs a finished account. Applies only while the interaction tier is enforced and the person can preview communities',
	),
	'GuildJoinGate',
);

export const DateOfBirthPolicySchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[DateOfBirthPolicies.OFF, 'OFF', 'The date of birth is never asked for'],
			[DateOfBirthPolicies.ASK, 'ASK', 'The date of birth is asked for, but nothing waits on it'],
			[
				DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS,
				'REQUIRE_NEW_ACCOUNTS',
				'Accounts created without a date of birth must enter one before using the app',
			],
			[DateOfBirthPolicies.REQUIRE_ALL, 'REQUIRE_ALL', 'Every account without a date of birth must enter one'],
		],
		'Whether the instance asks for a date of birth, and who must enter one',
	),
	'DateOfBirthPolicy',
);

export const IdentifierFirstModeSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[IdentifierFirstModes.AUTO, 'AUTO', 'On while people sign in with an email and the instance sends email'],
			[IdentifierFirstModes.ON, 'ON', 'On while people sign in with an email'],
			[IdentifierFirstModes.OFF, 'OFF', 'Off'],
		],
		'Whether clients show the single sign-in entry that starts with an email',
	),
	'IdentifierFirstMode',
);

export const VerificationBasisSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[
				VerificationBases.AUTO,
				'AUTO',
				'A confirmed email while the instance sends email, otherwise a password or passkey',
			],
			[VerificationBases.CONFIRMED_EMAIL, 'CONFIRMED_EMAIL', 'Accounts finish setting up by confirming an email'],
			[
				VerificationBases.LOCAL_CREDENTIAL,
				'LOCAL_CREDENTIAL',
				'Accounts finish setting up by adding a password or passkey',
			],
		],
		'What an account needs to finish setting up',
	),
	'VerificationBasis',
);

export const EffectiveVerificationBasisSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[VerificationBases.CONFIRMED_EMAIL, 'CONFIRMED_EMAIL', 'Accounts finish setting up by confirming an email'],
			[
				VerificationBases.LOCAL_CREDENTIAL,
				'LOCAL_CREDENTIAL',
				'Accounts finish setting up by adding a password or passkey',
			],
		],
		'What an account needs to finish setting up on this instance right now',
	),
	'EffectiveVerificationBasis',
);

export const EmailDeliveryModeSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[EmailDeliveryModes.OFF, 'OFF', 'The instance does not send email'],
			[EmailDeliveryModes.SMTP, 'SMTP', 'The instance sends email through SMTP'],
		],
		'How the instance sends email',
	),
	'EmailDeliveryMode',
);

export const CleanupModeSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[CleanupModes.OFF, 'OFF', 'Accounts are never deleted by this policy'],
			[
				CleanupModes.AFTER_INACTIVITY,
				'AFTER_INACTIVITY',
				'Accounts are deleted after the set number of days without use',
			],
		],
		'Whether a cleanup policy deletes accounts',
	),
	'CleanupMode',
);

export const LastSessionCleanupModeSchema = withOpenApiType(
	createNamedStringLiteralUnion(
		[
			[LastSessionCleanupModes.OFF, 'OFF', 'Signing out of the last session deletes nothing'],
			[
				LastSessionCleanupModes.AFTER_GRACE,
				'AFTER_GRACE',
				'Signing out of the last session schedules the account for deletion after a grace period',
			],
		],
		'What happens when an account with no way back in signs out of its last session',
	),
	'LastSessionCleanupMode',
);
