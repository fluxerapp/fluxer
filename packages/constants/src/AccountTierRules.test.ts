// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	collectsDateOfBirth,
	DateOfBirthPolicies,
	dateOfBirthPolicyFromLegacy,
	InstantAccountAvailability,
	InteractionTierModes,
	LegacyDateOfBirthEnforcements,
	RegistrationModes,
	VerificationBases,
} from '@fluxer/constants/src/AccountConfigConstants';
import {
	AccountIdentityModes,
	normalizeAccountIdentity,
	TagStyles,
} from '@fluxer/constants/src/AccountIdentityConstants';
import {
	AccountTiers,
	AccountVerificationNeeds,
	LocalCredentialKinds,
	localCredentialKindOf,
	type TierDenialError,
	TierDenialErrors,
	TierExemptions,
	type TierGatedAction,
	TierGatedActions,
} from '@fluxer/constants/src/AccountTierConstants';
import {
	type AccountTierInput,
	evaluateTierGate,
	requiresLocalCredential,
	resolveAccountTier,
	resolveEffectiveVerificationBasis,
	resolveEmailCodeAvailability,
	resolveEmailDeliveryState,
	resolveIdentifierFirst,
	resolveInstantAccountAvailability,
	resolveVerificationNeed,
	type TierGateInput,
} from '@fluxer/constants/src/AccountTierRules';
import {CaptchaProviders} from '@fluxer/constants/src/CaptchaConstants';
import {UserTraits} from '@fluxer/constants/src/UserTraitConstants';
import {describe, expect, it} from 'vitest';

const ALL_ACTIONS = Object.values(TierGatedActions);
const ALL_IDENTITY_MODES = Object.values(AccountIdentityModes);

const E = TierDenialErrors;

type Row = [
	action: TierGatedAction,
	unclaimed: TierDenialError | null,
	needsPassword: TierDenialError | null,
	unverified: TierDenialError | null,
];

const LEGACY_EXPECTED: Array<Row> = [
	[TierGatedActions.MESSAGE, E.UNCLAIMED_ACCOUNT_CANNOT_SEND_MESSAGES, E.ACCOUNT_VERIFICATION_REQUIRED, null],
	[TierGatedActions.TYPING, null, null, null],
	[
		TierGatedActions.REACTION,
		E.UNCLAIMED_ACCOUNT_CANNOT_ADD_REACTIONS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.REACTION_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.DIRECT_MESSAGE,
		E.UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		null,
	],
	[
		TierGatedActions.DIRECT_MESSAGE_OPEN,
		E.UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED,
	],
	[TierGatedActions.GROUP_DM, E.UNCLAIMED_ACCOUNT_CANNOT_JOIN_GROUP_DMS, E.ACCOUNT_VERIFICATION_REQUIRED, null],
	[
		TierGatedActions.FRIEND_REQUEST,
		E.UNCLAIMED_ACCOUNT_CANNOT_SEND_FRIEND_REQUESTS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.FRIEND_ACCEPT,
		E.UNCLAIMED_ACCOUNT_CANNOT_ACCEPT_FRIEND_REQUESTS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		null,
	],
	[TierGatedActions.INVITE_CREATE, null, null, null],
	[TierGatedActions.GUILD_JOIN, null, null, null],
	[
		TierGatedActions.GUILD_CREATE,
		E.UNCLAIMED_ACCOUNT_CANNOT_CREATE_GUILDS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.GUILD_CREATION_EMAIL_VERIFICATION_REQUIRED,
	],
	[TierGatedActions.VOICE, E.UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS, E.ACCOUNT_VERIFICATION_REQUIRED, null],
	[
		TierGatedActions.CALL,
		E.UNCLAIMED_ACCOUNT_CANNOT_JOIN_ONE_ON_ONE_VOICE_CALLS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		null,
	],
	[TierGatedActions.GROUP_CALL, null, null, null],
	[TierGatedActions.THREAD_JOIN, null, null, null],
	[TierGatedActions.CHANNEL_FOLLOW, null, null, null],
	[TierGatedActions.OWNERSHIP_TRANSFER, null, null, null],
	[
		TierGatedActions.APPLICATION_CREATE,
		E.UNCLAIMED_ACCOUNT_CANNOT_CREATE_APPLICATIONS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		null,
	],
	[TierGatedActions.GIFT_REDEEM, E.ACCOUNT_VERIFICATION_REQUIRED, E.ACCOUNT_VERIFICATION_REQUIRED, null],
	[
		TierGatedActions.PROFILE_CUSTOMIZE,
		E.UNCLAIMED_ACCOUNTS_CAN_ONLY_SET_EMAIL_VIA_TOKEN,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.PROFILE_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.REPORT,
		E.UNCLAIMED_ACCOUNT_CANNOT_SUBMIT_REPORTS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.REPORT_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.MFA,
		E.MFA_EMAIL_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.MFA_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.GUILD_VERIFICATION,
		E.GUILD_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.GUILD_EMAIL_VERIFICATION_REQUIRED,
	],
];

const ENFORCED_EXPECTED: Array<Row> = [
	[
		TierGatedActions.MESSAGE,
		E.UNCLAIMED_ACCOUNT_CANNOT_SEND_MESSAGES,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.TYPING,
		E.UNCLAIMED_ACCOUNT_CANNOT_SEND_MESSAGES,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.REACTION,
		E.UNCLAIMED_ACCOUNT_CANNOT_ADD_REACTIONS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.REACTION_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.DIRECT_MESSAGE,
		E.UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.DIRECT_MESSAGE_OPEN,
		E.UNCLAIMED_ACCOUNT_CANNOT_SEND_DIRECT_MESSAGES,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.GROUP_DM,
		E.UNCLAIMED_ACCOUNT_CANNOT_JOIN_GROUP_DMS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.FRIEND_REQUEST,
		E.UNCLAIMED_ACCOUNT_CANNOT_SEND_FRIEND_REQUESTS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.FRIEND_ACCEPT,
		E.UNCLAIMED_ACCOUNT_CANNOT_ACCEPT_FRIEND_REQUESTS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.INVITE_CREATE,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.GUILD_JOIN,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.GUILD_CREATE,
		E.UNCLAIMED_ACCOUNT_CANNOT_CREATE_GUILDS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.GUILD_CREATION_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.VOICE,
		E.UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.CALL,
		E.UNCLAIMED_ACCOUNT_CANNOT_JOIN_ONE_ON_ONE_VOICE_CALLS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.GROUP_CALL,
		E.UNCLAIMED_ACCOUNT_CANNOT_JOIN_ONE_ON_ONE_VOICE_CALLS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.THREAD_JOIN,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.CHANNEL_FOLLOW,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.OWNERSHIP_TRANSFER,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.APPLICATION_CREATE,
		E.UNCLAIMED_ACCOUNT_CANNOT_CREATE_APPLICATIONS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
	],
	[TierGatedActions.GIFT_REDEEM, E.ACCOUNT_VERIFICATION_REQUIRED, E.ACCOUNT_VERIFICATION_REQUIRED, null],
	[TierGatedActions.PROFILE_CUSTOMIZE, null, null, null],
	[
		TierGatedActions.REPORT,
		E.UNCLAIMED_ACCOUNT_CANNOT_SUBMIT_REPORTS,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.REPORT_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.MFA,
		E.MFA_EMAIL_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.MFA_EMAIL_VERIFICATION_REQUIRED,
	],
	[
		TierGatedActions.GUILD_VERIFICATION,
		E.GUILD_VERIFICATION_REQUIRED,
		E.ACCOUNT_VERIFICATION_REQUIRED,
		E.GUILD_EMAIL_VERIFICATION_REQUIRED,
	],
];

function gate(overrides: Partial<TierGateInput>) {
	return evaluateTierGate({
		tier: AccountTiers.UNCLAIMED,
		mode: InteractionTierModes.LEGACY,
		action: TierGatedActions.MESSAGE,
		identityMode: AccountIdentityModes.EMAIL,
		hasEmail: false,
		guildJoinGateApplies: true,
		exemptions: [],
		...overrides,
	});
}

function codeOf(result: ReturnType<typeof evaluateTierGate>): TierDenialError | null {
	return result.allowed ? null : result.errorCode;
}

describe('evaluateTierGate matrix', () => {
	const accounts = [
		['unclaimed', 'email', false, 'add_email', 0],
		['unclaimed', 'email', true, 'password', 1],
		['unclaimed', 'username', false, 'password', 1],
		['unclaimed', 'username', true, 'password', 1],
		['unverified', 'email', false, 'confirm_email', 2],
		['unverified', 'email', true, 'confirm_email', 2],
		['unverified', 'username', false, 'confirm_email', 2],
		['unverified', 'username', true, 'confirm_email', 2],
		['verified', 'email', false, null, 3],
		['verified', 'email', true, null, 3],
		['verified', 'username', false, null, 3],
		['verified', 'username', true, null, 3],
	] as const;
	const cases = [
		{mode: InteractionTierModes.LEGACY, rows: LEGACY_EXPECTED},
		{mode: InteractionTierModes.ENFORCED, rows: ENFORCED_EXPECTED},
	].flatMap(({mode, rows}) =>
		rows.flatMap(([action, unclaimed, needsPassword, unverified]) => {
			const denials = [unclaimed, needsPassword, unverified, null];
			return accounts.map(([tier, identityMode, hasEmail, need, denialColumn]) => {
				const errorCode = denials[denialColumn];
				const expected = errorCode === null ? {allowed: true} : {allowed: false, action, tier, need, errorCode};
				return {mode, action, tier, identityMode, hasEmail, expected};
			});
		}),
	);

	it('has an expectation row for every action', () => {
		expect(LEGACY_EXPECTED.map(([action]) => action).sort()).toEqual([...ALL_ACTIONS].sort());
		expect(ENFORCED_EXPECTED.map(([action]) => action).sort()).toEqual([...ALL_ACTIONS].sort());
	});

	it.each(cases)('$mode $tier $action $identityMode email=$hasEmail', ({expected, ...input}) => {
		expect(gate(input)).toEqual(expected);
	});
});

describe('evaluateTierGate rewrites and conditions', () => {
	it('tells unclaimed accounts on username instances to set a password', () => {
		const result = gate({
			identityMode: AccountIdentityModes.USERNAME,
			action: TierGatedActions.REPORT,
		});
		expect(result).toEqual({
			allowed: false,
			action: TierGatedActions.REPORT,
			tier: AccountTiers.UNCLAIMED,
			need: AccountVerificationNeeds.SET_PASSWORD,
			errorCode: E.ACCOUNT_VERIFICATION_REQUIRED,
		});
		expect(
			codeOf(gate({identityMode: AccountIdentityModes.USERNAME, action: TierGatedActions.GUILD_VERIFICATION})),
		).toBe(E.ACCOUNT_VERIFICATION_REQUIRED);
	});

	it('tells unclaimed email accounts that have an email to set a password', () => {
		const result = gate({hasEmail: true, action: TierGatedActions.MESSAGE});
		expect(result).toMatchObject({
			need: AccountVerificationNeeds.SET_PASSWORD,
			errorCode: E.ACCOUNT_VERIFICATION_REQUIRED,
		});
	});

	it('keeps the specific error for unclaimed email accounts with no email', () => {
		expect(gate({action: TierGatedActions.MESSAGE})).toMatchObject({
			need: AccountVerificationNeeds.ADD_EMAIL,
			errorCode: E.UNCLAIMED_ACCOUNT_CANNOT_SEND_MESSAGES,
		});
	});

	it('applies the enforced join gate only when it applies to the person', () => {
		const join = {mode: InteractionTierModes.ENFORCED, action: TierGatedActions.GUILD_JOIN};
		expect(gate({...join, guildJoinGateApplies: false}).allowed).toBe(true);
		expect(codeOf(gate({...join, guildJoinGateApplies: true}))).toBe(E.ACCOUNT_VERIFICATION_REQUIRED);
	});

	it('lets community owners into voice under legacy, never under enforced', () => {
		const owner = {action: TierGatedActions.VOICE, exemptions: [TierExemptions.GUILD_OWNER]};
		expect(gate({...owner, mode: InteractionTierModes.LEGACY}).allowed).toBe(true);
		expect(codeOf(gate({...owner, mode: InteractionTierModes.ENFORCED}))).toBe(
			E.UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS,
		);
		expect(codeOf(gate({action: TierGatedActions.VOICE, mode: InteractionTierModes.LEGACY}))).toBe(
			E.UNCLAIMED_ACCOUNT_CANNOT_JOIN_VOICE_CHANNELS,
		);
	});
});

describe('resolveVerificationNeed', () => {
	it.each([
		[AccountTiers.VERIFIED, AccountIdentityModes.EMAIL, true, null],
		[AccountTiers.VERIFIED, AccountIdentityModes.USERNAME, false, null],
		[AccountTiers.UNVERIFIED, AccountIdentityModes.EMAIL, true, AccountVerificationNeeds.CONFIRM_EMAIL],
		[AccountTiers.UNCLAIMED, AccountIdentityModes.EMAIL, false, AccountVerificationNeeds.ADD_EMAIL],
		[AccountTiers.UNCLAIMED, AccountIdentityModes.EMAIL, true, AccountVerificationNeeds.SET_PASSWORD],
		[AccountTiers.UNCLAIMED, AccountIdentityModes.USERNAME, false, AccountVerificationNeeds.SET_PASSWORD],
		[AccountTiers.UNCLAIMED, AccountIdentityModes.USERNAME, true, AccountVerificationNeeds.SET_PASSWORD],
	] as const)('%s on %s with email=%s needs %s', (tier, identityMode, hasEmail, need) => {
		expect(resolveVerificationNeed({tier, identityMode, hasEmail})).toBe(need);
	});
});

describe('resolveEffectiveVerificationBasis', () => {
	it.each([
		['email', 'auto', 'on', 'confirmed_email'],
		['email', 'auto', 'off_username_mode', 'local_credential'],
		['email', 'auto', 'off_disabled', 'local_credential'],
		['email', 'auto', 'off_incomplete', 'local_credential'],
		['email', 'confirmed_email', 'on', 'confirmed_email'],
		['email', 'confirmed_email', 'off_username_mode', 'local_credential'],
		['email', 'confirmed_email', 'off_disabled', 'local_credential'],
		['email', 'confirmed_email', 'off_incomplete', 'local_credential'],
		['email', 'local_credential', 'on', 'local_credential'],
		['email', 'local_credential', 'off_username_mode', 'local_credential'],
		['email', 'local_credential', 'off_disabled', 'local_credential'],
		['email', 'local_credential', 'off_incomplete', 'local_credential'],
		['username', 'auto', 'on', 'local_credential'],
		['username', 'auto', 'off_username_mode', 'local_credential'],
		['username', 'auto', 'off_disabled', 'local_credential'],
		['username', 'auto', 'off_incomplete', 'local_credential'],
		['username', 'confirmed_email', 'on', 'local_credential'],
		['username', 'confirmed_email', 'off_username_mode', 'local_credential'],
		['username', 'confirmed_email', 'off_disabled', 'local_credential'],
		['username', 'confirmed_email', 'off_incomplete', 'local_credential'],
		['username', 'local_credential', 'on', 'local_credential'],
		['username', 'local_credential', 'off_username_mode', 'local_credential'],
		['username', 'local_credential', 'off_disabled', 'local_credential'],
		['username', 'local_credential', 'off_incomplete', 'local_credential'],
	] as const)('%s %s %s uses %s', (identityMode, basis, deliveryState, expected) => {
		expect(resolveEffectiveVerificationBasis({identityMode, basis, deliveryState})).toBe(expected);
	});
});

describe('resolveAccountTier', () => {
	const base: AccountTierInput = {
		isBot: false,
		isSystem: false,
		traits: new Set<string>(),
		identityMode: AccountIdentityModes.EMAIL,
		effectiveBasis: VerificationBases.CONFIRMED_EMAIL,
		hasEmail: true,
		emailVerified: true,
		localCredential: LocalCredentialKinds.NONE,
	};

	it('treats bots, system accounts and SSO accounts as verified everywhere', () => {
		for (const identityMode of ALL_IDENTITY_MODES) {
			const anon = {...base, identityMode, hasEmail: false, emailVerified: false};
			expect(resolveAccountTier({...anon, isBot: true})).toBe(AccountTiers.VERIFIED);
			expect(resolveAccountTier({...anon, isSystem: true})).toBe(AccountTiers.VERIFIED);
			expect(resolveAccountTier({...anon, traits: new Set([UserTraits.SSO])})).toBe(AccountTiers.VERIFIED);
			expect(resolveAccountTier({...anon, traits: [UserTraits.SSO]})).toBe(AccountTiers.VERIFIED);
		}
	});

	const credentials = ['none', 'password', 'passkey', 'password_and_passkey'] as const;
	const cases = [
		['email', 'confirmed_email', false, false, ['unclaimed', 'unclaimed', 'unclaimed', 'unclaimed']],
		['email', 'confirmed_email', false, true, ['unclaimed', 'unclaimed', 'unclaimed', 'unclaimed']],
		['email', 'confirmed_email', true, false, ['unverified', 'unverified', 'unverified', 'unverified']],
		['email', 'confirmed_email', true, true, ['verified', 'verified', 'verified', 'verified']],
		['email', 'local_credential', false, false, ['unclaimed', 'unclaimed', 'unclaimed', 'unclaimed']],
		['email', 'local_credential', false, true, ['unclaimed', 'unclaimed', 'unclaimed', 'unclaimed']],
		['email', 'local_credential', true, false, ['unclaimed', 'verified', 'verified', 'verified']],
		['email', 'local_credential', true, true, ['unclaimed', 'verified', 'verified', 'verified']],
		['username', 'confirmed_email', false, false, ['unclaimed', 'verified', 'verified', 'verified']],
		['username', 'confirmed_email', false, true, ['unclaimed', 'verified', 'verified', 'verified']],
		['username', 'confirmed_email', true, false, ['unclaimed', 'verified', 'verified', 'verified']],
		['username', 'confirmed_email', true, true, ['unclaimed', 'verified', 'verified', 'verified']],
		['username', 'local_credential', false, false, ['unclaimed', 'verified', 'verified', 'verified']],
		['username', 'local_credential', false, true, ['unclaimed', 'verified', 'verified', 'verified']],
		['username', 'local_credential', true, false, ['unclaimed', 'verified', 'verified', 'verified']],
		['username', 'local_credential', true, true, ['unclaimed', 'verified', 'verified', 'verified']],
	] as const;
	const credentialCases = cases.flatMap(([identityMode, effectiveBasis, hasEmail, emailVerified, expectedTiers]) =>
		credentials.map((localCredential, index) => ({
			identityMode,
			effectiveBasis,
			hasEmail,
			emailVerified,
			localCredential,
			expected: expectedTiers[index],
		})),
	);

	it.each(credentialCases)(
		'$identityMode $effectiveBasis email=$hasEmail verified=$emailVerified $localCredential',
		({expected, ...input}) => {
			expect(resolveAccountTier({...base, ...input})).toBe(expected);
		},
	);
});

describe('localCredentialKindOf', () => {
	it('combines a password and a passkey', () => {
		expect(localCredentialKindOf({hasPassword: false, hasPasskey: false})).toBe(LocalCredentialKinds.NONE);
		expect(localCredentialKindOf({hasPassword: true, hasPasskey: false})).toBe(LocalCredentialKinds.PASSWORD);
		expect(localCredentialKindOf({hasPassword: false, hasPasskey: true})).toBe(LocalCredentialKinds.PASSKEY);
		expect(localCredentialKindOf({hasPassword: true, hasPasskey: true})).toBe(
			LocalCredentialKinds.PASSWORD_AND_PASSKEY,
		);
	});
});

describe('resolveEmailDeliveryState', () => {
	it.each([
		['email', 'off', false, null, 'off_disabled'],
		['email', 'off', false, '', 'off_disabled'],
		['email', 'off', false, ' ', 'off_disabled'],
		['email', 'off', false, 'noreply@example.com', 'off_disabled'],
		['email', 'off', true, null, 'off_disabled'],
		['email', 'off', true, '', 'off_disabled'],
		['email', 'off', true, ' ', 'off_disabled'],
		['email', 'off', true, 'noreply@example.com', 'off_disabled'],
		['email', 'smtp', false, null, 'off_incomplete'],
		['email', 'smtp', false, '', 'off_incomplete'],
		['email', 'smtp', false, ' ', 'off_incomplete'],
		['email', 'smtp', false, 'noreply@example.com', 'off_incomplete'],
		['email', 'smtp', true, null, 'off_incomplete'],
		['email', 'smtp', true, '', 'off_incomplete'],
		['email', 'smtp', true, ' ', 'off_incomplete'],
		['email', 'smtp', true, 'noreply@example.com', 'on'],
		['username', 'off', false, null, 'off_username_mode'],
		['username', 'off', false, '', 'off_username_mode'],
		['username', 'off', false, ' ', 'off_username_mode'],
		['username', 'off', false, 'noreply@example.com', 'off_username_mode'],
		['username', 'off', true, null, 'off_username_mode'],
		['username', 'off', true, '', 'off_username_mode'],
		['username', 'off', true, ' ', 'off_username_mode'],
		['username', 'off', true, 'noreply@example.com', 'off_username_mode'],
		['username', 'smtp', false, null, 'off_username_mode'],
		['username', 'smtp', false, '', 'off_username_mode'],
		['username', 'smtp', false, ' ', 'off_username_mode'],
		['username', 'smtp', false, 'noreply@example.com', 'off_username_mode'],
		['username', 'smtp', true, null, 'off_username_mode'],
		['username', 'smtp', true, '', 'off_username_mode'],
		['username', 'smtp', true, ' ', 'off_username_mode'],
		['username', 'smtp', true, 'noreply@example.com', 'off_username_mode'],
	] as const)('%s %s complete=%s from=%s is %s', (identityMode, delivery, smtpComplete, fromAddress, expected) => {
		expect(resolveEmailDeliveryState({identityMode, delivery, smtpComplete, fromAddress})).toBe(expected);
	});
});

describe('resolveEmailCodeAvailability', () => {
	it.each([
		['email', 'on', false, false, 'no_owned_email'],
		['email', 'on', false, true, 'no_owned_email'],
		['email', 'on', true, false, 'no_owned_email'],
		['email', 'on', true, true, 'available'],
		['email', 'off_username_mode', false, false, 'delivery_off'],
		['email', 'off_username_mode', false, true, 'delivery_off'],
		['email', 'off_username_mode', true, false, 'delivery_off'],
		['email', 'off_username_mode', true, true, 'delivery_off'],
		['email', 'off_disabled', false, false, 'delivery_off'],
		['email', 'off_disabled', false, true, 'delivery_off'],
		['email', 'off_disabled', true, false, 'delivery_off'],
		['email', 'off_disabled', true, true, 'delivery_off'],
		['email', 'off_incomplete', false, false, 'delivery_off'],
		['email', 'off_incomplete', false, true, 'delivery_off'],
		['email', 'off_incomplete', true, false, 'delivery_off'],
		['email', 'off_incomplete', true, true, 'delivery_off'],
		['username', 'on', false, false, 'identity_mode_username'],
		['username', 'on', false, true, 'identity_mode_username'],
		['username', 'on', true, false, 'identity_mode_username'],
		['username', 'on', true, true, 'identity_mode_username'],
		['username', 'off_username_mode', false, false, 'identity_mode_username'],
		['username', 'off_username_mode', false, true, 'identity_mode_username'],
		['username', 'off_username_mode', true, false, 'identity_mode_username'],
		['username', 'off_username_mode', true, true, 'identity_mode_username'],
		['username', 'off_disabled', false, false, 'identity_mode_username'],
		['username', 'off_disabled', false, true, 'identity_mode_username'],
		['username', 'off_disabled', true, false, 'identity_mode_username'],
		['username', 'off_disabled', true, true, 'identity_mode_username'],
		['username', 'off_incomplete', false, false, 'identity_mode_username'],
		['username', 'off_incomplete', false, true, 'identity_mode_username'],
		['username', 'off_incomplete', true, false, 'identity_mode_username'],
		['username', 'off_incomplete', true, true, 'identity_mode_username'],
	] as const)('%s %s email=%s verified=%s is %s', (identityMode, deliveryState, hasEmail, emailVerified, expected) => {
		expect(resolveEmailCodeAvailability({identityMode, deliveryState, hasEmail, emailVerified})).toBe(expected);
	});
});

describe('resolveIdentifierFirst', () => {
	it.each([
		['auto', 'email', 'on', true, true],
		['auto', 'email', 'off_username_mode', false, false],
		['auto', 'email', 'off_disabled', false, false],
		['auto', 'email', 'off_incomplete', false, false],
		['auto', 'username', 'on', false, false],
		['auto', 'username', 'off_username_mode', false, false],
		['auto', 'username', 'off_disabled', false, false],
		['auto', 'username', 'off_incomplete', false, false],
		['on', 'email', 'on', true, true],
		['on', 'email', 'off_username_mode', true, false],
		['on', 'email', 'off_disabled', true, false],
		['on', 'email', 'off_incomplete', true, false],
		['on', 'username', 'on', false, false],
		['on', 'username', 'off_username_mode', false, false],
		['on', 'username', 'off_disabled', false, false],
		['on', 'username', 'off_incomplete', false, false],
		['off', 'email', 'on', false, false],
		['off', 'email', 'off_username_mode', false, false],
		['off', 'email', 'off_disabled', false, false],
		['off', 'email', 'off_incomplete', false, false],
		['off', 'username', 'on', false, false],
		['off', 'username', 'off_username_mode', false, false],
		['off', 'username', 'off_disabled', false, false],
		['off', 'username', 'off_incomplete', false, false],
	] as const)('%s %s %s resolves to %s %s', (mode, identityMode, deliveryState, identifierFirst, emailCodes) => {
		expect(resolveIdentifierFirst({mode, identityMode, deliveryState})).toEqual({identifierFirst, emailCodes});
	});
});

describe('resolveInstantAccountAvailability', () => {
	const open = {
		enabled: true,
		captchaProvider: CaptchaProviders.ALTCHA,
		registrationMode: RegistrationModes.OPEN,
		registrationUrl: null,
	};

	it('is available while enabled, with a captcha and open sign-ups', () => {
		expect(resolveInstantAccountAvailability(open)).toBe(InstantAccountAvailability.AVAILABLE);
	});

	it('reports why it is unavailable, in order', () => {
		expect(resolveInstantAccountAvailability({...open, enabled: false, captchaProvider: CaptchaProviders.NONE})).toBe(
			InstantAccountAvailability.DISABLED,
		);
		expect(resolveInstantAccountAvailability({...open, captchaProvider: CaptchaProviders.NONE})).toBe(
			InstantAccountAvailability.CAPTCHA_OFF,
		);
		expect(resolveInstantAccountAvailability({...open, registrationMode: RegistrationModes.APPROVAL})).toBe(
			InstantAccountAvailability.APPROVAL_REQUIRED,
		);
		expect(resolveInstantAccountAvailability({...open, registrationMode: RegistrationModes.CLOSED})).toBe(
			InstantAccountAvailability.REGISTRATION_CLOSED,
		);
	});

	it('follows a registration URL over the registration mode', () => {
		for (const registrationMode of Object.values(RegistrationModes)) {
			expect(
				resolveInstantAccountAvailability({...open, registrationMode, registrationUrl: {approvalRequired: false}}),
			).toBe(InstantAccountAvailability.AVAILABLE);
			expect(
				resolveInstantAccountAvailability({...open, registrationMode, registrationUrl: {approvalRequired: true}}),
			).toBe(InstantAccountAvailability.APPROVAL_REQUIRED);
		}
	});
});

describe('requiresLocalCredential', () => {
	it.each([
		['email', 'on', false, false],
		['email', 'on', true, false],
		['email', 'off_username_mode', false, true],
		['email', 'off_username_mode', true, false],
		['email', 'off_disabled', false, true],
		['email', 'off_disabled', true, false],
		['email', 'off_incomplete', false, true],
		['email', 'off_incomplete', true, false],
		['username', 'on', false, true],
		['username', 'on', true, true],
		['username', 'off_username_mode', false, true],
		['username', 'off_username_mode', true, true],
		['username', 'off_disabled', false, true],
		['username', 'off_disabled', true, true],
		['username', 'off_incomplete', false, true],
		['username', 'off_incomplete', true, true],
	] as const)('%s %s proven=%s needs credentials: %s', (identityMode, deliveryState, emailProven, expected) => {
		expect(requiresLocalCredential({identityMode, deliveryState, emailProven})).toBe(expected);
	});
});

describe('normalizeAccountIdentity', () => {
	it('drops the tag on username instances and keeps it on email instances', () => {
		for (const tagStyle of Object.values(TagStyles)) {
			expect(normalizeAccountIdentity(AccountIdentityModes.USERNAME, tagStyle)).toEqual({
				mode: AccountIdentityModes.USERNAME,
				tagStyle: TagStyles.NONE,
			});
			expect(normalizeAccountIdentity(AccountIdentityModes.EMAIL, tagStyle)).toEqual({
				mode: AccountIdentityModes.EMAIL,
				tagStyle,
			});
		}
	});
});

describe('dateOfBirthPolicyFromLegacy', () => {
	it.each([
		[false, LegacyDateOfBirthEnforcements.OFF, DateOfBirthPolicies.OFF],
		[false, LegacyDateOfBirthEnforcements.NEW_ACCOUNTS, DateOfBirthPolicies.OFF],
		[false, LegacyDateOfBirthEnforcements.ALL, DateOfBirthPolicies.OFF],
		[true, LegacyDateOfBirthEnforcements.OFF, DateOfBirthPolicies.ASK],
		[true, LegacyDateOfBirthEnforcements.NEW_ACCOUNTS, DateOfBirthPolicies.REQUIRE_NEW_ACCOUNTS],
		[true, LegacyDateOfBirthEnforcements.ALL, DateOfBirthPolicies.REQUIRE_ALL],
	] as const)('collect=%s enforcement=%s gives %s', (collect, enforcement, policy) => {
		expect(dateOfBirthPolicyFromLegacy({collect, enforcement})).toBe(policy);
		expect(collectsDateOfBirth(policy)).toBe(collect);
	});
});
