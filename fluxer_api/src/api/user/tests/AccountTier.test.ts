// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {EMPTY_USER_ROW, type UserRow} from '@app/api/database/types/UserTypes';
import {setCachedGuildVisitsConfig} from '@app/api/experiment/GuildVisitsGate';
import {setCachedAccountIdentity} from '@app/api/instance/AccountIdentityModeCache';
import {
	getEffectiveVerificationBasis,
	hostedAccountsDefaults,
	setCachedAccountsConfig,
	setCachedEmailDeliveryState,
} from '@app/api/instance/AccountsConfigCache';
import {User} from '@app/api/models/User';
import {WebAuthnCredential} from '@app/api/models/WebAuthnCredential';
import {
	assertCanRegisterPasskey,
	assertTierAllows,
	GUILD_VISITS_CAPABILITY,
	getAccountTier,
	hasWayBackIn,
	NO_TIER_CONTEXT,
	PAYMENT_DISPUTE_REASON_CAPABILITY,
	parseTierCapabilities,
	type TierContext,
} from '@app/api/user/AccountTier';
import {EmailDeliveryStates, VerificationBases} from '@fluxer/constants/src/AccountConfigConstants';
import {AccountIdentityModes, TagStyles} from '@fluxer/constants/src/AccountIdentityConstants';
import type {TierGatedAction} from '@fluxer/constants/src/AccountTierConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {UserAuthenticatorTypes} from '@fluxer/constants/src/UserConstants';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {GuildVisitsConfigSchema} from '@fluxer/schema/src/domains/admin/GuildVisitsSchemas';
import {afterEach, describe, expect, it} from 'vitest';

function createUser(overrides: Partial<UserRow> = {}): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: createUserID(1n),
		username: 'tier_user',
		discriminator: 1,
		bot: false,
		email: 'person@example.com',
		email_verified: true,
		password_hash: null,
		traits: new Set<string>(),
		...overrides,
	});
}

const UNCLAIMED = createUser({email: null, email_verified: false});
const UNVERIFIED = createUser({email_verified: false});
const VERIFIED = createUser();

function useInteractionTier(mode: 'legacy' | 'enforced', guildJoinGate: 'visits_capable' | 'always' = 'always') {
	setCachedAccountsConfig({
		...hostedAccountsDefaults().accounts,
		interaction_tier: mode,
		guild_join_gate: guildJoinGate,
	});
}

function useGuildVisits(enabled: boolean): void {
	setCachedGuildVisitsConfig(GuildVisitsConfigSchema.parse({enabled, included_user_ids: enabled ? ['1'] : []}));
}

function reachCode(user: User, action: TierGatedAction, ctx: TierContext = NO_TIER_CONTEXT): string | null {
	try {
		assertTierAllows(user, action, ctx);
		return null;
	} catch (error) {
		if (error instanceof FluxerError) return error.code;
		throw error;
	}
}

const CAPABLE: TierContext = {capabilities: new Set([GUILD_VISITS_CAPABILITY])};

afterEach(() => {
	setCachedAccountsConfig(null);
	setCachedGuildVisitsConfig(null);
	setCachedEmailDeliveryState(null);
});

describe('getAccountTier', () => {
	it('decides by email on instances that sign in with an email', () => {
		expect(getAccountTier(UNCLAIMED)).toBe('unclaimed');
		expect(getAccountTier(UNVERIFIED)).toBe('unverified');
		expect(getAccountTier(VERIFIED)).toBe('verified');
		expect(getAccountTier(createUser({email: null, password_hash: 'hash'}))).toBe('unclaimed');
	});

	it('treats bots, system accounts and SSO accounts as verified', () => {
		expect(getAccountTier(createUser({email: null, bot: true}))).toBe('verified');
		expect(getAccountTier(createUser({email: null, system: true}))).toBe('verified');
		expect(getAccountTier(createUser({email: null, traits: new Set(['sso'])}))).toBe('verified');
	});

	it('needs a password or passkey next to the email when the instance cannot send email', () => {
		setCachedEmailDeliveryState(EmailDeliveryStates.OFF_DISABLED);
		expect(getAccountTier(createUser({email_verified: true}))).toBe('unclaimed');
		expect(getAccountTier(createUser({password_hash: 'hash'}))).toBe('verified');
		expect(getAccountTier(createUser({authenticator_types: new Set([UserAuthenticatorTypes.WEBAUTHN])}))).toBe(
			'verified',
		);
	});

	it('reads a stored confirmed email basis as a local credential while email cannot be delivered', () => {
		setCachedAccountsConfig({
			...hostedAccountsDefaults().accounts,
			verification_basis: VerificationBases.CONFIRMED_EMAIL,
		});
		expect(getAccountTier(createUser({email_verified: false, password_hash: 'hash'}))).toBe('unverified');
		setCachedEmailDeliveryState(EmailDeliveryStates.OFF_INCOMPLETE);
		expect(getEffectiveVerificationBasis()).toBe(VerificationBases.LOCAL_CREDENTIAL);
		expect(getAccountTier(createUser({email_verified: false, password_hash: 'hash'}))).toBe('verified');
		expect(getAccountTier(createUser({email_verified: false}))).toBe('unclaimed');
	});
});

describe('guild join gate', () => {
	it('keeps joining open while the visit rollout does not cover the account', () => {
		useInteractionTier('enforced');
		useGuildVisits(false);
		expect(reachCode(UNCLAIMED, 'guild_join', CAPABLE)).toBeNull();
	});

	it('keeps joining open for a client that cannot visit when the gate is limited to visit clients', () => {
		useInteractionTier('enforced', 'visits_capable');
		useGuildVisits(true);
		expect(reachCode(UNCLAIMED, 'guild_join', NO_TIER_CONTEXT)).toBeNull();
	});

	it('refuses joining for a visit capable client in the rollout', () => {
		useInteractionTier('enforced');
		useGuildVisits(true);
		expect(reachCode(UNCLAIMED, 'guild_join', CAPABLE)).toBe(APIErrorCodes.ACCOUNT_VERIFICATION_REQUIRED);
		expect(reachCode(UNVERIFIED, 'guild_join', CAPABLE)).toBe(APIErrorCodes.ACCOUNT_VERIFICATION_REQUIRED);
		expect(reachCode(VERIFIED, 'guild_join', CAPABLE)).toBeNull();
	});

	it('refuses joining for every client by default', () => {
		useInteractionTier('enforced');
		useGuildVisits(true);
		expect(reachCode(UNCLAIMED, 'guild_join', NO_TIER_CONTEXT)).toBe(APIErrorCodes.ACCOUNT_VERIFICATION_REQUIRED);
	});

	it('stays open in legacy mode', () => {
		useInteractionTier('legacy', 'always');
		useGuildVisits(true);
		expect(reachCode(UNCLAIMED, 'guild_join', CAPABLE)).toBeNull();
	});

	it('parses only known capabilities', () => {
		expect(parseTierCapabilities('guild_visits, other')).toEqual(new Set([GUILD_VISITS_CAPABILITY]));
		expect(parseTierCapabilities('Guild_Visits')).toEqual(new Set([GUILD_VISITS_CAPABILITY]));
		expect(parseTierCapabilities('guild_visits,payment_dispute_reason')).toEqual(
			new Set([GUILD_VISITS_CAPABILITY, PAYMENT_DISPUTE_REASON_CAPABILITY]),
		);
		expect(parseTierCapabilities(undefined).size).toBe(0);
		expect(parseTierCapabilities('x'.repeat(600)).size).toBe(0);
	});
});

describe('hasWayBackIn', () => {
	function deps(credentials: Array<WebAuthnCredential> = []) {
		return {
			users: {
				listWebAuthnCredentials: async () => credentials,
			},
		};
	}

	it('counts a confirmed email, a password, a passkey and SSO', async () => {
		expect(await hasWayBackIn(deps(), VERIFIED)).toBe(true);
		expect(await hasWayBackIn(deps(), createUser({email_verified: false, password_hash: 'hash'}))).toBe(true);
		const credential = new WebAuthnCredential({
			user_id: UNCLAIMED.id,
			credential_id: 'stored-passkey',
			public_key: Buffer.from([1, 2, 3]),
			counter: 0n,
			transports: new Set(['internal']),
			name: 'Passkey',
			created_at: new Date(0),
			last_used_at: null,
			version: 1,
			rp_id: 'example.com',
			superseded_by: null,
		});
		expect(await hasWayBackIn(deps([credential]), UNCLAIMED)).toBe(true);
		expect(await hasWayBackIn(deps(), createUser({email: null, traits: new Set(['sso'])}))).toBe(true);
	});

	it('counts an unconfirmed address when the instance sends email', async () => {
		expect(await hasWayBackIn(deps(), UNVERIFIED)).toBe(true);
	});

	it('counts a password without an email only where sign-in takes a username', async () => {
		const passwordOnly = createUser({email: null, email_verified: false, password_hash: 'hash'});
		expect(await hasWayBackIn(deps(), passwordOnly)).toBe(false);
		const selfHosted = Config.instance.selfHosted;
		Config.instance.selfHosted = true;
		setCachedAccountIdentity({mode: AccountIdentityModes.USERNAME, tagStyle: TagStyles.NONE});
		try {
			expect(await hasWayBackIn(deps(), passwordOnly)).toBe(true);
		} finally {
			setCachedAccountIdentity(null);
			Config.instance.selfHosted = selfHosted;
		}
	});

	it('finds no way back for a username-only account', async () => {
		expect(await hasWayBackIn(deps(), UNCLAIMED)).toBe(false);
	});

	it('counts no email at all when the instance cannot send email, whatever the stored flag says', async () => {
		setCachedEmailDeliveryState(EmailDeliveryStates.OFF_DISABLED);
		expect(await hasWayBackIn(deps(), VERIFIED)).toBe(false);
		expect(await hasWayBackIn(deps(), UNVERIFIED)).toBe(false);
		expect(await hasWayBackIn(deps(), createUser({password_hash: 'hash'}))).toBe(true);
	});

	it('counts the passkey trait without reading stored credentials', async () => {
		const passkeyOnly = createUser({email: null, email_verified: false, traits: new Set(['passkey'])});
		const users = {
			listWebAuthnCredentials: async () => {
				throw new Error('should not be read');
			},
		};
		expect(await hasWayBackIn({users}, passkeyOnly)).toBe(true);
	});
});

describe('assertCanRegisterPasskey', () => {
	function registerCode(user: User): string | null {
		try {
			assertCanRegisterPasskey(user);
			return null;
		} catch (error) {
			if (error instanceof FluxerError) return error.code;
			throw error;
		}
	}

	it('keeps the MFA gate where an email is what the account still needs', () => {
		useInteractionTier('legacy');
		expect(registerCode(UNCLAIMED)).toBe(APIErrorCodes.MFA_EMAIL_VERIFICATION_REQUIRED);
		expect(registerCode(UNVERIFIED)).toBe(APIErrorCodes.MFA_EMAIL_VERIFICATION_REQUIRED);
		expect(registerCode(VERIFIED)).toBeNull();
	});

	it('lets an account that needs a credential register a passkey', () => {
		setCachedEmailDeliveryState(EmailDeliveryStates.OFF_DISABLED);
		expect(registerCode(createUser({email_verified: false}))).toBeNull();
		const selfHosted = Config.instance.selfHosted;
		Config.instance.selfHosted = true;
		setCachedAccountIdentity({mode: AccountIdentityModes.USERNAME, tagStyle: TagStyles.NONE});
		try {
			expect(registerCode(UNCLAIMED)).toBeNull();
		} finally {
			setCachedAccountIdentity(null);
			Config.instance.selfHosted = selfHosted;
		}
	});
});
