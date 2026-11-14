// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthSession} from '@app/api/auth/AuthSession';
import {getSudoModeService} from '@app/api/auth/services/SudoModeService';
import {createTotpSecret, totpCodeNow} from '@app/api/auth/tests/AuthTestUtils';
import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import {createApiContext} from '@app/api/CreateApiContext';
import {LocalDate} from '@app/api/database/LocalDate';
import {EMPTY_USER_ROW, type UserRow} from '@app/api/database/types/UserTypes';
import {setCachedAccountIdentity} from '@app/api/instance/AccountIdentityModeCache';
import {
	hostedAccountsDefaults,
	setCachedAccountsConfig,
	setCachedEmailDeliveryState,
} from '@app/api/instance/AccountsConfigCache';
import {
	getGuildRepository,
	getInstanceConfigRepository,
	getUserRepository,
} from '@app/api/middleware/ServiceSingletons';
import {SUDO_MODE_HEADER} from '@app/api/middleware/SudoModeMiddleware';
import {User} from '@app/api/models/User';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {EmailDeliveryStates} from '@fluxer/constants/src/AccountConfigConstants';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {UserAuthenticatorTypes, UserFlags} from '@fluxer/constants/src/UserConstants';
import {MfaBackupCodesResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

function createUser(overrides: Partial<UserRow> = {}): User {
	return new User({
		...EMPTY_USER_ROW,
		user_id: createUserID(10n),
		username: 'service_user',
		discriminator: 1,
		bot: false,
		email: 'person@example.com',
		email_verified: true,
		password_hash: 'hash',
		traits: new Set<string>(),
		flags: UserFlags.HAS_SESSION_STARTED,
		date_of_birth: LocalDate.fromString('1990-01-01'),
		...overrides,
	});
}

const UNCLAIMED = createUser({email: null, email_verified: false, password_hash: null});
const UNVERIFIED = createUser({email_verified: false});
const VERIFIED = createUser();

const selfHosted = Config.instance.selfHosted;

afterEach(() => {
	setCachedAccountsConfig(null);
	setCachedEmailDeliveryState(null);
	setCachedAccountIdentity(null);
	Config.instance.selfHosted = selfHosted;
});

let harness: ApiTestHarness;
let recipient: User;
let nextUserId = 100n;

async function authenticate(fixture: User): Promise<{user: User; token: string}> {
	const id = createUserID(nextUserId++);
	const user = await getUserRepository().create({
		...fixture.toRow(),
		user_id: id,
		username: `service_${id}`,
		email: fixture.email === null ? null : `service_${id}@example.com`,
	});
	const [token] = await createAuthSession(createApiContext(), {
		user,
		origin: {ip: '127.0.0.1', userAgent: 'AccountTierServiceGates', clientOs: null},
	});
	return {user, token};
}

beforeAll(async () => {
	harness = await createApiTestHarness();
});

beforeEach(async () => {
	await harness.reset();
	setCachedAccountsConfig({...hostedAccountsDefaults().accounts, interaction_tier: 'legacy'});
	setCachedEmailDeliveryState(EmailDeliveryStates.ON);
	recipient = (await authenticate(VERIFIED)).user;
});

afterAll(async () => {
	await harness.shutdown();
});

describe('account tier service wiring', () => {
	it('requires a confirmed email to open a DM and stores the allowed conversation', async () => {
		const unverified = await authenticate(UNVERIFIED);
		await createBuilder(harness, unverified.token)
			.post('/users/@me/channels')
			.body({recipient_id: recipient.id.toString()})
			.expect(HttpStatus.FORBIDDEN, APIErrorCodes.DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED)
			.execute();
		expect(await getUserRepository().findExistingDmState(unverified.user.id, recipient.id)).toBeNull();
		const verified = await authenticate(VERIFIED);
		const channel = ChannelResponse.parse(
			await createBuilder(harness, verified.token)
				.post('/users/@me/channels')
				.body({recipient_id: recipient.id.toString()})
				.execute(),
		);
		expect((await getUserRepository().findExistingDmState(verified.user.id, recipient.id))?.id.toString()).toBe(
			channel.id,
		);
	});

	it('gates TOTP enrollment before storing the credential and backup codes', async () => {
		const unverified = await authenticate(UNVERIFIED);
		const secret = createTotpSecret();
		const unverifiedSudoToken = await getSudoModeService().generateSudoToken(unverified.user.id);
		await createBuilder(harness, unverified.token)
			.post('/users/@me/mfa/totp/enable')
			.header(SUDO_MODE_HEADER, unverifiedSudoToken)
			.body({secret, code: totpCodeNow(secret)})
			.expect(HttpStatus.FORBIDDEN, APIErrorCodes.MFA_EMAIL_VERIFICATION_REQUIRED)
			.execute();
		expect((await getUserRepository().findUnique(unverified.user.id))?.totpSecret).toBeNull();
		const verified = await authenticate(VERIFIED);
		const verifiedSudoToken = await getSudoModeService().generateSudoToken(verified.user.id);
		const codes = MfaBackupCodesResponse.parse(
			await createBuilder(harness, verified.token)
				.post('/users/@me/mfa/totp/enable')
				.header(SUDO_MODE_HEADER, verifiedSudoToken)
				.body({secret, code: totpCodeNow(secret)})
				.execute(),
		);
		expect(codes.backup_codes).toHaveLength(10);
		const stored = await getUserRepository().findUnique(verified.user.id);
		expect(stored?.totpSecret).toBe(secret);
		expect(stored?.authenticatorTypes.has(UserAuthenticatorTypes.TOTP)).toBe(true);
	});

	it('gates guild creation and stores the allowed community and membership', async () => {
		const unclaimed = await authenticate(UNCLAIMED);
		await createBuilder(harness, unclaimed.token)
			.post('/guilds')
			.body({name: 'Gate'})
			.expect(HttpStatus.FORBIDDEN, APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_CREATE_GUILDS)
			.execute();
		const verified = await authenticate(VERIFIED);
		const created = await createBuilder<{id: string; owner_id: string}>(harness, verified.token)
			.post('/guilds')
			.body({name: 'Gate'})
			.execute();
		expect(created.owner_id).toBe(verified.user.id.toString());
		const guildId = createGuildID(BigInt(created.id));
		expect((await getGuildRepository().findUnique(guildId))?.name).toBe('Gate');
		expect((await getGuildRepository().getMember(guildId, verified.user.id))?.userId).toBe(verified.user.id);
	});

	it('refuses the stock community path for an unclaimed owner', async () => {
		await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: false}});
		const admin = await authenticate(
			createUser({
				...UNCLAIMED.toRow(),
				acls: new Set([AdminACLs.AUTHENTICATE, AdminACLs.INSTANCE_CONFIG_UPDATE]),
			}),
		);
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({policy: {single_community_enabled: true, single_community_name: 'Stock'}})
			.expect(HttpStatus.FORBIDDEN, APIErrorCodes.UNCLAIMED_ACCOUNT_CANNOT_CREATE_GUILDS)
			.execute();
		expect((await getInstanceConfigRepository().getInstancePolicyConfig()).single_community_guild_id).toBeNull();
	});
});
