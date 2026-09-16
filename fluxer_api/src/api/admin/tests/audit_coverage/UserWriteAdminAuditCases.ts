// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	AdminAuditCoverageCase,
	AdminAuditCoverageContext,
} from '@app/api/admin/tests/audit_coverage/AdminAuditCoverage';
import {
	createTestAccount,
	createTotpSecret,
	createUniqueEmail,
	createUniqueUsername,
	generateTotpCode,
	type TestAccount,
} from '@app/api/auth/tests/AuthTestUtils';
import {
	createRegistrationResponse,
	createWebAuthnDevice,
	type WebAuthnCredentialMetadata,
	type WebAuthnRegistrationOptions,
} from '@app/api/auth/tests/WebAuthnTestUtils';
import {createUserID} from '@app/api/BrandedTypes';
import {createFriendship} from '@app/api/channel/tests/ChannelTestUtils';
import {getUserRepository} from '@app/api/middleware/ServiceSingletons';
import type {User} from '@app/api/models/User';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {DeletionReasons} from '@fluxer/constants/src/Core';
import {PremiumFlags, SuspiciousActivityFlags, UserFlags} from '@fluxer/constants/src/UserConstants';
import {expect} from 'vitest';

async function loadUser(account: TestAccount): Promise<User> {
	const user = await getUserRepository().findUnique(createUserID(BigInt(account.userId)));
	if (!user) {
		throw new Error(`User ${account.userId} not found`);
	}
	return user;
}

function adminBuilder({harness, admin}: AdminAuditCoverageContext) {
	return createBuilder(harness, admin.token);
}

async function enableTotp(context: AdminAuditCoverageContext, account: TestAccount): Promise<string> {
	const secret = createTotpSecret();
	await createBuilder(context.harness, account.token)
		.post('/users/@me/mfa/totp/enable')
		.body({secret, code: generateTotpCode(secret), password: account.password})
		.execute();
	return secret;
}

export const UserWriteAdminAuditCases: ReadonlyArray<AdminAuditCoverageCase> = [
	{
		method: 'DELETE',
		route: '/admin/users/:user_id/relationships',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			const friend = await createTestAccount(harness);
			await createFriendship(harness, target, friend);
			return {
				request: {path: `/admin/users/${target.userId}/relationships?category=friend`},
				expected: {
					action: 'remove_relationships_by_category',
					targetType: 'user',
					targetId: target.userId,
					metadata: {category: 'friend', removed_count: '1'},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/users/:user_id/relationships/:target_user_id',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			const friend = await createTestAccount(harness);
			await createFriendship(harness, target, friend);
			return {
				request: {
					path: `/admin/users/${target.userId}/relationships/${friend.userId}?category=friend`,
					expectStatus: 204,
				},
				expected: {
					action: 'remove_relationship',
					targetType: 'user',
					targetId: target.userId,
					metadata: {target_user_id: friend.userId, category: 'friend'},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/users/:user_id/sessions',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users/${target.userId}/sessions`},
				expected: {
					action: 'terminate_sessions',
					targetType: 'user',
					targetId: target.userId,
					metadata: {},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/users/:user_id/webauthn-credentials/:credential_id',
		async prepare(context) {
			const {harness} = context;
			const target = await createTestAccount(harness);
			const secret = await enableTotp(context, target);
			const device = createWebAuthnDevice();
			const registrationOptions = await createBuilder<WebAuthnRegistrationOptions>(harness, target.token)
				.post('/users/@me/mfa/webauthn/credentials/registration-options')
				.body({mfa_method: 'totp', mfa_code: generateTotpCode(secret)})
				.execute();
			if (registrationOptions.rp.id) {
				device.rpId = registrationOptions.rp.id;
			}
			await createBuilder(harness, target.token)
				.post('/users/@me/mfa/webauthn/credentials')
				.body({
					response: createRegistrationResponse(device, registrationOptions, 'Coverage Passkey'),
					challenge: registrationOptions.challenge,
					name: 'Coverage Passkey',
					mfa_method: 'totp',
					mfa_code: generateTotpCode(secret),
				})
				.expect(204)
				.execute();
			const credentials = await createBuilder<Array<WebAuthnCredentialMetadata>>(harness, target.token)
				.get('/users/@me/mfa/webauthn/credentials')
				.execute();
			const credentialId = credentials[0]!.id;
			return {
				request: {
					path: `/admin/users/${target.userId}/webauthn-credentials/${credentialId}`,
					expectStatus: 204,
				},
				expected: {
					action: 'delete_webauthn_credential',
					targetType: 'user',
					targetId: target.userId,
					metadata: {credential_id: credentialId},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/users/:user_id/mfa',
		async prepare(context) {
			const target = await createTestAccount(context.harness);
			await enableTotp(context, target);
			return {
				request: {path: `/admin/users/${target.userId}/mfa`, expectStatus: 204},
				expected: {
					action: 'disable_mfa',
					targetType: 'user',
					targetId: target.userId,
					metadata: {},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/users/:user_id/profile-fields',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {
					path: `/admin/users/${target.userId}/profile-fields`,
					body: {fields: ['bio', 'pronouns', 'global_name']},
				},
				expected: {
					action: 'clear_fields',
					targetType: 'user',
					targetId: target.userId,
					metadata: {fields: 'bio,pronouns,global_name'},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/users/:user_id/bot-status',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users/${target.userId}/bot-status`, body: {bot: true}},
				expected: {
					action: 'set_bot_status',
					targetType: 'user',
					targetId: target.userId,
					metadata: {bot: 'true'},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/users/:user_id/system-status',
		async prepare(context) {
			const target = await createTestAccount(context.harness);
			await adminBuilder(context).put(`/admin/users/${target.userId}/bot-status`).body({bot: true}).execute();
			return {
				request: {path: `/admin/users/${target.userId}/system-status`, body: {system: true}},
				expected: {
					action: 'set_system_status',
					targetType: 'user',
					targetId: target.userId,
					metadata: {system: 'true'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/users/:user_id/username',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			const username = createUniqueUsername('renamed');
			return {
				request: {path: `/admin/users/${target.userId}/username`, body: {username}},
				expected: {
					action: 'change_username',
					targetType: 'user',
					targetId: target.userId,
					metadata: {
						old_username: (await loadUser(target)).username,
						new_username: username,
						discriminator: expect.any(String),
					},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/users/:user_id/email',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			const email = createUniqueEmail('changed');
			return {
				request: {path: `/admin/users/${target.userId}/email`, body: {email}},
				expected: {
					action: 'change_email',
					targetType: 'user',
					targetId: target.userId,
					metadata: {old_email: (await loadUser(target)).email!, new_email: email},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/users/:user_id/email-verification',
		async prepare({harness}) {
			const target = await createTestAccount(harness, {skipEmailVerification: true});
			return {
				request: {path: `/admin/users/${target.userId}/email-verification`},
				expected: {
					action: 'verify_email',
					targetType: 'user',
					targetId: target.userId,
					metadata: {email: (await loadUser(target)).email!},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/users/:user_id/verification-email',
		async prepare({harness}) {
			const target = await createTestAccount(harness, {skipEmailVerification: true});
			return {
				request: {path: `/admin/users/${target.userId}/verification-email`, expectStatus: 204},
				expected: {
					action: 'resend_verification_email',
					targetType: 'user',
					targetId: target.userId,
					metadata: {email: (await loadUser(target)).email!},
				},
			};
		},
	},
	{
		method: 'POST',
		route: '/admin/users/:user_id/password-reset',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users/${target.userId}/password-reset`, expectStatus: 204},
				expected: {
					action: 'send_password_reset',
					targetType: 'user',
					targetId: target.userId,
					metadata: {email: (await loadUser(target)).email!},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/users/:user_id/ban',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {
					path: `/admin/users/${target.userId}/ban`,
					body: {duration_hours: 24, reason: 'Coverage ban'},
				},
				expected: {
					action: 'temp_ban',
					targetType: 'user',
					targetId: target.userId,
					metadata: {duration_hours: '24', reason: 'Coverage ban', banned_until: expect.any(String)},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/users/:user_id/ban',
		async prepare(context) {
			const target = await createTestAccount(context.harness);
			await adminBuilder(context).put(`/admin/users/${target.userId}/ban`).body({duration_hours: 24}).execute();
			return {
				request: {path: `/admin/users/${target.userId}/ban`},
				expected: {
					action: 'unban',
					targetType: 'user',
					targetId: target.userId,
					metadata: {},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/users/:user_id/deletion',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {
					path: `/admin/users/${target.userId}/deletion`,
					body: {reason_code: DeletionReasons.USER_REQUESTED, days_until_deletion: 30},
				},
				expected: {
					action: 'schedule_deletion',
					targetType: 'user',
					targetId: target.userId,
					metadata: {days: '30', reason_code: DeletionReasons.USER_REQUESTED.toString()},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/users/:user_id/deletion',
		async prepare(context) {
			const target = await createTestAccount(context.harness);
			await adminBuilder(context)
				.put(`/admin/users/${target.userId}/deletion`)
				.body({reason_code: DeletionReasons.USER_REQUESTED, days_until_deletion: 30})
				.execute();
			return {
				request: {path: `/admin/users/${target.userId}/deletion`},
				expected: {
					action: 'cancel_deletion',
					targetType: 'user',
					targetId: target.userId,
					metadata: {},
				},
			};
		},
	},
	{
		method: 'DELETE',
		route: '/admin/users/:user_id/message-deletion',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			await createBuilder(harness, target.token)
				.post('/users/@me/messages/delete')
				.body({password: target.password})
				.expect(204)
				.execute();
			return {
				request: {path: `/admin/users/${target.userId}/message-deletion`},
				expected: {
					action: 'cancel_bulk_message_deletion',
					targetType: 'user',
					targetId: target.userId,
					metadata: {},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/users/:user_id/acls',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			const acls = [AdminACLs.AUTHENTICATE, AdminACLs.USER_LOOKUP];
			return {
				request: {path: `/admin/users/${target.userId}/acls`, body: {acls}},
				expected: {
					action: 'set_acls',
					targetType: 'user',
					targetId: target.userId,
					metadata: {acls: acls.join(',')},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/users/:user_id/traits',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users/${target.userId}/traits`, body: {traits: ['coverage_trait']}},
				expected: {
					action: 'set_traits',
					targetType: 'user',
					targetId: target.userId,
					metadata: {traits: 'coverage_trait'},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/users/:user_id/flags',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			const {flags} = await loadUser(target);
			return {
				request: {
					path: `/admin/users/${target.userId}/flags`,
					body: {add_flags: [UserFlags.PARTNER.toString()], remove_flags: [UserFlags.SPAMMER.toString()]},
				},
				expected: {
					action: 'update_flags',
					targetType: 'user',
					targetId: target.userId,
					metadata: {
						add_flags: UserFlags.PARTNER.toString(),
						remove_flags: UserFlags.SPAMMER.toString(),
						new_flags: ((flags | UserFlags.PARTNER) & ~UserFlags.SPAMMER).toString(),
					},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/users/:user_id/premium-flags',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			const {premiumFlags} = await loadUser(target);
			return {
				request: {
					path: `/admin/users/${target.userId}/premium-flags`,
					body: {add_flags: [PremiumFlags.BADGE_HIDDEN], remove_flags: [PremiumFlags.PURCHASE_DISABLED]},
				},
				expected: {
					action: 'update_premium_flags',
					targetType: 'user',
					targetId: target.userId,
					metadata: {
						add_flags: PremiumFlags.BADGE_HIDDEN.toString(),
						remove_flags: PremiumFlags.PURCHASE_DISABLED.toString(),
						new_flags: ((premiumFlags | PremiumFlags.BADGE_HIDDEN) & ~PremiumFlags.PURCHASE_DISABLED).toString(),
					},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/users/:user_id/phone-verification',
		async prepare(context) {
			const target = await createTestAccount(context.harness);
			const flags = SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL | SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE;
			await adminBuilder(context)
				.put(`/admin/users/${target.userId}/suspicious-activity-flags`)
				.body({flags})
				.execute();
			return {
				request: {
					path: `/admin/users/${target.userId}/phone-verification`,
					body: {has_verified_phone: true},
				},
				expected: {
					action: 'update_has_verified_phone',
					targetType: 'user',
					targetId: target.userId,
					metadata: {
						has_verified_phone: 'true',
						suspicious_activity_flags_before: flags.toString(),
						suspicious_activity_flags_after: SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL.toString(),
					},
				},
			};
		},
	},
	{
		method: 'PATCH',
		route: '/admin/users/:user_id/date-of-birth',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			return {
				request: {path: `/admin/users/${target.userId}/date-of-birth`, body: {date_of_birth: '1995-06-15'}},
				expected: {
					action: 'change_dob',
					targetType: 'user',
					targetId: target.userId,
					metadata: {old_dob: (await loadUser(target)).dateOfBirth!, new_dob: '1995-06-15'},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/users/:user_id/suspicious-activity-flags',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			const flags = SuspiciousActivityFlags.REQUIRE_VERIFIED_EMAIL | SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE;
			return {
				request: {path: `/admin/users/${target.userId}/suspicious-activity-flags`, body: {flags}},
				expected: {
					action: 'update_suspicious_activity_flags',
					targetType: 'user',
					targetId: target.userId,
					metadata: {flags: flags.toString()},
				},
			};
		},
	},
	{
		method: 'PUT',
		route: '/admin/users/:user_id/suspicious-activity-disablement',
		async prepare({harness}) {
			const target = await createTestAccount(harness);
			const flags = SuspiciousActivityFlags.REQUIRE_VERIFIED_PHONE;
			return {
				request: {path: `/admin/users/${target.userId}/suspicious-activity-disablement`, body: {flags}},
				expected: {
					action: 'disable_suspicious_activity',
					targetType: 'user',
					targetId: target.userId,
					metadata: {flags: flags.toString()},
				},
			};
		},
	},
];
