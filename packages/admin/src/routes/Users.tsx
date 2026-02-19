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

/** @jsxRuntime automatic */
/** @jsxImportSource hono/jsx */

import {PATCHABLE_FLAGS} from '@fluxer/admin/src/AdminPackageConstants';
import * as messagesApi from '@fluxer/admin/src/api/Messages';
import * as usersApi from '@fluxer/admin/src/api/Users';
import {redirectWithFlash} from '@fluxer/admin/src/middleware/Auth';
import {UserDetailPage} from '@fluxer/admin/src/pages/UserDetailPage';
import {UsersPage} from '@fluxer/admin/src/pages/UsersPage';
import {getRouteContext} from '@fluxer/admin/src/routes/RouteContext';
import type {RouteFactoryDeps} from '@fluxer/admin/src/routes/RouteTypes';
import {getPageConfig} from '@fluxer/admin/src/SelfHostedOverride';
import type {AppVariables} from '@fluxer/admin/src/types/App';
import {hasBigIntFlag, tryParseBigInt} from '@fluxer/admin/src/utils/Bigint';
import {getOptionalString, getRequiredString, getStringArray, type ParsedBody} from '@fluxer/admin/src/utils/Forms';
import {Hono} from 'hono';

function parseCsvEntries(
	csvData: string,
): {ok: true; entries: Array<messagesApi.ShredEntry>} | {ok: false; error: string} {
	const normalized = csvData.trim();
	const lines = normalized.split('\n');
	const entries: Array<messagesApi.ShredEntry> = [];

	for (const line of lines) {
		const trimmed = line.trim();
		if (!trimmed) continue;

		const normalizedLower = trimmed.toLowerCase();
		if (normalizedLower === 'channel_id,message_id') continue;

		const parts = trimmed.split(',');
		if (parts.length !== 2) {
			return {ok: false, error: 'Each row must contain channel_id and message_id separated by a comma'};
		}

		const channelRaw = parts[0]?.trim();
		const messageRaw = parts[1]?.trim();

		if (!channelRaw || !messageRaw) {
			return {ok: false, error: `Invalid row format: ${trimmed}`};
		}

		const channelValue = parseInt(channelRaw, 10);
		const messageValue = parseInt(messageRaw, 10);

		if (Number.isNaN(channelValue)) {
			return {ok: false, error: `Invalid channel_id on row: ${trimmed}`};
		}

		if (Number.isNaN(messageValue)) {
			return {ok: false, error: `Invalid message_id on row: ${trimmed}`};
		}

		entries.push({
			channel_id: String(channelValue),
			message_id: String(messageValue),
		});
	}

	return {ok: true, entries};
}

export function createUsersRoutes({config, assetVersion, requireAuth}: RouteFactoryDeps) {
	const router = new Hono<{Variables: AppVariables}>();

	router.get('/users', requireAuth, async (c) => {
		const {session, currentAdmin, flash, csrfToken} = getRouteContext(c);
		const pageConfig = getPageConfig(c, config);
		const searchQuery = c.req.query('q');
		const page = parseInt(c.req.query('page') ?? '0', 10);

		return c.html(
			<UsersPage
				config={pageConfig}
				session={session}
				currentAdmin={currentAdmin}
				flash={flash}
				searchQuery={searchQuery}
				page={page}
				assetVersion={assetVersion}
				csrfToken={csrfToken}
			/>,
		);
	});

	router.get('/users/:userId', requireAuth, async (c) => {
		const {session, currentAdmin, flash, csrfToken} = getRouteContext(c);
		const pageConfig = getPageConfig(c, config);
		const userId = c.req.param('userId');
		const tab = c.req.query('tab');

		const guildsBefore = c.req.query('guilds_before');
		const guildsAfter = c.req.query('guilds_after');
		const guildsLimit = c.req.query('guilds_limit');
		const guildsWithCounts = c.req.query('guilds_with_counts');
		const dmBefore = c.req.query('dm_before');
		const dmAfter = c.req.query('dm_after');
		const dmLimit = c.req.query('dm_limit');

		const messageShredJobId = c.req.query('message_shred_job_id');
		const deleteAllMessagesDryRun = c.req.query('delete_all_messages_dry_run');
		const deleteAllMessagesChannelCount = c.req.query('delete_all_messages_channel_count');
		const deleteAllMessagesMessageCount = c.req.query('delete_all_messages_message_count');

		return c.html(
			<UserDetailPage
				config={pageConfig}
				session={session}
				currentAdmin={currentAdmin}
				flash={flash}
				userId={userId}
				tab={tab}
				guildsBefore={guildsBefore}
				guildsAfter={guildsAfter}
				guildsLimit={guildsLimit}
				guildsWithCounts={guildsWithCounts}
				dmBefore={dmBefore}
				dmAfter={dmAfter}
				dmLimit={dmLimit}
				messageShredJobId={messageShredJobId}
				deleteAllMessagesDryRun={deleteAllMessagesDryRun}
				deleteAllMessagesChannelCount={deleteAllMessagesChannelCount}
				deleteAllMessagesMessageCount={deleteAllMessagesMessageCount}
				assetVersion={assetVersion}
				csrfToken={csrfToken}
			/>,
		);
	});

	router.post('/users/:userId', requireAuth, async (c) => {
		const session = c.get('session')!;
		const userId = c.req.param('userId');
		const tab = c.req.query('tab') ?? 'overview';
		const action = c.req.query('action');
		const redirectUrl = `${config.basePath}/users/${userId}?tab=${tab}`;

		if (!action) {
			return redirectWithFlash(c, redirectUrl, {
				message: 'No action specified',
				type: 'error',
			});
		}

		try {
			const formData = (await c.req.parseBody()) as ParsedBody;

			switch (action) {
				case 'update_flags': {
					const userResult = await usersApi.lookupUser(config, session, userId);
					if (!userResult.ok || !userResult.data) {
						return redirectWithFlash(c, redirectUrl, {
							message: 'User not found',
							type: 'error',
						});
					}

					const user = userResult.data;

					const submittedFlagsRaw = getStringArray(formData, 'flags[]');
					const submittedFlags = submittedFlagsRaw.map((v) => tryParseBigInt(v)).filter((v): v is bigint => v !== null);
					const selectedFlagSet = new Set<bigint>(submittedFlags);

					const currentFlags = tryParseBigInt(user.flags) ?? 0n;
					const addFlags = submittedFlags
						.filter((flag) => !hasBigIntFlag(currentFlags, flag))
						.map((flag) => flag.toString());

					const removeFlags = PATCHABLE_FLAGS.map((f) => f.value)
						.filter((flag) => hasBigIntFlag(currentFlags, flag) && !selectedFlagSet.has(flag))
						.map((flag) => flag.toString());

					const result = await usersApi.updateUserFlags(config, session, userId, addFlags, removeFlags);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'User flags updated successfully' : 'Failed to update user flags',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'update_suspicious_flags': {
					const flagValues = getStringArray(formData, 'suspicious_flags[]').map((v) => parseInt(v, 10));

					const totalFlags = flagValues.reduce((acc, flag) => acc | flag, 0);
					const result = await usersApi.updateSuspiciousActivityFlags(config, session, userId, totalFlags);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok
							? 'Suspicious activity flags updated successfully'
							: 'Failed to update suspicious activity flags',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'update_acls': {
					const acls = getStringArray(formData, 'acls[]');

					const result = await usersApi.setUserAcls(config, session, userId, acls);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'ACLs updated successfully' : 'Failed to update ACLs',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'update_traits': {
					const selectedTraits = getStringArray(formData, 'traits[]')
						.map((v) => v.trim())
						.filter((v) => v !== '');

					const customInput = getOptionalString(formData, 'custom_traits') ?? '';
					const normalizedCustom = customInput.replace(/\n/g, ',');
					const customTraits = normalizedCustom
						.split(',')
						.map((v) => v.trim())
						.filter((v) => v !== '');

					const submittedTraits = [...new Set([...selectedTraits, ...customTraits])];
					const result = await usersApi.setUserTraits(config, session, userId, submittedTraits);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'Traits updated successfully' : 'Failed to update traits',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'disable_mfa': {
					const result = await usersApi.disableMfa(config, session, userId);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'MFA disabled successfully' : 'Failed to disable MFA',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'verify_email': {
					const result = await usersApi.verifyEmail(config, session, userId);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'Email verified successfully' : 'Failed to verify email',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'unlink_phone': {
					const result = await usersApi.unlinkPhone(config, session, userId);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'Phone unlinked successfully' : 'Failed to unlink phone',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'terminate_sessions': {
					const result = await usersApi.terminateSessions(config, session, userId);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'Sessions terminated successfully' : 'Failed to terminate sessions',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'clear_fields': {
					const fields = getStringArray(formData, 'fields[]');

					if (fields.length === 0) {
						return redirectWithFlash(c, redirectUrl, {
							message: 'No fields selected to clear',
							type: 'error',
						});
					}

					const result = await usersApi.clearUserFields(config, session, userId, fields);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'User fields cleared successfully' : 'Failed to clear user fields',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'set_bot_status': {
					const status = c.req.query('status') ?? 'false';
					const bot = status === 'true';
					const result = await usersApi.setBotStatus(config, session, userId, bot);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'Bot status updated successfully' : 'Failed to update bot status',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'set_system_status': {
					const status = c.req.query('status') ?? 'false';
					const system = status === 'true';
					const result = await usersApi.setSystemStatus(config, session, userId, system);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'System status updated successfully' : 'Failed to update system status',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'change_username': {
					const username = getRequiredString(formData, 'username');
					const discriminatorStr = getOptionalString(formData, 'discriminator');
					const discriminator = discriminatorStr ? parseInt(discriminatorStr, 10) : undefined;

					if (!username) {
						return redirectWithFlash(c, redirectUrl, {
							message: 'Username cannot be empty',
							type: 'error',
						});
					}

					const result = await usersApi.changeUsername(config, session, userId, username, discriminator);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'Username changed successfully' : 'Failed to change username',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'change_email': {
					const email = getRequiredString(formData, 'email');

					if (!email) {
						return redirectWithFlash(c, redirectUrl, {
							message: 'Email cannot be empty',
							type: 'error',
						});
					}

					const result = await usersApi.changeEmail(config, session, userId, email);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'Email changed successfully' : 'Failed to change email',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'temp_ban': {
					const durationStr = getOptionalString(formData, 'duration') ?? '24';
					const duration = parseInt(durationStr, 10);
					const publicReason = getOptionalString(formData, 'reason');
					const privateReason = getOptionalString(formData, 'private_reason');

					const result = await usersApi.tempBanUser(config, session, userId, duration, publicReason, privateReason);
					const isPermanent = duration <= 0;
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok
							? isPermanent
								? 'User banned permanently'
								: 'User temporarily banned successfully'
							: isPermanent
								? 'Failed to permanently ban user'
								: 'Failed to temporarily ban user',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'unban': {
					const result = await usersApi.unbanUser(config, session, userId);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'User unbanned successfully' : 'Failed to unban user',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'schedule_deletion': {
					const reasonCodeStr = getOptionalString(formData, 'reason_code') ?? '0';
					const reasonCode = parseInt(reasonCodeStr, 10);
					const daysStr = getOptionalString(formData, 'days') ?? '30';
					const days = parseInt(daysStr, 10);
					const publicReason = getOptionalString(formData, 'public_reason');
					const privateReason = getOptionalString(formData, 'private_reason');

					const result = await usersApi.scheduleDeletion(
						config,
						session,
						userId,
						reasonCode,
						publicReason,
						days,
						privateReason,
					);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'Account deletion scheduled successfully' : 'Failed to schedule account deletion',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'cancel_deletion': {
					const result = await usersApi.cancelDeletion(config, session, userId);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'Account deletion cancelled successfully' : 'Failed to cancel account deletion',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'cancel_bulk_message_deletion': {
					const result = await usersApi.cancelBulkMessageDeletion(config, session, userId);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok
							? 'Bulk message deletion cancelled successfully'
							: 'Failed to cancel bulk message deletion',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'delete_all_messages': {
					const dryRunValue = getOptionalString(formData, 'dry_run') ?? 'true';
					const dryRun = !(dryRunValue.toLowerCase() === 'false' || dryRunValue === '0');

					const result = await messagesApi.deleteAllUserMessages(config, session, userId, dryRun);
					if (!result.ok) {
						return redirectWithFlash(c, redirectUrl, {
							message: 'Failed to delete all user messages',
							type: 'error',
						});
					}

					if (dryRun) {
						const location = `${redirectUrl}&delete_all_messages_dry_run=true&delete_all_messages_channel_count=${result.data.channel_count}&delete_all_messages_message_count=${result.data.message_count}`;
						return redirectWithFlash(c, location, {
							message: `Dry run found ${result.data.message_count} messages across ${result.data.channel_count} channels. Confirm to delete them permanently.`,
							type: 'success',
						});
					} else {
						const location = result.data.job_id
							? `${redirectUrl}&message_shred_job_id=${result.data.job_id}`
							: redirectUrl;
						const message = result.data.job_id
							? 'Delete job queued. Monitor progress in the status panel.'
							: 'No messages found for deletion.';
						return redirectWithFlash(c, location, {
							message,
							type: 'success',
						});
					}
				}

				case 'change_dob': {
					const dateOfBirth = getRequiredString(formData, 'date_of_birth');

					if (!dateOfBirth) {
						return redirectWithFlash(c, redirectUrl, {
							message: 'Invalid date of birth',
							type: 'error',
						});
					}

					const result = await usersApi.changeDob(config, session, userId, dateOfBirth);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'Date of birth changed successfully' : 'Failed to change date of birth',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'delete_webauthn_credential': {
					const credentialId = getRequiredString(formData, 'credential_id');

					if (!credentialId) {
						return redirectWithFlash(c, redirectUrl, {
							message: 'Credential ID is required',
							type: 'error',
						});
					}

					const result = await usersApi.deleteWebAuthnCredential(config, session, userId, credentialId);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'WebAuthn credential deleted successfully' : 'Failed to delete WebAuthn credential',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'send_password_reset': {
					const result = await usersApi.sendPasswordReset(config, session, userId);
					return redirectWithFlash(c, redirectUrl, {
						message: result.ok ? 'Password reset email sent successfully' : 'Failed to send password reset email',
						type: result.ok ? 'success' : 'error',
					});
				}

				case 'message_shred': {
					const csvData = getOptionalString(formData, 'csv_data') ?? '';
					const parsedEntries = parseCsvEntries(csvData);

					if (!parsedEntries.ok) {
						return redirectWithFlash(c, redirectUrl, {
							message: parsedEntries.error,
							type: 'error',
						});
					}

					if (parsedEntries.entries.length === 0) {
						return redirectWithFlash(c, redirectUrl, {
							message: 'CSV did not contain any valid channel_id,message_id pairs.',
							type: 'error',
						});
					}

					const result = await messagesApi.queueMessageShred(config, session, userId, parsedEntries.entries);
					if (!result.ok) {
						return redirectWithFlash(c, redirectUrl, {
							message: 'Failed to queue message shred job',
							type: 'error',
						});
					}

					const location = `${redirectUrl}&message_shred_job_id=${result.data.job_id}`;
					return redirectWithFlash(c, location, {
						message: 'Message shred job queued',
						type: 'success',
					});
				}

				default:
					return redirectWithFlash(c, redirectUrl, {
						message: `Unknown action: ${action}`,
						type: 'error',
					});
			}
		} catch (e) {
			return redirectWithFlash(c, redirectUrl, {
				message: `Error processing action: ${(e as Error).message}`,
				type: 'error',
			});
		}
	});

	return router;
}
