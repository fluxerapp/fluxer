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

import type {ApiError} from '@fluxer/admin/src/api/Errors';
import {Grid} from '@fluxer/admin/src/components/ui/Grid';
import {Input} from '@fluxer/admin/src/components/ui/Input';
import {HStack} from '@fluxer/admin/src/components/ui/Layout/HStack';
import {VStack} from '@fluxer/admin/src/components/ui/Layout/VStack';
import {Heading, Text} from '@fluxer/admin/src/components/ui/Typography';
import type {AdminConfig as Config} from '@fluxer/admin/src/types/Config';
import type {
	ListUserSessionsResponse,
	UserAdminResponse,
	UserSessionResponse,
} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import type {
	WebAuthnCredentialListResponse,
	WebAuthnCredentialResponse,
} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import {Button} from '@fluxer/ui/src/components/Button';
import {Card} from '@fluxer/ui/src/components/Card';
import {CsrfInput} from '@fluxer/ui/src/components/CsrfInput';
import {Checkbox} from '@fluxer/ui/src/components/Form';
import type {FC} from 'hono/jsx';

interface AccountTabProps {
	config: Config;
	user: UserAdminResponse;
	userId: string;
	sessionsResult: {ok: true; data: ListUserSessionsResponse} | {ok: false; error: ApiError} | null;
	webAuthnCredentials: WebAuthnCredentialListResponse | null;
	csrfToken: string;
}

export function AccountTab({
	config: _config,
	user,
	userId: _userId,
	sessionsResult,
	webAuthnCredentials,
	csrfToken,
}: AccountTabProps) {
	return (
		<VStack gap={6}>
			<Card padding="md">
				<VStack gap={4}>
					<Heading level={2} size="base">
						Edit Account Information
					</Heading>
					<Grid cols={2} gap="md">
						<VStack gap={2}>
							<form
								method="post"
								action="?action=change_username&tab=account"
								onsubmit="return confirm('Are you sure you want to change this user\\'s username?')"
								class="w-full"
							>
								<CsrfInput token={csrfToken} />
								<VStack gap={2}>
									<Text size="sm" weight="medium" class="text-neutral-700">
										Change Username:
									</Text>
									<Input type="text" name="username" placeholder="New username" required />
									<Input type="number" name="discriminator" placeholder="Discriminator (optional)" min="0" max="9999" />
									<Button type="submit" variant="primary" fullWidth>
										Change Username
									</Button>
								</VStack>
							</form>
						</VStack>

						<VStack gap={2}>
							<form
								method="post"
								action="?action=change_email&tab=account"
								onsubmit="return confirm('Are you sure you want to change this user\\'s email address?')"
								class="w-full"
							>
								<CsrfInput token={csrfToken} />
								<VStack gap={2}>
									<Text size="sm" weight="medium" class="text-neutral-700">
										Change Email:
									</Text>
									<Input type="email" name="email" placeholder="New email address" required />
									<Button type="submit" variant="primary" fullWidth>
										Change Email
									</Button>
								</VStack>
							</form>
						</VStack>

						<VStack gap={2}>
							<form
								method="post"
								action="?action=change_dob&tab=account"
								onsubmit="return confirm('Are you sure you want to change this user\\'s date of birth?')"
								class="w-full"
							>
								<CsrfInput token={csrfToken} />
								<VStack gap={2}>
									<Text size="sm" weight="medium" class="text-neutral-700">
										Change Date of Birth:
									</Text>
									<Input type="date" name="date_of_birth" value={user.date_of_birth ?? ''} required />
									<Button type="submit" variant="primary" fullWidth>
										Change Date of Birth
									</Button>
								</VStack>
							</form>
						</VStack>
					</Grid>
				</VStack>
			</Card>

			{sessionsResult?.ok && (
				<Card padding="md">
					<VStack gap={4}>
						<Heading level={3} size="base">
							Active Sessions
						</Heading>
						{sessionsResult.data.sessions.length === 0 ? (
							<Text size="sm" color="muted">
								No active sessions
							</Text>
						) : (
							<VStack gap={3}>
								{sessionsResult.data.sessions.map((session) => (
									<SessionCard session={session} />
								))}
							</VStack>
						)}
					</VStack>
				</Card>
			)}

			<Card padding="md">
				<VStack gap={4}>
					<Heading level={2} size="base">
						Quick Actions
					</Heading>
					<HStack gap={3} class="flex-wrap">
						{!user.email_verified && (
							<form method="post" action="?action=verify_email&tab=account">
								<CsrfInput token={csrfToken} />
								<Button type="submit" variant="primary">
									Verify Email
								</Button>
							</form>
						)}
						{user.phone && (
							<form
								method="post"
								action="?action=unlink_phone&tab=account"
								onsubmit="return confirm('Are you sure you want to unlink this user\\'s phone number?')"
							>
								<CsrfInput token={csrfToken} />
								<Button type="submit" variant="primary">
									Unlink Phone
								</Button>
							</form>
						)}
						<form method="post" action="?action=send_password_reset&tab=account">
							<CsrfInput token={csrfToken} />
							<Button type="submit" variant="primary">
								Send Password Reset
							</Button>
						</form>
					</HStack>
				</VStack>
			</Card>

			{(user.avatar || user.banner || user.bio || user.pronouns || user.global_name) && (
				<Card padding="md">
					<VStack gap={4}>
						<Heading level={2} size="base">
							Clear Profile Fields
						</Heading>
						<form
							method="post"
							action="?action=clear_fields&tab=account"
							onsubmit="return confirm('Are you sure you want to clear the selected fields for this user?')"
						>
							<CsrfInput token={csrfToken} />
							<VStack gap={4}>
								<div class="grid grid-cols-2 gap-3 md:grid-cols-3">
									{user.avatar && <Checkbox name="fields[]" value="avatar" label="Avatar" />}
									{user.banner && <Checkbox name="fields[]" value="banner" label="Banner" />}
									{user.bio && <Checkbox name="fields[]" value="bio" label="Bio" />}
									{user.pronouns && <Checkbox name="fields[]" value="pronouns" label="Pronouns" />}
									{user.global_name && <Checkbox name="fields[]" value="global_name" label="Display Name" />}
								</div>
								<Button type="submit" variant="primary" fullWidth>
									Clear Selected Fields
								</Button>
							</VStack>
						</form>
					</VStack>
				</Card>
			)}

			<Card padding="md">
				<VStack gap={4}>
					<Heading level={2} size="base">
						User Status
					</Heading>
					<div class="grid grid-cols-1 gap-4 md:grid-cols-2">
						<form
							method="post"
							action={`?action=set_bot_status&status=${user.bot ? 'false' : 'true'}&tab=account`}
							onsubmit={`return confirm('Are you sure you want to ${user.bot ? 'remove' : 'set'} bot status for this user?')`}
						>
							<CsrfInput token={csrfToken} />
							<Button type="submit" variant="primary" fullWidth>
								{user.bot ? 'Remove Bot Status' : 'Set Bot Status'}
							</Button>
						</form>
						<form
							method="post"
							action={`?action=set_system_status&status=${user.system ? 'false' : 'true'}&tab=account`}
							onsubmit={`return confirm('Are you sure you want to ${user.system ? 'remove' : 'set'} system status for this user?')`}
						>
							<CsrfInput token={csrfToken} />
							<Button type="submit" variant="primary" fullWidth>
								{user.system ? 'Remove System Status' : 'Set System Status'}
							</Button>
						</form>
					</div>
				</VStack>
			</Card>

			<Card padding="md">
				<VStack gap={4}>
					<Heading level={2} size="base">
						Security Actions
					</Heading>
					<div class="grid grid-cols-1 gap-3 md:grid-cols-2">
						{user.has_totp && (
							<form
								method="post"
								action="?action=disable_mfa&tab=account"
								onsubmit="return confirm('Are you sure you want to disable MFA/TOTP for this user?')"
							>
								<CsrfInput token={csrfToken} />
								<Button type="submit" variant="primary" fullWidth>
									Disable MFA/TOTP
								</Button>
							</form>
						)}
						<form
							method="post"
							action="?action=terminate_sessions&tab=account"
							onsubmit="return confirm('Are you sure you want to terminate all sessions for this user?')"
						>
							<CsrfInput token={csrfToken} />
							<Button type="submit" variant="primary" fullWidth>
								Terminate All Sessions
							</Button>
						</form>
					</div>
				</VStack>
			</Card>

			{webAuthnCredentials && webAuthnCredentials.length > 0 && (
				<Card padding="md">
					<VStack gap={4}>
						<Heading level={2} size="base">
							WebAuthn Credentials
						</Heading>
						<div class="overflow-x-auto">
							<table class="w-full text-sm">
								<thead>
									<tr class="border-neutral-200 border-b text-left">
										<th class="pb-2 font-medium text-neutral-600">Name</th>
										<th class="pb-2 font-medium text-neutral-600">Created</th>
										<th class="pb-2 font-medium text-neutral-600">Last Used</th>
										<th class="pb-2 font-medium text-neutral-600" />
									</tr>
								</thead>
								<tbody>
									{webAuthnCredentials.map((credential) => (
										<WebAuthnCredentialRow credential={credential} csrfToken={csrfToken} />
									))}
								</tbody>
							</table>
						</div>
					</VStack>
				</Card>
			)}
		</VStack>
	);
}

const WebAuthnCredentialRow: FC<{credential: WebAuthnCredentialResponse; csrfToken: string}> = ({
	credential,
	csrfToken,
}) => {
	function formatTimestamp(value: string): string {
		const [datePart, timePartRaw] = value.split('T');
		if (!datePart || !timePartRaw) return value;
		const timePart = timePartRaw.replace('Z', '').split('.')[0] ?? timePartRaw;
		return `${datePart} ${timePart}`;
	}

	return (
		<tr class="border-neutral-100 border-b">
			<td class="py-2 pr-4">
				<Text size="sm" class="text-neutral-900">
					{credential.name}
				</Text>
			</td>
			<td class="py-2 pr-4">
				<Text size="sm" class="text-neutral-900">
					{formatTimestamp(credential.created_at)}
				</Text>
			</td>
			<td class="py-2 pr-4">
				<Text size="sm" class="text-neutral-900">
					{credential.last_used_at ? formatTimestamp(credential.last_used_at) : 'Never'}
				</Text>
			</td>
			<td class="py-2">
				<form
					method="post"
					action="?action=delete_webauthn_credential&tab=account"
					onsubmit={`return confirm('Are you sure you want to delete the WebAuthn credential "${credential.name}"?')`}
				>
					<CsrfInput token={csrfToken} />
					<input type="hidden" name="credential_id" value={credential.id} />
					<Button type="submit" variant="primary" size="small">
						Delete
					</Button>
				</form>
			</td>
		</tr>
	);
};

const SessionCard: FC<{session: UserSessionResponse}> = ({session}) => {
	function formatSessionTimestamp(value: string): string {
		const [datePart, timePartRaw] = value.split('T');
		if (!datePart || !timePartRaw) return value;
		const timePart = timePartRaw.replace('Z', '').split('.')[0] ?? timePartRaw;
		return `${datePart} ${timePart}`;
	}

	return (
		<div class="rounded-lg border border-neutral-200 bg-neutral-50 p-4">
			<div class="grid grid-cols-2 gap-x-6 gap-y-3 text-sm md:grid-cols-3">
				<div>
					<Text size="sm" weight="medium" color="muted">
						Session ID
					</Text>
					<Text size="xs" class="text-neutral-900">
						<span class="font-mono">{session.session_id_hash}</span>
					</Text>
				</div>
				<div>
					<Text size="sm" weight="medium" color="muted">
						Created
					</Text>
					<Text size="sm" class="text-neutral-900">
						{formatSessionTimestamp(session.created_at)}
					</Text>
				</div>
				<div>
					<Text size="sm" weight="medium" color="muted">
						Last Used
					</Text>
					<Text size="sm" class="text-neutral-900">
						{formatSessionTimestamp(session.approx_last_used_at)}
					</Text>
				</div>
				<div class="md:col-span-3">
					<Text size="sm" weight="medium" color="muted">
						IP Address
					</Text>
					<Text size="sm" class="text-neutral-900">
						<span class="font-mono">{session.client_ip}</span>
						{session.client_ip_reverse && <span class="ml-2 text-neutral-600">({session.client_ip_reverse})</span>}
					</Text>
				</div>
				{session.client_platform && (
					<div>
						<Text size="sm" weight="medium" color="muted">
							Platform
						</Text>
						<Text size="sm" class="text-neutral-900">
							{session.client_platform}
						</Text>
					</div>
				)}
				{session.client_os && (
					<div>
						<Text size="sm" weight="medium" color="muted">
							OS
						</Text>
						<Text size="sm" class="text-neutral-900">
							{session.client_os}
						</Text>
					</div>
				)}
				{session.client_location && (
					<div>
						<Text size="sm" weight="medium" color="muted">
							Location
						</Text>
						<Text size="sm" class="text-neutral-900">
							{session.client_location}
						</Text>
					</div>
				)}
			</div>
		</div>
	);
};
