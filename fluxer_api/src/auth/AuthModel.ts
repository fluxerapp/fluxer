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

import {uint8ArrayToBase64} from 'uint8array-extras';
import type {AuthSession} from '~/Models';
import {createStringType, EmailType, GlobalNameType, PasswordType, UsernameType, z} from '~/Schema';
import {getLocationLabelFromIp} from '~/utils/IpUtils';
import {resolveSessionClientInfo} from '~/utils/UserAgentUtils';

export const RegisterRequest = z.object({
	email: EmailType.optional(),
	username: UsernameType.optional(),
	global_name: GlobalNameType.optional(),
	password: PasswordType.optional(),
	beta_code: createStringType(0, 256).nullish(),
	date_of_birth: createStringType(10, 10).refine((value) => /^\d{4}-\d{2}-\d{2}$/.test(value), 'Invalid date format'),
	consent: z.boolean(),
	invite_code: createStringType(0, 256).nullish(),
});

export const UsernameSuggestionsRequest = z.object({
	global_name: GlobalNameType,
});

export type RegisterRequest = z.infer<typeof RegisterRequest>;

export type UsernameSuggestionsRequest = z.infer<typeof UsernameSuggestionsRequest>;

export const LoginRequest = z.object({
	email: EmailType,
	password: PasswordType,
	invite_code: createStringType(0, 256).nullish(),
});

export type LoginRequest = z.infer<typeof LoginRequest>;

export const LogoutAuthSessionsRequest = z.object({
	session_id_hashes: z.array(createStringType()).max(100),
	password: PasswordType.optional(),
});

export const ForgotPasswordRequest = z.object({
	email: EmailType,
});

export type ForgotPasswordRequest = z.infer<typeof ForgotPasswordRequest>;

export const ResetPasswordRequest = z.object({
	token: createStringType(64, 64),
	password: PasswordType,
});

export type ResetPasswordRequest = z.infer<typeof ResetPasswordRequest>;

export const EmailRevertRequest = z.object({
	token: createStringType(64, 64),
	password: PasswordType,
});

export type EmailRevertRequest = z.infer<typeof EmailRevertRequest>;

export const VerifyEmailRequest = z.object({
	token: createStringType(64, 64),
});

export type VerifyEmailRequest = z.infer<typeof VerifyEmailRequest>;

async function resolveAuthSessionLocation(session: AuthSession): Promise<string | null> {
	try {
		return await getLocationLabelFromIp(session.clientIp);
	} catch {
		return null;
	}
}

export const mapAuthSessionsToResponse = async ({
	authSessions,
}: {
	authSessions: Array<AuthSession>;
}): Promise<Array<AuthSessionResponse>> => {
	const sortedSessions = [...authSessions].sort((a, b) => {
		const aTime = a.approximateLastUsedAt?.getTime() || 0;
		const bTime = b.approximateLastUsedAt?.getTime() || 0;
		return bTime - aTime;
	});

	const locationResults = await Promise.allSettled(
		sortedSessions.map((session) => resolveAuthSessionLocation(session)),
	);

	return sortedSessions.map((authSession, index): AuthSessionResponse => {
		const locationResult = locationResults[index];
		const clientLocation = locationResult?.status === 'fulfilled' ? locationResult.value : null;
		const {clientOs, clientPlatform} = resolveSessionClientInfo({
			userAgent: authSession.clientUserAgent,
			isDesktopClient: authSession.clientIsDesktop,
		});

		return {
			id: uint8ArrayToBase64(authSession.sessionIdHash, {urlSafe: true}),
			approx_last_used_at: authSession.approximateLastUsedAt?.toISOString() || null,
			client_os: clientOs,
			client_platform: clientPlatform,
			client_location: clientLocation,
		};
	});
};

export const AuthSessionResponse = z.object({
	id: z.string(),
	approx_last_used_at: z.iso.datetime().nullish(),
	client_os: z.string(),
	client_platform: z.string(),
	client_location: z.string().nullable(),
});

export type AuthSessionResponse = z.infer<typeof AuthSessionResponse>;
