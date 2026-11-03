// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import Users from '@app/features/user/state/Users';
import type {
	ClaimAccountResponse,
	DateOfBirthUpdateResponse,
	EmailVerificationCodeResponse,
	GuildEntryResult,
	SudoEmailCodeResponse,
} from '@fluxer/schema/src/domains/user/AccountSchemas';
import type {UserPrivateResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

export async function setDateOfBirth(dateOfBirth: string): Promise<GuildEntryResult | null> {
	const response = await http.put<DateOfBirthUpdateResponse>(Endpoints.USER_DATE_OF_BIRTH, {
		body: {date_of_birth: dateOfBirth},
	});
	Users.handleUserUpdate(response.body.user);
	return response.body.entry;
}

export async function claimAccount(email: string, password: string | null): Promise<ClaimAccountResponse> {
	const response = await http.post<ClaimAccountResponse>(Endpoints.USER_CLAIM, {
		body: password === null || password === '' ? {email} : {email, password},
	});
	Users.handleUserUpdate(response.body.user);
	return response.body;
}

export async function sendEmailVerificationCode(): Promise<EmailVerificationCodeResponse> {
	const response = await http.post<EmailVerificationCodeResponse>(Endpoints.USER_EMAIL_VERIFICATION_SEND, {
		body: {},
	});
	return response.body;
}

export async function confirmEmailVerification(code: string): Promise<void> {
	const response = await http.post<UserPrivateResponse>(Endpoints.USER_EMAIL_VERIFICATION_CONFIRM, {
		body: {code},
	});
	Users.handleUserUpdate(response.body);
}

export async function sendSudoEmailCode(): Promise<SudoEmailCodeResponse> {
	const response = await http.post<SudoEmailCodeResponse>(Endpoints.USER_SUDO_EMAIL_CODE, {body: {}});
	return response.body;
}
