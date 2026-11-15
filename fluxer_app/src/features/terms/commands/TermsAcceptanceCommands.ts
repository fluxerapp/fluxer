// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import Users from '@app/features/user/state/Users';
import type {User} from '@fluxer/schema/src/domains/user/UserResponseSchemas';

const logger = new Logger('TermsAcceptance');

type AcceptedTermsUser = User;

async function submitTermsAcceptance(): Promise<AcceptedTermsUser> {
	const response = await http.post<User>(Endpoints.USER_TERMS_ACCEPTANCE);
	return response.body;
}

function hydrateAcceptedUser(user: AcceptedTermsUser): void {
	Users.handleUserUpdate(user, {clearMissingOptionalFields: true});
}

function rethrowAcceptanceFailure(error: unknown): never {
	logger.error('Failed to accept terms', error);
	throw error;
}

export async function acceptTerms(): Promise<void> {
	try {
		const user = await submitTermsAcceptance();
		hydrateAcceptedUser(user);
		logger.debug('Terms and privacy policy accepted');
	} catch (error) {
		rethrowAcceptanceFailure(error);
	}
}
