// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ITraktOAuthService} from '../../trakt/ITraktOAuthService';
import {Logger} from '../../Logger';
import {TraktOAuthNotEnabledError} from '../errors/TraktOAuthNotEnabledError';
import type {ConnectionVerificationParams, IConnectionVerifier} from './IConnectionVerifier';

export class TraktOAuthVerifier implements IConnectionVerifier {
	constructor(private readonly oauthService: ITraktOAuthService) {}

	async verify(params: ConnectionVerificationParams): Promise<boolean> {
		try {
			const result = await this.oauthService.restoreAndVerify(params.identifier);
			return result !== null;
		} catch (error) {
			if (error instanceof TraktOAuthNotEnabledError) {
				throw error;
			}
			Logger.error(
				{
					identifier: params.identifier,
					error: error instanceof Error ? error.message : String(error),
				},
				'Failed to verify Trakt connection',
			);
			return false;
		}
	}
}
