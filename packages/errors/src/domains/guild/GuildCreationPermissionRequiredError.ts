// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';

export class GuildCreationPermissionRequiredError extends ForbiddenError {
	constructor({instanceEmail}: {instanceEmail?: string | null} = {}) {
		const contact = instanceEmail?.trim() ? instanceEmail.trim() : null;
		super({
			code: APIErrorCodes.GUILD_CREATION_PERMISSION_REQUIRED,
			messageVariables: {
				hasContact: contact === null ? 'no' : 'yes',
				instanceEmail: contact ?? '',
			},
		});
	}
}
