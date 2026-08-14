// SPDX-License-Identifier: AGPL-3.0-or-later

import {type APIErrorCode, APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ForbiddenError} from '@fluxer/errors/src/domains/core/ForbiddenError';

export class EmailVerificationRequiredError extends ForbiddenError {
	constructor(code: APIErrorCode = APIErrorCodes.EMAIL_VERIFICATION_REQUIRED) {
		super({
			code,
		});
	}
}

export class DirectMessageEmailVerificationRequiredError extends EmailVerificationRequiredError {
	constructor() {
		super(APIErrorCodes.DIRECT_MESSAGE_EMAIL_VERIFICATION_REQUIRED);
	}
}

export class FriendRequestEmailVerificationRequiredError extends EmailVerificationRequiredError {
	constructor() {
		super(APIErrorCodes.FRIEND_REQUEST_EMAIL_VERIFICATION_REQUIRED);
	}
}

export class GuildCreationEmailVerificationRequiredError extends EmailVerificationRequiredError {
	constructor() {
		super(APIErrorCodes.GUILD_CREATION_EMAIL_VERIFICATION_REQUIRED);
	}
}

export class GuildEmailVerificationRequiredError extends EmailVerificationRequiredError {
	constructor() {
		super(APIErrorCodes.GUILD_EMAIL_VERIFICATION_REQUIRED);
	}
}

export class MfaEmailVerificationRequiredError extends EmailVerificationRequiredError {
	constructor() {
		super(APIErrorCodes.MFA_EMAIL_VERIFICATION_REQUIRED);
	}
}

export class ProfileEmailVerificationRequiredError extends EmailVerificationRequiredError {
	constructor() {
		super(APIErrorCodes.PROFILE_EMAIL_VERIFICATION_REQUIRED);
	}
}

export class PurchaseEmailVerificationRequiredError extends EmailVerificationRequiredError {
	constructor() {
		super(APIErrorCodes.PURCHASE_EMAIL_VERIFICATION_REQUIRED);
	}
}

export class ReactionEmailVerificationRequiredError extends EmailVerificationRequiredError {
	constructor() {
		super(APIErrorCodes.REACTION_EMAIL_VERIFICATION_REQUIRED);
	}
}

export class ReportEmailVerificationRequiredError extends EmailVerificationRequiredError {
	constructor() {
		super(APIErrorCodes.REPORT_EMAIL_VERIFICATION_REQUIRED);
	}
}
