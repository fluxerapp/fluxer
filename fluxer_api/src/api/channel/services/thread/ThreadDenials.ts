// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {ThreadDenial} from '@fluxer/constants/src/ThreadPermissionUtils';
import {ThreadArchivedError} from '@fluxer/errors/src/domains/channel/ThreadArchivedError';
import {ThreadLockedError} from '@fluxer/errors/src/domains/channel/ThreadLockedError';
import {UnknownThreadMemberError} from '@fluxer/errors/src/domains/channel/UnknownThreadMemberError';
import {MissingAccessError} from '@fluxer/errors/src/domains/core/MissingAccessError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {CommunicationDisabledError} from '@fluxer/errors/src/domains/moderation/CommunicationDisabledError';

export function threadDenialError(denial: ThreadDenial): Error {
	switch (denial) {
		case APIErrorCodes.MISSING_ACCESS:
			return new MissingAccessError();
		case APIErrorCodes.MISSING_PERMISSIONS:
			return new MissingPermissionsError();
		case APIErrorCodes.COMMUNICATION_DISABLED:
			return new CommunicationDisabledError();
		case APIErrorCodes.THREAD_ARCHIVED:
			return new ThreadArchivedError();
		case APIErrorCodes.THREAD_LOCKED:
			return new ThreadLockedError();
		case APIErrorCodes.UNKNOWN_THREAD_MEMBER:
			return new UnknownThreadMemberError();
	}
}

export function assertThreadAllowed(denial: ThreadDenial | null): void {
	if (denial !== null) throw threadDenialError(denial);
}
