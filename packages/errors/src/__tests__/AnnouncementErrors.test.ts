// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {AnnouncementChannelRequiredError} from '@fluxer/errors/src/domains/channel/AnnouncementChannelRequiredError';
import {ChannelAlreadyFollowedError} from '@fluxer/errors/src/domains/channel/ChannelAlreadyFollowedError';
import {ChannelHasFollowedChannelsError} from '@fluxer/errors/src/domains/channel/ChannelHasFollowedChannelsError';
import {ChannelTypeConversionNotSupportedError} from '@fluxer/errors/src/domains/channel/ChannelTypeConversionNotSupportedError';
import {FollowTargetContentWarningRequiredError} from '@fluxer/errors/src/domains/channel/FollowTargetContentWarningRequiredError';
import {FollowTargetNotAgeRestrictedError} from '@fluxer/errors/src/domains/channel/FollowTargetNotAgeRestrictedError';
import {InvalidFollowTargetChannelError} from '@fluxer/errors/src/domains/channel/InvalidFollowTargetChannelError';
import {MessageAlreadyCrosspostedError} from '@fluxer/errors/src/domains/channel/MessageAlreadyCrosspostedError';
import {MessageNotCrosspostableError} from '@fluxer/errors/src/domains/channel/MessageNotCrosspostableError';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import {getErrorMessageUnsafe} from '@fluxer/errors/src/i18n/ErrorI18n';
import {describe, expect, it} from 'vitest';

describe.each([
	[APIErrorCodes.ANNOUNCEMENT_CHANNEL_REQUIRED, AnnouncementChannelRequiredError],
	[APIErrorCodes.CHANNEL_ALREADY_FOLLOWED, ChannelAlreadyFollowedError],
	[APIErrorCodes.CHANNEL_HAS_FOLLOWED_CHANNELS, ChannelHasFollowedChannelsError],
	[APIErrorCodes.CHANNEL_TYPE_CONVERSION_NOT_SUPPORTED, ChannelTypeConversionNotSupportedError],
	[APIErrorCodes.FOLLOW_TARGET_CONTENT_WARNING_REQUIRED, FollowTargetContentWarningRequiredError],
	[APIErrorCodes.FOLLOW_TARGET_NOT_AGE_RESTRICTED, FollowTargetNotAgeRestrictedError],
	[APIErrorCodes.INVALID_FOLLOW_TARGET_CHANNEL, InvalidFollowTargetChannelError],
	[APIErrorCodes.MESSAGE_ALREADY_CROSSPOSTED, MessageAlreadyCrosspostedError],
	[APIErrorCodes.MESSAGE_NOT_CROSSPOSTABLE, MessageNotCrosspostableError],
] as const)('%s', (code, ErrorClass) => {
	it('is a 400 with its own code', async () => {
		const error = new ErrorClass();
		expect(error).toBeInstanceOf(BadRequestError);
		expect(error).toMatchObject({status: HttpStatus.BAD_REQUEST, code});
		expect(await error.getResponse().json()).toEqual({code, message: code});
	});

	it('has an English message', () => {
		expect(getErrorMessageUnsafe(code, 'en-US')).not.toBe(code);
	});
});

describe.each([APIErrorCodes.MESSAGE_CROSSPOST_RATE_LIMITED, APIErrorCodes.PUBLISHED_MESSAGE_EDIT_RATE_LIMITED])(
	'%s',
	(code) => {
		it('has an English message', () => {
			expect(getErrorMessageUnsafe(code, 'en-US')).not.toBe(code);
		});
	},
);
