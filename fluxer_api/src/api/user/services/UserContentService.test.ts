// SPDX-License-Identifier: AGPL-3.0-or-later

import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {describe, expect, it} from 'vitest';
import {UserContentServiceTestHooks} from './UserContentService';

const {isUnreachableEntityError} = UserContentServiceTestHooks;

describe('isUnreachableEntityError', () => {
	it('treats a deleted or left community as unreachable rather than fatal', () => {
		expect(isUnreachableEntityError(new UnknownGuildError())).toBe(true);
	});

	it('treats a gone channel and a lost permission as unreachable', () => {
		expect(isUnreachableEntityError(new UnknownChannelError())).toBe(true);
		expect(isUnreachableEntityError(new MissingPermissionsError())).toBe(true);
	});

	it('leaves a deleted message to the delete path instead of marking it unavailable', () => {
		expect(isUnreachableEntityError(new UnknownMessageError())).toBe(false);
	});

	it('still lets unexpected failures surface', () => {
		expect(isUnreachableEntityError(new Error('database is on fire'))).toBe(false);
		expect(isUnreachableEntityError(null)).toBe(false);
	});
});
