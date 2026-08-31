// SPDX-License-Identifier: AGPL-3.0-or-later

import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import {describe, expect, it} from 'vitest';
import {svcErrorReplyReason, throwForSvcErrorReply} from '../SvcErrorReply';

describe('SvcErrorReply', () => {
	it('recognises the shard overload reply', () => {
		expect(svcErrorReplyReason({error: 'overloaded'})).toBe('overloaded');
	});

	it('maps an overload reply to a retryable service unavailable error', () => {
		try {
			throwForSvcErrorReply('users-service', {error: 'overloaded'});
			expect.unreachable('should have thrown');
		} catch (error) {
			expect(error).toBeInstanceOf(ServiceUnavailableError);
			expect((error as ServiceUnavailableError).status).toBe(503);
			expect((error as ServiceUnavailableError).headers?.['Retry-After']).toBe('1');
		}
	});

	it('maps any other structured error reply to service unavailable', () => {
		expect(() => throwForSvcErrorReply('users-service', {error: 'shard_unavailable'})).toThrow(ServiceUnavailableError);
	});

	it('ignores a reply that is not a structured error', () => {
		expect(svcErrorReplyReason({user: {id: '1'}})).toBeNull();
		expect(svcErrorReplyReason(null)).toBeNull();
		expect(svcErrorReplyReason('overloaded')).toBeNull();
		expect(svcErrorReplyReason({error: ''})).toBeNull();
		expect(() => throwForSvcErrorReply('users-service', {user: {id: '1'}})).not.toThrow();
	});
});
