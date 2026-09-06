// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {BadGatewayError} from '@fluxer/errors/src/domains/core/BadGatewayError';
import {BadRequestError} from '@fluxer/errors/src/domains/core/BadRequestError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {afterEach, describe, expect, it} from 'vitest';
import {createGuildID, createUserID} from '../../BrandedTypes';
import {GatewayRpcClient} from '../GatewayRpcClient';
import {GatewayRpcMethodError, GatewayRpcMethodErrorCodes} from '../GatewayRpcError';
import {GatewayService} from '../GatewayService';
import type {IGatewayRpcTransport} from '../IGatewayRpcTransport';

function failingTransport(code: string): IGatewayRpcTransport {
	return {
		async call(): Promise<unknown> {
			throw new GatewayRpcMethodError(code);
		},
		async destroy(): Promise<void> {},
	};
}

function serviceRaising(code: string): GatewayService {
	GatewayRpcClient.createForTests(failingTransport(code));
	return new GatewayService();
}

describe('GatewayService gateway error mapping', () => {
	afterEach(async () => {
		await GatewayRpcClient.resetForTests();
	});

	it('maps guild_not_found from guild.get_user_permissions to UnknownGuildError', async () => {
		const service = serviceRaising(GatewayRpcMethodErrorCodes.GUILD_NOT_FOUND);
		await expect(
			service.getUserPermissions({guildId: createGuildID(1n), userId: createUserID(2n)}),
		).rejects.toBeInstanceOf(UnknownGuildError);
	});

	it('maps guild_not_found from guild.check_permission to UnknownGuildError', async () => {
		const service = serviceRaising(GatewayRpcMethodErrorCodes.GUILD_NOT_FOUND);
		await expect(
			service.checkPermission({guildId: createGuildID(1n), userId: createUserID(2n), permission: 1n}),
		).rejects.toBeInstanceOf(UnknownGuildError);
	});

	it('maps an unrecognised gateway error from guild.check_permission to BadGatewayError', async () => {
		const service = serviceRaising('permission_check_error');
		await expect(
			service.checkPermission({guildId: createGuildID(1n), userId: createUserID(2n), permission: 1n}),
		).rejects.toBeInstanceOf(BadGatewayError);
	});

	it('maps invalid_params to a 400 instead of an upstream failure', async () => {
		const service = serviceRaising(GatewayRpcMethodErrorCodes.INVALID_PARAMS);
		const error = await service
			.checkPermission({guildId: createGuildID(1n), userId: createUserID(2n), permission: 1n})
			.catch((raised: unknown) => raised);
		expect(error).toBeInstanceOf(BadRequestError);
		expect((error as BadRequestError).status).toBe(400);
		expect((error as BadRequestError).code).toBe(APIErrorCodes.INVALID_FORM_BODY);
	});

	it('maps batch_too_large to a 400 instead of an upstream failure', async () => {
		const service = serviceRaising(GatewayRpcMethodErrorCodes.BATCH_TOO_LARGE);
		const error = await service
			.getUserPermissionsBatch({guildIds: [createGuildID(1n)], userId: createUserID(2n)})
			.catch((raised: unknown) => raised);
		expect(error).toBeInstanceOf(BadRequestError);
		expect((error as BadRequestError).status).toBe(400);
		expect((error as BadRequestError).code).toBe(APIErrorCodes.INVALID_FORM_BODY);
	});
});
