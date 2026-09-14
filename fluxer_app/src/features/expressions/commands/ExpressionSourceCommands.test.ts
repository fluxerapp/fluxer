// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterEach, describe, expect, it, vi} from 'vitest';

const {getMock} = vi.hoisted(() => ({getMock: vi.fn()}));

vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {get: getMock},
}));
vi.mock('@app/features/platform/utils/ResponseInspection', () => ({
	failureCode: (error: unknown) => (error as {code?: string}).code,
}));

const {fetchExpressionSource} = await import('@app/features/expressions/commands/ExpressionSourceCommands');

const SOURCE_BODY = {id: '20', name: 'Blob Club', icon: 'hash', features: ['DISCOVERABLE']};

describe('fetchExpressionSource', () => {
	afterEach(() => {
		getMock.mockReset();
	});

	it.each([
		['emoji', '/emojis/10/source'],
		['sticker', '/stickers/10/source'],
	] as const)('returns the %s source community from its endpoint', async (kind, path) => {
		getMock.mockResolvedValue({body: SOURCE_BODY});
		await expect(fetchExpressionSource(kind, '10')).resolves.toEqual({available: true, guild: SOURCE_BODY});
		expect(getMock).toHaveBeenCalledWith(path);
	});

	it('defaults a missing icon to null', async () => {
		getMock.mockResolvedValue({body: {id: '20', name: 'Blob Club', features: []}});
		await expect(fetchExpressionSource('emoji', '10')).resolves.toEqual({
			available: true,
			guild: {id: '20', name: 'Blob Club', icon: null, features: []},
		});
	});

	it('reports an unknown guild as unavailable instead of failing', async () => {
		getMock.mockRejectedValue({code: APIErrorCodes.UNKNOWN_GUILD});
		await expect(fetchExpressionSource('sticker', '10')).resolves.toEqual({available: false});
	});

	it('rethrows any other failure', async () => {
		const failure = new Error('gateway timeout');
		getMock.mockRejectedValue(failure);
		await expect(fetchExpressionSource('emoji', '10')).rejects.toBe(failure);
	});
});
