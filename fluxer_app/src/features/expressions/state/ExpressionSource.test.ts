// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, describe, expect, it, vi} from 'vitest';

const {fetchMock} = vi.hoisted(() => ({fetchMock: vi.fn()}));

vi.mock('@app/features/expressions/commands/ExpressionSourceCommands', () => ({
	fetchExpressionSource: fetchMock,
}));

const {default: ExpressionSource} = await import('@app/features/expressions/state/ExpressionSource');

const GUILD = {id: '20', name: 'Blob Club', icon: null, features: ['DISCOVERABLE']};

describe('ExpressionSource', () => {
	afterEach(() => {
		fetchMock.mockReset();
	});

	it('starts idle', () => {
		expect(ExpressionSource.getSource('emoji', 'idle')).toEqual({status: 'idle'});
	});

	it('loads and then exposes an available source community', async () => {
		let resolve: (value: unknown) => void = () => undefined;
		fetchMock.mockReturnValue(
			new Promise((next) => {
				resolve = next;
			}),
		);
		const pending = ExpressionSource.fetchSource('emoji', 'available');
		expect(ExpressionSource.getSource('emoji', 'available')).toEqual({status: 'loading'});
		resolve({available: true, guild: GUILD});
		await pending;
		expect(ExpressionSource.getSource('emoji', 'available')).toEqual({status: 'available', guild: GUILD});
	});

	it('marks an unavailable source community', async () => {
		fetchMock.mockResolvedValue({available: false});
		await ExpressionSource.fetchSource('sticker', 'unavailable');
		expect(ExpressionSource.getSource('sticker', 'unavailable')).toEqual({status: 'unavailable'});
	});

	it('treats a failed lookup as unavailable', async () => {
		fetchMock.mockRejectedValue(new Error('gateway timeout'));
		await ExpressionSource.fetchSource('emoji', 'failed');
		expect(ExpressionSource.getSource('emoji', 'failed')).toEqual({status: 'unavailable'});
	});

	it('does not refetch a source community that is loading or resolved', async () => {
		fetchMock.mockResolvedValue({available: true, guild: GUILD});
		await Promise.all([
			ExpressionSource.fetchSource('emoji', 'dedupe'),
			ExpressionSource.fetchSource('emoji', 'dedupe'),
		]);
		await ExpressionSource.fetchSource('emoji', 'dedupe');
		expect(fetchMock).toHaveBeenCalledTimes(1);
	});

	it('keeps emoji and sticker sources with the same id apart', async () => {
		fetchMock.mockResolvedValueOnce({available: true, guild: GUILD}).mockResolvedValueOnce({available: false});
		await ExpressionSource.fetchSource('emoji', 'shared');
		await ExpressionSource.fetchSource('sticker', 'shared');
		expect(ExpressionSource.getSource('emoji', 'shared').status).toBe('available');
		expect(ExpressionSource.getSource('sticker', 'shared').status).toBe('unavailable');
	});
});
