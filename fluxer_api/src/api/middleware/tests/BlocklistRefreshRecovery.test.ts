// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminRepository} from '@app/api/admin/AdminRepository';
import {IP_BAN_REFRESH_CHANNEL} from '@app/api/constants/IpBan';
import {Logger} from '@app/api/Logger';
import {ipBanCache} from '@app/api/middleware/IpBanMiddleware';
import {MockKVProvider} from '@pkgs/kv_client/src/testing/MockKVProvider';
import {afterEach, expect, it, vi} from 'vitest';

afterEach(async () => {
	await ipBanCache.shutdown();
	ipBanCache.setRefreshSubscriber(null);
	ipBanCache.resetCaches();
	vi.restoreAllMocks();
	vi.unstubAllEnvs();
	vi.useRealTimers();
});

it('keeps periodic failures at warn and resets notification escalation after a direct admin refresh', async () => {
	await ipBanCache.shutdown();
	vi.useFakeTimers({toFake: ['setInterval', 'clearInterval']});
	vi.stubEnv('FLUXER_IP_BAN_REFRESH_INTERVAL_MS', '10');
	const load = vi.spyOn(AdminRepository.prototype, 'loadAllBannedIpEntries').mockResolvedValue([]);
	const logger = Logger.child({});
	const warn = vi.spyOn(logger, 'warn').mockImplementation(() => {});
	const error = vi.spyOn(logger, 'error').mockImplementation(() => {});
	const provider = new MockKVProvider();
	ipBanCache.setRefreshSubscriber(provider);
	await ipBanCache.initialize();
	load.mockRejectedValue(new Error('database unavailable'));

	for (let count = 1; count <= 4; count++) {
		await provider.publish(IP_BAN_REFRESH_CHANNEL, 'changed');
		await new Promise<void>((resolve) => setImmediate(resolve));
		expect(warn).toHaveBeenCalledTimes(count);
	}
	await vi.advanceTimersByTimeAsync(10);
	expect(warn).toHaveBeenLastCalledWith({error: 'database unavailable'}, 'Periodic IP ban cache refresh failed');
	expect(error).not.toHaveBeenCalled();

	await provider.publish(IP_BAN_REFRESH_CHANNEL, 'changed');
	await new Promise<void>((resolve) => setImmediate(resolve));
	expect(error).toHaveBeenCalledTimes(1);
	load.mockResolvedValue([]);
	await ipBanCache.refresh();
	load.mockRejectedValue(new Error('database unavailable'));
	await provider.publish(IP_BAN_REFRESH_CHANNEL, 'changed');
	await new Promise<void>((resolve) => setImmediate(resolve));
	expect(warn).toHaveBeenCalledTimes(6);
	expect(error).toHaveBeenCalledTimes(1);
});
