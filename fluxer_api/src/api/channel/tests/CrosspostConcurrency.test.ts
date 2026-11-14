// SPDX-License-Identifier: AGPL-3.0-or-later

import {pinMessage} from '@app/api/channel/tests/ChannelTestUtils';
import {
	type AnnouncementWorld,
	createAnnouncementWorld,
	postMessage,
	publish,
	RecordingWorkerService,
	readMessageRow,
} from '@app/api/channel/tests/CrosspostTestUtils';
import {setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

describe('Publishing races with other message writers', () => {
	let harness: ApiTestHarness;
	let world: AnnouncementWorld;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		setInjectedWorkerService(new RecordingWorkerService());
		world = await createAnnouncementWorld(harness);
	});

	afterEach(() => {
		setInjectedWorkerService(new NoopWorkerService());
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	test('a pin concurrent with a publish keeps both', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Pin and publish');
		await Promise.all([
			pinMessage(harness, world.owner.token, world.announcement.id, message.id),
			publish(harness, world.author.token, world.announcement.id, message.id),
		]);
		const row = await readMessageRow(world.announcement.id, message.id);
		expect(row?.flags).toBe(MessageFlags.CROSSPOSTED);
		expect(row?.pinnedTimestamp).not.toBeNull();
	});
});
