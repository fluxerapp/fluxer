// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {guildEventDateToIso, snapshotGuildEventDate} from '@app/features/guild/utils/GuildEventDateUtils';
import {it} from 'vitest';

function inTimezone(timezone: string, run: () => void): void {
	const previous = process.env.TZ;
	process.env.TZ = timezone;
	try {
		run();
	} finally {
		if (previous === undefined) delete process.env.TZ;
		else process.env.TZ = previous;
	}
}

it('preserves seconds and milliseconds when only event text changes', () => {
	inTimezone('UTC', () => {
		const original = snapshotGuildEventDate('2026-10-20T18:45:37.123Z');
		assert.equal(original.input, '2026-10-20T18:45');
		assert.equal(guildEventDateToIso(original.input, original), original.iso);
	});
});

it('preserves the later repeated hour in New York', () => {
	inTimezone('America/New_York', () => {
		const original = snapshotGuildEventDate('2026-11-01T06:30:00.000Z');
		assert.equal(original.input, '2026-11-01T01:30');
		assert.equal(guildEventDateToIso(original.input, original), original.iso);
	});
});

it('preserves the later repeated hour in Berlin', () => {
	inTimezone('Europe/Berlin', () => {
		const original = snapshotGuildEventDate('2026-10-25T01:30:00.000Z');
		assert.equal(original.input, '2026-10-25T02:30');
		assert.equal(guildEventDateToIso(original.input, original), original.iso);
	});
});

it('preserves the later repeated half-hour on Lord Howe Island', () => {
	inTimezone('Australia/Lord_Howe', () => {
		const original = snapshotGuildEventDate('2026-04-04T15:15:00.000Z');
		assert.equal(original.input, '2026-04-05T01:45');
		assert.equal(guildEventDateToIso(original.input, original), original.iso);
	});
});

it('converts a deliberate date edit without reusing the old instant', () => {
	inTimezone('America/New_York', () => {
		const original = snapshotGuildEventDate('2026-11-01T06:30:37.123Z');
		assert.equal(guildEventDateToIso('2026-11-01T02:15', original), '2026-11-01T07:15:00.000Z');
	});
});

it('preserves an untouched end independently of an edited start', () => {
	inTimezone('UTC', () => {
		const start = snapshotGuildEventDate('2026-10-20T18:45:37.123Z');
		const end = snapshotGuildEventDate('2026-10-20T20:15:19.456Z');
		assert.equal(guildEventDateToIso('2026-10-20T19:00', start), '2026-10-20T19:00:00.000Z');
		assert.equal(guildEventDateToIso(end.input, end), end.iso);
	});
});

it('keeps new-event and newly-added-end conversion unchanged', () => {
	inTimezone('America/New_York', () => {
		assert.equal(guildEventDateToIso('2026-11-02T09:00'), '2026-11-02T14:00:00.000Z');
		assert.equal(guildEventDateToIso('2026-11-02T09:00', null), '2026-11-02T14:00:00.000Z');
	});
});

it('preserves the captured instant if the device timezone changes while editing', () => {
	inTimezone('America/New_York', () => {
		const original = snapshotGuildEventDate('2026-11-01T06:30:00.000Z');
		process.env.TZ = 'UTC';
		assert.equal(guildEventDateToIso(original.input, original), original.iso);
	});
});

it('preserves an original offset spelling and does not mutate its snapshot', () => {
	inTimezone('Asia/Kathmandu', () => {
		const original = snapshotGuildEventDate('2026-10-20T18:45:37.123+05:45');
		assert.equal(original.input, '2026-10-20T18:45');
		Object.freeze(original);
		assert.equal(guildEventDateToIso(original.input, original), original.iso);
	});
});

it('retains invalid-date errors for actual edits and new events', () => {
	inTimezone('UTC', () => {
		const original = snapshotGuildEventDate('2026-10-20T18:45:37.123Z');
		assert.throws(() => guildEventDateToIso('invalid', original), RangeError);
		assert.throws(() => guildEventDateToIso(''), RangeError);
	});
});
