// SPDX-License-Identifier: AGPL-3.0-or-later

import {FLUXER_EPOCH as FLUXER_EPOCH_NUMBER} from '@fluxer/constants/src/Core';
import {
	createSnowflake,
	createSnowflakeFromTimestamp,
	FLUXER_EPOCH,
	generateSnowflake,
	isValidSnowflake,
	parseSnowflake,
	SnowflakeGenerator,
	snowflakeToDate,
} from '@fluxer/snowflake/src/Snowflake';
import {describe, expect, it} from 'vitest';

const WORKER_ID_BITS = 10n;
const SEQUENCE_BITS = 12n;
const TIMESTAMP_SHIFT = SEQUENCE_BITS + WORKER_ID_BITS;
const WORKER_ID_SHIFT = SEQUENCE_BITS;

describe('FLUXER_EPOCH', () => {
	it('should match the epoch constant from @fluxer/constants', () => {
		expect(FLUXER_EPOCH).toBe(BigInt(FLUXER_EPOCH_NUMBER));
	});
	it('should equal January 1, 2015 00:00:00 UTC', () => {
		const expectedDate = new Date('2015-01-01T00:00:00.000Z');
		expect(Number(FLUXER_EPOCH)).toBe(expectedDate.getTime());
	});
});

describe('SnowflakeGenerator', () => {
	describe('generate', () => {
		it('should generate unique snowflakes', () => {
			const generator = new SnowflakeGenerator(1);
			const snowflakes = new Set<bigint>();
			for (let i = 0; i < 1000; i++) {
				snowflakes.add(generator.generate());
			}
			expect(snowflakes.size).toBe(1000);
		});
		it('should generate snowflakes with increasing values', () => {
			const generator = new SnowflakeGenerator(1);
			let previous = generator.generate();
			for (let i = 0; i < 100; i++) {
				const current = generator.generate();
				expect(current).toBeGreaterThan(previous);
				previous = current;
			}
		});
		it('should generate snowflakes with correct structure', () => {
			const generator = new SnowflakeGenerator(5);
			const snowflake = generator.generate();
			expect(typeof snowflake).toBe('bigint');
			expect(snowflake).toBeGreaterThan(0n);
		});
		it('should embed worker ID in generated snowflakes', () => {
			const workerId = 512;
			const generator = new SnowflakeGenerator(workerId);
			for (let i = 0; i < 10; i++) {
				const snowflake = generator.generate();
				const parsed = parseSnowflake(snowflake);
				expect(parsed.workerId).toBe(workerId);
			}
		});
		it('should increment sequence within same millisecond', () => {
			const generator = new SnowflakeGenerator(1);
			const snowflakes: Array<bigint> = [];
			for (let i = 0; i < 100; i++) {
				snowflakes.push(generator.generate());
			}
			const firstTimestamp = parseSnowflake(snowflakes[0]).timestamp.getTime();
			const sameMillisecond = snowflakes.filter((s) => parseSnowflake(s).timestamp.getTime() === firstTimestamp);
			if (sameMillisecond.length > 1) {
				const sameMillisecondSequences = sameMillisecond.map((s) => parseSnowflake(s).sequence);
				for (let i = 1; i < sameMillisecondSequences.length; i++) {
					expect(sameMillisecondSequences[i]).toBeGreaterThan(sameMillisecondSequences[i - 1]);
				}
			}
		});
		it('should reset sequence when timestamp changes', () => {
			const generator = new SnowflakeGenerator(1);
			const snowflake1 = generator.generate();
			const parsed1 = parseSnowflake(snowflake1);
			let snowflake2 = generator.generate();
			let parsed2 = parseSnowflake(snowflake2);
			let attempts = 0;
			while (parsed1.timestamp.getTime() === parsed2.timestamp.getTime() && attempts < 10000) {
				snowflake2 = generator.generate();
				parsed2 = parseSnowflake(snowflake2);
				attempts++;
			}
			if (parsed1.timestamp.getTime() !== parsed2.timestamp.getTime()) {
				expect(parsed2.sequence).toBe(0);
			}
		});
	});
	describe('sequence overflow handling', () => {
		it('should handle rapid generation without duplicates', () => {
			const generator = new SnowflakeGenerator(1);
			const snowflakes = new Set<bigint>();
			const count = 5000;
			for (let i = 0; i < count; i++) {
				snowflakes.add(generator.generate());
			}
			expect(snowflakes.size).toBe(count);
		});
		it('should remain monotonic when the clock moves backwards', () => {
			const baseTime = Number(FLUXER_EPOCH) + 1000;
			const times = [baseTime + 2, baseTime + 1, baseTime + 3];
			let index = 0;
			const generator = new SnowflakeGenerator({
				workerId: 1,
				now: () => {
					const nextIndex = Math.min(index, times.length - 1);
					index += 1;
					return times[nextIndex];
				},
			});
			const first = generator.generate();
			const second = generator.generate();
			expect(second).toBeGreaterThan(first);
		});
	});
});

describe('createSnowflake', () => {
	it('should create a snowflake with explicit worker and sequence', () => {
		const timestamp = Number(FLUXER_EPOCH) + 1000000;
		const snowflake = createSnowflake({
			timestamp,
			workerId: 3,
			sequence: 77,
		});
		const parsed = parseSnowflake(snowflake);
		expect(parsed.timestamp.getTime()).toBe(timestamp);
		expect(parsed.workerId).toBe(3);
		expect(parsed.sequence).toBe(77);
	});
	it('should throw error for out-of-range sequence', () => {
		const timestamp = Number(FLUXER_EPOCH) + 1000000;
		expect(() =>
			createSnowflake({
				timestamp,
				sequence: 4096,
			}),
		).toThrow('Sequence must be between 0 and 4095');
	});
});

describe('createSnowflakeFromTimestamp', () => {
	it('should create a snowflake from a numeric timestamp', () => {
		const timestamp = Number(FLUXER_EPOCH) + 1000000;
		const snowflake = createSnowflakeFromTimestamp(timestamp);
		const date = snowflakeToDate(snowflake);
		expect(date.getTime()).toBe(timestamp);
	});
	it('should create a snowflake from a bigint timestamp', () => {
		const timestamp = FLUXER_EPOCH + 1000000n;
		const snowflake = createSnowflakeFromTimestamp(timestamp);
		const date = snowflakeToDate(snowflake);
		expect(date.getTime()).toBe(Number(timestamp));
	});
	it('should create a snowflake with default worker ID of 0', () => {
		const timestamp = Number(FLUXER_EPOCH) + 1000000;
		const snowflake = createSnowflakeFromTimestamp(timestamp);
		const parsed = parseSnowflake(snowflake);
		expect(parsed.workerId).toBe(0);
	});
	it('should create a snowflake with specified worker ID', () => {
		const timestamp = Number(FLUXER_EPOCH) + 1000000;
		const snowflake = createSnowflakeFromTimestamp(timestamp, 100);
		const parsed = parseSnowflake(snowflake);
		expect(parsed.workerId).toBe(100);
	});
	it('should create a snowflake with sequence of 0', () => {
		const timestamp = Number(FLUXER_EPOCH) + 1000000;
		const snowflake = createSnowflakeFromTimestamp(timestamp);
		const parsed = parseSnowflake(snowflake);
		expect(parsed.sequence).toBe(0);
	});
	it('should throw error for timestamp before epoch', () => {
		const timestamp = Number(FLUXER_EPOCH) - 1;
		expect(() => createSnowflakeFromTimestamp(timestamp)).toThrow('Timestamp must be on or after the Fluxer epoch');
	});
	it('should accept timestamp exactly at epoch', () => {
		const timestamp = Number(FLUXER_EPOCH);
		const snowflake = createSnowflakeFromTimestamp(timestamp);
		const date = snowflakeToDate(snowflake);
		expect(date.getTime()).toBe(timestamp);
	});
	it('should throw error for invalid worker ID', () => {
		const timestamp = Number(FLUXER_EPOCH) + 1000000;
		expect(() => createSnowflakeFromTimestamp(timestamp, -1)).toThrow('Worker ID must be between 0 and 1023');
		expect(() => createSnowflakeFromTimestamp(timestamp, 1024)).toThrow('Worker ID must be between 0 and 1023');
	});
	it('should create different snowflakes for different worker IDs at same timestamp', () => {
		const timestamp = Number(FLUXER_EPOCH) + 1000000;
		const snowflake1 = createSnowflakeFromTimestamp(timestamp, 0);
		const snowflake2 = createSnowflakeFromTimestamp(timestamp, 1);
		expect(snowflake1).not.toBe(snowflake2);
	});
});

describe('snowflakeToDate', () => {
	it('should extract date from a generated snowflake', () => {
		const before = Date.now();
		const snowflake = generateSnowflake();
		const after = Date.now();
		const date = snowflakeToDate(snowflake);
		expect(date.getTime()).toBeGreaterThanOrEqual(before);
		expect(date.getTime()).toBeLessThanOrEqual(after);
	});
	it('should extract correct date from a snowflake created from timestamp', () => {
		const expectedTimestamp = Number(FLUXER_EPOCH) + 86400000;
		const snowflake = createSnowflakeFromTimestamp(expectedTimestamp);
		const date = snowflakeToDate(snowflake);
		expect(date.getTime()).toBe(expectedTimestamp);
	});
	it('should handle snowflake at epoch', () => {
		const snowflake = 0n;
		const date = snowflakeToDate(snowflake);
		expect(date.getTime()).toBe(Number(FLUXER_EPOCH));
	});
	it('should handle large snowflake values', () => {
		const futureTimestamp = Number(FLUXER_EPOCH) + 10 * 365 * 24 * 60 * 60 * 1000;
		const snowflake = createSnowflakeFromTimestamp(futureTimestamp);
		const date = snowflakeToDate(snowflake);
		expect(date.getTime()).toBe(futureTimestamp);
	});
});

describe('parseSnowflake', () => {
	it('should parse all components of a snowflake', () => {
		const workerId = 42;
		const generator = new SnowflakeGenerator(workerId);
		const snowflake = generator.generate();
		const parsed = parseSnowflake(snowflake);
		expect(parsed).toHaveProperty('timestamp');
		expect(parsed).toHaveProperty('workerId');
		expect(parsed).toHaveProperty('sequence');
		expect(parsed.timestamp).toBeInstanceOf(Date);
		expect(typeof parsed.workerId).toBe('number');
		expect(typeof parsed.sequence).toBe('number');
	});
	it('should extract correct worker ID', () => {
		const workerId = 777;
		const generator = new SnowflakeGenerator(workerId);
		const snowflake = generator.generate();
		const parsed = parseSnowflake(snowflake);
		expect(parsed.workerId).toBe(workerId);
	});
	it('should extract correct timestamp', () => {
		const before = Date.now();
		const generator = new SnowflakeGenerator(1);
		const snowflake = generator.generate();
		const after = Date.now();
		const parsed = parseSnowflake(snowflake);
		expect(parsed.timestamp.getTime()).toBeGreaterThanOrEqual(before);
		expect(parsed.timestamp.getTime()).toBeLessThanOrEqual(after);
	});
	it('should extract sequence starting from 0', () => {
		const timestamp = Number(FLUXER_EPOCH) + 1000000;
		const snowflake = createSnowflakeFromTimestamp(timestamp);
		const parsed = parseSnowflake(snowflake);
		expect(parsed.sequence).toBe(0);
	});
	it('should parse snowflake with maximum worker ID', () => {
		const generator = new SnowflakeGenerator(1023);
		const snowflake = generator.generate();
		const parsed = parseSnowflake(snowflake);
		expect(parsed.workerId).toBe(1023);
	});
	it('should correctly parse a manually constructed snowflake', () => {
		const relativeTimestamp = 1000000n;
		const workerId = 500n;
		const sequence = 100n;
		const snowflake = (relativeTimestamp << TIMESTAMP_SHIFT) | (workerId << WORKER_ID_SHIFT) | sequence;
		const parsed = parseSnowflake(snowflake);
		expect(parsed.timestamp.getTime()).toBe(Number(FLUXER_EPOCH) + Number(relativeTimestamp));
		expect(parsed.workerId).toBe(Number(workerId));
		expect(parsed.sequence).toBe(Number(sequence));
	});
});

describe('isValidSnowflake', () => {
	describe('valid snowflakes', () => {
		it('should return true for a generated snowflake', () => {
			const snowflake = generateSnowflake();
			expect(isValidSnowflake(snowflake)).toBe(true);
		});
		it('should return true for a snowflake created from timestamp', () => {
			const snowflake = createSnowflakeFromTimestamp(Date.now());
			expect(isValidSnowflake(snowflake)).toBe(true);
		});
		it('should return true for snowflake at epoch (0n)', () => {
			expect(isValidSnowflake(0n)).toBe(true);
		});
		it('should return true for snowflake with maximum worker ID', () => {
			const generator = new SnowflakeGenerator(1023);
			const snowflake = generator.generate();
			expect(isValidSnowflake(snowflake)).toBe(true);
		});
	});
	describe('invalid snowflakes', () => {
		it('should return false for non-bigint values', () => {
			expect(isValidSnowflake(123)).toBe(false);
			expect(isValidSnowflake('123456789')).toBe(false);
			expect(isValidSnowflake(null)).toBe(false);
			expect(isValidSnowflake(undefined)).toBe(false);
			expect(isValidSnowflake({})).toBe(false);
			expect(isValidSnowflake([])).toBe(false);
		});
		it('should return false for negative bigint', () => {
			expect(isValidSnowflake(-1n)).toBe(false);
			expect(isValidSnowflake(-1000000n)).toBe(false);
		});
		it('should return false for snowflake with timestamp before epoch', () => {
			const invalidSnowflake = -1n << TIMESTAMP_SHIFT;
			expect(isValidSnowflake(invalidSnowflake)).toBe(false);
		});
		it('should return false for snowflake with timestamp too far in the future', () => {
			const farFutureTimestamp = BigInt(Date.now() + 86400000 + 1000) - FLUXER_EPOCH;
			const invalidSnowflake = farFutureTimestamp << TIMESTAMP_SHIFT;
			expect(isValidSnowflake(invalidSnowflake)).toBe(false);
		});
		it('should return true for snowflake with timestamp within 24 hours in the future', () => {
			const nearFutureTimestamp = BigInt(Date.now() + 3600000) - FLUXER_EPOCH;
			const validSnowflake = nearFutureTimestamp << TIMESTAMP_SHIFT;
			expect(isValidSnowflake(validSnowflake)).toBe(true);
		});
	});
});
