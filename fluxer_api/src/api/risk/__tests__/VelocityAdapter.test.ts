// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVelocityAdapter,
	type IRegistrationEventsRepository,
	type RegistrationEventRecord,
} from '@app/api/risk/adapters/VelocityAdapter';
import {describe, expect, it} from 'vitest';

function makeMockRepo(records: ReadonlyArray<RegistrationEventRecord>): IRegistrationEventsRepository {
	return {
		recordEvent: async () => undefined,
		listByIp: async (ip, sinceTime) =>
			records.filter((r) => r.ip === ip && r.createdAt.getTime() >= sinceTime.getTime()),
		listBySubnet: async (subnet, sinceTime) =>
			records.filter(
				(r) =>
					r.ip.startsWith(subnet.replace('/24', '').replace(/0$/, '')) && r.createdAt.getTime() >= sinceTime.getTime(),
			),
		listByPlusAddressBase: async (plusAddressBase, sinceTime) =>
			records.filter((r) => {
				const email = r.email?.toLowerCase() ?? null;
				if (!email) return false;
				const [localPart, domain] = email.split('@');
				if (!localPart || !domain) return false;
				const plusIndex = localPart.indexOf('+');
				if (plusIndex <= 0) return false;
				return (
					`${localPart.slice(0, plusIndex)}@${domain}` === plusAddressBase &&
					r.createdAt.getTime() >= sinceTime.getTime()
				);
			}),
		listByEmailDomain: async (domain, sinceTime) =>
			records.filter((r) => r.emailDomain === domain && r.createdAt.getTime() >= sinceTime.getTime()),
	};
}

describe('VelocityAdapter', () => {
	const NOW = new Date('2026-04-11T12:00:00Z');
	const records: ReadonlyArray<RegistrationEventRecord> = [
		{
			userId: '1',
			email: 'a@b.com',
			emailDomain: 'b.com',
			ip: '1.2.3.4',
			locale: 'en-US',
			createdAt: new Date(NOW.getTime() - 30000),
		},
		{
			userId: '2',
			email: 'c@b.com',
			emailDomain: 'b.com',
			ip: '1.2.3.4',
			locale: 'en-US',
			createdAt: new Date(NOW.getTime() - 60000),
		},
		{
			userId: '3',
			email: 'd@b.com',
			emailDomain: 'b.com',
			ip: '1.2.3.4',
			locale: 'ru-RU',
			createdAt: new Date(NOW.getTime() - 5 * 60000),
		},
		{
			userId: '4',
			email: 'e@x.com',
			emailDomain: 'x.com',
			ip: '5.6.7.8',
			locale: null,
			createdAt: new Date(NOW.getTime() - 60000),
		},
		{
			userId: '5',
			email: 'alice+one@example.com',
			emailDomain: 'example.com',
			ip: '9.9.9.9',
			locale: 'en-US',
			createdAt: new Date(NOW.getTime() - 10000),
		},
		{
			userId: '6',
			email: 'alice+two@example.com',
			emailDomain: 'example.com',
			ip: '9.9.9.10',
			locale: 'en-US',
			createdAt: new Date(NOW.getTime() - 20000),
		},
	];
	it('counts registrations by IP within window', async () => {
		const adapter = createVelocityAdapter({repository: makeMockRepo(records), now: () => NOW});
		const r = await adapter.getRegistrationsByIp({ip: '1.2.3.4', windowHours: 24});
		expect(r.totalRegistrations).toBe(3);
		expect(r.uniqueEmails).toBe(3);
		expect([...r.uniqueLocales].sort()).toEqual(['en-US', 'ru-RU']);
	});
	it('counts registrations by email domain', async () => {
		const adapter = createVelocityAdapter({repository: makeMockRepo(records), now: () => NOW});
		const r = await adapter.getRegistrationsByEmailDomain({domain: 'b.com', windowHours: 168});
		expect(r.totalRegistrations).toBe(3);
	});
	it('returns empty for unknown identifier', async () => {
		const adapter = createVelocityAdapter({repository: makeMockRepo(records), now: () => NOW});
		const r = await adapter.getRegistrationsByIp({ip: '8.8.8.8', windowHours: 24});
		expect(r.totalRegistrations).toBe(0);
	});
	it('lower-cases the email domain key', async () => {
		const adapter = createVelocityAdapter({repository: makeMockRepo(records), now: () => NOW});
		const r = await adapter.getRegistrationsByEmailDomain({domain: 'B.COM', windowHours: 168});
		expect(r.identifier).toBe('b.com');
	});
	it('counts registrations by plus-address base', async () => {
		const adapter = createVelocityAdapter({repository: makeMockRepo(records), now: () => NOW});
		const r = await adapter.getRegistrationsByPlusAddressBase({
			plusAddressBase: 'Alice@Example.com',
			windowHours: 720,
		});
		expect(r.identifier).toBe('alice@example.com');
		expect(r.totalRegistrations).toBe(2);
		expect(r.uniqueEmails).toBe(2);
	});
});
