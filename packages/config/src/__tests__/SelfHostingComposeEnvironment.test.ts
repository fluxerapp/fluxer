// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, test} from 'vitest';
import {serviceEnvironment, serviceNames, sharedEnvironment} from './SelfHostingCompose';

describe('the shipped compose stack wires every service it starts', () => {
	test('no service sizes a pool for a connection it cannot make', () => {
		const pooledServices = serviceNames.filter((name) => 'FLUXER_POSTGRES_MAX_CONNECTIONS' in serviceEnvironment(name));
		expect(pooledServices.length).toBeGreaterThan(0);
		for (const name of pooledServices) {
			expect(serviceEnvironment(name), name).toMatchObject({
				FLUXER_DATABASE_BACKEND: 'postgres',
				FLUXER_POSTGRES_HOST: expect.stringMatching(/\S/u),
				FLUXER_POSTGRES_PORT: expect.stringMatching(/\S/u),
				FLUXER_POSTGRES_DATABASE: expect.stringMatching(/\S/u),
				FLUXER_POSTGRES_USERNAME: expect.stringMatching(/\S/u),
				FLUXER_POSTGRES_PASSWORD: expect.stringMatching(/\S/u),
			});
		}
	});

	test('the shared block sets the client-IP trust the merged services read', () => {
		expect(sharedEnvironment).toMatchObject({
			FLUXER_TRUST_CLIENT_IP_HEADER: `\${FLUXER_TRUST_CLIENT_IP_HEADER:-true}`,
			FLUXER_CLIENT_IP_HEADER_NAME: `\${FLUXER_CLIENT_IP_HEADER_NAME:-x-forwarded-for}`,
		});
	});
});
